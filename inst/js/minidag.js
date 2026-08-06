/* Minidag: compact list-shaped workflow editor (blockr.extra).
 *
 * One instance per `.minidag[data-ns]` container. The R side pushes the full
 * board model ('minidag-data') and per-block status badges ('minidag-badge');
 * user gestures go back as event-priority Shiny inputs (link_add, link_rm,
 * block_rm, block_rename, block_select, block_append, block_add, stack_add,
 * stack_rename, stack_rm). The client never mutates the model itself — every
 * edit round-trips through the board and comes back as a data push.
 *
 * Model: blocks [{id, name, category, color, icon, inputs[], variadic}],
 * links [{id, from, to, input}], stacks [{id, name, color, blocks[]}].
 * A named input slot is FULL when a link with that (to, input) exists;
 * variadic blocks accept unlimited links on the '' slot (blockr.core
 * semantics — mirrored here for instant feedback, enforced again in R).
 */
(() => {
  'use strict';

  const LANE_W = 16, ROW_H = 28, GAP = 6, PITCH = ROW_H + GAP;
  const RAIL_L = 10, RAIL_R = 8, DOT_R = 4;
  const LANE_COLORS = ['#9ca3af', '#2563eb', '#0d9488', '#7c3aed', '#b45309', '#be185d'];
  const STATUS_RANK = { failed: 3, waiting: 2, unset: 1 };

  const SVG = 'http://www.w3.org/2000/svg';
  const svgEl = (tag) => document.createElementNS(SVG, tag);

  const CHEV_D = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.4"><path d="m6 9 6 6 6-6"/></svg>';
  const CHEV_R = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.4"><path d="m9 6 6 6-6 6"/></svg>';
  const STACK_ICON = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2"><path d="m12 3 9 5-9 5-9-5 9-5z"/><path d="m3 13 9 5 9-5"/></svg>';
  const SEARCH_ICON = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="11" cy="11" r="7"/><path d="m21 21-4.3-4.3"/></svg>';

  const registry = new Map();

  const getInst = (elId) => {
    let inst = registry.get(elId);
    if (!inst) {
      const el = document.getElementById(elId);
      if (!el) return null;
      inst = createInstance(el);
      registry.set(elId, inst);
    }
    return inst;
  };

  const announceAll = () => {
    document.querySelectorAll('.minidag[data-ns]').forEach((el) => {
      const inst = getInst(el.id);
      if (inst && !inst.announced) {
        inst.announced = true;
        Shiny.setInputValue(inst.ns + 'ready', true);
      }
    });
  };

  if (window.Shiny) {
    $(document).on('shiny:connected', announceAll);
    Shiny.addCustomMessageHandler('minidag-data', (msg) => {
      const inst = getInst(msg.el);
      if (inst) inst.setData(msg);
    });
    Shiny.addCustomMessageHandler('minidag-badge', (msg) => {
      const inst = getInst(msg.el);
      if (inst) inst.setBadge(msg);
    });
  }

  // test/inspection hook
  window._minidag = (elId) => {
    const inst = elId ? registry.get(elId) : registry.values().next().value;
    return inst ? inst.inspect() : null;
  };

  function createInstance(rootEl) {
    const ns = rootEl.dataset.ns;
    const push = (name, payload) =>
      Shiny.setInputValue(ns + name, payload, { priority: 'event' });

    /* ---- skeleton ---- */

    rootEl.innerHTML = '';

    const searchRow = document.createElement('div');
    searchRow.className = 'md-search-row';
    searchRow.innerHTML = '<span class="md-search-icon">' + SEARCH_ICON + '</span>';
    const searchEl = document.createElement('input');
    searchEl.className = 'md-search';
    searchEl.placeholder = 'Search blocks…';
    searchRow.appendChild(searchEl);
    const hitsEl = document.createElement('span');
    hitsEl.className = 'md-hits';
    searchRow.appendChild(hitsEl);
    const addBtn = document.createElement('button');
    addBtn.className = 'md-add';
    addBtn.textContent = '+';
    addBtn.title = 'Add block';
    addBtn.addEventListener('click', () => push('block_add', true));
    searchRow.appendChild(addBtn);
    rootEl.appendChild(searchRow);

    const deckEl = document.createElement('div');
    deckEl.className = 'md-deck';
    rootEl.appendChild(deckEl);

    const barEl = document.createElement('div');
    barEl.className = 'md-actionbar';
    const selcountEl = document.createElement('span');
    barEl.appendChild(selcountEl);
    const mkstackBtn = document.createElement('button');
    mkstackBtn.className = 'md-go';
    mkstackBtn.textContent = 'Stack them';
    barEl.appendChild(mkstackBtn);
    const clearselBtn = document.createElement('button');
    clearselBtn.className = 'md-no';
    clearselBtn.textContent = 'Clear';
    barEl.appendChild(clearselBtn);
    rootEl.appendChild(barEl);

    /* ---- state ---- */

    let blocks = [], links = [], stacks = [];
    const statuses = new Map();   // block id -> {color, label, status} | null
    const collapsed = new Set();  // stack ids, client-side only (deck parity)
    let selection = new Set();
    let lastPos = new Map();
    let closePicker = null, dragging = false, pendingRender = false;

    const blockOf = (id) => blocks.find((b) => b.id === id);
    const stackOf = (id) => stacks.find((s) => s.blocks.includes(id)) || null;
    const parentsOf = (id) => links.filter((l) => l.to === id).map((l) => l.from);
    const childrenOf = (id) => links.filter((l) => l.from === id).map((l) => l.to);

    const upstream = (id) => {
      const seen = new Set(); const q = [id];
      while (q.length) {
        const x = q.shift();
        parentsOf(x).forEach((p) => { if (!seen.has(p)) { seen.add(p); q.push(p); } });
      }
      return seen;
    };
    const downstream = (id) => {
      const seen = new Set(); const q = [id];
      while (q.length) {
        const x = q.shift();
        childrenOf(x).forEach((c) => { if (!seen.has(c)) { seen.add(c); q.push(c); } });
      }
      return seen;
    };

    /* ---- arity ---- */

    const linksInto = (id) => links.filter((l) => l.to === id);

    // free input slots of a block: unoccupied named slots, plus the ''
    // (variadic) slot which never fills up
    const freeSlots = (b) => {
      const occ = new Set(linksInto(b.id).map((l) => l.input));
      const free = (b.inputs || []).filter((s) => !occ.has(s));
      if (b.variadic) free.push('');
      return free;
    };

    /* ---- ordering + lanes: the pure geometry lives in minidag-layout.js ---- */

    // `minidag-layout.js` owns row order and lane assignment so `node --test`
    // can hold it to its invariants (tests/js/). Everything below draws.
    const G = (typeof globalThis !== 'undefined' ? globalThis : window).minidagLayout;

    const model = () => ({ blocks, links, stacks, collapsed, lastPos });

    const displayRows = () => G.displayRows(model());
    const innerOrder = (s) => G.innerOrder(model(), s);
    const railIdOf = (id) => G.railIdOf(model(), id);
    const railModel = (rows) => G.railModel(model(), rows);
    const layout = (entries, rl, rowOf) => G.layout(entries, rl, rowOf);

    const laneX = (l) => RAIL_L + l * LANE_W;
    const dotY = (r) => r * PITCH + ROW_H / 2;

    /* ---- rail id -> concrete endpoints (arity-aware) ---- */

    // drag source: a collapsed stack wires from its last block
    const sinkOf = (railId) => {
      if (!railId.startsWith('stack:')) return railId;
      const s = stacks.find((x) => x.id === railId.slice(6));
      const inner = innerOrder(s);
      return inner[inner.length - 1];
    };

    // drop target: first member (topological) with a free input slot
    const targetBlock = (railId) => {
      if (!railId.startsWith('stack:')) {
        const b = blockOf(railId);
        return b && freeSlots(b).length ? b : null;
      }
      const s = stacks.find((x) => x.id === railId.slice(6));
      for (const id of innerOrder(s)) {
        const b = blockOf(id);
        if (b && freeSlots(b).length) return b;
      }
      return null;
    };

    // 'ok' | 'full' | 'cycle' | 'self' — why a drop on `toRail` would (not) work
    const dropVerdict = (fromRail, toRail) => {
      const from = sinkOf(fromRail);
      const b = toRail.startsWith('stack:')
        ? targetBlock(toRail)
        : blockOf(toRail);
      if (!b) return 'full';
      if (from === b.id) return 'self';
      if (!freeSlots(b).length) return 'full';
      if (upstream(from).has(b.id)) return 'cycle';
      return 'ok';
    };

    const doConnect = (fromRail, toRail, px, py) => {
      const from = sinkOf(fromRail);
      const b = targetBlock(toRail);
      if (!b || dropVerdict(fromRail, toRail) !== 'ok') return false;
      const free = freeSlots(b);
      if (free.length === 1) {
        push('link_add', { from, to: b.id, input: free[0] });
        return true;
      }
      openSlotPicker(px, py, b, free, (slot) =>
        push('link_add', { from, to: b.id, input: slot }));
      return true;
    };

    /* ---- render ---- */

    const editing = () => !!rootEl.querySelector('[contenteditable="true"]');

    const render = () => {
      if (dragging || editing()) { pendingRender = true; return; }
      pendingRender = false;
      if (closePicker) closePicker();
      hideEdgeXNow();
      clearFocus();
      deckEl.innerHTML = '';

      if (!blocks.length) {
        const empty = document.createElement('div');
        empty.className = 'md-empty';
        empty.innerHTML = '<p>No blocks yet.</p>';
        const b = document.createElement('button');
        b.className = 'md-empty-add';
        b.textContent = '+ Add a block';
        b.addEventListener('click', () => push('block_add', true));
        empty.appendChild(b);
        deckEl.appendChild(empty);
        updateBar();
        return;
      }

      const rows = displayRows();
      lastPos = new Map();
      rows.forEach((r, i) => {
        if (r.t === 'node') lastPos.set('n:' + r.node.id, i);
        else lastPos.set('s:' + r.stack.id, i);
      });
      const { entries, rowOf, rl } = railModel(rows);
      const { laneOf, edges, nLanes } = layout(entries, rl, rowOf);
      const railW = RAIL_L + nLanes * LANE_W + RAIL_R;
      const H = rows.length * PITCH;

      const svg = svgEl('svg');
      svg.setAttribute('class', 'md-rail');
      svg.setAttribute('width', railW);
      svg.setAttribute('height', H);

      // Siblings share their producer's bus, so their drawn paths overlap
      // above the first consumer. Hovering has to stay unambiguous: each edge
      // owns only the stretch of bus BELOW the previous consumer -- that band
      // is where its ✕ appears and where the hover highlight fires.
      const hitFrom = new Map();
      const byFrom = new Map();
      edges.forEach((e) => {
        const arr = byFrom.get(e.from) || [];
        arr.push(e);
        byFrom.set(e.from, arr);
      });
      byFrom.forEach((arr, from) => {
        arr.sort((a, b) => (rowOf.get(a.to) ?? 0) - (rowOf.get(b.to) ?? 0));
        arr.forEach((e, i) => hitFrom.set(
          e.from + '>' + e.to,
          i === 0 ? rowOf.get(from) : rowOf.get(arr[i - 1].to)
        ));
      });

      edges.forEach((e) => {
        const rF = rowOf.get(e.from), rT = rowOf.get(e.to);
        const xF = laneX(laneOf.get(e.from)), xT = laneX(laneOf.get(e.to));
        const xE = laneX(e.lane);
        const yF = dotY(rF), yT = dotY(rT);
        let d = 'M' + xF + ',' + yF;
        let y = yF;
        if (xE !== xF) {
          const y2 = yF + PITCH;
          d += ' C' + xF + ',' + (yF + PITCH * 0.8) + ' ' + xE + ',' + (y2 - PITCH * 0.8) + ' ' + xE + ',' + y2;
          y = y2;
        }
        const yIn = xE !== xT ? yT - PITCH : yT;
        if (yIn > y) { d += ' L' + xE + ',' + yIn; y = yIn; }
        if (xE !== xT) {
          d += ' C' + xE + ',' + (y + PITCH * 0.8) + ' ' + xT + ',' + (yT - PITCH * 0.8) + ' ' + xT + ',' + yT;
        }
        const p = svgEl('path');
        p.setAttribute('d', d);
        p.setAttribute('fill', 'none');
        p.setAttribute('stroke', LANE_COLORS[e.lane % LANE_COLORS.length]);
        p.setAttribute('stroke-width', '2');
        p.setAttribute('class', 'md-edge');
        p.dataset.from = e.from;
        p.dataset.to = e.to;
        svg.appendChild(p);
        const rH = hitFrom.get(e.from + '>' + e.to) ?? rF;
        const yH = Math.max(dotY(rH), yF);
        const yIn2 = xE !== xT ? yT - PITCH : yT;
        let dh = 'M' + xE + ',' + Math.min(yH, yIn2);
        if (yIn2 > yH) dh += ' L' + xE + ',' + yIn2;
        if (xE !== xT) {
          dh += ' C' + xE + ',' + (yIn2 + PITCH * 0.8) + ' ' + xT + ',' +
            (yT - PITCH * 0.8) + ' ' + xT + ',' + yT;
        }
        const hit = svgEl('path');
        hit.setAttribute('d', dh);
        hit.setAttribute('fill', 'none');
        hit.setAttribute('stroke', 'transparent');
        hit.setAttribute('stroke-width', '12');
        hit.setAttribute('class', 'md-edge-hit');
        hit.addEventListener('mouseenter', () => showEdgeX(e, p, hit));
        hit.addEventListener('mouseleave', hideEdgeXSoon);
        svg.appendChild(hit);
      });

      entries.forEach((e) => {
        const isStack = e.id.startsWith('stack:');
        const stackCol = isStack
          ? (stacks.find((s) => s.id === e.id.slice(6)) || {}).color
          : null;
        const laneCol = LANE_COLORS[laneOf.get(e.id) % LANE_COLORS.length];
        const baseR = isStack ? DOT_R + 1 : DOT_R;
        const c = svgEl('circle');
        c.setAttribute('cx', laneX(laneOf.get(e.id)));
        c.setAttribute('cy', dotY(e.row));
        c.setAttribute('r', baseR);
        c.setAttribute('fill', isStack ? (stackCol || laneCol) : '#fff');
        c.setAttribute('stroke', isStack ? '#fff' : laneCol);
        c.setAttribute('stroke-width', '2');
        c.setAttribute('class', 'md-dot');
        c.dataset.rail = e.id;
        svg.appendChild(c);
        const hc = svgEl('circle');
        hc.setAttribute('cx', laneX(laneOf.get(e.id)));
        hc.setAttribute('cy', dotY(e.row));
        hc.setAttribute('r', '11');
        hc.setAttribute('fill', 'transparent');
        hc.setAttribute('pointer-events', 'all');
        hc.style.cursor = 'crosshair';
        const tip = svgEl('title');
        tip.textContent = 'Drag to connect or append · click for connections';
        hc.appendChild(tip);
        hc.addEventListener('mouseenter', () => c.setAttribute('r', String(baseR + 1.5)));
        hc.addEventListener('mouseleave', () => c.setAttribute('r', String(baseR)));
        hc.addEventListener('mousedown', (ev) => {
          const anchor = deckEl.querySelector('.md-chip[data-id="' + CSS.escape(e.id) + '"]');
          startDrag(ev, e.id, hc, () => { if (anchor) openConn(anchor, e.id); });
        });
        svg.appendChild(hc);
      });
      deckEl.appendChild(svg);

      rows.forEach((r, i) => {
        if (r.t !== 'header') return;
        const size = r.stack.blocks.length;
        const frame = document.createElement('div');
        frame.className = 'md-stackframe';
        if (r.stack.color) {
          frame.style.borderColor = r.stack.color;
          frame.style.background = hexA(r.stack.color, 0.06);
        }
        frame.style.left = (railW - 6) + 'px';
        frame.style.right = '0px';
        frame.style.top = (i * PITCH - 3) + 'px';
        frame.style.height = ((1 + size) * PITCH - GAP + 6) + 'px';
        deckEl.appendChild(frame);
      });

      const list = document.createElement('div');
      list.className = 'md-rows';
      list.style.marginLeft = railW + 'px';
      rows.forEach((r) => {
        if (r.t === 'node') list.appendChild(chip(r.node, r.inStack));
        else if (r.t === 'stack') list.appendChild(stackChip(r.stack));
        else list.appendChild(stackHead(r.stack));
      });
      deckEl.appendChild(list);

      // One empty row's worth of canvas under the list. On a long board (the
      // CDEX one is 92 rows) the list fills the panel exactly, so scrolled to
      // the end there was nowhere left to release a drag for "append" -- and
      // nowhere for the ghost row to show.
      const tail = document.createElement('div');
      tail.className = 'md-tail';
      tail.style.height = PITCH + 'px';
      deckEl.appendChild(tail);

      const wire = svgEl('svg');
      wire.setAttribute('class', 'md-wire');
      deckEl.appendChild(wire);

      updateBar();
      applySearch();
      renderBadges();
    };

    const hexA = (hex, a) => {
      const h = hex.replace('#', '');
      const r = parseInt(h.slice(0, 2), 16), g = parseInt(h.slice(2, 4), 16),
        b = parseInt(h.slice(4, 6), 16);
      return 'rgba(' + r + ',' + g + ',' + b + ',' + a + ')';
    };

    /* ---- row builders ---- */

    const kindIcon = (b) => {
      const k = document.createElement('span');
      k.className = 'md-kind';
      if (b.icon) {
        const img = document.createElement('img');
        img.src = b.icon;
        img.alt = b.category || 'block';
        k.appendChild(img);
      } else {
        k.style.background = b.color || '#999';
        k.textContent = (b.name || '?').slice(0, 1).toUpperCase();
      }
      k.title = b.category || '';
      return k;
    };

    // input-slot pips: one open/filled circle per named slot, an ∞ pip for
    // variadic blocks; data blocks (no inputs) show nothing
    const portsStrip = (b) => {
      const wrap = document.createElement('span');
      wrap.className = 'md-ports';
      const occ = new Map();
      linksInto(b.id).forEach((l) => occ.set(l.input, l.from));
      (b.inputs || []).forEach((slot) => {
        const pip = document.createElement('span');
        pip.className = 'md-pip' + (occ.has(slot) ? ' filled' : '');
        if (occ.has(slot)) {
          const src = blockOf(occ.get(slot));
          pip.title = slot + ' ← ' + (src ? src.name : occ.get(slot));
          pip.style.background = b.color || '#2563eb';
          pip.style.borderColor = b.color || '#2563eb';
        } else {
          pip.title = slot + ' — free';
        }
        wrap.appendChild(pip);
      });
      if (b.variadic) {
        const n = linksInto(b.id).filter((l) =>
          !(b.inputs || []).includes(l.input)).length;
        const pip = document.createElement('span');
        pip.className = 'md-pip md-pip-inf' + (n ? ' filled' : '');
        pip.textContent = '∞';
        pip.title = n ? n + ' inputs (unlimited)' : 'unlimited inputs';
        if (n) pip.style.color = b.color || '#2563eb';
        wrap.appendChild(pip);
      }
      return wrap;
    };

    const chip = (b, inStack) => {
      const el = document.createElement('div');
      el.className = 'md-chip' + (inStack ? ' instack' : '') +
        (selection.has(b.id) ? ' sel' : '');
      el.dataset.id = b.id;

      el.appendChild(kindIcon(b));
      if ((b.inputs || []).length || b.variadic) el.appendChild(portsStrip(b));

      const name = document.createElement('span');
      name.className = 'md-name';
      name.textContent = b.name;
      name.title = 'Click to open · double-click to rename · ⌘-click to select';
      name.addEventListener('dblclick', () => {
        name.contentEditable = 'true';
        name.focus();
        document.getSelection().selectAllChildren(name);
      });
      name.addEventListener('blur', () => {
        if (name.contentEditable !== 'true') return;
        name.contentEditable = 'false';
        const nm = name.textContent.trim();
        if (nm && nm !== b.name) {
          b.name = nm;
          push('block_rename', { id: b.id, name: nm });
        }
        name.textContent = b.name;
        if (pendingRender) render();
      });
      name.addEventListener('keydown', (e) => {
        if (e.key === 'Enter') { e.preventDefault(); name.blur(); }
        if (e.key === 'Escape') { name.textContent = b.name; name.blur(); }
      });
      el.appendChild(name);

      const spring = document.createElement('span');
      spring.className = 'md-spring';
      el.appendChild(spring);

      const status = document.createElement('span');
      status.className = 'md-status';
      status.dataset.for = b.id;
      el.appendChild(status);

      const rm = document.createElement('button');
      rm.className = 'md-rm';
      rm.textContent = '×';
      rm.title = 'Remove block';
      rm.addEventListener('click', (e) => {
        e.stopPropagation();
        push('block_rm', { id: b.id });
      });
      el.appendChild(rm);

      el.addEventListener('click', (e) => {
        if (e.metaKey || e.ctrlKey) {
          if (stackOf(b.id)) return; // stacked blocks: dissolve first
          if (selection.has(b.id)) selection.delete(b.id); else selection.add(b.id);
          el.classList.toggle('sel');
          updateBar();
          return;
        }
        if (e.target.closest('button')) return;
        if (name.isContentEditable) return;
        push('block_select', { id: b.id });
      });

      return el;
    };

    const stackChip = (stack) => {
      const el = document.createElement('div');
      el.className = 'md-chip md-stackchip';
      el.dataset.id = 'stack:' + stack.id;

      const k = document.createElement('span');
      k.className = 'md-kind';
      k.innerHTML = STACK_ICON;
      if (stack.color) k.style.background = stack.color;
      k.title = 'Stack';
      el.appendChild(k);

      const name = document.createElement('span');
      name.className = 'md-name';
      name.textContent = stack.name;
      el.appendChild(name);

      const badge = document.createElement('span');
      badge.className = 'md-badge';
      badge.textContent = stack.blocks.length + ' blocks';
      el.appendChild(badge);

      const spring = document.createElement('span');
      spring.className = 'md-spring';
      el.appendChild(spring);

      const status = document.createElement('span');
      status.className = 'md-status';
      status.dataset.stack = stack.id;
      el.appendChild(status);

      const chev = document.createElement('button');
      chev.className = 'md-chev';
      chev.innerHTML = CHEV_R;
      chev.title = 'Expand stack';
      chev.addEventListener('click', (e) => {
        e.stopPropagation();
        collapsed.delete(stack.id);
        render();
      });
      el.appendChild(chev);

      el.addEventListener('click', (e) => {
        if (e.metaKey || e.ctrlKey || e.target.closest('button')) return;
        openConn(el, 'stack:' + stack.id);
      });

      return el;
    };

    const stackHead = (stack) => {
      const el = document.createElement('div');
      el.className = 'md-stackhead';
      el.dataset.stack = stack.id;

      const cap = document.createElement('span');
      cap.className = 'md-cap';
      cap.innerHTML = STACK_ICON;
      if (stack.color) cap.style.color = stack.color;
      el.appendChild(cap);

      const name = document.createElement('span');
      name.className = 'md-name';
      name.textContent = stack.name;
      name.title = 'Double-click to rename';
      name.addEventListener('dblclick', () => {
        name.contentEditable = 'true';
        name.focus();
        document.getSelection().selectAllChildren(name);
      });
      name.addEventListener('blur', () => {
        if (name.contentEditable !== 'true') return;
        name.contentEditable = 'false';
        const nm = name.textContent.trim();
        if (nm && nm !== stack.name) {
          stack.name = nm;
          push('stack_rename', { id: stack.id, name: nm });
        }
        name.textContent = stack.name;
        if (pendingRender) render();
      });
      name.addEventListener('keydown', (e) => {
        if (e.key === 'Enter') { e.preventDefault(); name.blur(); }
        if (e.key === 'Escape') { name.textContent = stack.name; name.blur(); }
      });
      el.appendChild(name);

      const badge = document.createElement('span');
      badge.className = 'md-badge';
      badge.textContent = String(stack.blocks.length);
      el.appendChild(badge);

      const spring = document.createElement('span');
      spring.className = 'md-spring';
      el.appendChild(spring);

      const rm = document.createElement('button');
      rm.className = 'md-rm';
      rm.textContent = '×';
      rm.title = 'Dissolve stack (blocks stay)';
      rm.addEventListener('click', () => push('stack_rm', { id: stack.id }));
      el.appendChild(rm);

      const chev = document.createElement('button');
      chev.className = 'md-chev';
      chev.innerHTML = CHEV_D;
      chev.title = 'Collapse stack';
      chev.addEventListener('click', () => {
        collapsed.add(stack.id);
        render();
      });
      el.appendChild(chev);

      return el;
    };

    /* ---- lineage on hover ---- */

    // ONE delegated listener with hover intent, not a pair per row. Rows are
    // 28px with a 6px gap, so per-row enter/leave made a pointer travelling
    // down the list clear and re-apply the whole highlight through every gap
    // -- a strobe. The gap now reads as "still on the last row", the enter is
    // debounced so passing over a row does not light it, and leaving has a
    // grace period so a wander through the gutter does not drop the focus.
    // cold start is slower than a move between rows (once you are reading
    // lineage you want it to follow), and both debounce: a pointer sweeping
    // past a row never paints it, it only paints where you settle.
    const FOCUS_IN_MS = 120, FOCUS_MOVE_MS = 60, FOCUS_OUT_MS = 260;

    let focusId = null, focusTimer = null;

    const relatedTo = (railId) => {
      if (railId.startsWith('stack:')) {
        const s = stacks.find((x) => x.id === railId.slice(6));
        const keep = new Set(s ? s.blocks : []);
        (s ? s.blocks : []).forEach((m) => {
          upstream(m).forEach((x) => keep.add(x));
          downstream(m).forEach((x) => keep.add(x));
        });
        return keep;
      }
      const keep = upstream(railId);
      downstream(railId).forEach((x) => keep.add(x));
      keep.add(railId);
      return keep;
    };

    // The focus is two classes -- one on the deck, `md-rel` on the few rows
    // that stay lit -- so CSS owns the fade and a hover costs a handful of DOM
    // writes instead of one per row and one per edge (184 of them on the CDEX
    // board, every time the pointer crossed a gap).
    const paintFocus = (railId) => {
      if (focusId === railId) return;
      focusId = railId;
      if (!railId) {
        deckEl.classList.remove('md-focused');
        deckEl.querySelectorAll('.md-rel').forEach((e) => e.classList.remove('md-rel'));
        return;
      }
      const keep = relatedTo(railId);
      const lit = (id) => {
        if (!id) return false;
        if (id.startsWith('stack:')) {
          const s = stacks.find((x) => x.id === id.slice(6));
          return !!s && s.blocks.some((m) => keep.has(m));
        }
        return keep.has(id);
      };
      deckEl.querySelectorAll('.md-rel').forEach((e) => e.classList.remove('md-rel'));
      deckEl.querySelectorAll('.md-chip').forEach((c) => {
        if (lit(c.dataset.id)) c.classList.add('md-rel');
      });
      deckEl.querySelectorAll('.md-stackhead').forEach((h) => {
        const s = stacks.find((x) => x.id === h.dataset.stack);
        if (s && s.blocks.some((m) => keep.has(m))) h.classList.add('md-rel');
      });
      deckEl.querySelectorAll('.md-edge').forEach((p) => {
        if (lit(p.dataset.from) && lit(p.dataset.to)) p.classList.add('md-rel');
      });
      deckEl.classList.add('md-focused');
    };

    const wantFocus = (railId) => {
      clearTimeout(focusTimer);
      if (railId === focusId) return;
      focusTimer = setTimeout(
        () => paintFocus(railId),
        railId ? (focusId ? FOCUS_MOVE_MS : FOCUS_IN_MS) : FOCUS_OUT_MS
      );
    };

    deckEl.addEventListener('mouseover', (ev) => {
      if (dragging || searchEl.value.trim()) return;
      const row = ev.target.closest('.md-chip, .md-stackhead');
      if (!row) return;
      const id = row.dataset.id ||
        (row.dataset.stack ? 'stack:' + row.dataset.stack : null);
      if (id) wantFocus(id);
    });

    deckEl.addEventListener('mouseleave', () => wantFocus(null));

    const clearFocus = () => { clearTimeout(focusTimer); paintFocus(null); };

    /* ---- status badges ---- */

    const stackStatus = (stack) => {
      let worst = null, worstRank = 0;
      stack.blocks.forEach((m) => {
        const st = statuses.get(m);
        if (st && (STATUS_RANK[st.status] || 0) > worstRank) {
          worstRank = STATUS_RANK[st.status] || 0;
          worst = st;
        }
      });
      return worst;
    };

    const renderBadges = () => {
      deckEl.querySelectorAll('.md-status[data-for]').forEach((el) => {
        const st = statuses.get(el.dataset.for);
        el.style.background = st ? st.color : 'transparent';
        el.title = st ? st.label : '';
        el.classList.toggle('on', !!st);
      });
      deckEl.querySelectorAll('.md-status[data-stack]').forEach((el) => {
        const s = stacks.find((x) => x.id === el.dataset.stack);
        const st = s ? stackStatus(s) : null;
        el.style.background = st ? st.color : 'transparent';
        el.title = st ? st.label : '';
        el.classList.toggle('on', !!st);
      });
    };

    /* ---- search ---- */

    const applySearch = () => {
      const q = searchEl.value.trim().toLowerCase();
      if (q) clearFocus();
      const chips = deckEl.querySelectorAll('.md-chip');
      const heads = deckEl.querySelectorAll('.md-stackhead');
      if (!q) {
        chips.forEach((c) => c.classList.remove('dim'));
        heads.forEach((h) => h.classList.remove('dim'));
        // drop the inline value rather than pinning it to 1: the lineage
        // focus dims edges from CSS, and an inline opacity would outrank it
        deckEl.querySelectorAll('.md-edge').forEach((p) => {
          p.style.removeProperty('opacity');
        });
        deckEl.querySelectorAll('.md-badge').forEach((b) => b.classList.remove('hit'));
        hitsEl.textContent = '';
        return;
      }
      const match = (b) => b.name.toLowerCase().includes(q);
      const hits = blocks.filter(match).map((b) => b.id);
      chips.forEach((c) => {
        const id = c.dataset.id;
        if (id.startsWith('stack:')) {
          const s = stacks.find((x) => x.id === id.slice(6));
          const inner = s.blocks.filter((m) => {
            const b = blockOf(m);
            return b && match(b);
          }).length;
          const lit = inner > 0 || s.name.toLowerCase().includes(q);
          c.classList.toggle('dim', !lit);
          const bd = c.querySelector('.md-badge');
          bd.classList.toggle('hit', inner > 0);
          bd.textContent = inner > 0
            ? inner + ' of ' + s.blocks.length + ' match'
            : s.blocks.length + ' blocks';
        } else {
          c.classList.toggle('dim', !hits.includes(id));
        }
      });
      heads.forEach((h) => {
        const s = stacks.find((x) => x.id === h.dataset.stack);
        const lit = s.name.toLowerCase().includes(q) ||
          s.blocks.some((m) => { const b = blockOf(m); return b && match(b); });
        h.classList.toggle('dim', !lit);
      });
      deckEl.querySelectorAll('.md-edge').forEach((p) => { p.style.opacity = '0.15'; });
      hitsEl.textContent = hits.length + ' / ' + blocks.length;
    };
    searchEl.addEventListener('input', applySearch);
    searchEl.addEventListener('keydown', (e) => {
      if (e.key === 'Escape') { searchEl.value = ''; applySearch(); searchEl.blur(); }
    });

    /* ---- selection / stack creation ---- */

    const updateBar = () => {
      barEl.classList.toggle('on', selection.size >= 2);
      barEl.classList.remove('err');
      selcountEl.textContent = selection.size + ' blocks selected';
    };

    clearselBtn.addEventListener('click', () => {
      selection.clear();
      render();
    });

    mkstackBtn.addEventListener('click', () => {
      const members = [...selection];
      const trial = [...stacks, { id: '_trial', name: '', blocks: members }];
      if (G.superOrder(model(), trial).hadCycle) {
        barEl.classList.add('err');
        selcountEl.textContent = 'That grouping would tangle the flow';
        return;
      }
      selection.clear();
      push('stack_add', { blocks: members });
    });

    /* ---- unlink: hover a rail edge for a ✕ at its midpoint ---- */

    let edgeXEl = null, edgeXTimer = null, edgeXPath = null;

    const linksBehind = (railFrom, railTo) =>
      links.filter((l) => railIdOf(l.from) === railFrom && railIdOf(l.to) === railTo);

    // `path` is what lights up, `band` the stretch this edge owns alone (they
    // differ once siblings share a bus) -- the ✕ goes on the band, so two
    // edges out of one block never put their ✕ in the same place.
    const showEdgeX = (e, path, band) => {
      if (dragging) return;
      clearTimeout(edgeXTimer);
      hideEdgeXNow();
      path.setAttribute('stroke-width', '3.2');
      edgeXPath = path;
      const at = band || path;
      const m = at.getPointAtLength(at.getTotalLength() / 2);
      const behind = linksBehind(e.from, e.to);
      edgeXEl = document.createElement('button');
      edgeXEl.className = 'md-edge-x';
      edgeXEl.textContent = '×';
      edgeXEl.title = behind.length > 1
        ? 'Remove connection (' + behind.length + ' links)'
        : 'Remove connection';
      edgeXEl.style.left = (m.x - 8) + 'px';
      edgeXEl.style.top = (m.y - 8) + 'px';
      edgeXEl.addEventListener('mouseenter', () => clearTimeout(edgeXTimer));
      edgeXEl.addEventListener('mouseleave', hideEdgeXSoon);
      edgeXEl.addEventListener('click', () => {
        push('link_rm', { ids: behind.map((l) => l.id) });
        hideEdgeXNow();
      });
      deckEl.appendChild(edgeXEl);
    };

    const hideEdgeXNow = () => {
      if (edgeXPath) { edgeXPath.setAttribute('stroke-width', '2'); edgeXPath = null; }
      if (edgeXEl) { edgeXEl.remove(); edgeXEl = null; }
    };
    const hideEdgeXSoon = () => {
      clearTimeout(edgeXTimer);
      edgeXTimer = setTimeout(hideEdgeXNow, 300);
    };

    /* ---- connections popover ---- */

    const slotLabel = (input) => input === '' ? 'new input' : input;

    const openConn = (el, railId) => {
      if (closePicker) closePicker();
      const ins = links.filter((l) => railIdOf(l.to) === railId && railIdOf(l.from) !== railId);
      const outs = links.filter((l) => railIdOf(l.from) === railId && railIdOf(l.to) !== railId);
      const pop = document.createElement('div');
      pop.className = 'md-picker md-conn';
      pop.style.left = Math.min(el.offsetLeft + 24, Math.max(0, deckEl.clientWidth - 230)) + 'px';
      const estH = 30 + (ins.length + outs.length) * 26 + (ins.length && outs.length ? 20 : 0);
      const below = el.offsetTop + ROW_H + 2;
      pop.style.top = (below + estH > deckEl.clientHeight + PITCH
        ? Math.max(0, el.offsetTop - estH - 4) : below) + 'px';
      const section = (title, list, dir, other, withSlot) => {
        if (!list.length) return;
        const cap = document.createElement('div');
        cap.className = 'md-sect';
        cap.textContent = title;
        pop.appendChild(cap);
        list.forEach((l) => {
          const row = document.createElement('div');
          row.className = 'md-crow';
          const nm = blockOf(other(l));
          row.innerHTML = '<span class="md-dir">' + dir + '</span>' +
            '<span class="md-who">' + escapeHtml(nm ? nm.name : other(l)) + '</span>' +
            (withSlot && l.input !== '' && (blockOf(l.to) || {}).inputs?.includes(l.input)
              ? '<span class="md-slot">' + escapeHtml(l.input) + '</span>' : '');
          const x = document.createElement('button');
          x.className = 'md-unlink';
          x.textContent = '×';
          x.title = 'Remove this connection';
          x.addEventListener('click', () => push('link_rm', { ids: [l.id] }));
          row.appendChild(x);
          pop.appendChild(row);
        });
      };
      section('Inputs', ins, 'from', (l) => l.from, true);
      section('Outputs', outs, 'to', (l) => l.to, true);
      if (!ins.length && !outs.length) {
        const none = document.createElement('div');
        none.className = 'md-none';
        none.textContent = 'No connections yet — drag this block’s dot.';
        pop.appendChild(none);
      }
      deckEl.appendChild(pop);
      const onDoc = (ev) => { if (!pop.contains(ev.target)) close(); };
      const close = () => {
        document.removeEventListener('mousedown', onDoc);
        pop.remove();
        closePicker = null;
      };
      setTimeout(() => document.addEventListener('mousedown', onDoc), 0);
      closePicker = close;
    };

    const escapeHtml = (s) => String(s).replace(/[&<>"']/g, (c) => ({
      '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'
    }[c]));

    /* ---- slot picker: which input to wire into ---- */

    const openSlotPicker = (x, y, block, free, onPick) => {
      if (closePicker) closePicker();
      const pop = document.createElement('div');
      pop.className = 'md-picker';
      pop.style.left = Math.max(0, Math.min(x, deckEl.clientWidth - 170)) + 'px';
      pop.style.top = Math.max(0, y) + 'px';
      const cap = document.createElement('div');
      cap.className = 'md-sect';
      cap.textContent = 'Into which input of ' + block.name + '?';
      pop.appendChild(cap);
      free.forEach((slot) => {
        const item = document.createElement('div');
        item.className = 'md-pick';
        item.innerHTML = '<b>' + escapeHtml(slotLabel(slot)) + '</b>';
        item.addEventListener('click', () => { close(); onPick(slot); });
        pop.appendChild(item);
      });
      deckEl.appendChild(pop);
      const onDoc = (ev) => { if (!pop.contains(ev.target)) close(); };
      const close = () => {
        document.removeEventListener('mousedown', onDoc);
        pop.remove();
        closePicker = null;
      };
      setTimeout(() => document.addEventListener('mousedown', onDoc), 0);
      closePicker = close;
    };

    /* ---- port drag ---- */

    const startDrag = (e, railId, port, onTap) => {
      e.preventDefault();
      e.stopPropagation();
      if (closePicker) closePicker();
      hideEdgeXNow();
      clearFocus();
      const wire = deckEl.querySelector('svg.md-wire');
      const deckBox = deckEl.getBoundingClientRect();
      const p0 = port.getBoundingClientRect();
      const x0 = p0.left + p0.width / 2 - deckBox.left;
      const y0 = p0.top + p0.height / 2 - deckBox.top;
      let moved = false, target = null, ghost = null;
      dragging = true;

      const path = svgEl('path');
      path.setAttribute('fill', 'none');
      path.setAttribute('stroke', '#2563eb');
      path.setAttribute('stroke-width', '1.6');
      path.setAttribute('stroke-dasharray', '4 3');
      wire.appendChild(path);

      // Two drop zones, one per mental model: in the CHIP column the whole
      // row band is a target (forgiving); in the RAIL gutter only the dots
      // themselves are — the line and the blank gutter read as canvas.
      const bandRows = displayRows();
      const rowRail = bandRows.map((r) => r.t === 'node' ? railIdOf(r.node.id)
        : r.t === 'stack' ? 'stack:' + r.stack.id : null);
      const listH = bandRows.length * PITCH;
      const railW = parseInt(deckEl.querySelector('.md-rows').style.marginLeft, 10) || 40;
      const dotPos = [...deckEl.querySelectorAll('svg.md-rail circle.md-dot')].map((c) => ({
        id: c.dataset.rail,
        x: +c.getAttribute('cx'), y: +c.getAttribute('cy')
      }));

      const bandAt = (x, y) => {
        if (x >= railW - 8 && x <= deckEl.clientWidth + 8) {
          let idx = -1;
          if (y >= -8 && y < listH) idx = Math.max(0, Math.floor(y / PITCH));
          else if (y >= listH && y < listH + 12) idx = rowRail.length - 1;
          if (idx < 0) return null;
          let band = rowRail[idx];
          if (!band && idx + 1 < rowRail.length) band = rowRail[idx + 1];
          return band === railId ? 'self' : band;
        }
        if (x >= -8 && x < railW - 8) {
          const near = dotPos.find((d) =>
            (d.x - x) * (d.x - x) + (d.y - y) * (d.y - y) <= 15 * 15);
          if (!near) return null;
          return near.id === railId ? 'self' : near.id;
        }
        return null;
      };

      const onMove = (ev) => {
        const x = ev.clientX - deckBox.left, y = ev.clientY - deckBox.top;
        if (Math.abs(x - x0) + Math.abs(y - y0) > 6) moved = true;
        const dx = x - x0, dy = y - y0;
        const bend = Math.min(36, Math.abs(dx) * 0.5) * (dx < 0 ? -1 : 1);
        path.setAttribute('d', 'M' + x0 + ',' + y0 +
          ' C' + (x0 + bend) + ',' + (y0 + dy * 0.2) +
          ' ' + (x - bend) + ',' + (y - dy * 0.2) + ' ' + x + ',' + y);
        deckEl.querySelectorAll('.md-chip').forEach((c) =>
          c.classList.remove('drop-ok', 'drop-no'));
        if (ghost) { ghost.remove(); ghost = null; }
        target = null;
        const band = bandAt(x, y);
        if (band && band !== 'self') {
          const verdict = dropVerdict(railId, band);
          const chipEl = deckEl.querySelector('.md-chip[data-id="' + CSS.escape(band) + '"]');
          if (chipEl) {
            chipEl.classList.add(verdict === 'ok' ? 'drop-ok' : 'drop-no');
            if (verdict === 'full') chipEl.title = 'All inputs are taken';
            else if (verdict === 'cycle') chipEl.title = 'Would create a cycle';
            else chipEl.title = '';
          }
          if (verdict === 'ok') target = band;
        } else if (!band && moved) {
          ghost = document.createElement('div');
          ghost.className = 'md-ghost';
          const rows = deckEl.querySelector('.md-rows');
          ghost.style.left = rows.style.marginLeft;
          ghost.style.right = '0';
          ghost.style.top = listH + 'px';
          deckEl.appendChild(ghost);
        }
      };

      const onUp = (ev) => {
        document.removeEventListener('mousemove', onMove);
        document.removeEventListener('mouseup', onUp);
        dragging = false;
        path.remove();
        if (ghost) ghost.remove();
        deckEl.querySelectorAll('.md-chip').forEach((c) =>
          c.classList.remove('drop-ok', 'drop-no'));
        const x = ev.clientX - deckBox.left, y = ev.clientY - deckBox.top;
        if (target) {
          doConnect(railId, target, x, Math.min(y, listH));
          if (pendingRender) render();
          return;
        }
        if (!moved) {
          if (onTap) onTap();
          if (pendingRender) render();
          return;
        }
        // released on the canvas: append a new block wired from the source
        // (opens the block browser). A release on a row that was no valid
        // target (own row, cycle, full) is a strict no-op.
        if (bandAt(x, y) !== null) {
          if (pendingRender) render();
          return;
        }
        push('block_append', { from: sinkOf(railId) });
        if (pendingRender) render();
      };

      document.addEventListener('mousemove', onMove);
      document.addEventListener('mouseup', onUp);
    };

    /* ---- inbound ---- */

    const setData = (msg) => {
      const asArr = (x) => x == null ? [] : (Array.isArray(x) ? x : [x]);
      blocks = asArr(msg.blocks).map((b) => ({
        id: b.id, name: b.name, category: b.category || '', color: b.color,
        icon: b.icon || null, inputs: asArr(b.inputs), variadic: !!b.variadic
      }));
      links = asArr(msg.links).map((l) => ({
        id: l.id, from: l.from, to: l.to,
        input: l.input == null ? '' : l.input
      }));
      stacks = asArr(msg.stacks).map((s) => ({
        id: s.id, name: s.name, color: s.color || null, blocks: asArr(s.blocks)
      }));
      const ids = new Set(blocks.map((b) => b.id));
      selection = new Set([...selection].filter((id) => ids.has(id)));
      [...collapsed].forEach((id) => {
        if (!stacks.some((s) => s.id === id)) collapsed.delete(id);
      });
      [...statuses.keys()].forEach((id) => {
        if (!ids.has(id)) statuses.delete(id);
      });
      render();
    };

    const setBadge = (msg) => {
      if (msg.color) {
        statuses.set(msg.id, {
          color: msg.color, label: msg.label || '', status: msg.status || ''
        });
      } else {
        statuses.delete(msg.id);
      }
      renderBadges();
    };

    return {
      ns,
      announced: false,
      setData,
      setBadge,
      inspect: () => ({
        blocks, links, stacks,
        statuses: Object.fromEntries(statuses),
        collapsed: [...collapsed],
        selection: [...selection]
      })
    };
  }
})();
