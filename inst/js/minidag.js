/* Minidag: the BOARD adapter for the list+rail editor (blockr.extra).
 *
 * The drawing and the gestures live in `minidag-rail.js`, which knows nothing
 * about blockr. This file is everything that does: the Shiny channels, the
 * one-instance-per-container registry, block arity, and how a block paints
 * itself in a row.
 *
 * One instance per `.minidag[data-ns]` container. The R side pushes the full
 * board model ('minidag-data') and per-block status badges ('minidag-badge');
 * user gestures come back out of the renderer as `emit(name, payload)` and go
 * on as event-priority Shiny inputs (link_add, link_rm, block_rm,
 * block_rename, block_select, block_append, block_add, stack_add,
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

    // the renderer hands the model back on every query, so arity is always
    // read off the board as it stands, never off a stale copy
    let blocks = [], links = [];

    const blockOf = (id) => blocks.find((b) => b.id === id);
    const linksInto = (id) => links.filter((l) => l.to === id);

    // free input slots of a block: unoccupied named slots, plus the ''
    // (variadic) slot which never fills up
    const freeSlots = (b) => {
      const occ = new Set(linksInto(b.id).map((l) => l.input));
      const free = (b.inputs || []).filter((s) => !occ.has(s));
      if (b.variadic) free.push('');
      return free;
    };

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

    const rail = minidagRail.create(rootEl, {
      emit: push,

      // a fragment, so icon and pips stay DIRECT children of the row: the
      // chip is a flex line and a wrapper span would collapse them into one
      // item with its own gap
      nodeLead: (b) => {
        const lead = document.createDocumentFragment();
        lead.appendChild(kindIcon(b));
        if ((b.inputs || []).length || b.variadic) lead.appendChild(portsStrip(b));
        return lead;
      },

      // a board constrains the CONSUMER: the edge occupies one of `to`'s
      // free input slots, and an empty answer is what refuses the drop
      slotsFor: (from, to) => freeSlots(to),

      // an auto-named variadic link is not a slot anyone chose, so the
      // connections popover keeps quiet about it
      showSlot: (l) => l.input !== '' &&
        ((blockOf(l.to) || {}).inputs || []).includes(l.input)
    });

    const asArr = (x) => x == null ? [] : (Array.isArray(x) ? x : [x]);

    const setData = (msg) => {
      blocks = asArr(msg.blocks).map((b) => ({
        id: b.id, name: b.name, category: b.category || '', color: b.color,
        icon: b.icon || null, inputs: asArr(b.inputs), variadic: !!b.variadic
      }));
      links = asArr(msg.links).map((l) => ({
        id: l.id, from: l.from, to: l.to,
        input: l.input == null ? '' : l.input
      }));
      rail.setData({ blocks, links, stacks: asArr(msg.stacks) });
    };

    return {
      ns,
      announced: false,
      setData,
      setBadge: rail.setBadge,
      inspect: rail.inspect
    };
  }
})();
