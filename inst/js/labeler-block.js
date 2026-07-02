// @ts-check
/**
 * LabelerBlock — JS-driven column label editor input binding.
 *
 * Dynamic rows: each row has a column picker (single select), an arrow
 * separator, a text input for the column's label, and a remove button.
 * Selecting a column prefills the input with the column's existing label
 * (from attr(col, "label"), delivered via the shared column-metadata
 * protocol); clearing the text removes the label. Auto-submits on change
 * (300ms debounce for text input).
 *
 * Mirrors blockr.dplyr's rename-block.js; depends on the shared
 * blockr-core.js and blockr-select.js from blockr.dplyr.
 */
(() => {
  'use strict';

  /**
   * Labeler-block state, mirroring make_labeler_expr() in R:
   * `labels` maps column name -> label ("" removes the label).
   * @typedef {Object} LabelerBlockState
   * @property {Record<string, string>} [labels]
   */

  /**
   * Internal per-row UI record. `touched` tracks whether the label text is
   * a deliberate user edit (or restored state) as opposed to a prefill of
   * the column's current label — only touched rows may emit "" (= remove).
   * @typedef {Object} LabelerRow
   * @property {number} id
   * @property {string} col
   * @property {string} label
   * @property {boolean} touched
   * @property {any} _colSelect
   * @property {HTMLDivElement | null} rowEl
   * @property {HTMLInputElement} [_labelInput]
   */

  class LabelerBlock {
    /** @param {HTMLElement} el */
    constructor(el) {
      this.el = el;
      /** @type {LabelerRow[]} */
      this.rows = [];
      this.nextId = 1;
      /** @type {Array<{value: string, label: string}>} */
      this.columnOptions = [];
      /** @type {Record<string, {name: string, label: string}>} */
      this.columnMeta = {};
      /** @type {((value: boolean) => void) | null} */
      this._callback = null;
      this._submitted = false;
      /** @type {ReturnType<typeof setTimeout> | null} */
      this._debounceTimer = null;

      this._buildDOM();
      this._addRow(null, '', false);
    }

    _autoSubmit() {
      clearTimeout(/** @type {ReturnType<typeof setTimeout>} */ (this._debounceTimer));
      this._debounceTimer = setTimeout(() => this._submit(), 300);
    }

    _buildDOM() {
      this.card = document.createElement('div');
      this.card.className = 'lb-card';
      this.el.appendChild(this.card);

      this.listEl = document.createElement('div');
      this.listEl.className = 'lb-rows';
      this.card.appendChild(this.listEl);

      // Add row bar
      const addRow = document.createElement('div');
      addRow.className = 'blockr-add-row';

      const addLink = document.createElement('span');
      addLink.className = 'blockr-add-link';
      addLink.innerHTML = `<span class="blockr-add-icon">${Blockr.icons.plus}</span> Add label`;
      addLink.addEventListener('click', () => this._addRow(null, '', false));
      addRow.appendChild(addLink);

      this.card.appendChild(addRow);
    }

    /** Current label of a column in the input data, '' when unlabelled. */
    /** @param {string} col */
    _existingLabel(col) {
      return (col && this.columnMeta[col] && this.columnMeta[col].label) || '';
    }

    /**
     * @param {string | null} col
     * @param {string} label
     * @param {boolean} touched
     */
    _addRow(col, label, touched) {
      const id = this.nextId++;
      /** @type {LabelerRow} */
      const row = {
        id,
        col: col || '',
        label: label || '',
        touched: !!touched,
        _colSelect: null,
        rowEl: null
      };

      const rowEl = document.createElement('div');
      rowEl.className = 'blockr-row';
      rowEl.setAttribute('data-row-id', /** @type {any} */ (id));
      row.rowEl = rowEl;

      // Column dropdown
      const colDiv = document.createElement('div');
      colDiv.className = 'lb-col-wrap';
      rowEl.appendChild(colDiv);
      row._colSelect = Blockr.Select.single(colDiv, {
        options: this.columnOptions,
        selected: /** @type {string | undefined} */ (col),
        placeholder: 'Column…',
        onChange: (value) => {
          row.col = value;
          // A newly picked column starts from its current label — the row
          // becomes an edit of that label, not of the previous column's.
          row.label = this._existingLabel(value);
          row.touched = false;
          /** @type {HTMLInputElement} */ (row._labelInput).value = row.label;
          this._syncColWidth();
          this._autoSubmit();
        }
      });

      // Arrow separator
      const arrow = document.createElement('span');
      arrow.className = 'lb-arrow';
      arrow.textContent = '→';
      rowEl.appendChild(arrow);

      // Label text input
      const labelInput = document.createElement('input');
      labelInput.type = 'text';
      labelInput.className = 'lb-label-input';
      labelInput.placeholder = 'Label…';
      labelInput.value = label || '';
      labelInput.addEventListener('input', () => {
        row.label = labelInput.value;
        row.touched = true;
        this._autoSubmit();
      });
      row._labelInput = labelInput;
      rowEl.appendChild(labelInput);

      // Remove button
      const rmBtn = document.createElement('button');
      rmBtn.className = 'blockr-row-remove';
      rmBtn.type = 'button';
      rmBtn.innerHTML = Blockr.icons.x;
      rmBtn.addEventListener('click', () => {
        this._removeRow(id);
        this._autoSubmit();
      });
      rowEl.appendChild(rmBtn);

      // Sync row.col with the select's actual value — the select may
      // auto-pick the first option when col is null/empty.
      row.col = row._colSelect.getValue() || '';
      this._prefill(row);

      /** @type {HTMLDivElement} */ (this.listEl).appendChild(rowEl);
      this.rows.push(row);
      this._updateUI();
    }

    /**
     * Fill an untouched row's empty input with the selected column's
     * existing label so editing starts from the current value.
     * @param {LabelerRow} row
     */
    _prefill(row) {
      if (row.touched || row.label) return;
      const existing = this._existingLabel(row.col);
      if (existing) {
        row.label = existing;
        /** @type {HTMLInputElement} */ (row._labelInput).value = existing;
      }
    }

    /** @param {number} id */
    _removeRow(id) {
      if (this.rows.length <= 1) return;

      const idx = this.rows.findIndex(r => r.id === id);
      if (idx < 0) return;

      const row = this.rows[idx];
      row._colSelect?.destroy();
      row.rowEl?.parentNode?.removeChild(row.rowEl);
      this.rows.splice(idx, 1);
      this._updateUI();
    }

    _updateUI() {
      const single = this.rows.length <= 1;
      for (const r of this.rows) {
        const btn = /** @type {HTMLElement | null | undefined} */ (r.rowEl?.querySelector('.blockr-row-remove'));
        if (btn) btn.style.visibility = single ? 'hidden' : 'visible';
      }
      this._syncColWidth();
    }

    /**
     * Size the shared column track to the widest selected value across
     * rows (same pattern as rename-block.js).
     */
    _syncColWidth() {
      let max = 0;
      for (const r of this.rows) {
        const v = r.rowEl?.querySelector('.blockr-select__value');
        if (v) max = Math.max(max, Blockr.contentWidth(v));
      }
      const listEl = /** @type {HTMLDivElement} */ (this.listEl);
      if (max > 0) {
        // + control padding (12) + gap (3) + arrow (16) + wrap padding (8)
        listEl.style.setProperty('--lb-col-w', `${max + 39}px`);
      } else {
        listEl.style.removeProperty('--lb-col-w');
      }
    }

    /** @returns {LabelerBlockState} */
    _compose() {
      /** @type {Record<string, string>} */
      const labels = {};
      for (const r of this.rows) {
        if (!r.col) continue;
        // Untouched rows only ever re-apply the existing label (a no-op);
        // an untouched empty input must NOT clear the column's label.
        if (!r.touched && !r.label) continue;
        labels[r.col] = r.label;
      }
      return { labels };
    }

    _submit() {
      this._submitted = true;
      this._callback?.(true);
    }

    getValue() {
      if (!this._submitted) return null;
      return this._compose();
    }

    /**
     * @param {LabelerBlockState | null | undefined} state
     * @param {boolean} [silent]
     */
    setState(state, silent) {
      // Clear existing rows
      while (this.rows.length > 0) {
        const row = this.rows[0];
        row._colSelect?.destroy();
        row.rowEl?.parentNode?.removeChild(row.rowEl);
        this.rows.splice(0, 1);
      }

      // Rebuild from state; restored entries are deliberate (may be ""),
      // so they count as touched.
      const labels = state?.labels || {};
      const entries = Object.entries(labels);
      if (entries.length === 0) {
        this._addRow(null, '', false);
      } else {
        for (const [col, label] of entries) {
          this._addRow(col, /** @type {string} */ (label) || '', true);
        }
      }
      this._updateUI();
    }

    /** @param {Array<{name: string, label: string}> | null | undefined} meta */
    updateColumns(meta) {
      this.columnMeta = {};
      this.columnOptions = [];
      for (const col of (meta || [])) {
        this.columnMeta[col.name] = col;
        this.columnOptions.push({ value: col.name, label: col.label || '' });
      }
      for (const r of this.rows) {
        if (r._colSelect) {
          const current = r._colSelect.getValue();
          r._colSelect.setOptions(this.columnOptions, current);
          r.col = r._colSelect.getValue();
          this._prefill(r);
        }
      }
      this._syncColWidth();
    }
  }

  // --- Shiny wiring (binding + message handlers via shared factory) ---

  Blockr.registerBlock({
    name: 'labeler',
    Block: LabelerBlock,
    messages: {
      'labeler-columns': (block, msg) => block.updateColumns(msg.columns),
      'labeler-block-update': (block, msg) => block.setState(msg.state)
    }
  });
})();
