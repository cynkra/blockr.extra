// The code editor's gear (gear_editor_ui() in R/code-editor.R).
//
// The gear opens the editor in the design system's gear tray
// (Blockr.gearTray, blockr.ui). When the block is wide, the open tray moves
// beside the output: the nearest element holding both the tray and the
// output (`data-side-with`) becomes a two-column grid. What comes before the
// tray's branch keeps the full width, the tray's branch takes the right
// column, and what follows it stacks in the left column. In the dock that
// host is the card's accordion (inputs panel right, output panel left); in
// the composer it is the table container (editor right, table left).
(function () {
  "use strict";

  // Host width (px) below which the tray stays above the output.
  var SIDE_MIN = 900;

  // The Run button's hint names the editor's Mod-Enter: Cmd on a Mac, Ctrl
  // elsewhere (code-block.css shows the one that applies).
  if (/Mac|iPhone|iPad/.test(navigator.platform || navigator.userAgent)) {
    document.documentElement.classList.add("blockr-mac");
  }

  /**
   * @param {HTMLElement} band
   * @returns {{host: HTMLElement, branch: HTMLElement} | null}
   */
  function sideHost(band) {
    var partner = document.getElementById(band.getAttribute("data-side-with") || "");
    if (!partner) return null;
    var branch = band;
    var host = band.parentElement;
    while (host && !host.contains(partner)) {
      branch = host;
      host = host.parentElement;
    }
    return host ? { host: host, branch: branch, partner: partner } : null;
  }

  /** @param {HTMLElement} band */
  function followSide(band) {
    var found = sideHost(band);
    if (!found) return;
    var host = found.host;
    var branch = found.branch;
    var partner = found.partner;

    function apply() {
      var open = band.classList.contains("blockr-settings--open");
      var kids = Array.prototype.slice.call(host.children);
      var at = kids.indexOf(branch);
      var shows = function (k) { return getComputedStyle(k).display !== "none"; };
      var row = kids.slice(0, at).filter(shows).length + 1;
      var main = kids.slice(at + 1).filter(shows);
      // A closed output panel leaves nothing to sit beside. (The output
      // itself may draw no box, so look for a hidden element above it.)
      var shown = true;
      for (var el = partner; el && el !== host; el = el.parentElement) {
        if (getComputedStyle(el).display === "none") { shown = false; break; }
      }
      var on = open && shown && main.length > 0 &&
        host.clientWidth >= SIDE_MIN;
      host.classList.toggle("blockr-gear-side", on);
      branch.classList.toggle("blockr-gear-side__tray", on);
      // The tray's branch spans the rows of everything stacked beside it.
      branch.style.gridRow = on ? row + " / span " + main.length : "";
      kids.slice(at + 1).forEach(function (k) {
        k.classList.toggle("blockr-gear-side__main", on);
      });
    }

    // --open is on from the start of the slide-in to the end of the
    // slide-out: the tray moves aside as it opens and back once it is shut.
    new MutationObserver(apply).observe(band, {
      attributes: true, attributeFilter: ["class"]
    });
    // Width decides the mode; height changes when the output panel opens.
    // Deferred a frame: apply() changes the host's size itself.
    if (typeof ResizeObserver === "function") {
      new ResizeObserver(function () {
        window.requestAnimationFrame(apply);
      }).observe(host);
    }
  }

  /** @param {HTMLElement} band */
  function refreshOnOpen(band) {
    var code = band.getAttribute("data-code");
    new MutationObserver(function () {
      if (band.classList.contains("blockr-settings--open") &&
          window.Blockr && window.Blockr.Code) {
        window.Blockr.Code.refresh(code);
      }
    }).observe(band, { attributes: true, attributeFilter: ["class"] });
  }

  /** @param {HTMLElement} band */
  function init(band) {
    if (band._blockrTray) return;
    var gear = document.querySelector('[data-tray="' + band.id + '"]');
    if (!gear) return;
    band._blockrTray = window.Blockr.gearTray(band, gear, {
      label: band.getAttribute("aria-label") || "Settings"
    });
    refreshOnOpen(band);
    followSide(band);
  }

  // The editor inside the tray is a Shiny input: once it is bound, the
  // tray, its gear and the output beside it are all in the document.
  $(document).on("shiny:bound", function (e) {
    var band = e.target.closest && e.target.closest(".blockr-gear-section");
    if (band) init(band);
  });
})();
