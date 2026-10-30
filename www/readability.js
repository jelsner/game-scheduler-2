(function () {
  "use strict";
  const preferenceKey = "ddc-scoring-display-v1";
  document.addEventListener("DOMContentLoaded", function () {
    const sunlight = document.getElementById("sunlight_mode");
    const textSize = document.getElementById("desk_text_size");
    if (!sunlight || !textSize) return;
    const allowedSizes = ["16", "19", "22"];
    try {
      const stored = JSON.parse(localStorage.getItem(preferenceKey));
      if (stored && typeof stored.sunlight === "boolean") sunlight.checked = stored.sunlight;
      if (stored && allowedSizes.includes(stored.textSize)) textSize.value = stored.textSize;
    } catch (_) { /* Storage can be unavailable in a private or restricted viewer. */ }
    function applyDisplay() {
      const size = allowedSizes.includes(textSize.value) ? textSize.value : "19";
      document.body.classList.toggle("sunlight-mode", sunlight.checked);
      document.body.style.setProperty("--desk-text-size", size + "px");
      try {
        localStorage.setItem(preferenceKey, JSON.stringify({sunlight: sunlight.checked, textSize: size}));
      } catch (_) { /* Scoring must work even when preferences cannot be saved. */ }
      // Recalculate visible scrollable tables when the font changes.
      if (window.jQuery && window.jQuery.fn.dataTable) {
        window.jQuery.fn.dataTable.tables({visible: true, api: true}).columns.adjust();
      }
    }
    sunlight.addEventListener("change", applyDisplay);
    textSize.addEventListener("change", applyDisplay);
    applyDisplay();
  });
})();
