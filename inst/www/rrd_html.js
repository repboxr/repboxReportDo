(function() {
  function qsa(selector, root) {
    return Array.prototype.slice.call((root || document).querySelectorAll(selector));
  }

  function activateDoTab(fileIdx) {
    var targetId = "rrd-do-tab-" + fileIdx;

    qsa(".rrd-do-tab-btn").forEach(function(btn) {
      btn.classList.toggle("active", btn.getAttribute("data-file-idx") === String(fileIdx));
    });

    qsa(".rrd-do-tab-pane").forEach(function(pane) {
      pane.classList.toggle("active", pane.id === targetId);
    });
  }

  function activateReportTab(targetId) {
    qsa(".rrd-report-title").forEach(function(btn) {
      btn.classList.toggle("active", btn.getAttribute("data-tab-target") === targetId);
    });

    qsa(".rrd-report-tab-pane").forEach(function(pane) {
      pane.classList.toggle("active", pane.id === targetId);
    });
  }

  function clearHighlights() {
    qsa(".rrd-issue-item.active").forEach(function(el) {
      el.classList.remove("active");
    });

    qsa(".rrd-code-row.rrd-active-line").forEach(function(el) {
      el.classList.remove("rrd-active-line");
    });
  }

  function lineSelector(fileIdx, line) {
    return '.rrd-code-row[data-file-idx="' + String(fileIdx) + '"][data-line="' + String(line) + '"]';
  }

  function highlightLine(fileIdx, line) {
    activateDoTab(fileIdx);

    window.setTimeout(function() {
      var row = document.querySelector(lineSelector(fileIdx, line));
      if (!row) return;

      row.classList.add("rrd-active-line");
      row.scrollIntoView({
        behavior: "smooth",
        block: "center"
      });
    }, 50);
  }

  document.addEventListener("DOMContentLoaded", function() {
    qsa(".rrd-do-tab-btn").forEach(function(btn) {
      btn.addEventListener("click", function() {
        activateDoTab(btn.getAttribute("data-file-idx"));
      });
    });

    qsa(".rrd-report-title").forEach(function(btn) {
      btn.addEventListener("click", function() {
        var targetId = btn.getAttribute("data-tab-target");
        if (targetId) {
          activateReportTab(targetId);
        }
      });
    });

    qsa(".rrd-issue-item").forEach(function(item) {
      item.addEventListener("click", function(event) {
        if (event.target && event.target.closest && event.target.closest("details")) {
          return;
        }

        clearHighlights();
        item.classList.add("active");

        var fileIdx = item.getAttribute("data-file-idx");
        var line = item.getAttribute("data-line");

        if (fileIdx && line) {
          highlightLine(fileIdx, line);
        }
      });
    });
  });
})();
