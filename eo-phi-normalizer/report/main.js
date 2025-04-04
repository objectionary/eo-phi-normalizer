// SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
// SPDX-License-Identifier: MIT

document.addEventListener("click", function (c) {
  try {
    function h(b, a) {
      return b.nodeName === a ? b : h(b.parentNode, a);
    }
    var v = c.shiftKey || c.altKey,
      d = h(c.target, "TH"),
      m = d.parentNode,
      n = m.parentNode,
      g = n.parentNode;
    function p(b) {
      var a;
      return v
        ? b.dataset.sortAlt
        : null !== (a = b.dataset.sort) && void 0 !== a
        ? a
        : b.textContent;
    }
    if (
      "THEAD" === n.nodeName &&
      g.classList.contains("sortable") &&
      !d.classList.contains("no-sort")
    ) {
      var q,
        f = m.cells,
        r = parseInt(d.dataset.sortTbr);
      for (c = 0; c < f.length; c++)
        f[c] === d
          ? (q = parseInt(d.dataset.sortCol) || c)
          : f[c].setAttribute("aria-sort", "none");
      f = "descending";
      if (
        "descending" === d.getAttribute("aria-sort") ||
        (g.classList.contains("asc") &&
          "ascending" !== d.getAttribute("aria-sort"))
      )
        f = "ascending";
      d.setAttribute("aria-sort", f);
      var w = "ascending" === f,
        x = g.classList.contains("n-last"),
        t = function (b, a, e) {
          a = p(a.cells[e]);
          b = p(b.cells[e]);
          if (x) {
            if ("" === a && "" !== b) return -1;
            if ("" === b && "" !== a) return 1;
          }
          e = Number(a) - Number(b);
          a = isNaN(e) ? a.localeCompare(b) : e;
          return w ? -a : a;
        };
      for (c = 0; c < g.tBodies.length; c++) {
        var k = g.tBodies[c],
          u = [].slice.call(k.rows, 0);
        u.sort(function (b, a) {
          var e = t(b, a, q);
          return 0 !== e || isNaN(r) ? e : t(b, a, r);
        });
        var l = k.cloneNode();
        l.append.apply(l, u);
        g.replaceChild(l, k);
      }
    }
  } catch (h) {}
});
