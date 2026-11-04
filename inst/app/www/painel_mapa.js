// painel_mapa.js -- painel flutuante dentro do mapa e abas do painel
//
// - a barra do topo recolhe/abre o painel inteiro;
// - cada secao (<details>) abre/fecha sozinha;
// - rolar/arrastar dentro do painel nao mexe no mapa (o painel e' irmao do
//   mapa, fora do container do Leaflet).
// Ao reabrir algo, avisa o Shiny (evento "shown") e os htmlwidgets (resize)
// para recalcularem o que estava escondido.

(function () {
  "use strict";

  function avisarVisivel(el) {
    $(el).trigger("shown");
    window.dispatchEvent(new Event("resize"));
  }

  $(document).on("click", ".pv-painel-alternar", function () {
    var painel = $(this).closest(".pv-painel");
    painel.toggleClass("recolhido");
    if (!painel.hasClass("recolhido")) avisarVisivel(painel);
  });

  // Abas (Mapa / Indicadores / Tabela): o Shiny gera li.active > a e o
  // Bootstrap so' troca a classe no <a> -- um observador mantem o <li> em
  // sincronia com o <a> ativo (vale para clique e para updateTabsetPanel)
  function sincronizarAbas(ul) {
    var ativo = ul.querySelector("li > a.active") || ul.querySelector("li.active > a");
    if (!ativo) return;
    Array.prototype.forEach.call(ul.children, function (li) {
      var deveSer = li === ativo.parentElement;
      if (li.classList.contains("active") !== deveSer) li.classList.toggle("active", deveSer);
      var a = li.querySelector("a");
      if (a && a.classList.contains("active") !== deveSer) a.classList.toggle("active", deveSer);
    });
  }
  $(function () {
    document.querySelectorAll(".pv-abas .nav-pills").forEach(function (ul) {
      sincronizarAbas(ul);
      new MutationObserver(function (mudancas) {
        // so' reage quando um <a> ganhou "active" (ignora as proprias mudancas)
        var ganhou = mudancas.some(function (m) {
          return m.target.tagName === "A" && m.target.classList.contains("active") &&
            !m.target.parentElement.classList.contains("active");
        });
        if (ganhou) {
          ul.querySelectorAll("li.active > a:not(.active)").forEach(function (a) {
            a.parentElement.classList.remove("active");
          });
          sincronizarAbas(ul);
        }
      }).observe(ul, { subtree: true, attributes: true, attributeFilter: ["class"] });
    });
  });

  // "toggle" do <details> nao borbulha: escuta na fase de captura
  document.addEventListener("toggle", function (e) {
    var secao = e.target;
    if (!secao.classList || !secao.classList.contains("pv-secao")) return;
    if (secao.open) avisarVisivel(secao);
  }, true);
})();
