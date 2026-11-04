// casos_layer.js -- camada de casos do mapa (todos os casos, sem amostra)
//
// O servidor manda os casos filtrados como arrays binarios em base64 (17 bytes
// por caso: lng/lat float32, id int32, evolucao/sexo/raca/idade/RA uint8) --
// em vez do JSON do leaflet do R, que levava ~60 s pra serializar a base
// inteira.
//
// Dois modos, decididos so' pelo zoom:
//   - zoom < ZOOM_INDIVIDUAL: um marcador FIXO por Regiao Administrativa, no
//     ponto interno do poligono da RA, com o total de casos da RA no filtro;
//   - zoom >= ZOOM_INDIVIDUAL: cada caso individual, em tiles de canvas.
// Casos sem RA identificada aparecem num quadro proprio no canto do mapa.

(function () {
  "use strict";

  var ZOOM_INDIVIDUAL = 13; // a partir desse zoom mostra os casos individuais

  function b64ParaBuffer(s) {
    var bin = atob(s), n = bin.length, u = new Uint8Array(n);
    for (var i = 0; i < n; i++) u[i] = bin.charCodeAt(i);
    return u.buffer;
  }

  function escaparHtml(s) {
    return String(s).replace(/[&<>"']/g, function (c) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c];
    });
  }

  // Chip com a contagem e uma ponta apontando o local da RA; o nome aparece
  // ao passar o mouse. A barrinha na base mostra o tamanho relativo a maior
  // RA. Estilo em inst/app/badges_inline.css (.pv-ra-*).
  function iconeRa(nome, n, maior) {
    var pct = Math.max(4, Math.round(100 * n / Math.max(1, maior)));
    return L.divIcon({
      className: "pv-ra-marcador",
      iconSize: null,
      iconAnchor: [0, 0],
      html: '<div class="pv-ra-chip" title="' + escaparHtml(nome) + ": " + n.toLocaleString("pt-BR") + ' caso(s) no filtro">' +
        '<span class="pv-ra-nome">' + escaparHtml(nome) + '</span>' +
        '<span class="pv-ra-n">' + n.toLocaleString("pt-BR") + '</span>' +
        '<span class="pv-ra-barra"><span style="width:' + pct + '%"></span></span>' +
        '</div>'
    });
  }


  // Casos individuais em tiles de canvas (L.GridLayer), como a camada de
  // cores: cada tile de 256 px e' pintado uma vez e fica guardado, os tiles
  // em volta da tela sao pre-desenhados aos poucos (sem travar o arraste) e,
  // durante o zoom, os tiles do zoom anterior ficam na tela esticados ate' os
  // novos ficarem prontos. Nenhum objeto do Leaflet por ponto; os tiles nao
  // capturam o mouse, entao hover e clique nos casos sao resolvidos pelo
  // indice espacial abaixo (e os tooltips das areas embaixo continuam).
  //
  // Para aguentar areas muito densas:
  //   - coordenadas projetadas 1x por conjunto de dados (Web Mercator no
  //     zoom 0), nada de seno/log por ponto a cada pintura;
  //   - um unico preenchimento por cor (todos os circulos da cor num so'
  //     caminho -- medido ~10x mais rapido que carimbar imagem por ponto);
  //   - casos da mesma cor no mesmo pixel (enderecos repetidos) sao
  //     desenhados uma vez so' -- a imagem e' identica.
  var TAMANHO_TILE = 256;
  var RAIO_PONTO = 4;
  var RAIO_TOQUE = 7;       // distancia (px) para o hover/clique pegar o ponto
  var OPACIDADE_PONTOS = 0.8;
  var ORCAMENTO_MS = 8;     // tempo maximo pintando tiles antes de devolver o navegador
  // celula do indice espacial = 1 tile no zoom dos casos individuais
  var CELULA = TAMANHO_TILE / Math.pow(2, ZOOM_INDIVIDUAL);
  var LINHAS_CELULA = Math.ceil(TAMANHO_TILE / CELULA) + 1;

  // Web Mercator no zoom 0 (0..256 px) -- no zoom z basta multiplicar por 2^z
  function px0x(lng) { return (lng + 180) / 360 * TAMANHO_TILE; }
  function px0y(lat) {
    var s = Math.sin(lat * Math.PI / 180);
    return (0.5 - Math.log((1 + s) / (1 - s)) / (4 * Math.PI)) * TAMANHO_TILE;
  }
  function chaveCelula(cx, cy) { return cx * LINHAS_CELULA + cy; }

  // Indice espacial compacto: casos ordenados por celula, com o intervalo
  // [inicio, fim) de cada celula num Map
  function indexar(d) {
    var n = d.n, x = new Float64Array(n), y = new Float64Array(n), cel = new Float64Array(n);
    var contagem = new Map();
    for (var i = 0; i < n; i++) {
      x[i] = px0x(d.lng[i]);
      y[i] = px0y(d.lat[i]);
      var k = chaveCelula(Math.floor(x[i] / CELULA), Math.floor(y[i] / CELULA));
      cel[i] = k;
      contagem.set(k, (contagem.get(k) || 0) + 1);
    }
    var faixas = new Map(), pos = 0;
    contagem.forEach(function (c, k) { faixas.set(k, [pos, pos]); pos += c; });
    var ordem = new Uint32Array(n);
    for (var j = 0; j < n; j++) {
      var f = faixas.get(cel[j]);
      ordem[f[1]++] = j;
    }
    d.x0 = x;
    d.y0 = y;
    d.ordem = ordem;
    d.faixas = faixas;
  }

  // chama fn(i) para cada caso na caixa [x0,x1]x[y0,y1] (coordenadas do zoom 0)
  function paraCadaCaso(d, x0, y0, x1, y1, fn) {
    var cx0 = Math.floor(x0 / CELULA), cx1 = Math.floor(x1 / CELULA);
    var cy0 = Math.floor(y0 / CELULA), cy1 = Math.floor(y1 / CELULA);
    for (var cx = cx0; cx <= cx1; cx++) {
      for (var cy = cy0; cy <= cy1; cy++) {
        var f = d.faixas.get(chaveCelula(cx, cy));
        if (!f) continue;
        for (var j = f[0]; j < f[1]; j++) {
          var i = d.ordem[j];
          if (d.x0[i] >= x0 && d.x0[i] <= x1 && d.y0[i] >= y0 && d.y0[i] <= y1) fn(i);
        }
      }
    }
  }

  var MARGEM_TILE = RAIO_PONTO + 1;
  var LADO_OCUPACAO = TAMANHO_TILE + 2 * MARGEM_TILE + 1;
  var ocupacao = new Uint8Array(LADO_OCUPACAO * LADO_OCUPACAO);

  function pintarTilePontos(canvas, coords, d) {
    var dpr = window.devicePixelRatio || 1;
    var ctx = canvas.getContext("2d");
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, TAMANHO_TILE, TAMANHO_TILE);
    if (!d || !d.n) return;

    var escala = Math.pow(2, coords.z);
    var ox = coords.x * TAMANHO_TILE, oy = coords.y * TAMANHO_TILE;
    var m = MARGEM_TILE / escala;

    // casos do tile (com a margem do raio), por cor, sem repetir pixel
    ocupacao.fill(0);
    var porCor = {};
    paraCadaCaso(d, ox / escala - m, oy / escala - m, (ox + TAMANHO_TILE) / escala + m, (oy + TAMANHO_TILE) / escala + m, function (i) {
      var px = Math.round(d.x0[i] * escala - ox), py = Math.round(d.y0[i] * escala - oy);
      var cel = (py + MARGEM_TILE) * LADO_OCUPACAO + (px + MARGEM_TILE);
      var cor = d.evo[i];
      if (ocupacao[cel] === cor + 1) return;
      ocupacao[cel] = cor + 1;
      (porCor[cor] || (porCor[cor] = [])).push(px, py);
    });

    Object.keys(porCor).forEach(function (cor) {
      var lista = porCor[cor];
      ctx.fillStyle = d.cores[cor] || "#aaaaaa";
      ctx.beginPath();
      for (var j = 0; j < lista.length; j += 2) {
        ctx.moveTo(lista[j] + RAIO_PONTO, lista[j + 1]);
        ctx.arc(lista[j], lista[j + 1], RAIO_PONTO, 0, 2 * Math.PI);
      }
      ctx.fill();
    });
  }

  // Fila de pintura com orcamento de tempo: o navegador volta a responder ao
  // mouse entre um lote e outro
  function criarFila() {
    var fila = [], agendado = false;
    function rodar() {
      agendado = false;
      var t0 = performance.now();
      while (fila.length && performance.now() - t0 < ORCAMENTO_MS) fila.shift()();
      if (fila.length) agendar();
    }
    function agendar() { if (!agendado) { agendado = true; setTimeout(rodar, 0); } }
    return function (tarefa) { fila.push(tarefa); agendar(); };
  }

  var TilesPontos = null;
  function criarClasseTiles() {
    TilesPontos = L.GridLayer.extend({
      createTile: function (coords, pronto) {
        var tile = L.DomUtil.create("canvas", "leaflet-tile");
        var dpr = window.devicePixelRatio || 1;
        tile.width = TAMANHO_TILE * dpr;
        tile.height = TAMANHO_TILE * dpr;
        var camada = this;
        this._pvFila(function () {
          pintarTilePontos(tile, coords, camada._pvDados);
          pronto(null, tile);
        });
        return tile;
      },

      // pre-desenha um anel de tiles em volta da tela
      _getTiledPixelBounds: function (centro) {
        var b = L.GridLayer.prototype._getTiledPixelBounds.call(this, centro);
        return new L.Bounds(b.min.subtract([TAMANHO_TILE, TAMANHO_TILE]), b.max.add([TAMANHO_TILE, TAMANHO_TILE]));
      },

      // dados novos: repinta os tiles existentes no lugar (sem apaga-los),
      // os mais perto do centro da tela primeiro
      setDados: function (d) {
        this._pvDados = d;
        if (!this._map || this._tileZoom === undefined) return;
        var camada = this;
        var c = this._map.project(this._map.getCenter(), this._tileZoom).divideBy(TAMANHO_TILE);
        var tiles = Object.keys(this._tiles).map(function (k) { return camada._tiles[k]; });
        tiles.sort(function (a, b) { return a.coords.distanceTo(c) - b.coords.distanceTo(c); });
        tiles.forEach(function (t) {
          camada._pvFila(function () { if (t.el) pintarTilePontos(t.el, t.coords, camada._pvDados); });
        });
      }
    });
  }

  // criada so' no primeiro uso: este script carrega antes do leaflet
  var CasosLayer = null;
  function criarClasse() {
    if (!TilesPontos) criarClasseTiles();
    return L.Layer.extend({
    initialize: function (mapId) {
      this._mapId = mapId;
      this._selos = L.layerGroup();       // marcadores fixos das RAs
      this._modo = null;                  // "ras" | "pontos"
      this._quadroSemRa = null;
      this._d = null;
      this._tiles = new TilesPontos({
        pane: "topLayer",
        tileSize: TAMANHO_TILE,
        minZoom: ZOOM_INDIVIDUAL,
        opacity: OPACIDADE_PONTOS,
        keepBuffer: 4,
        updateWhenZooming: false
      });
      this._tiles._pvFila = criarFila();
      this._tooltip = null;
      this._sobre = -1;                   // caso sob o cursor
      this._movendo = false;
    },

    setData: function (d) {
      indexar(d);
      this._d = d;
      this._tiles.setDados(d);
      this._modo = null;
      if (this._map) this._atualizar();
    },

    onAdd: function (map) {
      this._tooltip = L.tooltip({ direction: "top", offset: [0, -RAIO_PONTO] });
      this._tiles.addTo(map);
      map.on("zoomend", this._atualizar, this);
      map.on("mousemove", this._aoMover, this);
      map.on("click", this._aoClicar, this);
      map.on("dragstart zoomstart", this._inicioMovimento, this);
      map.on("dragend zoomend", this._fimMovimento, this);
      this._modo = null;
      this._atualizar();
    },

    onRemove: function (map) {
      map.off("zoomend", this._atualizar, this);
      map.off("mousemove", this._aoMover, this);
      map.off("click", this._aoClicar, this);
      map.off("dragstart zoomstart", this._inicioMovimento, this);
      map.off("dragend zoomend", this._fimMovimento, this);
      this._tiles.remove();
      this._selos.remove();
      this._soltarCursor();
      this._modo = null;
      this._atualizarQuadroSemRa(0);
    },

    // So' faz trabalho quando muda o modo (cruzou o zoom dos casos
    // individuais) ou os dados -- os tiles dos pontos cuidam de si
    _atualizar: function () {
      var map = this._map, d = this._d;
      if (!map || !d) return;
      this._atualizarQuadroSemRa(d.totalRa.get(255) || 0);

      var modo = map.getZoom() >= ZOOM_INDIVIDUAL ? "pontos" : "ras";
      if (modo === this._modo) return;
      if (modo === "ras") {
        this._soltarCursor();
        this._desenharRas();
        this._selos.addTo(map);
      } else {
        this._selos.remove();
      }
      this._modo = modo;
    },

    // enquanto o mapa se move, nada de procurar caso sob o cursor
    _inicioMovimento: function () {
      this._movendo = true;
      this._soltarCursor();
    },
    _fimMovimento: function () { this._movendo = false; },

    // caso mais proximo do cursor (dentro de RAIO_TOQUE px), ou -1
    _casoEm: function (latlng) {
      var d = this._d, escala = Math.pow(2, this._map.getZoom());
      var mx = px0x(latlng.lng), my = px0y(latlng.lat), r = RAIO_TOQUE / escala;
      var melhor = -1, melhorDist = r * r;
      paraCadaCaso(d, mx - r, my - r, mx + r, my + r, function (i) {
        var dx = d.x0[i] - mx, dy = d.y0[i] - my, dist = dx * dx + dy * dy;
        if (dist <= melhorDist) { melhorDist = dist; melhor = i; }
      });
      return melhor;
    },

    _aoMover: function (e) {
      if (this._modo !== "pontos" || !this._d || this._movendo) return;
      var agora = Date.now();
      if (this._ultimoMover && agora - this._ultimoMover < 25) return;
      this._ultimoMover = agora;

      var i = this._casoEm(e.latlng);
      if (i === this._sobre) return;
      this._sobre = i;
      var map = this._map, d = this._d;
      if (i < 0) { this._soltarCursor(); return; }

      var idade = d.idade[i] === 255 ? "?" : d.idade[i];
      this._tooltip.setLatLng([d.lat[i], d.lng[i]]).setContent(
        (d.nomes.sexo[d.sexo[i]] || "?") + ", " + idade + " ano" + (idade === 1 ? "" : "s") + ", " +
        (d.nomes.raca[d.raca[i]] || "?") + ": <b>" + (d.nomes.evolucao[d.evo[i]] || "") + "</b>"
      );
      map.openTooltip(this._tooltip);
      map.getContainer().style.cursor = "pointer";
      map._pvCasoSobCursor = true;  // territorio_layer.js esconde o tooltip da area
    },

    _soltarCursor: function () {
      this._sobre = -1;
      if (!this._map) return;
      this._map._pvCasoSobCursor = false;
      if (this._tooltip) this._map.closeTooltip(this._tooltip);
      this._map.getContainer().style.cursor = "";
    },

    _aoClicar: function (e) {
      if (this._modo !== "pontos" || !this._d) return;
      var i = this._casoEm(e.latlng);
      if (i < 0) return;
      // mesmo input que o leaflet do R usaria -- o servidor monta o popup
      Shiny.setInputValue(this._mapId + "_marker_click",
        { id: this._d.id[i], lat: this._d.lat[i], lng: this._d.lng[i], nonce: Math.random() },
        { priority: "event" });
    },

    // Um marcador fixo por RA (ponto interno do poligono), com o total de
    // casos da RA no filtro -- a posicao nunca depende de onde os casos estao.
    // Montados so' quando os dados mudam ou o zoom volta abaixo do limite.
    _desenharRas: function () {
      var d = this._d, map = this._map, grupo = this._selos;
      grupo.clearLayers();
      var maior = 1;
      d.totalRa.forEach(function (n, ra) { if (ra !== 255 && n > maior) maior = n; });

      d.ras.nome.forEach(function (nome, ra) {
        var n = d.totalRa.get(ra) || 0;
        if (n === 0) return;
        var m = L.marker([d.ras.lat[ra], d.ras.lng[ra]], {
          icon: iconeRa(nome, n, maior), riseOnHover: true
        });
        m.on("click", function () {
          // aproxima ate' os casos individuais, centrado na RA
          var caixa = L.latLngBounds([d.ras.s[ra], d.ras.w[ra]], [d.ras.n[ra], d.ras.e[ra]]);
          var z = Math.max(map.getBoundsZoom(caixa), ZOOM_INDIVIDUAL);
          map.setView([d.ras.lat[ra], d.ras.lng[ra]], z);
        });
        grupo.addLayer(m);
      });
    },


    // Casos sem RA (coordenada fora das RAs e sem RA no SINAN) nao tem onde
    // ser ancorados -- ficam contados num quadro fixo no canto do mapa.
    _atualizarQuadroSemRa: function (n) {
      if (!n) {
        if (this._quadroSemRa) { this._quadroSemRa.remove(); this._quadroSemRa = null; }
        return;
      }
      if (!this._quadroSemRa) {
        var Quadro = L.Control.extend({
          onAdd: function () {
            var div = L.DomUtil.create("div", "leaflet-bar");
            div.style.cssText = "background:#fff;padding:4px 8px;font:12px sans-serif;color:#333;";
            div.title = "Coordenada fora das RAs e sem RA registrada no SINAN";
            return div;
          }
        });
        this._quadroSemRa = new Quadro({ position: "bottomleft" }).addTo(this._map);
      }
      this._quadroSemRa.getContainer().innerHTML =
        "RA não identificada: <b>" + n.toLocaleString("pt-BR") + "</b> caso(s)";
    }
  });
  }

  var camadas = {};

  $(function () {
    Shiny.addCustomMessageHandler("pvdengue-casos", function (msg) {
      var widget = HTMLWidgets.find("#" + msg.map);
      if (!widget) return;
      var map = widget.getMap();
      if (!map || !map.layerManager) return;
      if (!CasosLayer) CasosLayer = criarClasse();

      var d = {
        n: msg.n,
        lng: new Float32Array(b64ParaBuffer(msg.lng)),
        lat: new Float32Array(b64ParaBuffer(msg.lat)),
        id: new Int32Array(b64ParaBuffer(msg.id)),
        evo: new Uint8Array(b64ParaBuffer(msg.evo)),
        sexo: new Uint8Array(b64ParaBuffer(msg.sexo)),
        raca: new Uint8Array(b64ParaBuffer(msg.raca)),
        idade: new Uint8Array(b64ParaBuffer(msg.idade)),
        ra: new Uint8Array(b64ParaBuffer(msg.ra)),
        ras: msg.ras,
        nomes: msg.nomes,
        cores: msg.cores
      };
      // total de casos do filtro por RA (255 = RA nao identificada)
      d.totalRa = new Map();
      for (var i = 0; i < d.n; i++) d.totalRa.set(d.ra[i], (d.totalRa.get(d.ra[i]) || 0) + 1);

      var camada = camadas[msg.map];
      if (!camada || !map.layerManager.getLayer("shape", "pv_casos")) {
        camada = camadas[msg.map] = new CasosLayer(msg.map);
        // registrado no grupo "Casos" do leaflet do R: o controle de camadas
        // liga/desliga normalmente
        map.layerManager.addLayer(camada, "shape", "pv_casos", msg.group);
      }
      camada.setData(d);
    });
  });
})();
