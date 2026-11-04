// territorio_layer.js -- mapa coropletico (RA ou setor censitario) em tiles
//
// A cor do mapa e' desenhada em tiles de 256 px (L.GridLayer), como os mapas
// de fundo: cada tile e' pintado uma vez e fica guardado, os tiles em volta
// da tela sao pre-desenhados, e durante o zoom os tiles do zoom anterior
// continuam na tela (esticados) ate' os novos ficarem prontos -- a cor nunca
// "some" ao arrastar ou dar zoom. Trocar indicador/filtro repinta os tiles no
// lugar, sem apaga-los antes.
//
// Geometrias chegam 1x por sessao (mensagem "pvdengue-territorio-geo", por
// unidade: "ra" ou "setor"); a cada troca de indicador chegam so' as cores e
// os valores do tooltip ("pvdengue-coropletico"). Tooltip e destaque do
// poligono sob o mouse sao resolvidos por ponto-em-poligono aqui mesmo (tiles
// de canvas nao sao interativos).

(function () {
  "use strict";

  var TAMANHO_TILE = 256;
  var OPACIDADE_PREENCHIMENTO = 0.75;

  var geometrias = {};  // unidade -> { feicoes: [...], nomes: [...], geojson }
  var estados = {};     // mapa -> estado da camada

  // Web Mercator no zoom 0 (0..256 px) -- no zoom z basta multiplicar por 2^z
  function px0(lng, lat) {
    var s = Math.sin(lat * Math.PI / 180);
    return [
      (lng + 180) / 360 * TAMANHO_TILE,
      (0.5 - Math.log((1 + s) / (1 - s)) / (4 * Math.PI)) * TAMANHO_TILE
    ];
  }

  // Pre-projeta cada poligono (aneis em Float64Array x,y intercalados) e
  // guarda a caixa envolvente, para o desenho dos tiles e o hover
  function prepararFeicoes(geo) {
    return geo.features.map(function (f) {
      var g = f.geometry;
      var poligonos = g.type === "Polygon" ? [g.coordinates] : g.type === "MultiPolygon" ? g.coordinates : [];
      var caixa = [Infinity, Infinity, -Infinity, -Infinity];
      var polys = poligonos.map(function (aneis) {
        return aneis.map(function (anel) {
          var a = new Float64Array(anel.length * 2);
          for (var k = 0; k < anel.length; k++) {
            var p = px0(anel[k][0], anel[k][1]);
            a[2 * k] = p[0];
            a[2 * k + 1] = p[1];
            if (p[0] < caixa[0]) caixa[0] = p[0];
            if (p[1] < caixa[1]) caixa[1] = p[1];
            if (p[0] > caixa[2]) caixa[2] = p[0];
            if (p[1] > caixa[3]) caixa[3] = p[1];
          }
          return a;
        });
      });
      return { i: f.properties.i, polys: polys, caixa: caixa, feature: f };
    });
  }

  // Nivel de detalhe: cada vertice e' arredondado para o pixel e vertices
  // repetidos sao pulados (nos zooms baixos isso corta a maior parte do
  // trabalho). O arredondamento depende so' do vertice -- nao do anterior --,
  // entao dois vizinhos desenham a divisa comum exatamente igual (sem frestas
  // nem sobreposicao). Poligonos menores que 1 px viram um quadradinho.
  function tracar(ctx, feicao, escala, ox, oy) {
    var c = feicao.caixa;
    if ((c[2] - c[0]) * escala < 1 && (c[3] - c[1]) * escala < 1) {
      ctx.rect(Math.round(c[0] * escala - ox), Math.round(c[1] * escala - oy), 1, 1);
      return;
    }
    feicao.polys.forEach(function (aneis) {
      aneis.forEach(function (a) {
        var px = Math.round(a[0] * escala - ox), py = Math.round(a[1] * escala - oy);
        ctx.moveTo(px, py);
        for (var k = 2; k < a.length; k += 2) {
          var x = Math.round(a[k] * escala - ox), y = Math.round(a[k + 1] * escala - oy);
          if (x === px && y === py) continue;
          ctx.lineTo(x, y);
          px = x;
          py = y;
        }
        ctx.closePath();
      });
    });
  }

  function cruza(caixa, x0, y0, x1, y1) {
    return caixa[2] >= x0 && caixa[0] <= x1 && caixa[3] >= y0 && caixa[1] <= y1;
  }

  // Ponto-em-poligono (par-impar, respeita buracos) em coordenadas do zoom 0
  function contem(feicao, x, y) {
    if (x < feicao.caixa[0] || x > feicao.caixa[2] || y < feicao.caixa[1] || y > feicao.caixa[3]) return false;
    var dentro = false;
    feicao.polys.forEach(function (aneis) {
      aneis.forEach(function (a) {
        for (var k = 0, j = a.length - 2; k < a.length; j = k, k += 2) {
          var xi = a[k], yi = a[k + 1], xj = a[j], yj = a[j + 1];
          if ((yi > y) !== (yj > y) && x < (xj - xi) * (y - yi) / (yj - yi) + xi) dentro = !dentro;
        }
      });
    });
    return dentro;
  }

  function pintarTile(canvas, coords, estado) {
    var dpr = window.devicePixelRatio || 1;
    var ctx = canvas.getContext("2d");
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, TAMANHO_TILE, TAMANHO_TILE);

    var escala = Math.pow(2, coords.z);
    var ox = coords.x * TAMANHO_TILE, oy = coords.y * TAMANHO_TILE;
    // caixa do tile no zoom 0 (com folga para os tracos da borda)
    var folga = 2 / escala;
    var x0 = ox / escala - folga, y0 = oy / escala - folga;
    var x1 = (ox + TAMANHO_TILE) / escala + folga, y1 = (oy + TAMANHO_TILE) / escala + folga;

    var geo = geometrias[estado.unidade];
    if (!geo) return;
    var visiveis = geo.feicoes.filter(function (f) { return cruza(f.caixa, x0, y0, x1, y1); });

    // preenchimento: um unico fill por cor (as unidades nao se sobrepoem)
    var porCor = {};
    visiveis.forEach(function (f) {
      var cor = estado.cores[f.i] || "#e5e5e5";
      (porCor[cor] || (porCor[cor] = [])).push(f);
    });
    ctx.globalAlpha = OPACIDADE_PREENCHIMENTO;
    Object.keys(porCor).forEach(function (cor) {
      ctx.beginPath();
      porCor[cor].forEach(function (f) { tracar(ctx, f, escala, ox, oy); });
      ctx.fillStyle = cor;
      ctx.fill("evenodd");
    });

    // divisas entre as unidades
    ctx.globalAlpha = 0.9;
    ctx.strokeStyle = "#ffffff";
    ctx.lineWidth = estado.unidade === "setor" ? (coords.z >= 13 ? 0.6 : 0.3) : 1;
    ctx.beginPath();
    visiveis.forEach(function (f) { tracar(ctx, f, escala, ox, oy); });
    ctx.stroke();

    // no modo setor, o contorno das RAs por cima (como nos mapas do relatorio)
    if (estado.contornoRA && geometrias.ra) {
      ctx.globalAlpha = 0.85;
      ctx.strokeStyle = "#1f2937";
      ctx.lineWidth = 1.3;
      ctx.beginPath();
      geometrias.ra.feicoes.forEach(function (f) {
        if (cruza(f.caixa, x0, y0, x1, y1)) tracar(ctx, f, escala, ox, oy);
      });
      ctx.stroke();
    }
    ctx.globalAlpha = 1;
  }

  var CamadaTiles = null;
  function criarClasse() {
    CamadaTiles = L.GridLayer.extend({
      createTile: function (coords, pronto) {
        var tile = L.DomUtil.create("canvas", "leaflet-tile");
        var dpr = window.devicePixelRatio || 1;
        tile.width = TAMANHO_TILE * dpr;
        tile.height = TAMANHO_TILE * dpr;
        var estado = this._pvEstado;
        // desenha fora do fluxo do evento: enquanto isso, os tiles antigos
        // continuam na tela
        setTimeout(function () {
          pintarTile(tile, coords, estado);
          pronto(null, tile);
        }, 0);
        return tile;
      },

      // pre-desenha um anel de tiles em volta da tela
      _getTiledPixelBounds: function (centro) {
        var b = L.GridLayer.prototype._getTiledPixelBounds.call(this, centro);
        return new L.Bounds(b.min.subtract([TAMANHO_TILE, TAMANHO_TILE]), b.max.add([TAMANHO_TILE, TAMANHO_TILE]));
      },

      // repinta os tiles existentes no lugar (sem apaga-los), aos poucos
      repintar: function () {
        var estado = this._pvEstado, tiles = this._tiles;
        var fila = Object.keys(tiles);
        var passo = function () {
          var t0 = Date.now();
          while (fila.length && Date.now() - t0 < 30) {
            var t = tiles[fila.shift()];
            if (t && t.el) pintarTile(t.el, t.coords, estado);
          }
          if (fila.length) setTimeout(passo, 0);
        };
        passo();
      },

      onAdd: function (map) {
        L.GridLayer.prototype.onAdd.call(this, map);
        map.on("mousemove", this._aoMover, this);
        map.on("mouseout", this._soltar, this);
      },

      onRemove: function (map) {
        map.off("mousemove", this._aoMover, this);
        map.off("mouseout", this._soltar, this);
        this._soltar();
        L.GridLayer.prototype.onRemove.call(this, map);
      },

      _aoMover: function (e) {
        // enquanto arrasta ou anima o zoom, nada de ponto-em-poligono
        if ((this._map.dragging && this._map.dragging.moving()) || this._map._animatingZoom) return;
        var agora = Date.now();
        if (this._ultimo && agora - this._ultimo < 30) return;
        this._ultimo = agora;
        var map = this._map, estado = this._pvEstado, geo = geometrias[estado.unidade];
        if (!geo) return;

        // o tooltip do caso (casos_layer.js) tem prioridade sobre o da area
        if (map._pvCasoSobCursor) { this._soltar(); return; }

        var p = px0(e.latlng.lng, e.latlng.lat);
        var achada = null;
        for (var k = 0; k < geo.feicoes.length; k++) {
          if (contem(geo.feicoes[k], p[0], p[1])) { achada = geo.feicoes[k]; break; }
        }
        if (!achada) { this._soltar(); return; }

        if (this._sobre !== achada) {
          this._sobre = achada;
          if (this._destaque) this._destaque.remove();
          this._destaque = L.geoJSON(achada.feature, {
            interactive: false,
            pane: "contornoRA",
            style: { fill: false, color: "#333333", weight: 2, opacity: 1 }
          }).addTo(map);
          var i = achada.i;
          var html = "<strong>" + geo.nomes[i] + "</strong>";
          estado.linhas.forEach(function (l) { html += "<br>" + l[0] + ": <b>" + l[1][i] + "</b>"; });
          this._tooltip = this._tooltip || L.tooltip({ direction: "auto", offset: [12, 0], className: "pv-tooltip-area" });
          this._tooltip.setContent(html);
        }
        this._tooltip.setLatLng(e.latlng);
        if (!map.hasLayer(this._tooltip)) map.openTooltip(this._tooltip);
      },

      _soltar: function () {
        this._sobre = null;
        if (this._destaque) { this._destaque.remove(); this._destaque = null; }
        if (this._tooltip && this._map) this._map.closeTooltip(this._tooltip);
      }
    });
  }

  $(function () {
    Shiny.addCustomMessageHandler("pvdengue-territorio-geo", function (msg) {
      var geo = JSON.parse(msg.geojson);
      geometrias[msg.unidade] = { feicoes: prepararFeicoes(geo), nomes: msg.nomes };
    });

    Shiny.addCustomMessageHandler("pvdengue-coropletico", function (msg) {
      var widget = HTMLWidgets.find("#" + msg.map);
      if (!widget) return;
      var map = widget.getMap();
      if (!map || !map.layerManager) return;
      if (!CamadaTiles) criarClasse();

      var estado = estados[msg.map];
      var camada = map.layerManager.getLayer("shape", "pv_coropletico");
      if (camada && !estado) camada = null;

      if (msg.remover) {
        if (camada) map.layerManager.removeLayer("shape", "pv_coropletico");
        return;
      }
      if (!geometrias[msg.unidade]) return;

      var novo = { unidade: msg.unidade, cores: msg.cores, linhas: msg.linhas, contornoRA: !!msg.contornoRA };
      if (!camada) {
        camada = new CamadaTiles({
          pane: "coropletico",
          tileSize: TAMANHO_TILE,
          keepBuffer: 4,
          updateWhenZooming: false
        });
        camada._pvEstado = novo;
        estados[msg.map] = novo;
        // no grupo do leaflet do R: o controle de camadas liga/desliga
        map.layerManager.addLayer(camada, "shape", "pv_coropletico", msg.group);
      } else {
        // mesmo objeto de estado (os tiles em desenho o referenciam)
        L.extend(estado, novo);
        camada._soltar();
        camada.repintar();
      }
    });
  });
})();
