# Painel de Vigilância de Dengue — DF · **versão de demonstração**

> 🧪 **Somente dados simulados.** Esta é uma vitrine (*showcase*) do
> [Painel de Vigilância de Dengue do DF](https://github.com/InVictorLopes/Dengue_vigilancia_df).
> **Nenhum dado real de notificação é usado nem versionado aqui.** Todas as
> notificações, pacientes, endereços, coordenadas e unidades notificadoras são
> **sorteadas por computador** e não correspondem a pessoas nem a casos
> reais. Os números **não representam** a situação epidemiológica do Distrito
> Federal.
>
> Os únicos dados reais são **públicos**: as malhas das Regiões
> Administrativas e dos setores censitários e a população do **Censo 2022
> (IBGE)**, em `inst/app/www/shapes/`.

O painel é o mesmo do projeto original (R + Shiny, pacote **golem**, com
**bs4Dash**, **leaflet** e **DuckDB**): mapa de casos individuais, filtros
clínicos e demográficos, indicadores por RA e por setor censitário e
estratificação de risco. A única diferença é a **fonte dos dados**: no lugar
do banco PostgreSQL com o SINAN geocodificado, uma rotina gera uma tabela
fictícia **no mesmo formato** e ela passa pelo **mesmo tratamento (ETL)** do
painel real.

---

## Como rodar

1. Instale o R (≥ 4.1) e os pacotes:

   ```r
   install.packages("remotes")
   remotes::install_deps(dependencies = TRUE)
   install.packages("pkgload")
   ```

   > No Linux, `sf` precisa de GDAL, GEOS e PROJ
   > (`sudo apt install libgdal-dev libgeos-dev libproj-dev`).

2. Abra `PVDengueDFDemo.Rproj` no RStudio (ou fique na raiz do projeto) e rode:

   ```r
   shiny::runApp()
   ```

Na **primeira abertura** o painel sorteia a base simulada sozinho (~30 s) e a
grava em `local/`. Não há banco, senha nem `.Renviron`.

Para sortear de novo com outra semente ou outra data de referência:

```r
source("dev/gerar_dados_simulados.R")
```

O botão de sincronizar no topo do painel faz o mesmo com uma semente
aleatória (no painel real ele baixa os dados do banco, com senha de
administrador).

---

## Como os dados são simulados (`R/fct_simulacao.R`)

| Elemento | Como é gerado |
|---|---|
| **Volume** | ~210 mil notificações de 2020 até a data de referência (padrão: 3 dias atrás), com anos epidêmicos e não epidêmicos fictícios. |
| **Sazonalidade** | Curva com pico entre março e abril, largura variando a cada ano. |
| **Território** | Setor censitário sorteado com probabilidade ∝ população do Censo 2022 × um "risco" fictício por RA, por RA-ano e por setor (gera heterogeneidade e recorrência para a estratificação). |
| **Coordenada** | Ponto aleatório **dentro do setor sorteado** — não é endereço de ninguém. ~5% dos casos ficam sem coordenada, como na vida real. |
| **Pessoa** | Idade, sexo, raça/cor, gestação e comorbidades com proporções plausíveis, mas inventadas. Nome = `PACIENTE SIMULADO 0000001`; endereço = "Logradouro fictício …". |
| **Desfecho** | Classificação final e evolução com risco de gravidade crescente com idade e comorbidades; casos recentes têm maior chance de estar "em investigação". |
| **Unidades notificadoras** | UBS e hospitais **fictícios** (códigos `SIM0001`…, inexistentes no CNES), posicionados em setores aleatórios de cada RA. |

A tabela sorteada tem as mesmas colunas e códigos do Dicionário de Dados do
SINAN Online (`NU_NOTIFIC`, `DT_SIN_PRI`, `CLASSI_FIN`, `EVOLUCAO`,
`NU_IDADE_N`, `CS_RACA`, …), então todo o tratamento de
`R/fct_db_update.R` — atribuição de RA e setor por ponto-em-polígono, tempo
até encerramento, estratificação de risco — roda exatamente como no painel
real.

---

## O que mudou em relação ao painel original

| Arquivo | Mudança |
|---|---|
| `R/fct_simulacao.R` | **Novo**: gerador da base simulada. |
| `dev/gerar_dados_simulados.R` | **Novo**: substitui `dev/setup_databases.R`. |
| `R/fct_db_update.R` | `update_dengue_data()` sorteia nova base em vez de ler o PostgreSQL. ETL inalterado. |
| `R/fct_data_cache.R` | Gera a base simulada automaticamente se `local/` estiver vazia. |
| `R/app_server.R` | Aviso de demonstração na abertura; botão de atualização sem senha. |
| `R/app_ui.R` | Selo "DEMONSTRAÇÃO · DADOS SIMULADOS" no cabeçalho. |
| `DESCRIPTION` | Sem `RPostgres`. |
| removidos | `.Renviron.example`, `dev/setup_databases.R`, `dev/build_unidades_cnes.R`. |

A documentação completa da metodologia (indicadores, estratificação de risco,
desempenho, publicação) está no
[README do painel original](https://github.com/InVictorLopes/Dengue_vigilancia_df#5-metodologia).

---

## Publicação

Como não há dado sensível, esta versão pode ser publicada abertamente
(Shiny Server, shinyapps.io, Posit Connect). A pasta `local/` continua fora
do Git (`.gitignore`): ela é regenerada na primeira abertura em cada servidor.

---
Desenvolvido com R + Shiny + bs4Dash + leaflet + DuckDB · **dados simulados para demonstração**
