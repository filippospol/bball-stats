# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script includes the server and UI options for the Basketball Stats app.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Setup environment:
pacman::p_load(tidyverse,purrr,rvest,httr,jsonlite,glue,janitor,stringi,vroom,
               shiny,shinyBS,lubridate,shinydashboard,shinymanager,
               reactable,reactablefmtr,shinycssloaders,shinyWidgets)

# function that creates a list of empty tibbles:
empty_tbl = function() {
  list(
    p = tibble(PLAYER=character(),TEAM=character(),MATCHUP=character(),MIN=numeric(),
               PTS=numeric(),`2PM`=numeric(),`2PA`=numeric(),`3PM`=numeric(),
               `3PA`=numeric(),FTM=numeric(),FTA=numeric(),DREB=numeric(),
               OREB=numeric(),REB=numeric(),AST=numeric(),STL=numeric(),
               BLK=numeric(),TOV=numeric(),PF=numeric() ),
    t = tibble(TEAM=character(),MATCHUP=character(),PTS=numeric(),`2PM`=numeric(),
               `2PA`=numeric(),`3PM`=numeric(),`3PA`=numeric(),FTM=numeric(),
               FTA=numeric(),DREB=numeric(),OREB=numeric(),REB=numeric(),
               AST=numeric(),STL=numeric(),BLK=numeric(),TOV=numeric(),PF=numeric() )
  ) %>% 
    return()
}

# Options:
options(
  readr.show_col_types = FALSE,      # no "Rows/Columns/Column specification" message
  shiny.maxRequestSize = 30*1024^2,  # allow larger file uploads
  timeout = 120,                     # longer timeout for the data download
  vroom.show_progress = FALSE       # no progress bars
)
Sys.setenv(RETICULATE_PYTHON = "")

# Quiet reader:
vroom = function(...) suppressWarnings(suppressMessages(vroom::vroom(...)))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# User database:
credentials = data.frame(
  user = c("fpolyzos","novibet"),
  password = c("b0PERWAY94!","Test123!@"),
  stringsAsFactors = FALSE
)

# Login page header + button/field labels
# (centered logo card with title and subtitle, styled by login_tags below):
set_labels(
  language = "en",
  "Please authenticate" = HTML('
    <div style="text-align:center; padding:4px 0 2px;">
      <img src="https://i.ibb.co/5XhxQYSq/Chat-GPT-Image-Oct-19-2025-12-38-25-PM.png"
           height="72" style="border-radius:16px; box-shadow:0 4px 14px rgba(58,68,160,.25);">
      <div style="font-family:Roboto,sans-serif; margin-top:12px;">
        <div style="font-size:19px; font-weight:700; color:#3a44a0;">Basketball Stats</div>
        <div style="font-size:12.5px; color:#5b6478; margin-top:2px;">Sign in to continue</div>
      </div>
    </div>'),
  "Username:" = "Username",
  "Password:" = "Password",
  "Login" = "Sign in"
)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Tab icons:
FIBA = '<img src="https://i.ibb.co/fd9mFsGy/196.png" ; height=15>'
EL = '<img src="https://mediacentre.euroleague.net/front/mediacentre/v2/images/all/thumbs/el.png" ; height=15>'
EC = '<img src="https://mediacentre.euroleague.net/front/mediacentre/v2/images/all/thumbs/ec.png" ; height=15>'
BCL = '<img src="https://i.ibb.co/VYGnx4z8/422.png" ; height=15>'
NBA = '<img src="https://a4.espncdn.com/combiner/i?img=%2Fi%2Fespn%2Fmisc_logos%2F500%2Fnba.png" ; height=22.5>'
WNBA = '<img src="https://cdn.bleacherreport.net/images/team_logos/328x328/wnba.png" ; height=22.5>'
GR = '<img src="https://raw.githubusercontent.com/HatScripts/circle-flags/dd5910007aee57e20f1b4350d9ec7cef1bb6dfc9/flags/gr.svg" ; height=15>'
ES = '<img src="https://raw.githubusercontent.com/HatScripts/circle-flags/dd5910007aee57e20f1b4350d9ec7cef1bb6dfc9/flags/es.svg" ; height=15>'
IT = '<img src="https://raw.githubusercontent.com/HatScripts/circle-flags/dd5910007aee57e20f1b4350d9ec7cef1bb6dfc9/flags/it.svg" ; height=15>'
FR = '<img src="https://raw.githubusercontent.com/HatScripts/circle-flags/dd5910007aee57e20f1b4350d9ec7cef1bb6dfc9/flags/fr.svg" ; height=15>'
DE = '<img src="https://raw.githubusercontent.com/HatScripts/circle-flags/dd5910007aee57e20f1b4350d9ec7cef1bb6dfc9/flags/de.svg" ; height=15>'
TR = '<img src="https://raw.githubusercontent.com/HatScripts/circle-flags/dd5910007aee57e20f1b4350d9ec7cef1bb6dfc9/flags/tr.svg" ; height=15>'
BR = '<img src="https://raw.githubusercontent.com/HatScripts/circle-flags/dd5910007aee57e20f1b4350d9ec7cef1bb6dfc9/flags/br.svg" ; height=15>'

# Single league selector (Players tab): choices, flag map and selectize renderer.
# Unquoted class=lg-opt keeps the JS string free of embedded double quotes.
league_choices = c(Euroleague="EL", Eurocup="EC", GBL="GR", ACB="ES", `Serie A`="IT",
                   `Pro A`="FR", BBL="DE", TBSL="TR", NBB="BR", NBA="NBA",
                   WNBA="WNBA", BCL="BCL", `Europe Cup`="FIBA")
# League option content for pickerInput: bootstrap-select renders raw HTML via
# its `content` option, so the flags survive without a selectize custom render
# (which is what mis-measured its own width on first paint).
league_icons = c(EL=EL,EC=EC,GR=GR,ES=ES,IT=IT,FR=FR,DE=DE,TR=TR,BR=BR,
                 NBA=NBA,WNBA=WNBA,BCL=BCL,FIBA=FIBA)
league_content = vapply(names(league_choices), function(nm) {
  code = unname(league_choices[[nm]])
  paste0("<span class='lg-opt'>", league_icons[[code]], "<span>", nm, "</span></span>")
}, character(1), USE.NAMES = FALSE)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Price bounds for the suggested odds. The floor stops a near-certain side from
# pricing at 1.00; the cap stops a long-shot side from showing an implausibly long
# price off a thin sample (a 1-in-40 read from 40 games is not a real 40.00). Both
# are display/pricing conventions, not model outputs.
ODDS_FLOOR = 1.01
ODDS_CAP   = 15.00

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Data:
#' [ Precomputed by bball-stats-app-build-data.R and refreshed by the two scraper
#'   workflows. The app loads one bundle (app-data.rds) instead of fetching ~20
#'   CSVs and re-running every transform on each cold start.
#'   To rebuild by hand: Rscript bball-stats/bball-stats-app-build-data.R ]
github_path = "https://raw.githubusercontent.com/filippospol/R-bball-projects/refs/heads/main/bball-stats/data/"

# Load the precomputed bundle. libcurl is the most reliable method for an https
# binary on Windows and errors on a 404 instead of silently saving the error page.
# We then verify the gzip magic (1f 8b), so a proxy/HTML page or a 404 body fails
# with a clear message instead of an opaque readRDS "unknown input format".
app_data = local({
  url = paste0(github_path, "bball-stats-app-data.rds")
  tmp = tempfile(fileext = ".rds")
  download.file(url, tmp, mode = "wb", quiet = TRUE, method = "libcurl")
  magic = tryCatch(readBin(tmp, "raw", 2), error = function(e) raw(0))
  if (!file.exists(tmp) || file.size(tmp) < 1000 ||
      !identical(magic, as.raw(c(0x1f, 0x8b)))) {
    stop("Downloaded bball-stats-app-data.rds is not a valid .rds (",
         if (file.exists(tmp)) file.size(tmp) else 0, " bytes). Likely a 404 or a ",
         "proxy/HTML page. Open this URL in a browser to check: ", url)
  }
  readRDS(tmp)
})

playersTab  = app_data$playersTab
playersAll  = app_data$playersAll
teams       = app_data$teams
teams       = map(teams, ~ select(.x, -any_of("CODE")))  # CODE not used anywhere - drop it from every team table
teamsNBA    = app_data$teamsNBA
teamsWNBA   = app_data$teamsWNBA
teamGamesFA = app_data$teamGamesFA
topMost     = app_data$topMost
elLeaders   = app_data$elLeaders
# Freshness stamp for the footer (from the build step in the .rds bundle)
data_updated = tryCatch(format(app_data$meta$built_at, "%d %b %Y"),
                        error = function(e) NA_character_)

# Players tab (per-league objects referenced by the UI inputs and observers):
playersEL  = playersTab$EL  ; playersEC     = playersTab$EC ; playersACB  = playersTab$ES
playersGBL = empty_tbl()$p  ; playersSerieA = playersTab$IT ; playersProA = playersTab$FR
playersBBL = playersTab$DE  ; playersTBSL   = playersTab$TR  ; playersNBB  = playersTab$BR
playersNBA = playersTab$NBA ; playersWNBA   = playersTab$WNBA
playersFIBA = empty_tbl()$p ; playersBCL    = empty_tbl()$p   # no data source yet

# Teams tab:
teamsEL  = teams$EL ; teamsEC     = teams$EC  ; teamsACB  = teams$ES 
teamsGBL = empty_tbl()$t ; teamsSerieA = teams$IT  ; teamsProA = teams$FR 
teamsBBL = teams$DE ; teamsTBSL   = teams$TR  ; teamsNBB  = teams$BR
teamsFIBA = empty_tbl()$t ; teamsBCL = empty_tbl()$t          # no data source yet

# League code -> table lookups (global so the UI can see which leagues have data):
players_by_league = list(
  EL=playersEL, EC=playersEC, GR=playersGBL, ES=playersACB, IT=playersSerieA,
  FR=playersProA, DE=playersBBL, TR=playersTBSL, BR=playersNBB,
  NBA=playersNBA, WNBA=playersWNBA, BCL=playersBCL, FIBA=playersFIBA
)
teams_by_league = list(
  EL=teamsEL, EC=teamsEC, GR=teamsGBL, ES=teamsACB, IT=teamsSerieA,
  FR=teamsProA, DE=teamsBBL, TR=teamsTBSL, BR=teamsNBB,
  BCL=teamsBCL, FIBA=teamsFIBA, NBA=teamsNBA, WNBA=teamsWNBA
)

# Keep only leagues whose table has rows, in the original league_choices order.
# Leagues reappear automatically once the data bundle carries rows for them.
leagues_with_data = function(lst) {
  codes = unname(league_choices)
  codes[vapply(codes, function(cd) { d = lst[[cd]]; !is.null(d) && nrow(d) > 0 },
               logical(1))]
}
pl_codes = leagues_with_data(players_by_league)
tm_codes = leagues_with_data(teams_by_league)
league_content_for = function(codes) league_content[match(codes, unname(league_choices))]
default_league = function(codes) if ("EL" %in% codes) "EL" else codes[1]

# Default Tools selections: Euroleague, plus the first (alphabetical) player and
# team that actually appear in Euroleague, so the player/team -> league cascades
# settle on Euroleague on first load. Falls back to the overall first entry if a
# data bundle ever ships without Euroleague rows.
EL_NAME = "Euroleague"
.el_players  = sort(unique(playersAll$PLAYER[playersAll$LEAGUE == EL_NAME]))
.all_players = sort(unique(playersAll$PLAYER))
first_el_player = if (length(.el_players)) .el_players[1] else .all_players[1]
.el_teams  = sort(unique(teamGamesFA$TEAM[teamGamesFA$LEAGUE == EL_NAME]))
.all_teams = sort(unique(teamGamesFA$TEAM))
first_el_team = if (length(.el_teams)) .el_teams[1] else .all_teams[1]

# ============================================================================
# Tools - pricing / probability engine (shared by the player Trend tool)
# ============================================================================

# Single-stat markets, additive combos, and the binary (Yes/No) markets.
# PRA/PR/PA/RA already exist as columns; STOCKS is computed on the fly.
TREND_SINGLES = c("MIN","PTS","2PM","2PA","3PM","3PA","FTM","FTA",
                  "DREB","OREB","REB","AST","STL","BLK","TOV","PF")
TREND_MARKETS = c("DD","TD")                      # double- / triple-double

# Grouped choices for the Stat dropdown (optgroups in selectInput).
trend_stat_choices = list(
  Singles = setNames(TREND_SINGLES, TREND_SINGLES),
  Combos  = setNames(c("PRA","PR","PA","RA","STOCKS"),
                     c("PRA","PR","PA","RA","Stocks (STL+BLK)")),
  Markets = setNames(c("DD","TD"), c("Double-double","Triple-double"))
)

# Model-toggle distribution map (used when the Model selector = "Model").
TREND_NORMAL_STATS = c("PTS","MIN","PRA","PR","PA","RA")       # sums / weighted -> Normal
TREND_NB_STATS     = c("REB","OREB","DREB","AST","2PA","3PA","FTA")  # overdispersed counts
TREND_POIS_STATS   = c("3PM","2PM","FTM","STL","BLK","TOV","PF","STOCKS")  # low-count
TREND_DIST_LABEL   = c(Empirical="Empirical", Normal="Normal",
                       Poisson="Poisson", NegBin="Neg. Binomial")

# Pick the distribution for a stat under the Model toggle. Negative Binomial is
# used for overdispersed counts, but only with enough games AND genuine
# overdispersion (sample variance > mean); otherwise it falls back to Poisson.
resolve_model = function(stat, vec, min_games = 15) {
  if (stat %in% TREND_NORMAL_STATS) return("Normal")
  if (stat %in% TREND_NB_STATS) {
    vec = vec[!is.na(vec)]
    m = if (length(vec)) mean(vec) else NA
    v = if (length(vec) > 1) var(vec) else NA
    if (length(vec) >= min_games && !is.na(v) && !is.na(m) && m > 0 && v > m)
      return("NegBin")
    return("Poisson")
  }
  "Poisson"
}

# Add the computed STOCKS column (STL+BLK) when it is not already present.
add_stocks = function(df) {
  if (!"STOCKS" %in% names(df) && all(c("STL","BLK") %in% names(df)))
    df$STOCKS = as.numeric(df$STL) + as.numeric(df$BLK)
  df
}

# Per-game double-/triple-double flags from the five counting categories.
ddtd_flags = function(df) {
  cats = c("PTS","REB","AST","STL","BLK"); cats = cats[cats %in% names(df)]
  m = sapply(cats, function(cc) as.numeric(df[[cc]]) >= 10)
  n10 = if (is.null(dim(m))) sum(m, na.rm=TRUE) else rowSums(m, na.rm=TRUE)
  list(dd = as.integer(n10 >= 2), td = as.integer(n10 >= 3))
}

# Recency blend weight: wider window -> more trust in recent form.
blend_w = function(n) n / (n + 10)

# Blended mean (and sd) over season + last-N, with the mean tilted by `tilt`.
blend_moments = function(lastn, season, w, tilt = 1) {
  lastn = lastn[!is.na(lastn)]; season = season[!is.na(season)]
  mu_l = if (length(lastn))  mean(lastn)  else NA
  mu_s = if (length(season)) mean(season) else mu_l
  mu   = if (is.na(mu_s)) mu_l else (1 - w) * mu_s + w * mu_l
  mu   = max(0, mu * tilt)
  sd_l = if (length(lastn)  > 1) sd(lastn)  else NA
  sd_s = if (length(season) > 1) sd(season) else NA
  sdv  = if (!is.na(sd_s) && !is.na(sd_l)) (1 - w) * sd_s + w * sd_l
  else if (!is.na(sd_s)) sd_s else sd_l
  list(mu = mu, sd = sdv)
}

# P(stat > line) for one line under the chosen method.
#   Empirical : season/last-N blended hit rate, tilt on the odds scale
#   Poisson   : lambda = blended (tilted) mean; 1 - ppois(floor(L), lambda)
#   Normal    : blended (tilted) mean + blended sd; 1 - pnorm(L, mean, sd)
prob_over = function(L, method, lastn, season, w, tilt = 1) {
  L = as.numeric(L)
  lastn = lastn[!is.na(lastn)]; season = season[!is.na(season)]
  if (method == "Empirical") {
    p_l = if (length(lastn))  mean(lastn  > L) else NA
    p_s = if (length(season)) mean(season > L) else NA
    p   = if (is.na(p_s)) p_l else (1 - w) * p_s + w * p_l
    if (!is.na(p) && tilt != 1 && p > 0 && p < 1)
      p = (p * tilt) / (p * tilt + (1 - p))
    return(p)
  }
  mo = blend_moments(lastn, season, w, tilt)
  if (is.na(mo$mu)) return(NA_real_)
  if (method == "Poisson") return(1 - ppois(floor(L), mo$mu))
  if (method == "NegBin") {
    # Dispersion from the broader (season) sample via method of moments:
    # var = mu + mu^2/size  ->  size = m^2 / (var - m). Fall back to Poisson
    # if the sample is not actually overdispersed.
    base = if (length(season) > 1) season else lastn
    m = mean(base, na.rm=TRUE); v = if (length(base) > 1) var(base, na.rm=TRUE) else NA
    if (is.na(v) || is.na(m) || m <= 0 || v <= m) return(1 - ppois(floor(L), mo$mu))
    size = m^2 / (v - m)
    return(1 - pnbinom(floor(L), size = size, mu = mo$mu))
  }
  if (is.na(mo$sd) || mo$sd <= 0) return(as.numeric(mo$mu > L))  # degenerate Normal
  1 - pnorm(L, mo$mu, mo$sd)
}

# Over/Under decimal prices from P(over): proportional overround + floor/cap.
prices_from_p = function(p, margin) {
  over  = if (is.na(p) || p <= 0) NA_real_ else
    min(ODDS_CAP, max(ODDS_FLOOR, round(1/((1+margin)*p), 2)))
  under = if (is.na(p) || p >= 1) NA_real_ else
    min(ODDS_CAP, max(ODDS_FLOOR, round(1/((1+margin)*(1-p)), 2)))
  c(over = over, under = under)
}

# Build the price ladder of ~ (2*span+1) lines around the blended mean.
# Returns a data.frame with a 'suggest_idx' attribute (row nearest a coin flip).
build_ladder = function(method, lastn, season, w, tilt, margin, span = 5) {
  mo = blend_moments(lastn, season, w, tilt)
  if (is.na(mo$mu)) return(NULL)
  center = round(mo$mu)
  lines  = sort(unique(pmax(0.5, seq(center - span, center + span, 1) + 0.5)))
  nlast  = length(lastn[!is.na(lastn)])
  rows = lapply(lines, function(L) {
    pp = prob_over(L, method, lastn, season, w, tilt)
    pr = prices_from_p(pp, margin)
    data.frame(Line = L,
               Hit  = paste0(sum(lastn > L, na.rm=TRUE), "/", nlast),
               `P(over)` = if (is.na(pp)) NA_real_ else round(100*pp, 1),
               Over = unname(pr["over"]), Under = unname(pr["under"]),
               check.names = FALSE)
  })
  out = do.call(rbind, rows)
  ps = vapply(lines, function(L) {
    v = prob_over(L, method, lastn, season, w, tilt)
    if (is.na(v)) Inf else abs(v - 0.5)
  }, numeric(1))
  attr(out, "suggest_idx") = if (all(is.infinite(ps))) NA_integer_ else which.min(ps)
  out
}

###

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Universal table style:
myAggregateStyle = JS("function(rowInfo) {
    if (rowInfo && rowInfo.level !== undefined) {
      if (rowInfo.level > 0) {
        return { background: '#d2d8f9' };
      } else {
        return {
          background: '#a6b1f3',
          borderLeft: '3px solid #4b57a5',
          fontWeight: 'bold',
          fontSize: '11px'
        };
      }
    }
    return {}; // default style if level is missing
  }")
myTheme = clean(header_font_size = 11,font_size = 10,font_color="#000")

# ------------------------------------------------------------------------------------------------
# Headline result cards, warning banners and the game-by-game bar chart (Trend tool).
# stat_card() renders one KPI card; cards_row() lays a set of them out as a wrapping flex row;
# warn_banner() renders an amber alert box (returns "" for empty messages so the slot collapses).
stat_card = function(label, value, sub = "", accent = FALSE) {
  bg  = if (accent) "#d2d8f9" else "#f1f3fa"
  col = if (accent) "#3a44a0" else "#5b6478"
  paste0(
    "<div style='flex:1 1 150px; min-width:150px; background:", bg,
    "; border-radius:14px; padding:13px 18px;'>",
    "<div style='font-size:12.5px; color:", col, ";'>", label, "</div>",
    "<div style='font-size:23px; font-weight:700; color:#1d2340; margin-top:1px;'>", value, "</div>",
    if (nzchar(sub)) paste0("<div style='font-size:12px; color:", col,
                            "; margin-top:2px;'>", sub, "</div>") else "",
    "</div>")
}
cards_row = function(...) paste0(
  "<div style='display:flex; flex-wrap:wrap; gap:12px; margin:6px 0 2px; max-width:860px;'>",
  paste0(c(...), collapse = ""), "</div>")
# Warning banner shown as its own row directly below the KPI cards
warn_banner = function(msg) {
  if (is.null(msg) || !nzchar(msg)) return("")
  paste0("<div style='max-width:860px; background:#fdf3e3; border:1px solid #e8c37a;",
         " border-radius:12px; padding:9px 14px; margin-top:10px; font-size:13px;",
         " line-height:1.4; color:#8a5a00; font-weight:600;'>&#9888;&#65039;&nbsp; ",
         msg, "</div>")
}

# Shared control panel for the Tools tabs: a white rounded card holding the
# selectors as a wrapping flex row, bottom-aligned on a common baseline.
# Wrap the Run button as div(style="margin-bottom:15px;", ...) so it lines up
# with the selects (shiny form-groups carry a 15px bottom margin of their own).
tool_panel = function(...) div(
  style = paste0("display:flex; flex-wrap:wrap; gap:14px; align-items:flex-end;",
                 " background:#fff; border-radius:16px; padding:12px 18px 0;",
                 " box-shadow:0 8px 26px rgba(58,68,160,.10); margin-bottom:18px;"),
  ...)

# Game-log date cell: shows only the date; the full MATCHUP value appears as a
# tooltip on hover (dotted underline hints that there is more to see).
matchup_cell = function(value) {
  d   = suppressWarnings(as.Date(substr(value, 1, 10)))
  lab = if (is.na(d)) substr(value, 1, 10) else format(d, "%d %b %y")
  htmltools::tags$span(title = value,
                       style = "cursor:help; border-bottom:1px dotted #9aa1c4;", lab)
}

# Bar chart of the last-N games against the selected line: one bar per game
# (oldest to newest), bars clearing the line in the app's indigo, the line itself
# as a clearly visible dashed marker with a label. Only rendered for N <= 15 games.
trend_bar_chart = function(df, stat_col, target) {
  plot_df = df %>%
    transmute(RAW  = MATCHUP,
              VAL  = suppressWarnings(as.numeric(.data[[stat_col]]))) %>%
    filter(!is.na(VAL)) %>%
    mutate(DATE = suppressWarnings(as.Date(substr(RAW, 1, 10))),
           LAB  = ifelse(is.na(DATE), substr(RAW, 1, 10), format(DATE, "%d %b")),
           ORD  = rev(seq_len(n())),              # oldest -> newest, left to right
           OVER = VAL > target)
  ggplot(plot_df, aes(x = factor(ORD, levels = sort(unique(ORD))), y = VAL, fill = OVER)) +
    geom_col(width = .62, show.legend = FALSE) +
    geom_hline(yintercept = target, linetype = "dashed",
               color = "#d85a30", linewidth = .9) +
    geom_text(aes(label = round(VAL, 1)), vjust = -0.55, size = 3.3, color = "#1d2340") +
    annotate("text", x = Inf, y = target, label = paste0("line ", target),
             hjust = 1.05, vjust = -0.7, color = "#d85a30", size = 3.6, fontface = "bold") +
    scale_fill_manual(values = c(`TRUE` = "#4b57c9", `FALSE` = "#c9cede")) +
    scale_x_discrete(labels = setNames(plot_df$LAB, plot_df$ORD)) +
    scale_y_continuous(expand = expansion(mult = c(0, .14))) +
    labs(x = NULL, y = stat_col) +
    theme_minimal(base_size = 12.5) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          axis.text  = element_text(color = "#5b6478"),
          axis.title.y = element_text(color = "#5b6478", size = 11),
          plot.background = element_rect(fill = "white", color = NA))
}
# ------------------------------------------------------------------------------------------------
myArrows = reactableLang(searchPlaceholder = "Search",
                         noData = "No available data (players/teams) found",
                         pageInfo = "{rowStart} to {rowEnd} of {rows} entries",
                         pagePrevious = "\u276e",
                         pageNext = "\u276f",
                         pagePreviousLabel = "Previous page",
                         pageNextLabel = "Next page")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Table preset functions:
react_players = function(mydata) {
  reactable(mydata,
            groupBy = "PLAYER",
            theme = myTheme,
            language = myArrows,
            sortable = T,
            showPageSizeOptions = T,
            defaultPageSize = 10,
            pageSizeOptions = c(5,10,15,20,25,30,35),
            columns = list(
              MATCHUP=colDef(width = 165,sortable=F,sticky="left",
                             style=list(boxShadow="2px 0 5px -2px rgba(0,0,0,.18)")),
              PLAYER=colDef(width = 210,sticky="left"),
              TEAM=colDef(width=200,sortable=F,name="TEAM",sticky="left",
                          style=list(whiteSpace="nowrap")),
              MIN = colDef(maxWidth = 55, sortable=F, aggregate = "mean",
                           format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              PTS = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              `2PM` = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              `2PA` = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              `3PM` = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              `3PA` = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              FTM = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              FTA = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              DREB = colDef(maxWidth = 65, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              OREB = colDef(maxWidth = 65, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              REB = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              AST = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              STL = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              BLK = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              TOV = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              PF = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1)))
            ),
            rowStyle = myAggregateStyle )
}
react_team2 = function(mydata,mysize,myoptions) {
  reactable(mydata %>% arrange(TEAM),
            groupBy = "TEAM",
            theme = myTheme,
            language = myArrows,
            sortable = T,
            showPageSizeOptions = T,
            defaultPageSize = mysize,
            pageSizeOptions = myoptions,
            columns = list(
              MATCHUP=colDef(width = 260,sortable=F,sticky="left",style=list(boxShadow="2px 0 5px -2px rgba(0,0,0,.18)")),
              TEAM=colDef(width=195,sortable=F,name="TEAM",sticky="left"),
              PTS = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              `2PM` = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              `2PA` = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              `3PM` = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              `3PA` = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              FTM = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              FTA = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              DREB = colDef(maxWidth = 65, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              OREB = colDef(maxWidth = 65, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              REB = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              AST = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              STL = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              BLK = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              TOV = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
              PF = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1)))
            ),
            rowStyle = myAggregateStyle )
}
react_team_us = function(mydata,mysize,myoptions) {
  reactable(mydata,
            theme = myTheme,
            language = myArrows,
            sortable = T,
            filterable = F,
            showPageSizeOptions = T,
            defaultPageSize = mysize,
            pageSizeOptions = myoptions,
            defaultSorted = list(PTS = "desc"),
            columns = list(
              TEAM = colDef(width = 200, name = "TEAM", sticky="left", style=list(boxShadow="2px 0 5px -2px rgba(0,0,0,.18)")),
              GP   = colDef(width = 50,  name = "GP"),
              PTS  = colDef(maxWidth = 55, format = colFormat(digits = 1)),
              `2PM` = colDef(maxWidth = 55, format = colFormat(digits = 1)),
              `2PA` = colDef(maxWidth = 55, format = colFormat(digits = 1)),
              `3PM` = colDef(maxWidth = 55, format = colFormat(digits = 1)),
              `3PA` = colDef(maxWidth = 55, format = colFormat(digits = 1)),
              FTM  = colDef(maxWidth = 55, format = colFormat(digits = 1)),
              FTA  = colDef(maxWidth = 55, format = colFormat(digits = 1)),
              DREB = colDef(maxWidth = 65, format = colFormat(digits = 1)),
              OREB = colDef(maxWidth = 65, format = colFormat(digits = 1)),
              REB  = colDef(maxWidth = 55, format = colFormat(digits = 1)),
              AST  = colDef(maxWidth = 55, format = colFormat(digits = 1)),
              STL  = colDef(maxWidth = 55, format = colFormat(digits = 1)),
              BLK  = colDef(maxWidth = 55, format = colFormat(digits = 1)),
              TOV  = colDef(maxWidth = 55, format = colFormat(digits = 1)),
              PF   = colDef(maxWidth = 55, format = colFormat(digits = 1))
            ))
}

font_tags = tags$head(
  tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;600;700&display=swap"
  ),
  tags$style(HTML("
      body, label, input, button, .selectize-input .panel-title {
        font-family: 'Roboto', sans-serif;
      }
      .gt_table, .gt_table th, .gt_table td {
      font-family: 'Roboto', sans-serif;
    }
    "))
)
# Shortcut icon:
icon_tags = tags$head(tags$link(rel="shortcut icon", href="https://i.ibb.co/5XhxQYSq/Chat-GPT-Image-Oct-19-2025-12-38-25-PM.png"))

# Login page styling ("Paperwhite Float" language: indigo gradient background,
# floating white card, rounded inputs, pill-shaped sign-in button):
login_tags = tags$head(tags$style(HTML("
  /* Page background */
  .panel-auth{ background:linear-gradient(160deg,#eef1f7 0%,#dfe3f7 100%) !important; }

  /* Floating white card */
  .panel-auth .panel{
    border:none !important; border-radius:18px; overflow:hidden;
    margin-top:12vh !important;
    box-shadow:0 12px 40px rgba(58,68,160,.18); }
  .panel-auth .panel-heading{
    background:#fff !important; border:none !important; padding:22px 26px 0; }
  .panel-auth .panel-body{ background:#fff; padding:20px 30px 30px; }

  /* Inputs */
  .panel-auth label{ color:#5b6478; font-weight:500; font-family:'Roboto',sans-serif; }
  .panel-auth .form-control{
    height:40px; border-radius:10px; border:1px solid #d5dbec;
    box-shadow:0 1px 4px rgba(58,68,160,.08); font-family:'Roboto',sans-serif; }
  .panel-auth .form-control:focus{
    border-color:#4b57c9; box-shadow:0 0 0 3px rgba(75,87,201,.15); }

  /* Sign-in button */
  .panel-auth .btn-primary{
    background:#4b57c9; border:none; border-radius:999px;
    font-weight:600; letter-spacing:.3px; padding:8px 0;
    box-shadow:0 4px 12px rgba(75,87,201,.4);
    transition:background .15s ease, box-shadow .15s ease; }
  .panel-auth .btn-primary:hover,
  .panel-auth .btn-primary:focus{
    background:#3a44a0; box-shadow:0 6px 16px rgba(58,68,160,.45); }
")))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# User Interface:
#' *To remove the login page, just get everything out of the secure_app() function*
ui = suppressWarnings(secure_app(
  theme=myTheme,
  head_auth = list(font_tags,icon_tags,login_tags,tags$title("Basketball Stats")),
  navbarPage(
    
    font_tags,
    
    # Navigation bar and title:
    # (navbar colors are handled inside the variant style block below)
    title =  strong("Basketball Stats", style = "color: #3a44a0" ),
    windowTitle = "Basketball Stats",
    footer = div(class = "app-credit",
                 div("Application developed by ",
                     a(href = "https://filippospol.github.io/R-bball-projects/", "Filippos Polyzos", target = "_blank")),
                 if (!is.na(data_updated)) div(class = "app-updated", "Data updated: ", data_updated)),
    tags$head(tags$style(HTML("a {color: #404b8d}"))),
    tags$script(HTML('
           $( document ).on("shiny:sessioninitialized", function(event) {
                $(\'span[data-toggle="tooltip"]\').tooltip({
                    html: true
                });
           });'
    )),
    
    # Paperwhite variation B - "Paperwhite Float": same white-navbar pill language, but soft
    # indigo shadows make the navbar, pills and cards float. Same colorway & Roboto.
    tags$style(HTML("
  :root{ --accent:#4b57c9; --accent-dark:#3a44a0; --bg:#eef1f7; --card:#fff; }
  body{ background:var(--bg); }

  /* Floating white navbar */
  .navbar-static-top, .navbar-default{
    background:#fff; border:none; box-shadow:0 4px 18px rgba(58,68,160,.14);
    position:sticky; top:0; z-index:1050; }
  .navbar-default .navbar-brand{ color:var(--accent-dark)!important; font-weight:700; letter-spacing:.3px; }
  .navbar-default .navbar-brand strong{ color:var(--accent-dark)!important; }
  .navbar-default .navbar-nav>li>a{
    color:#5b6478!important; font-weight:500; border-radius:999px;
    margin:9px 3px; padding:6px 16px; line-height:1.6;
    transition:box-shadow .15s ease, background .15s ease; }
  .navbar-default .navbar-nav>li>a:hover{
    background:#fff!important; color:var(--accent-dark)!important;
    box-shadow:0 2px 8px rgba(58,68,160,.2); }
  .navbar-default .navbar-nav>.active>a,
  .navbar-default .navbar-nav>.active>a:focus,
  .navbar-default .navbar-nav>.active>a:hover{
    background:var(--accent)!important; color:#fff!important; border-radius:999px;
    box-shadow:0 4px 12px rgba(75,87,201,.4); }

  /* Sub-tabs: floating segmented control */
  .nav-tabs{ border:none; background:#fff; display:inline-flex; padding:4px;
    border-radius:999px; gap:2px; box-shadow:0 2px 10px rgba(58,68,160,.12); }
  .nav-tabs>li>a{ border:none!important; border-radius:999px!important; color:#5b6478;
    padding:5px 14px; margin:0; }
  .nav-tabs>li>a:hover{ background:#dfe3f7; }
  .nav-tabs>li.active>a,.nav-tabs>li.active>a:hover,.nav-tabs>li.active>a:focus{
    background:var(--accent); color:#fff; font-weight:600;
    box-shadow:0 2px 8px rgba(75,87,201,.45); }

  /* Uniform gap between every pill toggle and the content below it
     (league pills -> filters, league pills -> Team/Opponent toggle,
      Team/Opponent -> filters, Leaders/Tools toggles -> filters).
     .nav-tabs + .tab-content only exists inside tabsetPanels, so the
     navbar and page layout are unaffected. */
  .nav-tabs + .tab-content > .tab-pane{ padding-top:14px; }

  /* View switcher (Tools > Trend): segmented pills, same look as the sub-tabs */
  .view-switch .btn-group{ background:#fff; display:inline-flex; padding:4px; gap:2px;
    border-radius:999px; box-shadow:0 2px 10px rgba(58,68,160,.12); }
  .view-switch .btn{ border:none!important; border-radius:999px!important; color:#5b6478;
    background:transparent; font-size:13px; padding:5px 16px; box-shadow:none!important; }
  .view-switch .btn:hover{ background:#dfe3f7; color:#5b6478; }
  .view-switch .btn.active,.view-switch .btn.active:hover,.view-switch .btn.active:focus{
    background:var(--accent)!important; color:#fff!important; font-weight:600; }

  /* Elevated cards */
  .selectize-input,.form-control{ border-radius:10px; border:1px solid #d5dbec;
    box-shadow:0 1px 4px rgba(58,68,160,.08); }
  .selectize-input.focus{ border-color:var(--accent); box-shadow:0 0 0 3px rgba(75,87,201,.15); }
  .btn-default{ border-radius:999px; border-color:var(--accent); color:var(--accent);
    box-shadow:0 1px 4px rgba(58,68,160,.12); }
  .btn-default:hover{ background:var(--accent); color:#fff; }
  .reactable,.Reactable,.html-widget{
    background:var(--card); border:none; border-radius:16px; padding:8px;
    box-shadow:0 8px 26px rgba(58,68,160,.14);
    max-width:100%; overflow-x:auto; }
  h5{ color:var(--accent-dark); font-weight:600; }
  a{ color:#404b8d; }

  /* League selector option: flag + name */
  .lg-opt{ display:flex; align-items:center; gap:8px; }
  .lg-opt img{ width:18px; height:18px; flex:0 0 18px; object-fit:contain; border-radius:2px; }

  /* pickerInput (bootstrap-select) to match the rounded input language */
  .bootstrap-select > .btn{ border-radius:10px !important; border:1px solid var(--line) !important;
    background:#fff !important; color:#333 !important; box-shadow:0 1px 4px rgba(58,68,160,.08); }
  .bootstrap-select .dropdown-menu{ border-radius:10px; border:1px solid var(--line);
    box-shadow:0 8px 26px rgba(58,68,160,.14); }
  .bootstrap-select .dropdown-menu li a{ padding:6px 12px; }

  /* Footer credit */
  .app-credit{ text-align:center; color:var(--muted); font-size:12px;
    padding:20px 0 26px; margin-top:26px; border-top:1px solid var(--line); }
  .app-credit a{ color:var(--accent); font-weight:600; text-decoration:none; }
  .app-credit a:hover{ text-decoration:underline; }
  .app-updated{ margin-top:4px; opacity:.75; }
")),
    
    # Shortcut icon:
    icon_tags,
    
    # Tabs:
    tabPanel( "Players",
              fluidRow(
                column(3, pickerInput("pl_league", "League",
                                      choices = pl_codes, selected = default_league(pl_codes),
                                      width = "100%",
                                      choicesOpt = list(content = league_content_for(pl_codes)))),
                column(3, selectizeInput("pl_player", "Player(s)", choices = NULL, multiple = TRUE)),
                column(3, selectizeInput("pl_team", "Team(s)", choices = NULL, multiple = TRUE))
              ),
              withSpinner(reactableOutput("players_table", width="100%"),
                          color="#4b57c9", color.background="#FFFFFF", type=3)
    ),
    tabPanel( "Teams",
              fluidRow(
                column(3, pickerInput("tm_league", "League",
                                      choices = tm_codes, selected = default_league(tm_codes),
                                      width = "100%",
                                      choicesOpt = list(content = league_content_for(tm_codes)))),
                column(3, conditionalPanel(
                  "input.tm_league != 'NBA' && input.tm_league != 'WNBA'",
                  radioButtons("tm_side", "View", inline = TRUE,
                               choices = c("Team","Opponent"), selected = "Team"))),
                column(3, selectizeInput("tm_team", "Team(s)", choices = NULL, multiple = TRUE)),
                column(3, conditionalPanel(
                  "input.tm_league == 'NBA' || input.tm_league == 'WNBA'",
                  uiOutput("tm_boxscore_link")))
              ),
              withSpinner(reactableOutput("teams_table", width="100%"),
                          color="#4b57c9", color.background="#FFFFFF", type=3)
    ),
    tabPanel(title="Stats Leaders", br(),
             tabsetPanel(
               tabPanel("NBA Top / Most",
                        fluidRow(column(2, offset = 0,
                                        selectInput("TM_team", "Team(s)", sort(unique(topMost$TEAM)),
                                                    multiple = TRUE))),
                        withSpinner(reactableOutput("top_most", width="950px"), color="#4b57c9", color.background="#FFFFFF", type=3)),
               tabPanel("Domestic / Euroleague",
                        fluidRow(column(2, offset = 0,
                                        selectInput("EL_team3", "Team(s)", sort(unique(elLeaders$TEAM)),
                                                    multiple = TRUE))),
                        withSpinner(reactableOutput("el_leaders", width="900px"), color="#4b57c9", color.background="#FFFFFF", type=3))
             )),
    tabPanel(title="Tools",
             tabsetPanel(
               tabPanel("Trend",br(),
                        # Controls laid out with flexbox: inputs keep a fixed width,
                        # wrap on narrow screens, and bottom-align with the Run button.
                        # Hidden inputs (Line for DD/TD, Opponent for NBA/WNBA) collapse
                        # cleanly instead of leaving a reserved empty column.
                        tool_panel(
                          div(style="flex:1 1 220px; min-width:200px; max-width:420px;",
                              selectizeInput("trend_players", "Player", choices = NULL, width="100%")),
                          div(selectInput("trend_season", "Season",
                                          sort(unique(unique(playersAll$SEASON))),
                                          selected="2026-27", width="140px")),
                          div(selectInput("trend_league", "League", unique(playersAll$LEAGUE),
                                          selected=EL_NAME, width="140px")),
                          div(selectInput("trend_stat", "Stat", trend_stat_choices,
                                          selected="3PM", width="170px")),
                          div(selectInput("trend_method", "Model", c("Empirical","Model"),
                                          selected="Empirical", width="130px")),
                          conditionalPanel(
                            condition = "input.trend_stat != 'DD' && input.trend_stat != 'TD'",
                            selectInput("trend_value", "Line", seq(0.5,55.5,1),
                                        selected=0.5, width="100px")),
                          div(selectInput("trend_overround", "Overround",
                                          setNames(1:10, paste0(1:10,"%")), selected=5, width="110px")),
                          div(radioButtons("last_n", "Last _ games", inline=T,
                                           choices = c(5,10,15,20,"All"), selected = 5)),
                          conditionalPanel(
                            condition = "input.trend_league != 'NBA' && input.trend_league != 'WNBA'",
                            selectInput("trend_opponent", "Opponent (optional)",
                                        choices = "None", width="180px")),
                          div(style="margin-bottom:15px;",
                              actionButton("trend_run", "Run", icon = icon("arrow-right")))
                        ),
                        htmlOutput("trend_wnba_note"),
                        # Headline row: KPI cards and any warning banners share one line
                        htmlOutput("trend_cards"),
                        htmlOutput("trend_model_note"),
                        # View switcher: one panel shown at a time below the headline cards.
                        # conditionalPanel toggles client-side, so switching is instant and
                        # the hidden tables stay rendered. The Chart option is removed by the
                        # server when the window is 20 / All (no chart for > 15 games).
                        div(class="view-switch", style="margin:14px 0 10px;",
                            radioGroupButtons("trend_view", NULL,
                                              choices  = c("Chart", "Price ladder", "Game log",
                                                           "Season vs recent"),
                                              selected = "Chart", size="sm")),
                        conditionalPanel(condition = "input.trend_view == 'Chart'",
                                         uiOutput("trend_chart_ui")),
                        conditionalPanel(condition = "input.trend_view == 'Price ladder'",
                                         withSpinner(reactableOutput("trend_ladder", width="auto"),
                                                     color="#4b57c9", color.background="#FFFFFF", type=3)),
                        conditionalPanel(condition = "input.trend_view == 'Game log'",
                                         reactableOutput("trend_table", width="auto")),
                        conditionalPanel(condition = "input.trend_view == 'Season vs recent'",
                                         reactableOutput("trend_avg_table", width="auto"))
               ),
               tabPanel("Team Trend",br(),
                        tool_panel(
                          div(style="flex:1 1 220px; min-width:200px; max-width:420px;",
                              selectInput("trendT_team", "Team",
                                          sort(unique(teamGamesFA$TEAM)),
                                          selected=first_el_team, width="100%")),
                          div(selectInput("trendT_league", "League",
                                          sort(unique(teamGamesFA$LEAGUE)), width="140px")),
                          div(radioButtons("trendT_side","Side",inline=T,
                                           choices=c("For","Against"),selected="For")),
                          div(selectInput("trendT_stat", "Stat",
                                          names(playersAll)[7:21],selected="PTS", width="110px")),
                          div(selectInput("trendT_value","Line",
                                          seq(0.5,150.5,1), selected=0.5, width="100px")),
                          div(selectInput("trendT_overround","Overround",
                                          setNames(1:10, paste0(1:10,"%")), selected=5, width="110px")),
                          div(radioButtons("trendT_lastn", "Last _ games",inline=T,
                                           choices = c(5,10,15,20,"All"),selected = 5)),
                          div(style="margin-bottom:15px;",
                              actionButton("trendT_run","Run",icon = icon("arrow-right")))
                        ),
                        p(HTML("<span style='color:#3a44a0; font-size:13px; font-style:italic;'>Note that the NBA and WNBA are not available in this tool, as their team data contains season averages only (no game-by-game logs). Please look at the official websites for team game logs.</span>")),
                        htmlOutput("trendT_odds"),
                        htmlOutput("trendT_prices"),
                        htmlOutput("trendT_line_warning"),br(),
                        reactableOutput("trendT_avg_table", width="1005px"),br(),
                        withSpinner(reactableOutput("trendT_table", width="600px"), color="#4b57c9", color.background="#FFFFFF", type=3),br()
               ),
               tabPanel("On/Off",br(),
                        tool_panel(
                          div(style="flex:1 1 220px; min-width:200px; max-width:420px;",
                              selectizeInput("onoff_p1", html('Player <span style="color:green">ON</span>'),
                                             choices = NULL, width="100%")),
                          div(selectInput("onoff_season", "Season",
                                          sort(unique(unique(playersAll$SEASON))),
                                          selected="2025-26", width="120px")),
                          div(selectInput("onoff_team", "Team",
                                          sort(unique(unique(playersAll$TEAM))), width="220px")),
                          div(selectInput("onoff_league", "League",unique(playersAll$LEAGUE),
                                          selected=EL_NAME, width="140px")),
                          div(style="flex:1 1 240px; min-width:220px;",
                              selectizeInput("onoff_p2", html('Player(s) <span style="color:red">OFF</span>'),
                                             choices = NULL, multiple = TRUE, width="100%")),
                          div(style="margin-bottom:15px;",
                              actionButton("onoff_run","Run",icon = icon("arrow-right")))
                        )
                        ,br(),withSpinner(reactableOutput("onofftable", width="100%"), color="#4b57c9", color.background="#FFFFFF", type=3)
               )
             )
    ),
    tabPanel(title="About",br(),
             div(style="max-width:950px; background:#fff; border-radius:16px; padding:24px 30px; box-shadow:0 8px 26px rgba(58,68,160,.14); font-size:13px; line-height:1.65;",
                 h4("About Basketball Stats", style="color:#3a44a0; font-weight:700; margin-top:0;"),
                 p("Basketball Stats takes the box scores from games around the world and turns
                    them into clean, searchable tables and quick pricing tools - so you can put
                    player and team props together in seconds. It currently covers the Euroleague,
                    Eurocup, GBL, ACB, Serie A, Pro A, BBL, TBSL, NBB, NBA and WNBA."),
                 h5("Players"),
                 p("Every game a player has had this season, in one table. Each player's row shows
                    their per-game averages; click it to open up the individual games behind those
                    averages. You can filter by one or more players or teams."),
                 h5("Teams"),
                 p("The same idea, for teams. The ", strong("Team"), " view shows what a team did;
                    the ", strong("Opponent"), " view shows what it let the other side do. (NBA and
                    WNBA show season averages rather than game-by-game.)"),
                 h5("Stats Leaders"),
                 p("Season leaderboards - one for the NBA, one for the European and domestic
                    leagues. Sort by any column or filter by team to see who leads in each stat."),
                 h5("Tools"),
                 p(strong("Trend"), " suggests Over/Under odds for a player stat - points, threes,
                    rebounds and so on - from how often that player has cleared the line. ",
                   strong("Team Trend"), " does the same for a team, either what it scores or what
                    it gives up. ", strong("On/Off"), " shows how a player's numbers change when a
                    chosen teammate is on the floor versus sitting out."),
                 p(strong("The price ladder."), " In the Trend tool you don't just price one line -
                    you get a whole ladder of lines around the player's recent average, with the
                    most likely main line highlighted, so you can read the full market at a glance.
                    As well as the usual stats you can price combos (points + rebounds + assists and
                    the like), steals + blocks, and double- or triple-doubles."),
                 h5("How the odds are worked out"),
                 p(strong("1. How likely is it?"), " The app looks at how often the player has
                    beaten the line, both over their recent games and over the whole season, and
                    blends the two. The more recent games you ask for, the more weight recent form
                    gets; choose 'All' and it simply uses the season."),
                 p(strong("2. A couple of nudges (players only)."), " It then adjusts the number
                    slightly for minutes - whether the player is on court more or less than usual
                    lately - and, if you pick an opponent, for how easy or tough that opponent is
                    for the stat you're pricing."),
                 p(strong("3. Turning it into odds."), " Finally it adds the margin you choose (the
                    overround), so the Over and Under prices together come to a little over 100%,
                    just like a real book."),
                 p(strong("Empirical or Model?"), " ", strong("Empirical"), " simply counts how
                    often it actually happened. ", strong("Model"), " smooths that out by fitting a
                    typical scoring pattern to the player, which gives steadier prices when there
                    are only a handful of games to go on. Use whichever you trust more for the
                    situation - the one in use is shown next to the result."),
                 h5("Good to know"),
                 p("A bet that has never happened in the games on record shows a dash instead of a
                    huge price, and all prices stay within a sensible 1.01 to 15.00 range. Nothing
                    updates until you press ", strong("Run"), ", so the probability and the prices
                    always describe the same set of games. Early in the season, when there is little
                    to go on, the tool flags it and suggests leaning on the full season rather than
                    a short recent run."),
                 div(style="background:#dfe3f7; border-left:4px solid #4b57c9; border-radius:8px;
                            padding:12px 16px; margin-top:16px; color:#3a44a0; font-weight:600;",
                     "These are reference prices built from past box scores only. They know nothing
                      about injuries, rest, line-ups, pace or how the market is moving - always
                      apply trading judgement on top.")
             )
    ))
))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Server:
server = function(input, output, session) {
  
  #' *To remove the login page, just delete the rows below*
  # Authentication
  res_auth = secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # Server-side selectize for the Tools player pickers: the full player universe
  # is streamed on demand instead of rendered into the page. onoff_p2 is filled
  # by its own cascade below.
  full_players = sort(unique(playersAll$PLAYER))
  updateSelectizeInput(session, "trend_players", choices = full_players,
                       selected = first_el_player, server = TRUE)
  updateSelectizeInput(session, "onoff_p1", choices = full_players,
                       selected = first_el_player, server = TRUE)
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # Tools - Trend:
  trendSettings = reactiveValues()
  
  # Update inputs after selecting player
  observeEvent(list(input$trend_players), {
    
    req(input$trend_players)
    updateSelectInput(session, "trend_season",
                      choices = playersAll %>% filter(PLAYER==input$trend_players) %>% 
                        pull(SEASON) %>% sort()
    )
  })
  observeEvent(list(input$trend_players,input$trend_season), {
    
    req(input$trend_players,input$trend_season)
    lg_trend = playersAll %>%
      filter(PLAYER==input$trend_players & SEASON==input$trend_season) %>%
      pull(LEAGUE) %>% unique() %>% sort()
    updateSelectInput(session, "trend_league", choices = lg_trend,
                      selected = if (EL_NAME %in% lg_trend) EL_NAME else lg_trend[1])
  })
  
  # Opponent choices follow the selected league. The filter is hidden for the
  # NBA and WNBA (no team game logs), and resetting it to "None" here makes
  # sure no opponent from a previous league selection lingers in the input.
  observeEvent(input$trend_league, {
    req(input$trend_league)
    opps = if (input$trend_league %in% c("NBA","WNBA")) character(0)
    else teamGamesFA %>% filter(LEAGUE==input$trend_league) %>%
      pull(TEAM) %>% unique() %>% sort()
    updateSelectInput(session, "trend_opponent",
                      choices = c("None", opps), selected = "None")
  })
  
  # run query
  observeEvent(input$trend_run, {
    
    stat       = input$trend_stat
    is_ddtd    = stat %in% TREND_MARKETS
    method_sel = input$trend_method
    
    trendFull = playersAll %>%
      filter(PLAYER==input$trend_players & LEAGUE==input$trend_league &
               SEASON==input$trend_season) %>%
      add_stocks()
    req(nrow(trendFull) > 0)
    
    if (input$last_n=="All") vals = nrow(trendFull) else
      vals = min(as.numeric(input$last_n), nrow(trendFull))
    is_all   = input$last_n=="All"
    w_recent = blend_w(vals)
    margin   = as.numeric(input$trend_overround) / 100
    
    # Minutes factor (shared): recent minutes vs season minutes, capped [0.7,1.3]
    min_season = mean(as.numeric(trendFull$MIN), na.rm=TRUE)
    min_recent = mean(head(as.numeric(trendFull$MIN), vals), na.rm=TRUE)
    min_factor = if (is_all || is.na(min_season) || min_season==0 || is.na(min_recent)) 1 else
      max(0.7, min(1.3, min_recent/min_season))
    
    season_n = nrow(trendFull)
    trendSettings$smallwarn = if (season_n <= 10 && season_n > 0) {
      paste0("Small sample: this player has only ", season_n, " games on record. ",
             "It is better not to rely on the Last N window here - consider ",
             "selecting 'All' instead.")
    } else { "" }
    
    # Season vs last-N averages, transposed: stats as rows beside a signed delta,
    # with the selected market stat pinned and highlighted on top. Sits next to
    # the price ladder and game log instead of as a wide strip at the bottom.
    trendStats = trendFull %>% select(MIN:PF)
    season_avg = trendStats %>%
      summarise(across(everything(), ~ mean(as.numeric(.), na.rm=TRUE)))
    avg_long   = tibble(STAT = names(season_avg), Season = as.numeric(season_avg[1, ]))
    last_lab   = paste0("Last ", vals)
    if (!is_all) {
      lastn_avg = trendStats %>% head(vals) %>%
        summarise(across(everything(), ~ mean(as.numeric(.), na.rm=TRUE)))
      avg_long[[last_lab]] = as.numeric(lastn_avg[1, ])
      avg_long[["Diff"]]   = avg_long[[last_lab]] - avg_long$Season
    }
    pin = stat %in% avg_long$STAT
    if (pin) avg_long = avg_long %>% arrange(STAT != stat)
    avg_cols = list(
      STAT   = colDef(name = "", width = 60, align = "left",
                      style = list(fontWeight = "bold")),
      Season = colDef(name = paste0("Season (", nrow(trendStats), " g)"),
                      width = 90, format = colFormat(digits = 1)))
    if (!is_all) {
      avg_cols[[last_lab]] = colDef(width = 80, format = colFormat(digits = 1))
      avg_cols[["Diff"]]   = colDef(
        name = "\u0394", width = 70,
        cell = function(value) sprintf("%+.1f", value),
        style = function(value) list(
          fontWeight = "bold",
          color = if (is.na(value) || abs(value) < .05) "#5b6478" else
            if (value > 0) "#0a7d32" else "#c62828"))
    }
    trendSettings$avg_table = reactable(avg_long, theme=myTheme, language=myArrows,
                                        sortable=F, pagination=F, fullWidth=FALSE,
                                        defaultColDef=colDef(align="center"),
                                        columns=avg_cols,
                                        rowStyle=function(index)
                                          if (pin && index == 1)
                                            list(background = "#d2d8f9", fontWeight = "bold"))
    
    # ---- Double-double / Triple-double (binary Yes/No market) -------------
    if (is_ddtd) {
      fl  = ddtd_flags(trendFull)
      vec_season = if (stat=="DD") fl$dd else fl$td
      vec_lastn  = head(vec_season, vals)
      p = if (is_all) mean(vec_season) else
        (1 - w_recent)*mean(vec_season) + w_recent*mean(vec_lastn)
      pr  = prices_from_p(p, margin)
      lbl = if (stat=="DD") "Double-double" else "Triple-double"
      ovr_lab = paste0(100 + as.numeric(input$trend_overround), "% overround")
      trendSettings$cards = c(
        stat_card(paste0("Hit rate (last ", length(vec_lastn), ")"),
                  paste0(sum(vec_lastn), " / ", length(vec_lastn)),
                  paste0("Season: ", sum(vec_season), " / ", length(vec_season))),
        stat_card(paste0("P(", tolower(lbl), ")"), paste0(round(100*p, 1), "%"),
                  "Observed hit rate"),
        stat_card(paste0(lbl, ": Yes"),
                  ifelse(is.na(pr["over"]),  "—", format(pr["over"],  nsmall=2)),
                  ovr_lab, accent=TRUE),
        stat_card(paste0(lbl, ": No"),
                  ifelse(is.na(pr["under"]), "—", format(pr["under"], nsmall=2)),
                  ovr_lab, accent=TRUE))
      trendSettings$chart = NULL   # binary market: no line to chart against
      trendSettings$linewarn  = ""
      trendSettings$modelnote = if (method_sel == "Model")
        "The Model toggle does not apply to double/triple-double; these use the observed hit rate directly." else ""
      lad = data.frame(Market = lbl,
                       Hit = paste0(sum(vec_lastn), "/", length(vec_lastn)),
                       `P(yes)` = round(100*p, 1),
                       Yes = unname(pr["over"]), No = unname(pr["under"]),
                       check.names = FALSE)
      trendSettings$ladder = reactable(lad, theme=myTheme, language=myArrows,
                                       sortable=F, pagination=F, fullWidth=FALSE,
                                       defaultColDef=colDef(align="center"),
                                       columns=list(Market=colDef(minWidth=150, align="left"),
                                                    `P(yes)`=colDef(width=110, na="—", format=colFormat(suffix="%", digits=1)),
                                                    Yes=colDef(width=100, na="—", format=colFormat(digits=2)),
                                                    No=colDef(width=100, na="—", format=colFormat(digits=2))))
      hit_rows = head(if (stat=="DD") fl$dd else fl$td, vals)
      show = trendFull %>% head(vals) %>% transmute(MATCHUP, PTS, REB, AST, STL, BLK)
      trendSettings$table = reactable(show, theme=myTheme, language=myArrows,
                                      showPageSizeOptions=F, defaultPageSize=vals, fullWidth=FALSE,
                                      defaultColDef=colDef(width=52, sortable=F, align="center"),
                                      columns=list(MATCHUP=colDef(name="DATE", width=95, sortable=F,
                                                                  align="left", cell=matchup_cell)),
                                      rowStyle=function(index) if (!is.na(hit_rows[index]) && hit_rows[index]==1)
                                        list(background="#98a4f1", color="white"))
      return(invisible(NULL))
    }
    
    # ---- Line markets (singles + additive combos, incl. STOCKS) ----------
    target    = as.numeric(input$trend_value)
    mycol     = stat
    extra_col = if (stat=="3PM") "3PA" else if (stat=="2PM") "2PA" else NULL
    
    season_vec = as.numeric(trendFull[[mycol]]); season_vec = season_vec[!is.na(season_vec)]
    lastn_vec  = head(season_vec, vals)
    
    # Resolve the distribution for the Model toggle now that we have the sample
    # (Negative Binomial needs the stat's dispersion and enough games).
    method = if (method_sel == "Empirical") "Empirical" else
      resolve_model(mycol, season_vec)
    
    # Opponent factor (base team stats only; hidden for NBA/WNBA)
    opp_factor = 1
    if (!is.null(input$trend_opponent) && input$trend_opponent != "None" &&
        mycol %in% names(playersAll)[7:21] &&
        !(input$trend_league %in% c("NBA","WNBA"))) {
      conc = teamGamesFA %>% filter(LEAGUE==input$trend_league)
      opp_col = paste0("OPP_", mycol)
      lg_avg  = mean(conc[[opp_col]], na.rm=TRUE)
      op_avg  = mean(conc[[opp_col]][conc$TEAM==input$trend_opponent], na.rm=TRUE)
      if (!is.na(lg_avg) && lg_avg > 0 && !is.na(op_avg))
        opp_factor = max(0.7, min(1.3, op_avg/lg_avg))
    }
    tilt = min_factor * opp_factor
    
    # Headline probability at the chosen line
    p_over     = prob_over(target, method, lastn_vec, season_vec, w_recent, tilt)
    hits_lastn = sum(lastn_vec  > target, na.rm=TRUE)
    hits_seas  = sum(season_vec > target, na.rm=TRUE)
    meth_tag   = if (method=="Empirical") "" else
      paste0(" &nbsp;|&nbsp; <em>", TREND_DIST_LABEL[[method]], "</em>")
    pr = prices_from_p(p_over, margin); over_price = pr["over"]; under_price = pr["under"]
    
    # Headline cards: hit rate, blended probability, and the two suggested prices
    hit_lab  = if (is_all) "Hit rate (season)" else paste0("Hit rate (last ", vals, ")")
    hit_sub  = if (is_all) paste0(length(season_vec), " games") else
      paste0("Season: ", hits_seas, " / ", length(season_vec))
    meth_lab = if (method == "Empirical") "Empirical blend" else TREND_DIST_LABEL[[method]]
    ovr_lab  = paste0(100 + as.numeric(input$trend_overround), "% overround")
    trendSettings$cards = c(
      stat_card(hit_lab, paste0(hits_lastn, " / ", vals), hit_sub),
      stat_card(paste0("P(over ", target, ")"), paste0(round(100*p_over, 1), "%"), meth_lab),
      stat_card(paste0("Over ",  target),
                ifelse(is.na(over_price),  "—", format(over_price,  nsmall=2)),
                ovr_lab, accent=TRUE),
      stat_card(paste0("Under ", target),
                ifelse(is.na(under_price), "—", format(under_price, nsmall=2)),
                ovr_lab, accent=TRUE))
    
    trendSettings$linewarn = if (any(is.na(c(over_price, under_price))) ||
                                 any(c(over_price, under_price) >= 2.5, na.rm=TRUE)) {
      paste0("Warning: this price is far from even money. Under normal ",
             "circumstances this would be an alternate line rather than the ",
             "main one - check the team's injury report and lineup news ",
             "before using these odds.")
    } else { "" }
    
    trendSettings$modelnote = if (method_sel == "Model") {
      paste0("Model prices use a ", TREND_DIST_LABEL[[method]],
             " distribution (auto-selected for ", mycol, ").")
    } else { "" }
    
    # Price ladder around the blended mean, suggested main line highlighted
    lad  = build_ladder(method, lastn_vec, season_vec, w_recent, tilt, margin)
    sidx = attr(lad, "suggest_idx")
    trendSettings$ladder = reactable(lad, theme=myTheme, language=myArrows,
                                     sortable=F, pagination=F, fullWidth=FALSE,
                                     defaultColDef=colDef(align="center"),
                                     columns=list(
                                       Line=colDef(width=70, format=colFormat(digits=1)),
                                       Hit=colDef(minWidth=70),
                                       `P(over)`=colDef(width=100, na="—", format=colFormat(suffix="%", digits=1)),
                                       Over=colDef(width=90, na="—", format=colFormat(digits=2)),
                                       Under=colDef(width=90, na="—", format=colFormat(digits=2))),
                                     rowStyle=function(index) {
                                       if (!is.na(sidx) && index==sidx) list(background="#a6b1f3", fontWeight="bold")
                                       else list(background="#eef1f7")
                                     })
    
    # Game log for the selected line (highlight games clearing it)
    trendAll = trendFull %>%
      select(all_of(unique(c("MATCHUP","MIN",mycol,extra_col))))
    hl = as.numeric(trendAll[[mycol]])
    
    # Game-by-game bar chart vs the line, capped at 15 games so the bars stay
    # readable; for 20 / All the chart slot collapses and the game log remains.
    if (vals <= 15) {
      trendSettings$chart = trend_bar_chart(trendAll %>% head(vals), mycol, target)
      trendSettings$chart_title = paste0(mycol, " by game vs line ", target,
                                         " (last ", vals, ", oldest to newest)")
    } else {
      trendSettings$chart = NULL
    }
    trendSettings$table = reactable(trendAll %>% head(vals),
                                    theme=myTheme, language=myArrows,
                                    showPageSizeOptions=F, defaultPageSize=vals, fullWidth=FALSE,
                                    defaultColDef=colDef(width=62, sortable=F, align="center"),
                                    columns=list(MATCHUP=colDef(name="DATE", width=95, sortable=F,
                                                                align="left", cell=matchup_cell)),
                                    rowStyle=function(index) {
                                      if (!is.na(hl[index]) && hl[index] > target) list(background="#98a4f1", color="white")
                                    })
    
  })
  
  # Live note when the WNBA is selected (data source may lag the schedule)
  output$trend_wnba_note = renderText({
    req(input$trend_league)
    if (input$trend_league == "WNBA") {
      paste0("<span style='color:#3a44a0; font-size:13px; font-style:italic;'>",
             "Note that the most recent games may be missing. ",
             "Please look at the official website for all games played.</span>")
    } else {
      ""
    }
  })
  
  output$trend_table = renderReactable(trendSettings$table)
  output$trend_cards = renderText(paste0(cards_row(trendSettings$cards),
                                         warn_banner(trendSettings$smallwarn),
                                         warn_banner(trendSettings$linewarn)))
  output$trend_chart_ui = renderUI({
    if (is.null(trendSettings$chart)) return(NULL)
    div(style="max-width:860px;",
        h5(trendSettings$chart_title),
        div(style="background:#fff; border-radius:16px; padding:12px 16px 4px;
                   box-shadow:0 8px 26px rgba(58,68,160,.10);",
            plotOutput("trend_chart", height="360px")))
  })
  
  # Drop the Chart view when there is no chart (last 20 / All, or DD/TD markets)
  # and restore it otherwise, keeping the user's current pick where still valid.
  observeEvent(trendSettings$chart, ignoreNULL = FALSE, {
    all_views = c("Chart", "Price ladder", "Game log", "Season vs recent")
    views = if (is.null(trendSettings$chart)) all_views[-1] else all_views
    cur   = isolate(input$trend_view)
    updateRadioGroupButtons(session, "trend_view", choices = views,
                            selected = if (!is.null(cur) && cur %in% views) cur else views[1])
  })
  output$trend_chart = renderPlot({ req(trendSettings$chart); trendSettings$chart }, res = 100)
  output$trend_avg_table = renderReactable(trendSettings$avg_table)
  output$trend_ladder = renderReactable(trendSettings$ladder)
  output$trend_model_note = renderText(paste0(
    "<span style='color:#3a44a0; font-size:13px; font-style:italic;'>",
    trendSettings$modelnote, "</span>"))
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # Tools - Team Trend:
  trendTSettings = reactiveValues()
  
  # Cascading input update: league follows team
  observeEvent(input$trendT_team, {
    req(input$trendT_team)
    lg_trendT = teamGamesFA %>%
      filter(TEAM==input$trendT_team) %>%
      pull(LEAGUE) %>% unique() %>% sort()
    updateSelectInput(session, "trendT_league", choices = lg_trendT,
                      selected = if (EL_NAME %in% lg_trendT) EL_NAME else lg_trendT[1])
  })
  
  observeEvent(input$trendT_run, {
    
    df = teamGamesFA %>%
      filter(TEAM==input$trendT_team & LEAGUE==input$trendT_league) %>%
      arrange(desc(MATCHUP))
    req(nrow(df) > 0)
    
    mycol    = input$trendT_stat
    side_col = if (input$trendT_side=="For") mycol else paste0("OPP_", mycol)
    target   = as.numeric(input$trendT_value)
    vals     = if (input$trendT_lastn=="All") nrow(df) else
      min(as.numeric(input$trendT_lastn), nrow(df))
    
    vec_season = as.numeric(df[[side_col]])
    vec_lastn  = head(vec_season, vals)
    
    p_lastn  = length(which(vec_lastn > target)) / vals
    p_season = length(which(vec_season > target)) / nrow(df)
    myprob   = paste0(length(which(vec_lastn > target)), " / ", vals)
    
    if (input$trendT_lastn=="All") {
      p_over = p_lastn
      trendTSettings$odds = paste0("Probability: <strong>", myprob, "</strong> (season)")
    } else {
      # Blend: 70% season baseline + 30% recent form
      p_over = 0.7*p_season + 0.3*p_lastn
      trendTSettings$odds = paste0(
        "Probability: <strong>", myprob, "</strong> (last ", vals, ")",
        " &nbsp;|&nbsp; Season: <strong>", length(which(vec_season > target)),
        " / ", nrow(df), "</strong>")
    }
    
    margin = as.numeric(input$trendT_overround) / 100
    over_price  = if (p_over==0) NA else
      min(ODDS_CAP, max(ODDS_FLOOR, round(1/((1+margin)*p_over), 2)))
    under_price = if (p_over==1) NA else
      min(ODDS_CAP, max(ODDS_FLOOR, round(1/((1+margin)*(1-p_over)), 2)))
    trendTSettings$prices = paste0(
      "Suggested odds (", 100 + as.numeric(input$trendT_overround), "% overround): ",
      "<strong>Over ", target, ": ",
      ifelse(is.na(over_price), "\u2014", format(over_price, nsmall=2)),
      " &nbsp;|&nbsp; Under ", target, ": ",
      ifelse(is.na(under_price), "\u2014", format(under_price, nsmall=2)),
      "</strong>")
    
    # Line-sanity check (see player Trend for rationale)
    trendTSettings$linewarn = if (any(is.na(c(over_price, under_price))) ||
                                  any(c(over_price, under_price) > 2.5, na.rm=TRUE)) {
      paste0("Warning: this price is far from even money. Under normal ",
             "circumstances this would be an alternate line rather than the ",
             "main one - check the team's injury report and lineup news ",
             "before using these odds.")
    } else { "" }
    
    # Season vs last-N averages across all team stats, for the chosen side
    stat_cols = if (input$trendT_side=="For") names(playersAll)[7:21] else
      paste0("OPP_", names(playersAll)[7:21])
    trendT_avg_tbl = df %>% select(all_of(stat_cols)) %>%
      summarise(across(everything(), ~ mean(as.numeric(.), na.rm=TRUE))) %>%
      mutate(SPLIT = paste0("Season avg (", nrow(df), " games)"), .before=1)
    if (input$trendT_lastn != "All") {
      trendT_avg_tbl = bind_rows(
        trendT_avg_tbl,
        df %>% head(vals) %>% select(all_of(stat_cols)) %>%
          summarise(across(everything(), ~ mean(as.numeric(.), na.rm=TRUE))) %>%
          mutate(SPLIT = paste0("Last ", vals, " avg"), .before=1)
      )
    }
    names(trendT_avg_tbl) = gsub("^OPP_", "", names(trendT_avg_tbl))
    
    trendTSettings$avg_table = reactable(trendT_avg_tbl,
                                         theme = myTheme, language = myArrows,
                                         sortable = F, pagination = F,
                                         defaultColDef = colDef(maxWidth = 55, format = colFormat(digits = 1)),
                                         columns = list(
                                           SPLIT = colDef(name = "", width = 150, style = list(fontWeight = "bold")),
                                           DREB = colDef(maxWidth = 65, format = colFormat(digits = 1)),
                                           OREB = colDef(maxWidth = 65, format = colFormat(digits = 1))
                                         ),
                                         rowStyle = function(index) list(background = "#d2d8f9"))
    
    # Game log: both sides of the chosen stat, highlight the priced side vs the line
    show_df = df %>% head(vals) %>%
      transmute(MATCHUP,
                FOR_VAL = as.numeric(.data[[mycol]]),
                AGN_VAL = as.numeric(.data[[paste0("OPP_", mycol)]]))
    names(show_df)[2:3] = c(paste0(mycol, " (For)"), paste0(mycol, " (Against)"))
    hl_vec = if (input$trendT_side=="For") show_df[[2]] else show_df[[3]]
    
    trendTSettings$table = reactable(show_df,
                                     theme = myTheme, language = myArrows,
                                     showPageSizeOptions = F, defaultPageSize = vals,
                                     fullWidth = FALSE,
                                     defaultColDef = colDef(maxWidth = 130, sortable=F),
                                     columns = list(MATCHUP = colDef(name = "DATE", width = 95,
                                                                     sortable=F, cell = matchup_cell)),
                                     rowStyle = function(index) {
                                       if (as.numeric(hl_vec[index]) > target) {
                                         list(background = "#98a4f1", color = "white")
                                       }
                                     })
  })
  
  output$trendT_odds = renderText(paste0("<span style='font-size: 15px;'>",
                                         trendTSettings$odds,"</span>"))
  output$trendT_prices = renderText(paste0("<span style='font-size: 15px;'>",
                                           trendTSettings$prices,"</span>"))
  output$trendT_line_warning = renderText(paste0(
    "<span style='color:#b00020; font-size:13.5px; font-weight:600;'>",
    trendTSettings$linewarn,"</span>"))
  output$trendT_avg_table = renderReactable(trendTSettings$avg_table)
  output$trendT_table = renderReactable(trendTSettings$table)
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # Tool - On/Off:
  onoffSettings = reactiveValues()
  
  # Update inputs after selecting player
  observeEvent(list(input$onoff_p1), {
    req(input$onoff_p1)
    updateSelectInput(session, "onoff_season",
                      choices = playersAll %>% filter(PLAYER==input$onoff_p1) %>% 
                        pull(SEASON) %>% sort()
    )
  })
  observeEvent(list(input$onoff_p1,input$onoff_season), {
    req(input$onoff_p1,input$onoff_season)
    updateSelectInput(session, "onoff_team",
                      choices = playersAll %>% 
                        filter(PLAYER==input$onoff_p1&SEASON==input$onoff_season) %>% 
                        pull(TEAM)
    )
  })
  observeEvent(list(input$onoff_p1,input$onoff_season,input$onoff_team), {
    req(input$onoff_p1,input$onoff_season,input$onoff_team)
    lg_onoff = playersAll %>%
      filter(PLAYER==input$onoff_p1 & SEASON==input$onoff_season & TEAM==input$onoff_team) %>%
      pull(LEAGUE) %>% unique() %>% sort()
    updateSelectInput(session, "onoff_league", choices = lg_onoff,
                      selected = if (EL_NAME %in% lg_onoff) EL_NAME else lg_onoff[1])
  })
  observeEvent(list(input$onoff_league,input$onoff_season,input$onoff_team,input$onoff_p1), {
    req(input$onoff_league,input$onoff_season,input$onoff_team,input$onoff_p1)
    updateSelectInput(session, "onoff_p2",
                      choices = playersAll %>% 
                        filter(SEASON==input$onoff_season & LEAGUE==input$onoff_league &
                                 TEAM==input$onoff_team & PLAYER != input$onoff_p1) %>% 
                        pull(PLAYER) %>% sort()
    )
  })
  
  observeEvent(input$onoff_run, {
    
    games_list = playersAll %>% 
      filter(SEASON==input$onoff_season&LEAGUE==input$onoff_league&TEAM==input$onoff_team) %>% 
      distinct(MATCHUP) %>% 
      pull()
    games_p1 = playersAll %>% 
      filter(PLAYER==input$onoff_p1&MATCHUP%in%games_list) %>% 
      distinct(MATCHUP) %>% 
      pull()
    req(length(input$onoff_p2) > 0)
    
    # One game list per selected OFF player. Each of the main player's games
    # is labelled with every selected player's status, e.g. with two players:
    #   "P1 ON, P2 ON" / "P1 ON, P2 OFF" / "P1 OFF, P2 ON" / "P1 OFF, P2 OFF"
    # ON is shown in green and OFF in red (rendered as HTML in the table).
    p2_games_each = lapply(input$onoff_p2, function(p) {
      playersAll %>% 
        filter(PLAYER==p & MATCHUP%in%games_list) %>% 
        distinct(MATCHUP) %>% 
        pull()
    })
    
    label_for_game = function(g) {
      paste(vapply(seq_along(input$onoff_p2), function(i) {
        on = g %in% p2_games_each[[i]]
        paste0("<span style='color:", if (on) "#0a7d32" else "#c62828",
               "; font-weight:bold;'>", input$onoff_p2[i],
               if (on) " ON" else " OFF", "</span>")
      }, character(1)), collapse = ", ")
    }
    n_off_for_game = function(g) {
      sum(!vapply(p2_games_each, function(gl) g %in% gl, logical(1)))
    }
    game_labels = tibble(
      MATCHUP = games_p1,
      ONOFF   = vapply(games_p1, label_for_game, character(1)),
      N_OFF   = vapply(games_p1, n_off_for_game, numeric(1))
    ) %>%
      mutate(ORD = case_when(N_OFF == 0 ~ 0,                          # everyone ON
                             N_OFF == length(input$onoff_p2) ~ 1,     # everyone OFF
                             TRUE ~ 2)) %>%                           # partial groups
      select(-N_OFF)
    
    xxx = playersAll %>% 
      filter(PLAYER==input$onoff_p1 & MATCHUP%in%games_p1) %>% 
      left_join(game_labels, by = "MATCHUP") %>% 
      relocate(ONOFF, .before = 1) %>% 
      arrange(ORD, ONOFF, desc(MATCHUP)) %>% 
      select(-ORD)
    
    onoffSettings$t1 =  reactable(xxx %>% 
                                    select(-c(PLAYER,SEASON,LEAGUE,TEAM)),
                                  groupBy = c("ONOFF"),
                                  theme = myTheme,
                                  language = myArrows,
                                  sortable = T,
                                  showPageSizeOptions = T,
                                  defaultPageSize = 10,
                                  pageSizeOptions = c(5,10,15,20,25,30,35),
                                  columns = list(
                                    MATCHUP=colDef(name="DATE", width = 95, sortable=F,
                                                   cell = matchup_cell),
                                    ONOFF=colDef(width = 340,sortable=F,show=T,html=TRUE,
                                                 style = list(fontWeight="bold",fontSize=11.5)),
                                    MIN = colDef(maxWidth = 55, sortable=F, aggregate = "mean",
                                                 format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
                                    PTS = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
                                    `2PM` = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
                                    `2PA` = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
                                    `3PM` = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
                                    `3PA` = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
                                    FTM = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
                                    FTA = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
                                    DREB = colDef(maxWidth = 65, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
                                    OREB = colDef(maxWidth = 65, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
                                    REB = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
                                    AST = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
                                    STL = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
                                    BLK = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
                                    TOV = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1))),
                                    PF = colDef(maxWidth = 55, aggregate = "mean", format = list(cell = colFormat(digits = 0), aggregated = colFormat(digits = 1)))
                                  ),
                                  rowStyle = myAggregateStyle )
    
    output$onofftable = renderReactable(onoffSettings$t1)
    
  })
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # Players:
  # players_by_league is defined globally (next to the data) so the UI can filter
  # the league picker down to leagues with rows.
  pl_data = reactive({ req(input$pl_league); players_by_league[[input$pl_league]] })
  
  # Repopulate the player/team pickers whenever the league changes (server-side
  # selectize, so the big NBA/EL name lists aren't rendered into the page up front)
  observeEvent(input$pl_league, {
    d = pl_data()
    updateSelectizeInput(session, "pl_player", choices = sort(unique(d$PLAYER)),
                         selected = character(0), server = TRUE)
    updateSelectizeInput(session, "pl_team",   choices = sort(unique(d$TEAM)),
                         selected = character(0), server = TRUE)
  })
  
  # One table, filtered by any selected players/teams. intersect() drops stale
  # selections from a previous league, so switching leagues never flashes empty.
  output$players_table = renderReactable({
    d = pl_data()
    selp = intersect(input$pl_player, d$PLAYER)
    selt = intersect(input$pl_team,   d$TEAM)
    if (length(selp) > 0) d = d[d$PLAYER %in% selp, ]
    if (length(selt) > 0) d = d[d$TEAM   %in% selt, ]
    react_players(d)
  })
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # Teams:
  # teams_by_league is defined globally (see above).
  tm_is_us = function(lg) lg %in% c("NBA","WNBA")
  
  # Data for the current league; for domestic leagues an Opponent view reverses
  # the numeric columns within each game (what the opponent produced). Reversing
  # here on the FULL data, before the team filter, fixes the old bug where
  # filtering first collapsed each game to one row and the reversal did nothing.
  tm_data = reactive({
    req(input$tm_league)
    d = teams_by_league[[input$tm_league]]
    if (!tm_is_us(input$tm_league) && identical(input$tm_side, "Opponent")) {
      d = d %>% group_by(MATCHUP) %>%
        mutate(across(where(is.numeric), ~ rev(.x))) %>% ungroup()
    }
    d
  })
  
  observeEvent(input$tm_league, {
    d = teams_by_league[[input$tm_league]]
    updateSelectizeInput(session, "tm_team", choices = sort(unique(d$TEAM)),
                         selected = character(0), server = TRUE)
  })
  
  # NBA/WNBA link out to the official box scores (only shown for those leagues)
  output$tm_boxscore_link = renderUI({
    req(tm_is_us(input$tm_league))
    url = if (input$tm_league == "NBA")
      "https://www.nba.com/stats/team/1610612750/boxscores-traditional?SeasonType=Regular%20Season"
    else "https://stats.wnba.com/team/1611661319/boxscores-traditional/"
    actionButton("tmlink", "Teams boxscore", icon = icon("basketball-ball"),
                 onclick = sprintf("window.open('%s','_blank')", url),
                 style = "margin-top:24.5px;")
  })
  
  output$teams_table = renderReactable({
    lg = input$tm_league; req(lg)
    d = tm_data()
    selt = intersect(input$tm_team, d$TEAM)
    if (length(selt) > 0) d = d[d$TEAM %in% selt, ]
    if (tm_is_us(lg)) react_team_us(d, 15, c(10,15,30))
    else            react_team2(d, 20, c(5,10,15,20,25))
  })
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # Stats Leaders - NBA Top/Most
  output$top_most = renderReactable(
    {
      reactable(topMost ,
                theme = myTheme,
                language = myArrows,
                sortable = T,
                showPageSizeOptions = T,
                defaultPageSize = 20,
                pageSizeOptions = c(10,15,20),
                columns = list(
                  PLAYER = colDef(width = 210),
                  TEAM  = colDef(width = 200),   # NBA carries full club names
                  GAMES = colDef(width = 75),
                  PTS = colDef(width = 60),
                  `3PM` = colDef(width = 60),
                  REB = colDef(width = 60),
                  AST = colDef(width = 60),
                  STL = colDef(width = 60),
                  BLK = colDef(width = 60),
                  TOV = colDef(width = 60)),
                rowStyle = myAggregateStyle
      )
    }
  )
  
  observe({
    # No req() here: clearing the team selection must re-run this and restore
    # the full table (req() would stop the observer on an empty selection).
    filtered = if (length(input$TM_team) > 0) {
      topMost[topMost$TEAM %in% input$TM_team, ]
    } else {
      topMost
    }
    updateReactable("top_most", data = filtered)
  })
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # Stats Leaders - Domestic / Euroleague
  output$el_leaders = renderReactable(
    {
      reactable(elLeaders ,
                theme = myTheme,
                language = myArrows,
                sortable = T,
                showPageSizeOptions = T,
                defaultPageSize = 20,
                pageSizeOptions = c(10,15,20),
                columns = list(
                  PLAYER = colDef(width = 210),
                  TEAM  = colDef(width = 100),   # Euroleague uses short team codes
                  GAMES = colDef(width = 75),
                  PTS = colDef(width = 60),
                  `3PM` = colDef(width = 60),
                  REB = colDef(width = 60),
                  AST = colDef(width = 60),
                  STL = colDef(width = 60),
                  BLK = colDef(width = 60),
                  TOV = colDef(width = 60)),
                rowStyle = myAggregateStyle
      )
    }
  )
  
  observe({
    filtered = if (length(input$EL_team3) > 0) {
      elLeaders[elLeaders$TEAM %in% input$EL_team3, ]
    } else {
      elLeaders
    }
    updateReactable("el_leaders", data = filtered)
  })
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
}

# Run the application 
shinyApp(ui = ui, server = server)
