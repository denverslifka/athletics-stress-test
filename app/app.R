# =============================================================================
# College Athletics Stress Test — Shiny app (Shinylive-ready)
# Denver Slifka
#
# Reads the CSVs made by athletics_stress_test.R (copy them into app/data/).
# Uses only packages that run in the browser through Shinylive:
# shiny, bslib, ggplot2, scales. No dplyr/readxl needed here.
#
# Source: Knight-Newhouse College Athletics Database (knightnewhousedata.org)
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)

# ---- Data -------------------------------------------------------------------
latest <- read.csv("data/stress_latest.csv", stringsAsFactors = FALSE)
panel  <- read.csv("data/ratios_panel.csv",  stringsAsFactors = FALSE)

TIERS    <- c("Power 4", "Group of 5", "FCS / other")
latest$tier <- factor(latest$tier, levels = TIERS)
panel$tier  <- factor(panel$tier,  levels = TIERS)
LATEST_YEAR <- max(latest$year, na.rm = TRUE)
REV_SHARE_CAP <- 21.5e6

pct  <- function(x, d = 1) {
  v <- round(100 * x, d); v[!is.na(v) & v == 0] <- 0          # no "-0.0%"
  ifelse(is.na(x), "-", paste0(formatC(v, format = "f", digits = d), "%"))
}
mil  <- function(x, d = 1) ifelse(is.na(x), "-", paste0("$", formatC(x / 1e6, format = "f", digits = d, big.mark = ","), "M"))
xmul <- function(x) ifelse(is.na(x), "-", paste0(formatC(x, format = "f", digits = 2), "x"))

share_losing <- mean(latest$stand_alone_margin < 0, na.rm = TRUE)

# Brand-neutral palette: one accent + muted greys; red only for "stress"
ACCENT <- "#1F4E79"
WARN   <- "#B03A2E"
MUTED  <- "#8A8F98"

theme_app <- function() {
  theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "top", legend.title = element_blank(),
          plot.title.position = "plot")
}

METRIC_LABEL <- c(subsidy_share = "Subsidy share", debt_service_share = "Debt service share",
                  debt_to_revenue = "Debt / revenue", media_conf_share = "Media / conference share",
                  comp_share = "Compensation share", growth_gap = "Growth gap",
                  stand_alone_margin = "Margin without subsidies")
METRIC_HELP <- list(
  subsidy_share      = "Institutional/government support + student fees as a share of revenue. Money the athletics program did not earn.",
  debt_service_share = "Annual debt service, leases and rent on athletic facilities as a share of revenue.",
  debt_to_revenue    = "Athletics-related debt outstanding divided by one year of revenue.",
  media_conf_share   = "Conference/NCAA distributions, media rights and bowl revenue as a share of revenue (concentration risk).",
  comp_share         = "Coach + non-coaching staff compensation as a share of expenses (costs that are hard to cut).",
  growth_gap         = "Expense growth minus revenue growth per year since 2014.",
  stand_alone_margin = "Margin if subsidies were removed: (revenue − subsidies − expenses) ÷ expenses. Shown but not in the score."
)

# ---- UI ---------------------------------------------------------------------
ui <- page_navbar(
  title = "College Athletics Stress Test",
  theme = bs_theme(version = 5, primary = ACCENT, base_font = font_collection("system-ui", "sans-serif")),
  fillable = FALSE,
  header = tags$style("td:nth-child(2), td:nth-child(3) { white-space: nowrap; } .value-box-value { font-size: 1.9rem; }"),

  nav_panel("Overview",
    layout_columns(col_widths = c(4, 4, 4),
      value_box(title = paste0("Lose money without subsidies (FY", LATEST_YEAR, ")"),
                value = pct(share_losing, 0),
                p(paste0(nrow(latest), " public Division I schools"))),
      value_box(title = "Median subsidy share, FCS / other",
                value = pct(median(latest$subsidy_share[latest$tier == "FCS / other"], na.rm = TRUE), 0),
                p("of athletics revenue comes from the university and students")),
      value_box(title = "Median debt / revenue, Power 4",
                value = xmul(median(latest$debt_to_revenue[latest$tier == "Power 4"], na.rm = TRUE)),
                p("facility debt relative to one year of revenue"))
    ),
    card(
      card_header("Why this exists"),
      p("In November 2026, former Iowa State athletic director Jamie Pollard warned that college sports could be ",
        "sitting on the next savings-and-loan crisis. The S&L comparison is about long-term commitments (facility debt, ",
        "multi-year contracts, and now revenue sharing) funded by revenue that can disappear quickly, with reporting ",
        "too thin to show the exposure."),
      p("This app scores every public Division I athletics department on six balance-sheet-style ratios, ranks each ",
        "school against its own tier, and stress-tests budgets against the $21.5M revenue-sharing cap.")
    ),
    card(
      card_header("Subsidy dependence by tier"),
      plotOutput("tier_box", height = "340px")
    )
  ),

  nav_panel("Rankings",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("rank_tier", "Tier", choices = TIERS, selected = "Power 4"),
        sliderInput("rank_n", "Show top", min = 10, max = 125, value = 25, step = 5),
        helpText("Stress score = average of six standardized ratios, compared within the tier. ",
                 "Positive = more stressed than the tier average.")
      ),
      card(card_header(textOutput("rank_title")), tableOutput("rank_table"))
    )
  ),

  nav_panel("School lookup",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("school", "School", choices = sort(latest$school),
                    selected = "The University of Tennessee"),
        uiOutput("school_badge")
      ),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Ratios vs. tier median"), tableOutput("school_ratios")),
        card(card_header("Where the money comes from"), plotOutput("school_trend", height = "300px"))
      ),
      card(card_header("Subsidy share over time"), plotOutput("school_subsidy", height = "260px"))
    )
  ),

  nav_panel("Revenue-sharing shock",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h6("Share of the $21.5M cap each tier pays"),
        sliderInput("rs_p4",  "Power 4",     min = 0, max = 100, value = 100, step = 5, post = "%"),
        sliderInput("rs_g5",  "Group of 5",  min = 0, max = 100, value = 50,  step = 5, post = "%"),
        sliderInput("rs_fcs", "FCS / other", min = 0, max = 100, value = 10,  step = 5, post = "%"),
        hr(),
        h6("Revenue shocks"),
        sliderInput("media_cut", "Media / conference revenue cut", min = 0, max = 50, value = 0, step = 5, post = "%"),
        sliderInput("donor_cut", "Donation drop",                  min = 0, max = 50, value = 0, step = 5, post = "%"),
        helpText("Opt-in levels are assumptions, not reported figures. Set them to the levels you believe.")
      ),
      layout_columns(col_widths = c(4, 4, 4),
        value_box(title = "Total new subsidy needed", value = textOutput("shock_total")),
        value_box(title = "Median Power 4 margin w/o subsidies", value = textOutput("shock_p4")),
        value_box(title = "Schools needing >$10M more", value = textOutput("shock_10m"))
      ),
      card(card_header("Margin without subsidies: today vs. shocked (median by tier)"),
           plotOutput("shock_plot", height = "300px")),
      card(card_header("Hardest hit: new subsidy needed as a share of today's revenue"), tableOutput("shock_table"))
    )
  ),

  nav_panel("Methodology",
    card(
      card_header("Data"),
      p(paste0("Knight-Newhouse College Athletics Database (knightnewhousedata.org), public NCAA Division I institutions, ",
               "FY2014-", LATEST_YEAR, ", as self-reported on NCAA Financial Reports. Service academies are excluded. ",
               "Dollar figures are nominal.")),
      p("Tiers: Power 4 = SEC, Big Ten, Big 12, ACC in the latest year; Group of 5 = other FBS; FCS / other = FCS and ",
        "non-football Division I.")
    ),
    card(
      card_header("Stress score"),
      tags$ul(lapply(names(METRIC_HELP), function(m) tags$li(tags$b(METRIC_LABEL[[m]]), ": ", METRIC_HELP[[m]]))),
      p("Each of the first six ratios is converted to a z-score within the school's tier (higher = more stressed), ",
        "and the score is their average. Schools with fewer than three available ratios are not scored.")
    ),
    card(
      card_header("Limitations"),
      tags$ul(
        tags$li("Self-reported data; some schools show one-year jumps worth checking before citing."),
        tags$li("Heavily subsidized schools often report revenue equal to expenses, so the growth gap is close to zero by construction."),
        tags$li("Revenue-sharing opt-in levels in the shock tab are user assumptions; FY2025 data predates the House settlement."),
        tags$li("Schools that changed division recently (e.g., UC San Diego, 2020) have distorted growth figures.")
      )
    ),
    p(class = "text-muted", "Built by Denver Slifka. Source: Knight-Newhouse College Athletics Database.")
  )
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Overview
  output$tier_box <- renderPlot({
    d <- latest[!is.na(latest$subsidy_share), ]
    ggplot(d, aes(tier, subsidy_share)) +
      geom_boxplot(fill = "#DCE6F1", colour = ACCENT, outlier.colour = MUTED, width = 0.5) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = NULL, y = "Subsidy share of revenue") + theme_app()
  })

  # Rankings
  output$rank_title <- renderText(paste0(input$rank_tier, ": most stressed (FY", LATEST_YEAR, ")"))
  output$rank_table <- renderTable({
    d <- latest[latest$tier == input$rank_tier & !is.na(latest$stress_score), ]
    d <- d[order(-d$stress_score), ][seq_len(min(input$rank_n, nrow(d))), ]
    data.frame(
      Rank = d$tier_rank, School = d$school,
      `Stress score` = formatC(d$stress_score, format = "f", digits = 2),
      `Subsidy share` = pct(d$subsidy_share), `Debt / revenue` = xmul(d$debt_to_revenue),
      `Debt service share` = pct(d$debt_service_share), `Media share` = pct(d$media_conf_share),
      `Growth gap` = pct(d$growth_gap), Revenue = mil(d$total_revenue),
      check.names = FALSE)
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "l")

  # School lookup
  sel <- reactive(latest[latest$school == input$school, ][1, ])

  output$school_badge <- renderUI({
    s <- sel()
    tagList(
      p(tags$b(as.character(s$tier)), if (!is.na(s$conference)) paste0(" · ", s$conference)),
      p("Stress rank: ", tags$b(paste0(s$tier_rank, " of ", s$tier_n)), " in tier"),
      p("FY", s$year, " revenue: ", tags$b(mil(s$total_revenue)))
    )
  })

  output$school_ratios <- renderTable({
    s <- sel(); peers <- latest[latest$tier == s$tier, ]
    m <- c("subsidy_share", "debt_service_share", "debt_to_revenue", "media_conf_share",
           "comp_share", "growth_gap", "stand_alone_margin")
    lab <- c("Subsidy share", "Debt service share", "Debt / revenue", "Media / conference share",
             "Compensation share", "Growth gap (per yr)", "Margin w/o subsidies")
    fmt <- function(v, k) if (k == "debt_to_revenue") xmul(v) else pct(v)
    data.frame(Metric = lab,
               School = mapply(function(k) fmt(s[[k]], k), m),
               `Tier median` = mapply(function(k) fmt(median(peers[[k]], na.rm = TRUE), k), m),
               check.names = FALSE, row.names = NULL)
  }, striped = TRUE, spacing = "s", align = "lrr")

  output$school_trend <- renderPlot({
    p <- panel[panel$school == input$school, ]
    p <- p[order(p$year), ]
    long <- rbind(
      data.frame(year = p$year, value = p$generated_revenue, series = "Earned revenue"),
      data.frame(year = p$year, value = p$allocated,         series = "Subsidies"),
      data.frame(year = p$year, value = p$total_expenses,    series = "Expenses"))
    ggplot(long, aes(year, value / 1e6, colour = series, linetype = series)) +
      geom_line(linewidth = 1) +
      scale_colour_manual(values = c("Earned revenue" = ACCENT, "Subsidies" = WARN, "Expenses" = MUTED)) +
      scale_linetype_manual(values = c("Earned revenue" = "solid", "Subsidies" = "solid", "Expenses" = "dashed")) +
      scale_x_continuous(breaks = seq(2014, LATEST_YEAR, 2)) +
      labs(x = NULL, y = "$ millions") + theme_app()
  })

  output$school_subsidy <- renderPlot({
    s <- sel()
    tier_med <- aggregate(subsidy_share ~ year, data = panel[panel$tier == s$tier, ], FUN = median)
    p <- panel[panel$school == input$school, ]
    ggplot() +
      geom_line(data = tier_med, aes(year, subsidy_share, colour = "Tier median"), linewidth = 0.8, linetype = "dashed") +
      geom_line(data = p, aes(year, subsidy_share, colour = "School"), linewidth = 1.1) +
      scale_colour_manual(values = c("School" = ACCENT, "Tier median" = MUTED)) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(breaks = seq(2014, LATEST_YEAR, 2)) +
      labs(x = NULL, y = NULL) + theme_app()
  })

  # Shock
  shock <- reactive({
    share <- c("Power 4" = input$rs_p4, "Group of 5" = input$rs_g5, "FCS / other" = input$rs_fcs) / 100
    d <- latest
    media <- ifelse(is.na(d$media_rights), 0, d$media_rights)
    don   <- ifelse(is.na(d$donations), 0, d$donations)
    d$rev_share_paid   <- REV_SHARE_CAP * share[as.character(d$tier)]
    d$lost             <- media * input$media_cut / 100 + don * input$donor_cut / 100
    d$shock_exp        <- d$total_expenses + d$rev_share_paid
    d$shock_gen        <- d$generated_revenue - d$lost
    d$base_margin      <- d$stand_alone_margin
    d$shock_margin     <- (d$shock_gen - d$shock_exp) / d$shock_exp
    d$extra_subsidy    <- pmax(0, d$shock_exp - d$shock_gen) - pmax(0, d$total_expenses - d$generated_revenue)
    d[!is.na(d$shock_margin), ]
  })

  output$shock_total <- renderText(paste0(mil(sum(shock()$extra_subsidy, na.rm = TRUE) * 1, 0), " / yr"))
  output$shock_p4    <- renderText({
    d <- shock(); d <- d[d$tier == "Power 4", ]
    paste0(pct(median(d$base_margin, na.rm = TRUE), 0), " to ", pct(median(d$shock_margin, na.rm = TRUE), 0))
  })
  output$shock_10m   <- renderText(sum(shock()$extra_subsidy > 10e6, na.rm = TRUE))

  output$shock_plot <- renderPlot({
    d <- shock()
    agg <- rbind(
      data.frame(tier = TIERS, when = "Today",
                 margin = sapply(TIERS, function(t) median(d$base_margin[d$tier == t], na.rm = TRUE))),
      data.frame(tier = TIERS, when = "Shocked",
                 margin = sapply(TIERS, function(t) median(d$shock_margin[d$tier == t], na.rm = TRUE))))
    agg$tier <- factor(agg$tier, levels = TIERS); agg$when <- factor(agg$when, levels = c("Today", "Shocked"))
    ggplot(agg, aes(tier, margin, fill = when)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_hline(yintercept = 0, colour = "grey30") +
      geom_text(aes(label = scales::percent(margin, accuracy = 1),
                    vjust = ifelse(margin < 0, 1.4, -0.4)),
                position = position_dodge(width = 0.7), size = 4) +
      scale_fill_manual(values = c("Today" = "#9DB4CC", "Shocked" = WARN)) +
      scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0.12, 0.08))) +
      labs(x = NULL, y = NULL) + theme_app()
  })

  output$shock_table <- renderTable({
    d <- shock(); d$hit <- d$extra_subsidy / d$total_revenue
    d <- d[order(-d$hit), ][1:15, ]
    data.frame(School = d$school, Tier = as.character(d$tier), `Hit (% of revenue)` = pct(d$hit),
               `Revenue sharing` = mil(d$rev_share_paid), `Lost revenue` = mil(d$lost),
               `New subsidy needed` = mil(d$extra_subsidy),
               `Margin today` = pct(d$base_margin), `Margin shocked` = pct(d$shock_margin),
               check.names = FALSE)
  }, striped = TRUE, spacing = "s", align = "l")
}

shinyApp(ui, server)
