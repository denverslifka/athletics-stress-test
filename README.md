# College Athletics Stress Test

**Live app:** https://denverslifka.github.io/athletics-stress-test/

In November 2026, former Iowa State athletic director Jamie Pollard warned that college sports could be heading toward the next savings-and-loan crisis. This project tests that claim with data: it scores every public NCAA Division I athletics department on six financial-stress ratios, ranks each school against its own tier, and stress-tests budgets against the $21.5M House settlement revenue-sharing cap.

## Key findings (FY2025)

- **94%** of 235 public D-I athletics departments lose money once institutional subsidies and student fees are removed.
- The median FCS / non-football program gets **76%** of its athletics revenue from the university and students.
- The median Power 4 program's subsidy share rose from **1.4% in 2020 to 6.0% in 2025**.
- At Carson-Newman (NCAA Division II), **40%** of full-time undergraduates are varsity athletes, vs. 5% at the median Division I school.
- If every Power 4 school paid the full $21.5M cap, the median Power 4 margin without subsidies would fall from **-6% to -17%**.

## What's in the app

| Tab | What it shows |
|---|---|
| Findings | Subsidy trend by tier since 2014, headline numbers, and every school as one square |
| Rankings | Most-stressed schools within Power 4, Group of 5, and FCS / other |
| School lookup | Any school's ratios vs. its tier median, revenue mix, and subsidy trend since 2014 |
| Revenue-sharing shock | Sliders for revenue-sharing opt-in, media cuts, and donation drops, with live results |
| Carson-Newman and Division II | Carson-Newman vs. South Atlantic Conference peers (EADA 2024-25) |
| Method | Definitions, scoring, and limitations |

## Method

Six ratios per school, standardized within tier and averaged into a stress score:
subsidy share of revenue, debt service share, debt / revenue, media and conference revenue concentration, compensation share of expenses, and expense growth minus revenue growth since 2014.

## Repo layout

```
athletics_stress_test.R   # data load, ratios, stress score, shock scenarios
eada_prep.R               # Carson-Newman / South Atlantic Conference data (EADA)
build_site.R              # rebuilds data and exports the site
app/app.R                 # Shiny app (runs in the browser via Shinylive)
app/data/                 # CSVs the app reads
docs/                     # exported static site served by GitHub Pages
```

Built in R (dplyr, Shiny, bslib, ggplot2) and deployed with Shinylive on GitHub Pages.

**Data:** Knight-Newhouse College Athletics Database (knightnewhousedata.org), self-reported by institutions on NCAA Financial Reports, used for non-commercial purposes with credit as required. U.S. Department of Education, Equity in Athletics Data Analysis (ope.ed.gov/athletics), 2024-25.

*Denver Slifka · Carson-Newman University*
