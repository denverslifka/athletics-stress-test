# =============================================================================
# build_site.R: refresh the app's data and export it as a static Shinylive site
# Run from the project folder (athletics-stress-test/) after the main script.
# =============================================================================

# 1. Rebuild the ratios from the raw data
source("athletics_stress_test.R")
source("eada_prep.R")        # Carson-Newman / South Atlantic Conference tab

# 2. Copy the outputs the app needs into app/data/
dir.create("app/data", recursive = TRUE, showWarnings = FALSE)
file.copy(c("output/stress_latest.csv", "output/ratios_panel.csv"), "app/data/", overwrite = TRUE)

# 3. Export the app as a static website into docs/ (GitHub Pages serves this folder)
#    First time only: install.packages(c("shinylive", "httpuv"))
shinylive::export(appdir = "app", destdir = "docs")

# 4. Preview it locally before pushing (open the URL it prints; Ctrl+C / Esc to stop)
httpuv::runStaticServer("docs")
