# 📊 RVizToolbox: R Functions for Visualisation 📈

## About this project
**RVizToolbox** is a curated collection of R functions designed to streamline **data visualisation** and **spatial mapping**. This toolbox helps researchers and analysts transform raw data into polished, publication-ready charts and maps with minimal code.

## 🚀 Quick Start 
You can call functions from this repository directly into your R environment (no download required). This ensures you are always using the latest, most optimized version of the visualisation tools.

### Step 1: Define the Loader Function
Copy and paste this helper function into your R script. It acts as a bridge to this repository.

```R
CallRVizToolbox <- function(file) {
  git_path <- "https://raw.githubusercontent.com/daotran-rnd/chart-map-functions/refs/heads/main/r_functions/"
  source(paste0(git_path, file))
}
```
### Step 2: Load a Specific Visualisation Tool
Call the specific R function you need. For example, to load a chart function `PlotLineChart`:

```R
CallRVizToolbox("PlotLineChart.R")
```

**Enjoy and Stay Strong!⚡**
