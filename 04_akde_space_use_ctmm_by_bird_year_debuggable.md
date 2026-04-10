---
title: "04_akde_space_use_ctmm_by_bird_year_debuggable"
output:
  html_document:
    toc: true
    toc_depth: 3
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE
)
```

# Purpose

This script estimates **colony-level space use per year** using **individual bird-year AKDEs** from the `ctmm` package.

This version is deliberately written to be **debuggable**. It separates the workflow into smaller stages so you can see exactly where time is being spent:

1. prepare datasets  
2. build bird-year IDs  
3. build telemetry objects  
4. compute `ctmm.guess()` objects  
5. fit `ctmm.select()` models  
6. test AKDE on one bird-year  
7. run AKDE bird-year by bird-year with a progress bar and timing table  
8. build year-level mean UDs  
9. compare **full** and **trimmed** datasets  

This is the safest structure for your data because:
- the movement model is fitted at the **bird-year** level
- UDs are estimated per **individual bird-year**
- year-level colony maps are produced by **averaging individual UDs within year**
- the slow AKDE step is timed per bird-year so you can identify bottlenecks

# Packages

```{r packages}
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(openxlsx)
library(sf)
library(sp)
library(ctmm)
library(tidyr)
library(purrr)
```

# Project folders

```{r folders}
project_dir <- getwd()
input_dir <- file.path(project_dir, "outputs", "00_setup_and_load")
output_dir <- file.path(project_dir, "outputs", "04_akde_space_use_ctmm_by_bird_year_debuggable")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
```

# User controls

```{r user-controls}
# Threshold used to define extended trips for the trimmed sensitivity dataset.
extended_trip_threshold_km <- 250

# Optional land polygon shapefile for hard-boundary AKDE.
# Example:
# land_shp <- "C:/Users/r01dp24/OneDrive - University of Aberdeen/Documents/PhD/05-Data/01 - Shapefile/boundaries/falklands_boundaries.shp"
land_shp <- NULL

# Buffer around the penguin extent when constructing the water boundary.
boundary_buffer_m <- 20000

# AKDE options
use_weighted_akde <- FALSE
use_debiased_akde <- TRUE

# UD levels to summarise
ud_levels <- c(0.50, 0.95)

# Minimum number of locations required for a bird-year to be retained
min_points_per_bird_year <- 30

# Which dataset to test first before running both:
# choose one of "full", "trimmed"
debug_dataset_name <- "full"

# If TRUE, first run AKDE only on one bird-year to diagnose performance
run_single_id_akde_test <- TRUE
```

# Helper functions

```{r helpers}
theme_paper <- function() {
  theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey85", linewidth = 0.4),
      axis.text = element_text(colour = "grey20"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right"
    )
}

extract_ud_area <- function(ud_obj, level_ud) {
  s <- summary(ud_obj, level.UD = level_ud, units = TRUE)
  ci_tbl <- as.data.frame(s$CI)
  ci_tbl$parameter <- rownames(ci_tbl)
  area_row <- ci_tbl %>% dplyr::filter(parameter == "area")
  tibble::tibble(
    ud_level = paste0(level_ud * 100, "%"),
    area_low = area_row[1, 1],
    area_est = area_row[1, 2],
    area_high = area_row[1, 3]
  )
}

prepare_bird_year_dataset <- function(dat, min_points_per_bird_year = 30) {
  dat %>%
    mutate(
      ctmm_id = paste(bird_id, year_season, sep = "__")
    ) %>%
    group_by(ctmm_id) %>%
    mutate(n_points_bird_year = n()) %>%
    ungroup() %>%
    filter(n_points_bird_year >= min_points_per_bird_year)
}

run_akde_one <- function(tele_obj, fit_obj, water_boundary_sp = NULL,
                         use_weighted_akde = FALSE,
                         use_debiased_akde = TRUE) {
  if (is.null(water_boundary_sp)) {
    ctmm::akde(
      tele_obj,
      fit_obj,
      weights = use_weighted_akde,
      debias = use_debiased_akde
    )
  } else {
    ctmm::akde(
      tele_obj,
      fit_obj,
      weights = use_weighted_akde,
      debias = use_debiased_akde,
      SP = water_boundary_sp,
      SP.in = TRUE
    )
  }
}
```

# Load the analysis-ready inputs

```{r load-inputs}
tracks <- readRDS(file.path(input_dir, "tracks_clean.rds"))
trips  <- readRDS(file.path(input_dir, "trips_clean.rds"))
qa     <- readRDS(file.path(input_dir, "qa_clean.rds"))
```

# Keep only points belonging to retained final trips

```{r subset-final-trip-points}
tracks_trip_points <- tracks %>%
  filter(!is.na(global_trip_id)) %>%
  arrange(bird_id, global_trip_id, datetime)

cat("Points in retained final trips:", nrow(tracks_trip_points), "\n")
cat("Birds represented:", dplyr::n_distinct(tracks_trip_points$bird_id), "\n")
cat("Trips represented:", dplyr::n_distinct(tracks_trip_points$global_trip_id), "\n")
```

# Build full and trimmed datasets

```{r build-full-and-trimmed-datasets}
trips <- trips %>%
  mutate(
    is_extended_trip = max_distance_colony_km > extended_trip_threshold_km
  )

extended_trip_ids <- trips %>%
  filter(is_extended_trip) %>%
  pull(global_trip_id)

tracks_use_full <- tracks_trip_points
tracks_use_trimmed <- tracks_trip_points %>%
  filter(!global_trip_id %in% extended_trip_ids)

tracks_use_full <- prepare_bird_year_dataset(tracks_use_full, min_points_per_bird_year)
tracks_use_trimmed <- prepare_bird_year_dataset(tracks_use_trimmed, min_points_per_bird_year)

readr::write_csv(tracks_use_full, file.path(output_dir, "tracks_use_full.csv"))
readr::write_csv(tracks_use_trimmed, file.path(output_dir, "tracks_use_trimmed.csv"))
readr::write_csv(trips, file.path(output_dir, "trips_with_extended_flag.csv"))

cat("FULL dataset - points:", nrow(tracks_use_full), "\n")
cat("TRIMMED dataset - points:", nrow(tracks_use_trimmed), "\n")
```

# Bird-year summaries

```{r bird-year-summaries}
bird_year_summary_full <- tracks_use_full %>%
  group_by(ctmm_id, bird_id, year_season) %>%
  summarise(
    n_points = n(),
    start_time = min(datetime),
    end_time = max(datetime),
    duration_h = as.numeric(difftime(end_time, start_time, units = "hours")),
    .groups = "drop"
  ) %>%
  arrange(n_points)

bird_year_summary_trimmed <- tracks_use_trimmed %>%
  group_by(ctmm_id, bird_id, year_season) %>%
  summarise(
    n_points = n(),
    start_time = min(datetime),
    end_time = max(datetime),
    duration_h = as.numeric(difftime(end_time, start_time, units = "hours")),
    .groups = "drop"
  ) %>%
  arrange(n_points)

readr::write_csv(bird_year_summary_full, file.path(output_dir, "bird_year_summary_full.csv"))
readr::write_csv(bird_year_summary_trimmed, file.path(output_dir, "bird_year_summary_trimmed.csv"))

bird_year_summary_full
bird_year_summary_trimmed
```

# Optional water boundary

```{r build-boundary}
water_boundary_sp <- NULL
boundary_diagnostic_tbl <- tibble::tibble(
  land_boundary_used = !is.null(land_shp)
)

if (!is.null(land_shp)) {
  land_sf <- sf::st_read(land_shp, quiet = TRUE) %>%
    sf::st_transform(32721)

  pts_sf <- tracks_use_full %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    sf::st_transform(32721)

  bbox_poly <- sf::st_as_sfc(sf::st_bbox(pts_sf)) %>%
    sf::st_buffer(boundary_buffer_m)

  land_union <- sf::st_union(land_sf)
  water_poly <- sf::st_difference(bbox_poly, land_union)

  water_boundary_sp <- as(sf::st_as_sf(water_poly), "Spatial")

  water_boundary_plot <- ggplot() +
    geom_sf(data = sf::st_as_sf(water_poly), fill = "lightblue", colour = "blue", alpha = 0.4) +
    geom_sf(data = land_sf, fill = "grey60", colour = "black") +
    geom_sf(data = pts_sf, size = 0.3, alpha = 0.3) +
    labs(
      title = "Water boundary used for AKDE",
      x = "Easting",
      y = "Northing"
    ) +
    theme_paper()

  water_boundary_plot
}
```

# Store the two analysis datasets

```{r analysis-dataset-list}
analysis_datasets <- list(
  full = tracks_use_full,
  trimmed = tracks_use_trimmed
)

names(analysis_datasets)
```

# Debug dataset only: build telemetry objects

```{r debug-build-telemetry}
tracks_debug <- analysis_datasets[[debug_dataset_name]]

ctmm_input_debug <- tracks_debug %>%
  transmute(
    individual.local.identifier = ctmm_id,
    timestamp = format(datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    location.long = long,
    location.lat = lat,
    bird_id = bird_id,
    year_season = year_season,
    ctmm_id = ctmm_id
  )

tele_list_debug <- ctmm::as.telemetry(
  ctmm_input_debug,
  timezone = "UTC"
)

length(tele_list_debug)
names(tele_list_debug)
```

# Debug dataset only: initial guesses with progress bar

```{r debug-guesses}
bird_year_ids_debug <- names(tele_list_debug)
n_ids_debug <- length(bird_year_ids_debug)

pb_guess <- txtProgressBar(min = 0, max = n_ids_debug, style = 3)

guess_list_debug <- vector("list", n_ids_debug)
names(guess_list_debug) <- bird_year_ids_debug

for (i in seq_along(bird_year_ids_debug)) {
  guess_list_debug[[i]] <- ctmm::ctmm.guess(
    tele_list_debug[[i]],
    interactive = FALSE
  )
  setTxtProgressBar(pb_guess, i)
}
close(pb_guess)
```

# Debug dataset only: model selection with progress bar and timing

```{r debug-model-selection}
pb_select <- txtProgressBar(min = 0, max = n_ids_debug, style = 3)

fit_list_debug <- vector("list", n_ids_debug)
names(fit_list_debug) <- bird_year_ids_debug

fit_time_tbl_debug <- tibble::tibble(
  dataset_version = debug_dataset_name,
  ctmm_id = bird_year_ids_debug,
  elapsed_sec = NA_real_,
  status = NA_character_
)

for (i in seq_along(bird_year_ids_debug)) {
  start_time <- Sys.time()

  fit_try <- try(
    ctmm::ctmm.select(
      tele_list_debug[[i]],
      guess_list_debug[[i]],
      trace = 0
    ),
    silent = TRUE
  )

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  if (inherits(fit_try, "try-error")) {
    fit_list_debug[[i]] <- NULL
    fit_time_tbl_debug$elapsed_sec[i] <- elapsed
    fit_time_tbl_debug$status[i] <- "failed"
  } else {
    fit_list_debug[[i]] <- fit_try
    fit_time_tbl_debug$elapsed_sec[i] <- elapsed
    fit_time_tbl_debug$status[i] <- "ok"
  }

  setTxtProgressBar(pb_select, i)
}

close(pb_select)

fit_time_tbl_debug
readr::write_csv(fit_time_tbl_debug, file.path(output_dir, paste0("ctmm_fit_time_table_", debug_dataset_name, ".csv")))
```

# Debug dataset only: single bird-year AKDE timing test

```{r debug-single-akde-test}
keep_ids_debug <- fit_time_tbl_debug %>%
  filter(status == "ok") %>%
  pull(ctmm_id)

tele_list_debug_ok <- tele_list_debug[keep_ids_debug]
fit_list_debug_ok <- fit_list_debug[keep_ids_debug]

single_akde_test_tbl <- NULL

if (run_single_id_akde_test && length(tele_list_debug_ok) > 0) {
  one_id <- names(tele_list_debug_ok)[1]

  start_time <- Sys.time()

  ud_one <- run_akde_one(
    tele_obj = tele_list_debug_ok[[one_id]],
    fit_obj = fit_list_debug_ok[[one_id]],
    water_boundary_sp = water_boundary_sp,
    use_weighted_akde = use_weighted_akde,
    use_debiased_akde = use_debiased_akde
  )

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  single_akde_test_tbl <- tibble::tibble(
    dataset_version = debug_dataset_name,
    ctmm_id = one_id,
    elapsed_sec = elapsed
  )

  single_akde_test_tbl
  readr::write_csv(single_akde_test_tbl, file.path(output_dir, paste0("single_akde_test_", debug_dataset_name, ".csv")))
}
```

# Debug dataset only: AKDE one bird-year at a time with progress bar and timing

```{r debug-akde-by-id}
pb_akde <- txtProgressBar(min = 0, max = length(tele_list_debug_ok), style = 3)

ud_list_debug <- vector("list", length(tele_list_debug_ok))
names(ud_list_debug) <- names(tele_list_debug_ok)

akde_time_tbl_debug <- tibble::tibble(
  dataset_version = debug_dataset_name,
  ctmm_id = names(tele_list_debug_ok),
  elapsed_sec = NA_real_,
  status = NA_character_
)

for (i in seq_along(tele_list_debug_ok)) {
  start_time <- Sys.time()

  akde_try <- try(
    run_akde_one(
      tele_obj = tele_list_debug_ok[[i]],
      fit_obj = fit_list_debug_ok[[i]],
      water_boundary_sp = water_boundary_sp,
      use_weighted_akde = use_weighted_akde,
      use_debiased_akde = use_debiased_akde
    ),
    silent = TRUE
  )

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  if (inherits(akde_try, "try-error")) {
    ud_list_debug[[i]] <- NULL
    akde_time_tbl_debug$elapsed_sec[i] <- elapsed
    akde_time_tbl_debug$status[i] <- "failed"
  } else {
    ud_list_debug[[i]] <- akde_try
    akde_time_tbl_debug$elapsed_sec[i] <- elapsed
    akde_time_tbl_debug$status[i] <- "ok"
  }

  setTxtProgressBar(pb_akde, i)
}

close(pb_akde)

akde_time_tbl_debug
readr::write_csv(akde_time_tbl_debug, file.path(output_dir, paste0("akde_time_table_", debug_dataset_name, ".csv")))
```

# Run the full workflow for both dataset versions

```{r run-full-workflow}
analysis_results <- list()

dataset_names <- names(analysis_datasets)
pb_dataset <- txtProgressBar(min = 0, max = length(dataset_names), style = 3)

for (d in seq_along(dataset_names)) {

  dataset_name <- dataset_names[d]
  tracks_use <- analysis_datasets[[dataset_name]]

  cat("\n----------------------------------------\n")
  cat("Starting dataset:", dataset_name, "\n")
  cat("----------------------------------------\n")

  dataset_dir <- file.path(output_dir, dataset_name)
  dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)

  ctmm_input <- tracks_use %>%
    transmute(
      individual.local.identifier = ctmm_id,
      timestamp = format(datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      location.long = long,
      location.lat = lat,
      bird_id = bird_id,
      year_season = year_season,
      ctmm_id = ctmm_id
    )

  tele_list <- ctmm::as.telemetry(
    ctmm_input,
    timezone = "UTC"
  )

  bird_year_ids <- names(tele_list)
  n_ids <- length(bird_year_ids)

  pb_guess <- txtProgressBar(min = 0, max = n_ids, style = 3)

  guess_list <- vector("list", n_ids)
  names(guess_list) <- bird_year_ids

  for (i in seq_along(bird_year_ids)) {
    guess_list[[i]] <- ctmm::ctmm.guess(
      tele_list[[i]],
      interactive = FALSE
    )
    setTxtProgressBar(pb_guess, i)
  }

  close(pb_guess)

  pb_select <- txtProgressBar(min = 0, max = n_ids, style = 3)

  fit_list <- vector("list", n_ids)
  names(fit_list) <- bird_year_ids

  fit_time_tbl <- tibble::tibble(
    dataset_version = dataset_name,
    ctmm_id = bird_year_ids,
    elapsed_sec = NA_real_,
    status = NA_character_
  )

  for (i in seq_along(bird_year_ids)) {
    start_time <- Sys.time()

    fit_try <- try(
      ctmm::ctmm.select(
        tele_list[[i]],
        guess_list[[i]],
        trace = 0
      ),
      silent = TRUE
    )

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    if (inherits(fit_try, "try-error")) {
      fit_list[[i]] <- NULL
      fit_time_tbl$elapsed_sec[i] <- elapsed
      fit_time_tbl$status[i] <- "failed"
    } else {
      fit_list[[i]] <- fit_try
      fit_time_tbl$elapsed_sec[i] <- elapsed
      fit_time_tbl$status[i] <- "ok"
    }

    setTxtProgressBar(pb_select, i)
  }

  close(pb_select)
  readr::write_csv(fit_time_tbl, file.path(dataset_dir, "ctmm_fit_time_table.csv"))

  keep_ids <- fit_time_tbl %>%
    filter(status == "ok") %>%
    pull(ctmm_id)

  tele_list_ok <- tele_list[keep_ids]
  fit_list_ok <- fit_list[keep_ids]

  pb_akde <- txtProgressBar(min = 0, max = length(tele_list_ok), style = 3)

  ud_list <- vector("list", length(tele_list_ok))
  names(ud_list) <- names(tele_list_ok)

  akde_time_tbl <- tibble::tibble(
    dataset_version = dataset_name,
    ctmm_id = names(tele_list_ok),
    elapsed_sec = NA_real_,
    status = NA_character_
  )

  for (i in seq_along(tele_list_ok)) {
    start_time <- Sys.time()

    akde_try <- try(
      run_akde_one(
        tele_obj = tele_list_ok[[i]],
        fit_obj = fit_list_ok[[i]],
        water_boundary_sp = water_boundary_sp,
        use_weighted_akde = use_weighted_akde,
        use_debiased_akde = use_debiased_akde
      ),
      silent = TRUE
    )

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    if (inherits(akde_try, "try-error")) {
      ud_list[[i]] <- NULL
      akde_time_tbl$elapsed_sec[i] <- elapsed
      akde_time_tbl$status[i] <- "failed"
    } else {
      ud_list[[i]] <- akde_try
      akde_time_tbl$elapsed_sec[i] <- elapsed
      akde_time_tbl$status[i] <- "ok"
    }

    setTxtProgressBar(pb_akde, i)
  }

  close(pb_akde)
  readr::write_csv(akde_time_tbl, file.path(dataset_dir, "akde_time_table.csv"))

  keep_ud_ids <- akde_time_tbl %>%
    filter(status == "ok") %>%
    pull(ctmm_id)

  ud_list_ok <- ud_list[keep_ud_ids]

  individual_area_tbl <- purrr::map_dfr(names(ud_list_ok), function(id) {

    this_info <- tracks_use %>%
      filter(ctmm_id == id) %>%
      summarise(
        bird_id = first(bird_id),
        year_season = first(year_season)
      )

    out <- purrr::map_dfr(ud_levels, function(lvl) {
      extract_ud_area(ud_list_ok[[id]], level_ud = lvl)
    })

    out %>%
      mutate(
        dataset_version = dataset_name,
        ctmm_id = id,
        bird_id = this_info$bird_id,
        year_season = this_info$year_season
      )
  }) %>%
    select(dataset_version, ctmm_id, bird_id, year_season, everything())

  individual_area_summary_by_year <- individual_area_tbl %>%
    group_by(dataset_version, year_season, ud_level) %>%
    summarise(
      n_bird_years = n(),
      mean_area = mean(area_est, na.rm = TRUE),
      sd_area = sd(area_est, na.rm = TRUE),
      min_area = min(area_est, na.rm = TRUE),
      max_area = max(area_est, na.rm = TRUE),
      .groups = "drop"
    )

  bird_year_tbl_ok <- tracks_use %>%
    distinct(ctmm_id, year_season) %>%
    filter(ctmm_id %in% names(ud_list_ok))

  year_ud_list <- split(
    ud_list_ok,
    bird_year_tbl_ok$year_season[match(names(ud_list_ok), bird_year_tbl_ok$ctmm_id)]
  )

  year_ud_list <- lapply(year_ud_list, function(x) {
    mean(x)
  })

  year_ud_50_sf <- purrr::imap_dfr(year_ud_list, function(ud_obj, yr) {
    out <- ctmm::as.sf(ud_obj, level.UD = 0.50)
    out$year_season <- yr
    out$dataset_version <- dataset_name
    out
  })

  year_ud_95_sf <- purrr::imap_dfr(year_ud_list, function(ud_obj, yr) {
    out <- ctmm::as.sf(ud_obj, level.UD = 0.95)
    out$year_season <- yr
    out$dataset_version <- dataset_name
    out
  })

  year_mean_area_tbl <- purrr::imap_dfr(year_ud_list, function(ud_obj, yr) {
    purrr::map_dfr(ud_levels, function(lvl) {
      extract_ud_area(ud_obj, level_ud = lvl) %>%
        mutate(
          dataset_version = dataset_name,
          year_season = yr
        )
    })
  }) %>%
    select(dataset_version, year_season, everything())

  readr::write_csv(individual_area_tbl, file.path(dataset_dir, "individual_akde_area_table.csv"))
  readr::write_csv(individual_area_summary_by_year, file.path(dataset_dir, "individual_akde_area_summary_by_year.csv"))
  readr::write_csv(year_mean_area_tbl, file.path(dataset_dir, "year_mean_akde_area_table.csv"))

  sf::write_sf(
    year_ud_50_sf,
    file.path(dataset_dir, "year_mean_ud_50.gpkg"),
    delete_dsn = TRUE
  )

  sf::write_sf(
    year_ud_95_sf,
    file.path(dataset_dir, "year_mean_ud_95.gpkg"),
    delete_dsn = TRUE
  )

  saveRDS(fit_list_ok, file.path(dataset_dir, "ctmm_fit_list.rds"))
  saveRDS(ud_list_ok, file.path(dataset_dir, "ctmm_ud_list.rds"))
  saveRDS(year_ud_list, file.path(dataset_dir, "ctmm_year_mean_ud_list.rds"))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "fit_time_table")
  openxlsx::writeData(wb, "fit_time_table", fit_time_tbl)
  openxlsx::addWorksheet(wb, "akde_time_table")
  openxlsx::writeData(wb, "akde_time_table", akde_time_tbl)
  openxlsx::addWorksheet(wb, "individual_area_table")
  openxlsx::writeData(wb, "individual_area_table", individual_area_tbl)
  openxlsx::addWorksheet(wb, "individual_area_summary")
  openxlsx::writeData(wb, "individual_area_summary", individual_area_summary_by_year)
  openxlsx::addWorksheet(wb, "year_mean_area_table")
  openxlsx::writeData(wb, "year_mean_area_table", year_mean_area_tbl)

  openxlsx::saveWorkbook(
    wb,
    file.path(dataset_dir, paste0("04_akde_outputs_", dataset_name, ".xlsx")),
    overwrite = TRUE
  )

  analysis_results[[dataset_name]] <- list(
    fit_time_tbl = fit_time_tbl,
    akde_time_tbl = akde_time_tbl,
    individual_area_tbl = individual_area_tbl,
    individual_area_summary_by_year = individual_area_summary_by_year,
    year_ud_50_sf = year_ud_50_sf,
    year_ud_95_sf = year_ud_95_sf,
    year_mean_area_tbl = year_mean_area_tbl
  )

  setTxtProgressBar(pb_dataset, d)
}

close(pb_dataset)
cat("\nAll AKDE workflows completed.\n")
```

# Combine outputs across full and trimmed datasets

```{r combine-results}
combined_fit_time_tbl <- bind_rows(
  analysis_results$full$fit_time_tbl,
  analysis_results$trimmed$fit_time_tbl
)

combined_akde_time_tbl <- bind_rows(
  analysis_results$full$akde_time_tbl,
  analysis_results$trimmed$akde_time_tbl
)

combined_individual_area_tbl <- bind_rows(
  analysis_results$full$individual_area_tbl,
  analysis_results$trimmed$individual_area_tbl
)

combined_individual_area_summary_by_year <- bind_rows(
  analysis_results$full$individual_area_summary_by_year,
  analysis_results$trimmed$individual_area_summary_by_year
)

combined_year_mean_area_tbl <- bind_rows(
  analysis_results$full$year_mean_area_tbl,
  analysis_results$trimmed$year_mean_area_tbl
)
```

# Figures: timing diagnostics

```{r figures-timing}
fig_fit_time <- ggplot(
  combined_fit_time_tbl,
  aes(x = reorder(ctmm_id, elapsed_sec), y = elapsed_sec, fill = status)
) +
  geom_col() +
  facet_wrap(~ dataset_version, scales = "free_x") +
  labs(
    title = "ctmm model fitting time by bird-year",
    x = "Bird-year ID",
    y = "Elapsed time (seconds)",
    fill = "Fit status"
  ) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

fig_akde_time <- ggplot(
  combined_akde_time_tbl,
  aes(x = reorder(ctmm_id, elapsed_sec), y = elapsed_sec, fill = status)
) +
  geom_col() +
  facet_wrap(~ dataset_version, scales = "free_x") +
  labs(
    title = "AKDE estimation time by bird-year",
    x = "Bird-year ID",
    y = "Elapsed time (seconds)",
    fill = "AKDE status"
  ) +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

fig_fit_time
fig_akde_time
```

# Figures: area comparisons

```{r figures-area}
fig_individual_area <- ggplot(
  combined_individual_area_tbl,
  aes(x = year_season, y = area_est, fill = ud_level)
) +
  geom_boxplot() +
  facet_wrap(~ dataset_version) +
  labs(
    title = "Individual bird-year AKDE area by year",
    subtitle = "Comparison of full and trimmed datasets",
    x = "Year",
    y = "Area estimate",
    fill = "UD level"
  ) +
  theme_paper()

year_ud_50_all <- bind_rows(
  analysis_results$full$year_ud_50_sf,
  analysis_results$trimmed$year_ud_50_sf
)

year_ud_95_all <- bind_rows(
  analysis_results$full$year_ud_95_sf,
  analysis_results$trimmed$year_ud_95_sf
)

fig_year_ud <- ggplot() +
  geom_sf(data = year_ud_95_all, fill = "grey70", alpha = 0.4, colour = "grey30") +
  geom_sf(data = year_ud_50_all, fill = "red", alpha = 0.5, colour = "red4") +
  facet_grid(dataset_version ~ year_season) +
  labs(
    title = "Year-level mean AKDE utilisation distributions",
    subtitle = "95% UD in grey, 50% UD in red",
    x = "Longitude / Easting",
    y = "Latitude / Northing"
  ) +
  theme_paper()

fig_individual_area
fig_year_ud
```

# Save combined figures

```{r save-figures}
ggsave(
  file.path(output_dir, "fig_ctmm_fit_time_by_bird_year.png"),
  fig_fit_time,
  width = 11,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(output_dir, "fig_akde_time_by_bird_year.png"),
  fig_akde_time,
  width = 11,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(output_dir, "fig_individual_akde_area_by_year_full_vs_trimmed.png"),
  fig_individual_area,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  file.path(output_dir, "fig_year_mean_akde_ud_full_vs_trimmed.png"),
  fig_year_ud,
  width = 10,
  height = 7,
  dpi = 300
)
```

# Save combined tables

```{r save-combined-tables}
readr::write_csv(boundary_diagnostic_tbl, file.path(output_dir, "boundary_diagnostic.csv"))
readr::write_csv(combined_fit_time_tbl, file.path(output_dir, "combined_ctmm_fit_time_table.csv"))
readr::write_csv(combined_akde_time_tbl, file.path(output_dir, "combined_akde_time_table.csv"))
readr::write_csv(combined_individual_area_tbl, file.path(output_dir, "combined_individual_akde_area_table.csv"))
readr::write_csv(combined_individual_area_summary_by_year, file.path(output_dir, "combined_individual_akde_area_summary_by_year.csv"))
readr::write_csv(combined_year_mean_area_tbl, file.path(output_dir, "combined_year_mean_akde_area_table.csv"))
```

# Save combined workbook

```{r workbook}
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "boundary_diagnostic")
openxlsx::writeData(wb, "boundary_diagnostic", boundary_diagnostic_tbl)

openxlsx::addWorksheet(wb, "fit_time_table")
openxlsx::writeData(wb, "fit_time_table", combined_fit_time_tbl)

openxlsx::addWorksheet(wb, "akde_time_table")
openxlsx::writeData(wb, "akde_time_table", combined_akde_time_tbl)

openxlsx::addWorksheet(wb, "combined_individual_area")
openxlsx::writeData(wb, "combined_individual_area", combined_individual_area_tbl)

openxlsx::addWorksheet(wb, "combined_area_summary")
openxlsx::writeData(wb, "combined_area_summary", combined_individual_area_summary_by_year)

openxlsx::addWorksheet(wb, "combined_year_mean_area")
openxlsx::writeData(wb, "combined_year_mean_area", combined_year_mean_area_tbl)

openxlsx::saveWorkbook(
  wb,
  file.path(output_dir, "04_akde_space_use_ctmm_by_bird_year_debuggable_outputs.xlsx"),
  overwrite = TRUE
)
```

# Interpretation summary

```{r interpretation-summary, results='asis'}
cat("## Interpretation of AKDE outputs\n\n")

cat(
  "This analysis estimated space use using **autocorrelated kernel density estimation (AKDE)** at the **bird-year** level. ",
  "That means each movement model was fitted to one bird during one breeding season, which is a more appropriate unit for continuous-time movement modelling than single trips or pooled colony-level raw points.\n\n",
  sep = ""
)

cat(
  "The workflow was run twice: once on the **full dataset** and once on a **trimmed dataset** excluding trips with maximum range greater than **",
  extended_trip_threshold_km,
  " km**. ",
  "Comparing these two versions allows you to evaluate how strongly a rare extended trip influences the estimated space-use areas.\n\n",
  sep = ""
)

cat(
  "At the individual level, the **95% UD** represents the broader overall space used by a bird-year, whereas the **50% UD** represents the more intensively used core area. ",
  "At the colony level, year-specific mean UDs were estimated by averaging the individual bird-year UDs within each year.\n\n",
  sep = ""
)

cat(
  "The timing tables are also diagnostic. ",
  "If one bird-year takes much longer than the others, or repeatedly fails, it may indicate an awkward track structure or a problematic movement model fit or AKDE estimate that should be investigated separately.\n\n",
  sep = ""
)

cat(
  "If a land shapefile was provided, the UDs were constrained to water using a hard boundary. ",
  "That allows the space-use model to respect the penguins' inability to cross land.\n"
)
```

# End

```{r end}
cat("04_akde_space_use_ctmm_by_bird_year_debuggable completed successfully.\n")
cat("Outputs saved in:", output_dir, "\n")
```

