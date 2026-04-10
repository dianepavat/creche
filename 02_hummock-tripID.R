###############################################################################
# ROCKHOPPER FORAGING TRIP ANALYSIS + VISUALISATIONS + QA LEAFLET TOOLS
###############################################################################

# ------------------------------- LIBRARIES -----------------------------------
library(dplyr)
library(readr)
library(lubridate)
library(openxlsx)
library(ggplot2)
library(sf)
library(viridis)
library(leaflet)
library(htmlwidgets)
library(leaflet.extras)
library(leaflet.extras2)
library(jsonlite)
library(yyjsonr)

# ------------------------------ USER SETTINGS --------------------------------
data_dir <- "C:/Users/r01dp24/OneDrive - University of Aberdeen/Documents/PhD/05-Data/02 - Tracking data/03 - Processed data"
shp_falklands <- "C:/Users/r01dp24/OneDrive - University of Aberdeen/Documents/PhD/05-Data/01 - Shapefile/boundaries/falklands_boundaries.shp"

# All outputs go here
viz_dir <- file.path(getwd(), "visualisation")

# # Time zone handling: input timestamps assumed UTC; shift to UTC-3
# tz_in <- "UTC"
# tz_shift_hours <- -3
# Fixed local study time used everywhere in this script
# This prevents R from falling back to the laptop timezone
local_tz <- "Etc/GMT+3"

# --- Robust state parameters (hysteresis + debounce) ---
inner_buffer_m <- 105
outer_buffer_m <- 250
min_inside_dwell_min  <- 10
min_outside_dwell_min <- 10

# --- Trip selection thresholds (applied to TRIP WINDOW: last IN + OUT + first IN) ---
min_trip_duration_hours <- 1
min_trip_total_distance_km <- 1

# --- QA facet padding (visual only): include 2 fixes before and 2 after OUT segment ---
qa_pad_n <- 2

# --- Manual inclusion overrides ----------------------------------------------
# One row = one OUT segment to force-keep as a trip.
# These are true overrides and bypass the normal duration/distance filter.
manual_trip_overrides <- tibble::tribble(
  ~deploymentid,        ~tagnumber, ~segment_id, ~override_reason,
   "2026-01-Hummock",   "14169",   6L,          "ended_outside_in_section",
   "2026-01-Hummock",   "31610",   8L,          "ended_outside_in_section"
)

# --- Manual exclusion overrides ----------------------------------------------
# One row = one OUT segment to force-remove from the final trip list.
manual_trip_exclusions <- tibble::tribble(
  ~deploymentid,        ~tagnumber, ~segment_id, ~exclusion_reason,
   "2026-01-Hummock",   "31440",   10L,         "colony_edge_jitter_not_real_trip",
   "2026-01-Hummock",   "32722",   14L,         "colony_edge_jitter_not_real_trip",
   "2025-01-Hummock",   "45802",   10L,         "colony_edge_jitter_not_real_trip"
)

# Plot extent padding (degrees)
pad_lon_dep  <- 0.03
pad_lat_dep  <- 0.03
pad_lon_bird <- 0.01
pad_lat_bird <- 0.01

# ----------------------------- COLONY LOOKUP ---------------------------------
colonies <- data.frame(
  deploymentid = c("2026-01-Hummock", "2025-01-Hummock"), #, "2025-12-Steeple", "2024-12-Saunders"),
  colony_name  = c("Hummock",         "Hummock"),#         "Steeple",         "Saunders"),
  colony_lat   = c(-51.613481,        -51.614323),#         -51.018367,         -51.30807),
  colony_lon   = c(-60.454515,        -60.452388),#         -61.251276,         -60.22585),
  stringsAsFactors = FALSE
)

# -------------------------- LOAD FALKLANDS SHP --------------------------------
fi <- st_read(shp_falklands, quiet = TRUE)
fi <- st_transform(fi, 4326)

# --------------------------- DISTANCE (HAVERSINE) ----------------------------
calculate_distance_km <- function(lon1, lat1, lon2, lat2) {
  lon1 <- lon1 * pi / 180
  lat1 <- lat1 * pi / 180
  lon2 <- lon2 * pi / 180
  lat2 <- lat2 * pi / 180
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  6371 * c
}

# ------------------------- ROBUST DATETIME PARSER ----------------------------
make_datetime_robust <- function(df, local_tz = local_tz) {
  if ("datetime" %in% names(df)) {
    dt_chr <- as.character(df$datetime)
    dt <- suppressWarnings(lubridate::ymd_hms(dt_chr, tz = local_tz))
    
    if (sum(is.na(dt)) < 0.5 * length(dt)) {
      df$datetime <- dt
      df$date <- as.Date(df$datetime)
      df$time <- format(df$datetime, "%H:%M:%S")
      return(df)
    }
  }
  needed <- c("day", "month", "year", "hour", "minute", "second")
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) stop("Missing datetime components: ", paste(miss, collapse = ", "))

  df %>%
    mutate(
      year_full = if ("year_full" %in% names(df) && !all(is.na(year_full))) {
        as.numeric(year_full)
      } else {
        ifelse(as.numeric(year) < 100, as.numeric(year) + 2000, as.numeric(year))
      },
      datetime = make_datetime(
        year_full,
        as.numeric(month), as.numeric(day),
        as.numeric(hour), as.numeric(minute), as.numeric(second),
        tz = local_tz
      ),
      date = as.Date(datetime),
      time = format(datetime, "%H:%M:%S")
    )
}

# --------------------- DUPLICATE COLUMN NORMALISER ----------------------------
normalise_colony_cols <- function(df) {
  co2 <- function(a, b) dplyr::coalesce(a, b)

  if ("colony_name.x" %in% names(df) || "colony_name.y" %in% names(df)) {
    df$colony_name <- co2(df[["colony_name.x"]], df[["colony_name.y"]])
    df <- df %>% select(-any_of(c("colony_name.x", "colony_name.y")))
  }
  if ("colony_lat.x" %in% names(df) || "colony_lat.y" %in% names(df)) {
    df$colony_lat <- co2(df[["colony_lat.x"]], df[["colony_lat.y"]])
    df <- df %>% select(-any_of(c("colony_lat.x", "colony_lat.y")))
  }
  if ("colony_lon.x" %in% names(df) || "colony_lon.y" %in% names(df)) {
    df$colony_lon <- co2(df[["colony_lon.x"]], df[["colony_lon.y"]])
    df <- df %>% select(-any_of(c("colony_lon.x", "colony_lon.y")))
  }
  df
}

# -------------------------- HYSTERESIS + DEBOUNCE -----------------------------
build_trip_state <- function(df,
                             inner_m = 100,
                             outer_m = 250,
                             min_in_dwell_min = 10,
                             min_out_dwell_min = 10) {
  stopifnot(all(c("datetime", "dist_to_colony_km") %in% names(df)))
  df <- df %>% arrange(datetime)

  inner_km <- inner_m / 1000
  outer_km <- outer_m / 1000

  state <- character(nrow(df))
  prev <- NA_character_

  for (i in seq_len(nrow(df))) {
    d <- df$dist_to_colony_km[i]

    if (is.na(d)) {
      state[i] <- if (!is.na(prev)) prev else "OUT"
      prev <- state[i]
      next
    }

    if (d <= inner_km) {
      state[i] <- "IN"
    } else if (d >= outer_km) {
      state[i] <- "OUT"
    } else {
      state[i] <- if (!is.na(prev)) prev else "OUT"
    }

    prev <- state[i]
  }

  r <- rle(state)
  ends <- cumsum(r$lengths)
  starts <- c(1, head(ends, -1) + 1)

  run_df <- data.frame(
    run_id = seq_along(r$lengths),
    state = r$values,
    start_i = starts,
    end_i = ends,
    stringsAsFactors = FALSE
  )

  run_df$start_t <- df$datetime[run_df$start_i]
  run_df$end_t   <- df$datetime[run_df$end_i]
  run_df$dur_min <- as.numeric(difftime(run_df$end_t, run_df$start_t, units = "mins"))

  min_dwell <- function(st) {
    if (is.na(st)) return(Inf)
    if (st == "IN") min_in_dwell_min else min_out_dwell_min
  }

  run_df$too_short <- mapply(function(st, dur) dur < min_dwell(st), run_df$state, run_df$dur_min)

  state2 <- state
  for (k in which(run_df$too_short)) {
    si <- run_df$start_i[k]
    ei <- run_df$end_i[k]
    prev_state <- if (k > 1) run_df$state[k - 1] else NA_character_
    next_state <- if (k < nrow(run_df)) run_df$state[k + 1] else NA_character_
    new_state <- if (!is.na(prev_state)) prev_state else next_state
    if (!is.na(new_state)) state2[si:ei] <- new_state
  }

  r2 <- rle(state2)
  seg_id <- rep(seq_along(r2$lengths), r2$lengths)

  df$state <- state2
  df$segment_id <- seg_id
  df
}

# ------------------- ADD FINAL TRIP IDS BACK TO TRACK POINTS ------------------
annotate_track_points_with_trip_id <- function(tracks_df, trips_df) {
  if (is.null(trips_df) || nrow(trips_df) == 0) {
    tracks_df$global_trip_id <- NA_character_
    tracks_df$foraging_trip <- NA_integer_
    return(tracks_df)
  }

  tracks_df2 <- tracks_df %>%
    arrange(deploymentid, tagnumber, datetime) %>%
    group_by(deploymentid, tagnumber) %>%
    mutate(track_row_id = row_number()) %>%
    ungroup()

  seg_pos <- tracks_df2 %>%
    group_by(deploymentid, tagnumber, segment_id) %>%
    summarise(
      first_track_row_id = min(track_row_id),
      last_track_row_id  = max(track_row_id),
      .groups = "drop"
    )

  trip_point_keys <- lapply(seq_len(nrow(trips_df)), function(i) {
    dep <- trips_df$deploymentid[i]
    tag <- as.character(trips_df$tag[i])
    sid <- trips_df$segment_id[i]
    gid <- trips_df$global_trip_id[i]
    ftrip <- trips_df$foraging_trip[i]

    bird_pts <- tracks_df2 %>%
      filter(deploymentid == dep, tagnumber == tag)

    if (nrow(bird_pts) == 0) return(NULL)

    out_pts <- bird_pts %>%
      filter(segment_id == sid) %>%
      select(deploymentid, tagnumber, track_row_id) %>%
      mutate(global_trip_id = gid, foraging_trip = ftrip)

    prev_last <- seg_pos %>%
      filter(deploymentid == dep, tagnumber == tag, segment_id == sid - 1) %>%
      transmute(deploymentid, tagnumber, track_row_id = last_track_row_id) %>%
      mutate(global_trip_id = gid, foraging_trip = ftrip)

    next_first <- seg_pos %>%
      filter(deploymentid == dep, tagnumber == tag, segment_id == sid + 1) %>%
      transmute(deploymentid, tagnumber, track_row_id = first_track_row_id) %>%
      mutate(global_trip_id = gid, foraging_trip = ftrip)

    bind_rows(prev_last, out_pts, next_first)
  })

  trip_point_keys <- bind_rows(trip_point_keys) %>%
    distinct(deploymentid, tagnumber, track_row_id, .keep_all = TRUE)

  tracks_df2 %>%
    left_join(trip_point_keys, by = c("deploymentid", "tagnumber", "track_row_id")) %>%
    arrange(deploymentid, tagnumber, datetime)
}

# ----------------------- TRUE BUFFER POLYGON FOR PLOTS ------------------------
make_colony_buffer_sf <- function(col_lon, col_lat, buffer_m) {
  pt <- st_as_sf(
    data.frame(colony_lon = col_lon, colony_lat = col_lat),
    coords = c("colony_lon", "colony_lat"),
    crs = 4326
  )
  pt %>% st_transform(32721) %>% st_buffer(dist = buffer_m) %>% st_transform(4326)
}

# ------------------------------ THEMES / EXTENT -------------------------------
theme_tracks <- function() {
  theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey85", linewidth = 0.4),
      axis.title = element_blank(),
      axis.text = element_text(colour = "grey20"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.key = element_rect(fill = "white", colour = NA)
    )
}

get_track_extent <- function(df, pad_lon = 0.02, pad_lat = 0.02) {
  xr <- range(df$long, na.rm = TRUE)
  yr <- range(df$lat, na.rm = TRUE)
  list(
    xlim = c(xr[1] - pad_lon, xr[2] + pad_lon),
    ylim = c(yr[1] - pad_lat, yr[2] + pad_lat)
  )
}

# ------------------------- QA / OVERRIDE HELPERS ------------------------------
build_trip_window_metrics <- function(bird_df, sid) {
  bird_df <- bird_df %>% arrange(datetime)

  prev_seg <- bird_df %>% filter(segment_id == sid - 1)
  out_seg  <- bird_df %>% filter(segment_id == sid)
  next_seg <- bird_df %>% filter(segment_id == sid + 1)

  if (nrow(out_seg) == 0) return(NULL)

  has_prev_in <- nrow(prev_seg) > 0 && first(prev_seg$state) == "IN"
  has_next_in <- nrow(next_seg) > 0 && first(next_seg$state) == "IN"

  trip_pts <- dplyr::bind_rows(
    if (has_prev_in) tail(prev_seg, 1),
    out_seg,
    if (has_next_in) head(next_seg, 1)
  ) %>%
    arrange(datetime) %>%
    mutate(
      step_km = calculate_distance_km(lag(long), lag(lat), long, lat),
      step_km = ifelse(is.na(step_km), 0, step_km)
    )

  if (nrow(trip_pts) == 0) return(NULL)

  window_type <- dplyr::case_when(
    has_prev_in & has_next_in ~ "full_window_prev_in_out_next_in",
    !has_prev_in & has_next_in ~ "partial_window_started_outside_track",
    has_prev_in & !has_next_in ~ "partial_window_ended_outside_track",
    TRUE ~ "out_segment_only_no_bracketing_in"
  )

  tibble::tibble(
    segment_id = sid,
    trip_window_start_datetime = min(trip_pts$datetime),
    trip_window_end_datetime   = max(trip_pts$datetime),
    trip_duration_hours = as.numeric(difftime(max(trip_pts$datetime), min(trip_pts$datetime), units = "hours")),
    max_distance_colony_km = max(trip_pts$dist_to_colony_km, na.rm = TRUE),
    total_distance_km = sum(trip_pts$step_km, na.rm = TRUE),
    trip_window_n_points = nrow(trip_pts),
    trip_window_type = window_type,
    has_prev_in = has_prev_in,
    has_next_in = has_next_in
  )
}

build_trip_candidate_qa <- function(seg_tbl,
                                    tracks_df,
                                    filename,
                                    min_trip_duration_hours,
                                    min_trip_total_distance_km,
                                    manual_trip_overrides = NULL,
                                    manual_trip_exclusions = NULL) {
  if (is.null(manual_trip_overrides)) {
    manual_trip_overrides <- tibble::tibble(
      deploymentid = character(),
      tagnumber = character(),
      segment_id = integer(),
      override_reason = character()
    )
  }

  if (is.null(manual_trip_exclusions)) {
    manual_trip_exclusions <- tibble::tibble(
      deploymentid = character(),
      tagnumber = character(),
      segment_id = integer(),
      exclusion_reason = character()
    )
  }

  out_candidates <- seg_tbl %>%
    filter(seg_state == "OUT") %>%
    mutate(
      starts_track_outside = is.na(prev_state),
      ends_track_outside   = is.na(next_state),
      normal_roundtrip     = !is.na(prev_state) & !is.na(next_state) &
        prev_state == "IN" & next_state == "IN"
    ) %>%
    left_join(
      manual_trip_overrides %>%
        mutate(
          tagnumber = as.character(tagnumber),
          segment_id = as.integer(segment_id),
          manual_override = TRUE
        ),
      by = c("deploymentid", "tagnumber", "segment_id")
    ) %>%
    left_join(
      manual_trip_exclusions %>%
        mutate(
          tagnumber = as.character(tagnumber),
          segment_id = as.integer(segment_id),
          manual_exclusion = TRUE
        ),
      by = c("deploymentid", "tagnumber", "segment_id")
    ) %>%
    mutate(
      manual_override = ifelse(is.na(manual_override), FALSE, manual_override),
      override_reason = ifelse(is.na(override_reason), NA_character_, override_reason),
      manual_exclusion = ifelse(is.na(manual_exclusion), FALSE, manual_exclusion),
      exclusion_reason = ifelse(is.na(exclusion_reason), NA_character_, exclusion_reason)
    )

  if (nrow(out_candidates) == 0) return(NULL)

  qa_rows <- lapply(seq_len(nrow(out_candidates)), function(i) {
    dep <- out_candidates$deploymentid[i]
    tag <- out_candidates$tagnumber[i]
    sid <- out_candidates$segment_id[i]

    bird_df <- tracks_df %>%
      filter(deploymentid == dep, tagnumber == tag) %>%
      arrange(datetime)

    trip_metrics <- build_trip_window_metrics(bird_df, sid)
    if (is.null(trip_metrics)) return(NULL)

    this_row <- out_candidates[i, ]

    decision_reason <- dplyr::case_when(
      this_row$manual_exclusion ~ paste0("manual_exclusion__", this_row$exclusion_reason),
      this_row$manual_override ~ paste0("manual_override__", this_row$override_reason),
      this_row$normal_roundtrip &&
        trip_metrics$trip_duration_hours < min_trip_duration_hours ~ "rejected_below_min_duration",
      this_row$normal_roundtrip &&
        trip_metrics$total_distance_km < min_trip_total_distance_km ~ "rejected_below_min_total_distance",
      this_row$normal_roundtrip ~ "accepted_normal_roundtrip",
      this_row$starts_track_outside & !this_row$ends_track_outside ~ "rejected_started_outside_in_section",
      !this_row$starts_track_outside & this_row$ends_track_outside ~ "rejected_ended_outside_in_section",
      this_row$starts_track_outside & this_row$ends_track_outside ~ "rejected_started_and_ended_outside_in_section",
      !is.na(this_row$prev_state) && this_row$prev_state != "IN" ~ "rejected_prev_segment_not_in",
      !is.na(this_row$next_state) && this_row$next_state != "IN" ~ "rejected_next_segment_not_in",
      TRUE ~ "rejected_other"
    )

    retained_as_trip <- dplyr::case_when(
      this_row$manual_exclusion ~ FALSE,
      this_row$manual_override ~ TRUE,
      this_row$normal_roundtrip ~
        trip_metrics$trip_duration_hours >= min_trip_duration_hours &
        trip_metrics$total_distance_km >= min_trip_total_distance_km,
      TRUE ~ FALSE
    )

    dplyr::bind_cols(
      this_row,
      trip_metrics %>%
        dplyr::select(
          trip_window_start_datetime,
          trip_window_end_datetime,
          trip_duration_hours,
          max_distance_colony_km,
          total_distance_km,
          trip_window_n_points,
          trip_window_type,
          has_prev_in,
          has_next_in
        ),
      tibble::tibble(
        source_file = basename(filename),
        retained_as_trip = retained_as_trip,
        qa_reason = decision_reason,
        candidate_key = paste(
          this_row$deploymentid,
          this_row$tagnumber,
          this_row$segment_id,
          format(this_row$start_datetime, "%Y-%m-%d %H:%M:%S"),
          format(this_row$end_datetime, "%Y-%m-%d %H:%M:%S"),
          sep = "__"
        )
      )
    )
  })

  dplyr::bind_rows(qa_rows)
}

make_trip_candidate_map_points <- function(tracks_df, qa_tbl) {
  if (is.null(qa_tbl) || nrow(qa_tbl) == 0) return(NULL)

  qa_pts <- lapply(seq_len(nrow(qa_tbl)), function(i) {
    dep <- qa_tbl$deploymentid[i]
    tag <- qa_tbl$tagnumber[i]
    sid <- qa_tbl$segment_id[i]

    bird_df <- tracks_df %>%
      filter(deploymentid == dep, tagnumber == tag) %>%
      arrange(datetime) %>%
      mutate(track_row_id = row_number())

    if (nrow(bird_df) == 0) return(NULL)

    bird_df %>%
      filter(segment_id %in% c(sid - 1, sid, sid + 1)) %>%
      mutate(
        qa_focus_segment = sid,
        qa_reason = qa_tbl$qa_reason[i],
        retained_as_trip = qa_tbl$retained_as_trip[i],
        trip_window_type = qa_tbl$trip_window_type[i],
        manual_override = qa_tbl$manual_override[i],
        manual_exclusion = qa_tbl$manual_exclusion[i],
        point_role = dplyr::case_when(
          segment_id == sid ~ "candidate_out_segment",
          segment_id == sid - 1 ~ "previous_segment",
          segment_id == sid + 1 ~ "next_segment",
          TRUE ~ "other"
        )
      )
  })

  dplyr::bind_rows(qa_pts)
}

# --------------------------- PROCESS ONE CSV FILE ----------------------------
process_RH_file <- function(filename,
                            colonies_tbl,
                            inner_buffer_m,
                            outer_buffer_m,
                            min_inside_dwell_min,
                            min_outside_dwell_min,
                            min_trip_duration_hours,
                            min_trip_total_distance_km,
                            local_tz) {
  cat("\nProcessing:", basename(filename), "\n")

  df <- readr::read_csv(filename, show_col_types = FALSE, progress = FALSE)
  names(df) <- trimws(names(df))

  required <- c("lat", "long", "tagnumber", "deploymentid")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    cat("  Skipping (missing columns):", paste(missing, collapse = ", "), "\n")
    return(list(trips = NULL, track = NULL, seg_tbl = NULL, qa_tbl = NULL))
  }

  df <- df %>%
    mutate(
      lat = as.numeric(lat),
      long = as.numeric(long),
      tagnumber = as.character(tagnumber),
      deploymentid = as.character(deploymentid),
      day = if ("day" %in% names(df)) as.numeric(day) else NA_real_,
      month = if ("month" %in% names(df)) as.numeric(month) else NA_real_,
      year = if ("year" %in% names(df)) as.numeric(year) else NA_real_,
      hour = if ("hour" %in% names(df)) as.numeric(hour) else NA_real_,
      minute = if ("minute" %in% names(df)) as.numeric(minute) else NA_real_,
      second = if ("second" %in% names(df)) as.numeric(second) else NA_real_
    ) %>%
    filter(lat != 0, long != 0, !is.na(lat), !is.na(long))

  if (nrow(df) == 0) {
    cat("  No valid points\n")
    return(list(trips = NULL, track = NULL, seg_tbl = NULL, qa_tbl = NULL))
  }

  df <- make_datetime_robust(df, local_tz = local_tz)

  df <- df %>% left_join(colonies_tbl, by = "deploymentid")
  df <- normalise_colony_cols(df)

  if (!all(c("colony_name", "colony_lat", "colony_lon") %in% names(df))) {
    cat("  ERROR: colony columns missing after join.\n")
    print(names(df))
    return(list(trips = NULL, track = df, seg_tbl = NULL, qa_tbl = NULL))
  }

  if (all(is.na(df$colony_lat)) || all(is.na(df$colony_lon))) {
    cat("  WARNING: deploymentid not found in colonies table.\n")
    cat("  deploymentid(s) in file:", paste(unique(df$deploymentid), collapse = ", "), "\n")
    return(list(trips = NULL, track = df, seg_tbl = NULL, qa_tbl = NULL))
  }

  df <- df %>% mutate(bird_id = paste(deploymentid, tagnumber, sep = "__"))

  df <- df %>%
    arrange(deploymentid, tagnumber, datetime) %>%
    group_by(deploymentid, tagnumber) %>%
    mutate(dist_to_colony_km = mapply(calculate_distance_km, long, lat, colony_lon, colony_lat)) %>%
    ungroup()

  df <- df %>%
    group_by(deploymentid, tagnumber) %>%
    group_modify(~ build_trip_state(
      .x,
      inner_m = inner_buffer_m,
      outer_m = outer_buffer_m,
      min_in_dwell_min = min_inside_dwell_min,
      min_out_dwell_min = min_outside_dwell_min
    )) %>%
    ungroup()

  seg_tbl <- df %>%
    group_by(deploymentid, tagnumber, segment_id) %>%
    summarise(
      seg_state = first(state),
      start_datetime = min(datetime),
      end_datetime   = max(datetime),
      seg_duration_min = as.numeric(difftime(end_datetime, start_datetime, units = "mins")),
      max_dist_km = max(dist_to_colony_km, na.rm = TRUE),
      n_points = n(),
      colony_name = first(colony_name),
      colony_lat = first(colony_lat),
      colony_lon = first(colony_lon),
      .groups = "drop"
    ) %>%
    arrange(deploymentid, tagnumber, start_datetime) %>%
    group_by(deploymentid, tagnumber) %>%
    mutate(
      prev_state = lag(seg_state),
      next_state = lead(seg_state),
      is_roundtrip_out_segment = (seg_state == "OUT" & prev_state == "IN" & next_state == "IN")
    ) %>%
    ungroup()

  qa_tbl <- build_trip_candidate_qa(
    seg_tbl = seg_tbl,
    tracks_df = df,
    filename = filename,
    min_trip_duration_hours = min_trip_duration_hours,
    min_trip_total_distance_km = min_trip_total_distance_km,
    manual_trip_overrides = manual_trip_overrides,
    manual_trip_exclusions = manual_trip_exclusions
  )

  if (is.null(qa_tbl) || nrow(qa_tbl) == 0) {
    cat("  Trips found: 0\n")
    return(list(trips = NULL, track = df, seg_tbl = seg_tbl, qa_tbl = NULL))
  }

  trip_rows <- qa_tbl %>%
    filter(retained_as_trip) %>%
    arrange(deploymentid, tagnumber, trip_window_start_datetime) %>%
    group_by(deploymentid, tagnumber) %>%
    mutate(foraging_trip = row_number()) %>%
    ungroup()
  
  # Add the within-bird trip number back into the QA table for accepted trips
  qa_tbl <- qa_tbl %>%
    left_join(
      trip_rows %>%
        select(deploymentid, tagnumber, segment_id, foraging_trip),
      by = c("deploymentid", "tagnumber", "segment_id")
    ) %>%
    rename(candidate_trip_number = foraging_trip)
  
  # Now continue building the final trip table
  trip_rows <- trip_rows %>%
    mutate(
      tag = tagnumber,
      bird_id = paste(deploymentid, tag, sep = "__"),
      start_date = as.Date(trip_window_start_datetime),
      start_time = format(trip_window_start_datetime, "%H:%M:%S"),
      end_date = as.Date(trip_window_end_datetime),
      end_time = format(trip_window_end_datetime, "%H:%M:%S"),
      inner_buffer_m = inner_buffer_m,
      outer_buffer_m = outer_buffer_m,
      min_inside_dwell_min = min_inside_dwell_min,
      min_outside_dwell_min = min_outside_dwell_min,
      min_trip_duration_hours = min_trip_duration_hours,
      min_trip_total_distance_km = min_trip_total_distance_km
    ) %>%
    select(
      source_file, deploymentid, colony_name,
      bird_id, tag, foraging_trip,
      start_date, start_time, end_date, end_time,
      trip_duration_hours, max_distance_colony_km, total_distance_km, trip_window_n_points,
      colony_lat, colony_lon,
      inner_buffer_m, outer_buffer_m,
      min_inside_dwell_min, min_outside_dwell_min,
      min_trip_duration_hours, min_trip_total_distance_km,
      segment_id, qa_reason, trip_window_type, manual_override, manual_exclusion
    ) %>%
    rename(
      colony = colony_name,
      n_points = trip_window_n_points
    )

  cat("  Candidate OUT segments reviewed:", nrow(qa_tbl), "\n")
  cat("  Trips found (after thresholds + overrides):", nrow(trip_rows), "\n")

  list(trips = trip_rows, track = df, seg_tbl = seg_tbl, qa_tbl = qa_tbl)
}

# ------------------------- PROCESS ALL FILES (MAIN) ---------------------------
process_all_RH_files <- function(data_dir, colonies_tbl) {
  cat("\n========================================\n")
  cat("RH FORAGING TRIP ANALYSIS (ROBUST + QA)\n")
  cat("========================================\n")

  if (!dir.exists(data_dir)) stop("data_dir does not exist: ", data_dir)
  if (!dir.exists(viz_dir)) dir.create(viz_dir, recursive = TRUE)

  data_files <- list.files(
    path = data_dir,
    pattern = "*.(csv|CSV)$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  cat("\nFound", length(data_files), "csv file(s)\n")
  if (length(data_files) == 0) stop("No *.csv files found in: ", data_dir)

  trips_list  <- list()
  tracks_list <- list()
  segs_list   <- list()
  qa_list     <- list()

  for (f in data_files) {
    res <- process_RH_file(
      f,
      colonies_tbl = colonies_tbl,
      inner_buffer_m = inner_buffer_m,
      outer_buffer_m = outer_buffer_m,
      min_inside_dwell_min = min_inside_dwell_min,
      min_outside_dwell_min = min_outside_dwell_min,
      min_trip_duration_hours = min_trip_duration_hours,
      min_trip_total_distance_km = min_trip_total_distance_km,
      local_tz = local_tz
    )

    if (!is.null(res$track) && nrow(res$track) > 0) tracks_list[[basename(f)]] <- res$track
    if (!is.null(res$seg_tbl) && nrow(res$seg_tbl) > 0) segs_list[[basename(f)]] <- res$seg_tbl
    if (!is.null(res$qa_tbl) && nrow(res$qa_tbl) > 0) qa_list[[basename(f)]] <- res$qa_tbl
    if (!is.null(res$trips) && nrow(res$trips) > 0) trips_list[[basename(f)]] <- res$trips
  }

  combined_tracks <- bind_rows(tracks_list)
  combined_segs   <- bind_rows(segs_list)
  combined_qa     <- bind_rows(qa_list)
  combined_trips  <- bind_rows(trips_list)

  if (is.null(combined_trips) || nrow(combined_trips) == 0) {
    cat("\nNo trips found across all files with current thresholds.\n")
    if (nrow(combined_qa) > 0) {
      write_csv(combined_qa, file.path(viz_dir, "RH_TRIP_QA_TABLE.csv"))
    }
    return(list(trips = NULL, tracks = combined_tracks, segs = combined_segs, qa = combined_qa, summary = NULL))
  }

  combined_trips <- combined_trips %>%
    group_by(bird_id) %>%
    arrange(start_date, start_time, .by_group = TRUE) %>%
    mutate(global_trip_id = paste("BIRD", bird_id, "TRIP", sprintf("%03d", row_number()), sep = "_")) %>%
    ungroup() %>%
    select(global_trip_id, everything())
  
  combined_qa <- combined_qa %>%
    left_join(
      combined_trips %>%
        select(deploymentid, tag, segment_id, global_trip_id, foraging_trip) %>%
        rename(
          tagnumber = tag,
          candidate_global_trip_id = global_trip_id,
          candidate_trip_number = foraging_trip
        ),
      by = c("deploymentid", "tagnumber", "segment_id")
    )

  combined_tracks_with_trip_id <- annotate_track_points_with_trip_id(
    tracks_df = combined_tracks,
    trips_df = combined_trips
  )

  summary_by_bird <- combined_trips %>%
    group_by(deploymentid, tag, bird_id, colony) %>%
    summarise(
      n_trips = n(),
      min_duration_hours  = min(trip_duration_hours, na.rm = TRUE),
      max_duration_hours  = max(trip_duration_hours, na.rm = TRUE),
      mean_duration_hours = mean(trip_duration_hours, na.rm = TRUE),
      sd_duration_hours   = sd(trip_duration_hours, na.rm = TRUE),
      median_duration_hours = median(trip_duration_hours, na.rm = TRUE),
      min_maxdist_km  = min(max_distance_colony_km, na.rm = TRUE),
      max_maxdist_km  = max(max_distance_colony_km, na.rm = TRUE),
      mean_maxdist_km = mean(max_distance_colony_km, na.rm = TRUE),
      sd_maxdist_km   = sd(max_distance_colony_km, na.rm = TRUE),
      min_totaldist_km  = min(total_distance_km, na.rm = TRUE),
      max_totaldist_km  = max(total_distance_km, na.rm = TRUE),
      mean_totaldist_km = mean(total_distance_km, na.rm = TRUE),
      sd_totaldist_km   = sd(total_distance_km, na.rm = TRUE),
      first_departure = min(as.POSIXct(paste(start_date, start_time))),
      last_arrival    = max(as.POSIXct(paste(end_date, end_time))),
      .groups = "drop"
    )

  trips_per_bird <- combined_trips %>%
    group_by(deploymentid, bird_id) %>%
    summarise(n_trips = n(), .groups = "drop")

  summary_by_deployment <- combined_trips %>%
    group_by(deploymentid, colony) %>%
    summarise(
      n_birds = n_distinct(bird_id),
      n_trips_total = n(),
      min_trips_per_bird = min(trips_per_bird$n_trips[trips_per_bird$deploymentid == first(deploymentid)]),
      max_trips_per_bird = max(trips_per_bird$n_trips[trips_per_bird$deploymentid == first(deploymentid)]),
      mean_trips_per_bird = mean(trips_per_bird$n_trips[trips_per_bird$deploymentid == first(deploymentid)]),
      min_duration_hours  = min(trip_duration_hours, na.rm = TRUE),
      max_duration_hours  = max(trip_duration_hours, na.rm = TRUE),
      mean_duration_hours = mean(trip_duration_hours, na.rm = TRUE),
      sd_duration_hours   = sd(trip_duration_hours, na.rm = TRUE),
      min_totaldist_km  = min(total_distance_km, na.rm = TRUE),
      max_totaldist_km  = max(total_distance_km, na.rm = TRUE),
      mean_totaldist_km = mean(total_distance_km, na.rm = TRUE),
      sd_totaldist_km   = sd(total_distance_km, na.rm = TRUE),
      min_maxdist_km  = min(max_distance_colony_km, na.rm = TRUE),
      max_maxdist_km  = max(max_distance_colony_km, na.rm = TRUE),
      mean_maxdist_km = mean(max_distance_colony_km, na.rm = TRUE),
      sd_maxdist_km   = sd(max_distance_colony_km, na.rm = TRUE),
      earliest_departure = min(as.POSIXct(paste(start_date, start_time))),
      latest_departure   = max(as.POSIXct(paste(start_date, start_time))),
      earliest_arrival   = min(as.POSIXct(paste(end_date, end_time))),
      latest_arrival     = max(as.POSIXct(paste(end_date, end_time))),
      .groups = "drop"
    )

  summary_overall <- combined_trips %>%
    summarise(
      n_deployments = n_distinct(deploymentid),
      n_birds = n_distinct(bird_id),
      n_trips = n(),
      trips_per_bird_mean = n_trips / n_birds,
      mean_duration_hours = mean(trip_duration_hours, na.rm = TRUE),
      sd_duration_hours   = sd(trip_duration_hours, na.rm = TRUE),
      mean_max_distance_km = mean(max_distance_colony_km, na.rm = TRUE),
      sd_max_distance_km   = sd(max_distance_colony_km, na.rm = TRUE),
      mean_total_distance_km = mean(total_distance_km, na.rm = TRUE),
      total_distance_all_trips_km = sum(total_distance_km, na.rm = TRUE),
      earliest_trip_start = min(as.POSIXct(paste(start_date, start_time), tz = "UTC"), na.rm = TRUE),
      latest_trip_end     = max(as.POSIXct(paste(end_date, end_time), tz = "UTC"), na.rm = TRUE)
    )

  write_csv(combined_trips, file.path(viz_dir, "RH_FORAGING_TRIPS.csv"))
  write_csv(combined_tracks_with_trip_id, file.path(viz_dir, "RH_TRACK_POINTS_WITH_GLOBAL_TRIP_ID.csv"))
  write_csv(combined_qa, file.path(viz_dir, "RH_TRIP_QA_TABLE.csv"))
  write_csv(summary_by_bird,       file.path(viz_dir, "RH_SUMMARY_BY_BIRD.csv"))
  write_csv(summary_by_deployment, file.path(viz_dir, "RH_SUMMARY_BY_DEPLOYMENT.csv"))
  write_csv(summary_overall,       file.path(viz_dir, "RH_SUMMARY_OVERALL.csv"))

  wb <- createWorkbook()
  addWorksheet(wb, "Summary by Bird")
  writeData(wb, "Summary by Bird", summary_by_bird)
  addWorksheet(wb, "Summary by Deployment")
  writeData(wb, "Summary by Deployment", summary_by_deployment)
  addWorksheet(wb, "Summary Overall")
  writeData(wb, "Summary Overall", summary_overall)
  addWorksheet(wb, "QA Table")
  writeData(wb, "QA Table", combined_qa)
  addWorksheet(wb, "Parameters")

  params <- data.frame(
    Parameter = c(
      "Inner buffer (m)", "Outer buffer (m)",
      "Min inside dwell (min)", "Min outside dwell (min)",
      "Min trip duration (hours)", "Min trip total distance (km)",
      "Trip stats include", "QA facet padding fixes (before/after)",
      "TZ shift hours", "Analysis Date", "Files Processed",
      "Total Trips", "Unique Birds"
    ),
    Value = c(
      inner_buffer_m, outer_buffer_m,
      min_inside_dwell_min, min_outside_dwell_min,
      min_trip_duration_hours, min_trip_total_distance_km,
      "last IN + all OUT + first IN",
      qa_pad_n,
      local_tz, as.character(Sys.Date()), length(list.files(data_dir, pattern = "*.(csv|CSV)$", ignore.case = TRUE)),
      nrow(combined_trips), length(unique(combined_trips$bird_id))
    ),
    stringsAsFactors = FALSE
  )

  writeData(wb, "Parameters", params)
  addWorksheet(wb, "Trips by File")
  file_summary <- combined_trips %>%
    group_by(source_file, deploymentid, tag) %>%
    summarise(n_trips = n(), .groups = "drop") %>%
    arrange(source_file, deploymentid, tag)
  writeData(wb, "Trips by File", file_summary)
  saveWorkbook(wb, file.path(viz_dir, "RH_ANALYSIS_RESULTS.xlsx"), overwrite = TRUE)

  cat("\nSaved to:", viz_dir, "\n")

  list(
    trips = combined_trips,
    tracks = combined_tracks_with_trip_id,
    segs = combined_segs,
    qa = combined_qa,
    summary_by_bird = summary_by_bird,
    summary_by_deployment = summary_by_deployment,
    summary_overall = summary_overall
  )
}

# ------------------------- QA FACET DATA BUILDER ------------------------------
make_tripcheck_facet_points <- function(tracks_df, trips_df, n_pad = 2) {
  if (is.null(trips_df) || nrow(trips_df) == 0) return(NULL)

  keys <- trips_df %>%
    select(deploymentid, tag, bird_id, foraging_trip, segment_id) %>%
    distinct()

  facet_points <- lapply(seq_len(nrow(keys)), function(i) {
    dep <- keys$deploymentid[i]
    tag <- as.character(keys$tag[i])
    bid <- keys$bird_id[i]
    trip_n <- keys$foraging_trip[i]
    sid <- keys$segment_id[i]

    bird_track <- tracks_df %>%
      filter(deploymentid == dep, tagnumber == tag) %>%
      arrange(datetime)

    if (nrow(bird_track) == 0) return(NULL)

    prev_pts <- bird_track %>% filter(segment_id == sid - 1) %>% tail(n_pad)
    out_pts  <- bird_track %>% filter(segment_id == sid)
    next_pts <- bird_track %>% filter(segment_id == sid + 1) %>% head(n_pad)

    if (nrow(out_pts) == 0) return(NULL)

    bind_rows(prev_pts, out_pts, next_pts) %>%
      arrange(datetime) %>%
      mutate(foraging_trip = trip_n, bird_id = bid)
  })

  bind_rows(facet_points)
}

# ------------------------ STATIC INTERACTIVE QA MAP ---------------------------
create_trip_qa_leaflet <- function(tracks_df,
                                   qa_tbl,
                                   deployment_id,
                                   tag_id,
                                   output_html = NULL) {
  tag_id <- as.character(tag_id)

  bird_track <- tracks_df %>%
    filter(deploymentid == deployment_id, tagnumber == tag_id) %>%
    arrange(datetime)

  bird_qa <- qa_tbl %>%
    filter(deploymentid == deployment_id, tagnumber == tag_id) %>%
    arrange(segment_id)

  if (nrow(bird_track) == 0) stop("No track data found for this deployment/tag")
  if (nrow(bird_qa) == 0) stop("No QA candidate segments found for this deployment/tag")

  if (!"manual_exclusion" %in% names(bird_qa)) {
    bird_qa <- bird_qa %>% mutate(manual_exclusion = FALSE, exclusion_reason = NA_character_)
  }

  map_pts <- make_trip_candidate_map_points(bird_track, bird_qa)
  if (is.null(map_pts) || nrow(map_pts) == 0) stop("No map points created for this deployment/tag")

  if (!"manual_exclusion" %in% names(map_pts)) {
    map_pts <- map_pts %>% mutate(manual_exclusion = FALSE, exclusion_reason = NA_character_)
  }

  col_xy <- bird_track %>%
    distinct(colony_name, colony_lon, colony_lat) %>%
    slice(1)

  inner_buf <- make_colony_buffer_sf(col_xy$colony_lon, col_xy$colony_lat, inner_buffer_m)
  outer_buf <- make_colony_buffer_sf(col_xy$colony_lon, col_xy$colony_lat, outer_buffer_m)

  map_pts <- map_pts %>%
    mutate(
      qa_display_group = case_when(
        point_role != "candidate_out_segment" ~ "Neighbouring context",
        manual_exclusion ~ "Manually removed trip",
        manual_override ~ "Manually added trip",
        retained_as_trip ~ "Accepted trip",
        TRUE ~ "Rejected candidate"
      ),
      popup_txt = paste0(
        "<b>Deployment:</b> ", deploymentid, "<br>",
        "<b>Tag:</b> ", tagnumber, "<br>",
        "<b>Date-time:</b> ", datetime, "<br>",
        "<b>Segment ID:</b> ", segment_id, "<br>",
        "<b>Focus segment:</b> ", qa_focus_segment, "<br>",
        "<b>State:</b> ", state, "<br>",
        "<b>Point role:</b> ", point_role, "<br>",
        "<b>Distance to colony (km):</b> ", round(dist_to_colony_km, 3), "<br>",
        "<b>Status on map:</b> ", qa_display_group, "<br>",
        "<b>QA reason:</b> ", qa_reason, "<br>",
        "<b>Retained as trip:</b> ", retained_as_trip, "<br>",
        "<b>Manual add override:</b> ", manual_override, "<br>",
        "<b>Manual remove override:</b> ", manual_exclusion, "<br>",
        "<b>Trip window type:</b> ", trip_window_type,
        if ("foraging_trip" %in% names(map_pts)) paste0("<br><b>Foraging trip:</b> ", ifelse(is.na(foraging_trip), "NA", foraging_trip)) else "",
        if ("global_trip_id" %in% names(map_pts)) paste0("<br><b>Global trip ID:</b> ", ifelse(is.na(global_trip_id), "NA", global_trip_id)) else ""
      )
    )

  pts_context <- map_pts %>% filter(qa_display_group == "Neighbouring context")
  pts_accept  <- map_pts %>% filter(qa_display_group == "Accepted trip")
  pts_reject  <- map_pts %>% filter(qa_display_group == "Rejected candidate")
  pts_add     <- map_pts %>% filter(qa_display_group == "Manually added trip")
  pts_remove  <- map_pts %>% filter(qa_display_group == "Manually removed trip")

  has_trip_cols <- all(c("foraging_trip", "global_trip_id") %in% names(bird_track))

  if (has_trip_cols) {
    trip_line_df <- bird_track %>%
      filter(!is.na(foraging_trip), !is.na(global_trip_id)) %>%
      arrange(foraging_trip, datetime)

    trip_starts <- trip_line_df %>%
      group_by(foraging_trip, global_trip_id) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(trip_label = paste0("Trip ", foraging_trip))
  } else {
    trip_line_df <- NULL
    trip_starts <- NULL
  }

  bird_track <- bird_track %>%
    mutate(
      point_popup = paste0(
        "<b>Date-time:</b> ", datetime, "<br>",
        "<b>Segment ID:</b> ", segment_id, "<br>",
        "<b>State:</b> ", state, "<br>",
        "<b>Distance to colony (km):</b> ", round(dist_to_colony_km, 3),
        if ("foraging_trip" %in% names(bird_track)) paste0("<br><b>Foraging trip:</b> ", ifelse(is.na(foraging_trip), "NA", foraging_trip)) else "",
        if ("global_trip_id" %in% names(bird_track)) paste0("<br><b>Global trip ID:</b> ", ifelse(is.na(global_trip_id), "NA", global_trip_id)) else ""
      )
    )

  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = outer_buf, color = "red", weight = 1, fillColor = "red", fillOpacity = 0.06,
                group = "Outer buffer (250 m)") %>%
    addPolygons(data = inner_buf, color = "blue", weight = 1, fillColor = "blue", fillOpacity = 0.06,
                group = "Inner buffer (100 m)") %>%
    addPolylines(data = bird_track, lng = ~long, lat = ~lat, color = "grey60", weight = 2, opacity = 0.5,
                 group = "Full track line", popup = "Full track") %>%
    addCircleMarkers(data = bird_track, lng = ~long, lat = ~lat, radius = 2, stroke = FALSE,
                     fillOpacity = 0.30, color = "grey60", group = "All bird points (full track)",
                     popup = ~point_popup)

  if (!is.null(trip_line_df) && nrow(trip_line_df) > 0) {
    trip_ids <- sort(unique(trip_line_df$foraging_trip))

    for (trip_i in trip_ids) {
      one_trip <- trip_line_df %>%
        filter(foraging_trip == trip_i) %>%
        arrange(datetime)

      m <- m %>%
        addPolylines(
          data = one_trip,
          lng = ~long,
          lat = ~lat,
          weight = 4,
          opacity = 0.9,
          group = "Accepted trip lines",
          popup = paste0(
            "<b>Foraging trip:</b> ", unique(one_trip$foraging_trip), "<br>",
            "<b>Global trip ID:</b> ", unique(one_trip$global_trip_id)
          )
        )
    }

    m <- m %>%
      addLabelOnlyMarkers(
        data = trip_starts,
        lng = ~long,
        lat = ~lat,
        label = ~trip_label,
        group = "Trip number labels",
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textsize = "12px",
          style = list(
            "font-weight" = "bold",
            "color" = "black",
            "background-color" = "white",
            "padding" = "2px 4px",
            "border" = "1px solid black"
          )
        )
      )
  }

  m <- m %>%
    addCircleMarkers(data = pts_context, lng = ~long, lat = ~lat, radius = 4, stroke = TRUE,
                     weight = 1, color = "darkorchid4", fillOpacity = 0.75,
                     group = "Neighbouring context", popup = ~popup_txt) %>%
    addCircleMarkers(data = pts_accept, lng = ~long, lat = ~lat, radius = 5, stroke = TRUE,
                     weight = 1, color = "darkgreen", fillOpacity = 0.9,
                     group = "Accepted QA candidates", popup = ~popup_txt) %>%
    addCircleMarkers(data = pts_reject, lng = ~long, lat = ~lat, radius = 5, stroke = TRUE,
                     weight = 1, color = "orange", fillOpacity = 0.9,
                     group = "Rejected QA candidates", popup = ~popup_txt) %>%
    addCircleMarkers(data = pts_add, lng = ~long, lat = ~lat, radius = 6, stroke = TRUE,
                     weight = 2, color = "cyan", fillOpacity = 0.95,
                     group = "Manually added trips", popup = ~popup_txt) %>%
    addCircleMarkers(data = pts_remove, lng = ~long, lat = ~lat, radius = 6, stroke = TRUE,
                     weight = 2, color = "black", fillOpacity = 0.95,
                     group = "Manually removed trips", popup = ~popup_txt) %>%
    addMarkers(lng = col_xy$colony_lon, lat = col_xy$colony_lat,
               popup = paste0("<b>Colony:</b> ", col_xy$colony_name, "<br>",
                              "<b>Lon:</b> ", round(col_xy$colony_lon, 5), "<br>",
                              "<b>Lat:</b> ", round(col_xy$colony_lat, 5)),
               group = "Colony") %>%
    addLayersControl(
      overlayGroups = c(
        "Full track line",
        "All bird points (full track)",
        "Accepted trip lines",
        "Trip number labels",
        "Accepted QA candidates",
        "Rejected QA candidates",
        "Manually added trips",
        "Manually removed trips",
        "Neighbouring context",
        "Inner buffer (100 m)",
        "Outer buffer (250 m)",
        "Colony"
      ),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addControl(
      html = paste0(
        "<div style='background: white; padding: 10px; border: 1px solid #ccc; font-size: 13px; line-height: 1.4;'>",
        "<b>Trip QA map</b><br>",
        "Grey line = full track<br>",
        "Green points = accepted QA candidate<br>",
        "Orange points = rejected QA candidate<br>",
        "Cyan points = manually added trip<br>",
        "Black points = manually removed trip<br>",
        "Purple points = neighbouring context<br>",
        "Trip labels = final retained trip numbers<br>",
        "Blue/Red = inner / outer colony buffers",
        "</div>"
      ),
      position = "bottomright"
    )

  if (!is.null(output_html)) {
    htmlwidgets::saveWidget(m, file = output_html, selfcontained = TRUE)
  }

  m
}

# ---------------------- TIME-SLIDER INTERACTIVE MAP --------------------------
make_date_colour_lookup <- function(dates_vec) {
  date_levels <- sort(unique(as.Date(dates_vec)))
  cols <- viridis::viridis(length(date_levels), option = "viridis")
  
  data.frame(
    date = as.Date(date_levels),
    date_colour = cols,
    stringsAsFactors = FALSE
  )
}

create_trip_time_slider_leaflet <- function(tracks_df,
                                            deployment_id,
                                            tag_id,
                                            output_html = NULL) {
  tag_id <- as.character(tag_id)
  
  bird_track <- tracks_df %>%
    filter(deploymentid == deployment_id, tagnumber == tag_id) %>%
    arrange(datetime)
  
  if (nrow(bird_track) == 0) stop("No track data found for this deployment/tag")
  
  if (!all(c("datetime", "long", "lat") %in% names(bird_track))) {
    stop("tracks_df must contain: datetime, long, lat")
  }
  
  if (!"foraging_trip" %in% names(bird_track)) bird_track$foraging_trip <- NA_integer_
  if (!"global_trip_id" %in% names(bird_track)) bird_track$global_trip_id <- NA_character_
  
  bird_track <- bird_track %>%
    mutate(date = as.Date(datetime))
  
  # ---------------------------------------------------------------------------
  # Assign one viridis colour per date
  # ---------------------------------------------------------------------------
  date_lookup <- make_date_colour_lookup(bird_track$date)
  
  bird_track <- bird_track %>%
    left_join(date_lookup, by = "date")
  
  col_xy <- bird_track %>%
    distinct(colony_name, colony_lon, colony_lat) %>%
    slice(1)
  
  inner_buf <- make_colony_buffer_sf(col_xy$colony_lon, col_xy$colony_lat, inner_buffer_m)
  outer_buf <- make_colony_buffer_sf(col_xy$colony_lon, col_xy$colony_lat, outer_buffer_m)
  
  # ---------------------------------------------------------------------------
  # Time-aware points
  # ---------------------------------------------------------------------------
  time_pts <- bird_track %>%
    mutate(
      popup = paste0(
        "<b>Date-time:</b> ", datetime, "<br>",
        "<b>Date:</b> ", date, "<br>",
        "<b>State:</b> ", state, "<br>",
        "<b>Segment ID:</b> ", segment_id, "<br>",
        "<b>Distance to colony (km):</b> ", round(dist_to_colony_km, 3), "<br>",
        "<b>Foraging trip:</b> ", ifelse(is.na(foraging_trip), "NA", foraging_trip), "<br>",
        "<b>Global trip ID:</b> ", ifelse(is.na(global_trip_id), "NA", global_trip_id)
      ),
      time = as.POSIXct(datetime, tz = "UTC")
    ) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
  
  # ---------------------------------------------------------------------------
  # Time-aware short line segments
  # Each segment uses the colour of its end point's date
  # ---------------------------------------------------------------------------
  seg_df <- bird_track %>%
    mutate(
      long_next = lead(long),
      lat_next = lead(lat),
      datetime_next = lead(datetime),
      date_next = lead(date),
      date_colour_next = lead(date_colour),
      foraging_trip_next = lead(foraging_trip),
      global_trip_id_next = lead(global_trip_id)
    ) %>%
    filter(!is.na(long_next), !is.na(lat_next), !is.na(datetime_next))
  
  if (nrow(seg_df) > 0) {
    seg_sf_list <- lapply(seq_len(nrow(seg_df)), function(i) {
      row <- seg_df[i, ]
      
      line_geom <- st_linestring(matrix(
        c(row$long, row$long_next, row$lat, row$lat_next),
        ncol = 2
      ))
      
      st_sf(
        time = as.POSIXct(row$datetime_next, tz = "UTC"),
        popup = paste0(
          "<b>Track segment ending:</b> ", row$datetime_next, "<br>",
          "<b>Date:</b> ", row$date_next, "<br>",
          "<b>From:</b> ", row$datetime, "<br>",
          "<b>To:</b> ", row$datetime_next, "<br>",
          "<b>Foraging trip:</b> ", ifelse(is.na(row$foraging_trip_next), "NA", row$foraging_trip_next), "<br>",
          "<b>Global trip ID:</b> ", ifelse(is.na(row$global_trip_id_next), "NA", row$global_trip_id_next),
          "<br><b>Colour meaning:</b> date-based viridis colour"
        ),
        colour_value = row$date_colour_next,
        geometry = st_sfc(line_geom, crs = 4326)
      )
    })
    
    seg_sf <- do.call(rbind, seg_sf_list)
  } else {
    seg_sf <- NULL
  }
  
  # ---------------------------------------------------------------------------
  # Build map
  # ---------------------------------------------------------------------------
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    addPolygons(
      data = outer_buf,
      color = "red",
      weight = 1,
      fillColor = "red",
      fillOpacity = 0.06,
      group = "Outer buffer (250 m)"
    ) %>%
    
    addPolygons(
      data = inner_buf,
      color = "blue",
      weight = 1,
      fillColor = "blue",
      fillOpacity = 0.06,
      group = "Inner buffer (100 m)"
    ) %>%
    
    # faint static background path
    addPolylines(
      data = bird_track,
      lng = ~long,
      lat = ~lat,
      color = "grey60",
      weight = 2,
      opacity = 0.20,
      group = "Full track line"
    ) %>%
    
    addMarkers(
      lng = col_xy$colony_lon,
      lat = col_xy$colony_lat,
      popup = paste0(
        "<b>Colony:</b> ", col_xy$colony_name, "<br>",
        "<b>Lon:</b> ", round(col_xy$colony_lon, 5), "<br>",
        "<b>Lat:</b> ", round(col_xy$colony_lat, 5)
      ),
      group = "Colony"
    )
  
  # ---------------------------------------------------------------------------
  # Time slider for points
  # ---------------------------------------------------------------------------
  m <- m %>%
    leaflet.extras2::addTimeslider(
      data = time_pts,
      options = timesliderOptions(
        position = "topleft",
        timeStrLength = 16,
        follow = FALSE,
        sameDate = FALSE,
        range = TRUE,
        alwaysShowDate = TRUE
      ),
      popup = ~popup,
      radius = 5,
      color = ~date_colour,
      fillOpacity = 0.95,
      stroke = TRUE
    )
  
  # ---------------------------------------------------------------------------
  # Time slider for short line segments
  # ---------------------------------------------------------------------------
  if (!is.null(seg_sf) && nrow(seg_sf) > 0) {
    m <- m %>%
      leaflet.extras2::addTimeslider(
        data = seg_sf,
        options = timesliderOptions(
          position = "topleft",
          timeStrLength = 16,
          follow = FALSE,
          sameDate = FALSE,
          range = TRUE,
          alwaysShowDate = TRUE
        ),
        popup = ~popup,
        color = ~colour_value,
        weight = 4,
        opacity = 0.95
      )
  }
  
  # ---------------------------------------------------------------------------
  # Date legend
  # ---------------------------------------------------------------------------
  legend_html <- paste0(
    "<div style='background: white; padding: 10px; border: 1px solid #ccc; font-size: 12px; line-height: 1.4;'>",
    "<b>Trip time map</b><br>",
    "Faint grey = full track context<br>",
    "Slider points and segments are coloured by <b>date</b><br><br>",
    paste(
      paste0(
        "<span style='display:inline-block;width:12px;height:12px;background:",
        date_lookup$date_colour,
        ";margin-right:6px;border:1px solid #999;'></span>",
        as.character(date_lookup$date)
      ),
      collapse = "<br>"
    ),
    "</div>"
  )
  
  m <- m %>%
    addLayersControl(
      overlayGroups = c(
        "Full track line",
        "Inner buffer (100 m)",
        "Outer buffer (250 m)",
        "Colony"
      ),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addControl(
      html = legend_html,
      position = "bottomright"
    )
  
  if (!is.null(output_html)) {
    htmlwidgets::saveWidget(m, file = output_html, selfcontained = TRUE)
  }
  
  m
}

# ------------------------------- PLOTS ---------------------------------------
plot_all_birds_map <- function(track_data, fi) {
  ggplot() +
    geom_sf(data = fi, fill = "grey80", colour = "grey70", linewidth = 0.3) +
    geom_path(
      data = track_data %>% arrange(deploymentid, tagnumber, datetime),
      aes(x = long, y = lat, group = interaction(bird_id, date), colour = as.factor(deploymentid)),
      alpha = 0.45, linewidth = 0.4
    ) +
    labs(
      title = "All tracks (all deployments)",
      subtitle = paste("Birds:", length(unique(track_data$bird_id)), "| GPS fixes:", nrow(track_data)),
      colour = "Deployment"
    ) +
    theme_tracks()
}

plot_deployment_map_zoom <- function(track_data, fi, deployment_id,
                                     pad_lon = 0.03, pad_lat = 0.03,
                                     buffer_m = 250) {
  pd <- track_data %>% filter(deploymentid == deployment_id)
  if (nrow(pd) == 0) return(NULL)

  col_xy <- pd %>% distinct(colony_name, colony_lon, colony_lat) %>% slice(1)
  col_buf <- make_colony_buffer_sf(col_xy$colony_lon, col_xy$colony_lat, buffer_m = buffer_m)
  ext <- get_track_extent(pd, pad_lon = pad_lon, pad_lat = pad_lat)

  ggplot() +
    geom_sf(data = fi, fill = "grey80", colour = "grey70", linewidth = 0.3) +
    geom_sf(data = col_buf, fill = "red", alpha = 0.08, colour = "red", linewidth = 0.6) +
    geom_path(
      data = pd %>% arrange(tagnumber, datetime),
      aes(x = long, y = lat, group = interaction(tagnumber, date), colour = as.factor(tagnumber)),
      alpha = 0.55, linewidth = 0.5
    ) +
    annotate("point", x = col_xy$colony_lon, y = col_xy$colony_lat, colour = "red", size = 3.2, shape = 17) +
    coord_sf(xlim = ext$xlim, ylim = ext$ylim, expand = FALSE) +
    labs(
      title = paste("Tracks:", deployment_id),
      subtitle = paste("Birds:", length(unique(pd$bird_id)), "| GPS fixes:", nrow(pd)),
      colour = "Tag"
    ) +
    theme_tracks()
}

plot_bird_map_zoom <- function(track_data, fi, deployment_id, tag_id,
                               buffer_m = 250,
                               pad_lon = 0.01, pad_lat = 0.01) {
  tag_id <- as.character(tag_id)
  bird_id_val <- paste(deployment_id, tag_id, sep = "__")

  pd <- track_data %>% filter(bird_id == bird_id_val) %>% arrange(datetime)
  if (nrow(pd) == 0) return(NULL)

  col_xy <- pd %>% distinct(colony_name, colony_lon, colony_lat) %>% slice(1)
  col_buf <- make_colony_buffer_sf(col_xy$colony_lon, col_xy$colony_lat, buffer_m = buffer_m)
  ext <- get_track_extent(pd, pad_lon = pad_lon, pad_lat = pad_lat)

  n_fixes <- nrow(pd)
  n_days <- length(unique(pd$date))

  ggplot() +
    geom_sf(data = fi, fill = "grey80", colour = "grey70", linewidth = 0.3) +
    geom_sf(data = col_buf, fill = "red", alpha = 0.08, colour = "red", linewidth = 0.6) +
    geom_path(data = pd, aes(x = long, y = lat, group = 1), colour = "grey70", alpha = 0.8, linewidth = 0.7) +
    geom_point(data = pd, aes(x = long, y = lat, colour = as.factor(date)), alpha = 0.9, size = 1.1) +
    annotate("point", x = col_xy$colony_lon, y = col_xy$colony_lat, colour = "red", size = 3.2, shape = 17) +
    scale_colour_viridis_d(option = "viridis", name = "Date") +
    coord_sf(xlim = ext$xlim, ylim = ext$ylim, expand = FALSE) +
    labs(
      title = paste("Bird", tag_id, "—", deployment_id),
      subtitle = paste("GPS fixes:", n_fixes, "| Number of days:", n_days)
    ) +
    theme_tracks()
}

plot_distance_time_deployment <- function(track_data, deployment_id, inner_buffer_m, outer_buffer_m) {
  pd <- track_data %>% filter(deploymentid == deployment_id)
  if (nrow(pd) == 0) return(NULL)

  inner_km <- inner_buffer_m / 1000
  outer_km <- outer_buffer_m / 1000

  xmin <- min(pd$datetime, na.rm = TRUE)
  xmax <- max(pd$datetime, na.rm = TRUE)

  ggplot(pd, aes(x = datetime, y = dist_to_colony_km, colour = as.factor(tagnumber))) +
    geom_line(alpha = 0.6, linewidth = 0.35) +
    geom_hline(yintercept = inner_km, linetype = "dashed", alpha = 0.8) +
    geom_hline(yintercept = outer_km, linetype = "dotted", alpha = 0.8) +
    scale_x_datetime(limits = c(xmin, xmax), date_labels = "%b %Y") +
    labs(
      title = paste("Distance from colony:", deployment_id),
      subtitle = paste0("Dashed = inner (", inner_buffer_m, " m), dotted = outer (", outer_buffer_m, " m)"),
      colour = "Tag"
    ) +
    theme_tracks() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_bird_trip_facets_map <- function(tripcheck_points, fi, deployment_id, tag_id,
                                      buffer_m = 250) {
  tag_id <- as.character(tag_id)
  bird_id_val <- paste(deployment_id, tag_id, sep = "__")

  td <- tripcheck_points %>%
    filter(bird_id == bird_id_val) %>%
    arrange(datetime)

  if (nrow(td) == 0) return(NULL)

  col_xy <- td %>% distinct(colony_name, colony_lon, colony_lat) %>% slice(1)
  col_buf <- make_colony_buffer_sf(col_xy$colony_lon, col_xy$colony_lat, buffer_m = buffer_m)
  ext <- get_track_extent(td, pad_lon = 0.02, pad_lat = 0.02)

  ggplot() +
    geom_sf(data = fi, fill = "grey80", colour = "grey70", linewidth = 0.25) +
    geom_sf(data = col_buf, fill = "red", alpha = 0.08, colour = "red", linewidth = 0.5) +
    geom_path(data = td, aes(x = long, y = lat, group = 1), colour = "grey70", alpha = 0.8, linewidth = 0.55) +
    geom_point(data = td, aes(x = long, y = lat, colour = as.factor(date)), alpha = 0.9, size = 0.9) +
    annotate("point", x = col_xy$colony_lon, y = col_xy$colony_lat, colour = "red", size = 2.8, shape = 17) +
    scale_colour_viridis_d(option = "viridis", name = "Date") +
    coord_sf(xlim = ext$xlim, ylim = ext$ylim, expand = FALSE) +
    facet_wrap(~ foraging_trip, scales = "fixed") +
    labs(
      title = paste("Trip check (map facets):", bird_id_val),
      subtitle = paste0("Each facet = one detected trip | QA padding: ", qa_pad_n, " fixes before/after OUT")
    ) +
    theme_tracks()
}

plot_bird_trip_facets_distance <- function(tripcheck_points, deployment_id, tag_id,
                                           inner_buffer_m, outer_buffer_m) {
  tag_id <- as.character(tag_id)
  bird_id_val <- paste(deployment_id, tag_id, sep = "__")

  td <- tripcheck_points %>%
    filter(bird_id == bird_id_val) %>%
    arrange(datetime)

  if (nrow(td) == 0) return(NULL)

  inner_km <- inner_buffer_m / 1000
  outer_km <- outer_buffer_m / 1000

  ggplot(td, aes(x = datetime, y = dist_to_colony_km)) +
    geom_line(alpha = 0.85, linewidth = 0.55) +
    geom_hline(yintercept = inner_km, linetype = "dashed", alpha = 0.8) +
    geom_hline(yintercept = outer_km, linetype = "dotted", alpha = 0.8) +
    facet_wrap(~ foraging_trip, scales = "free_x") +
    labs(
      title = paste("Trip check (distance facets):", bird_id_val),
      subtitle = paste0("Dashed = inner (", inner_buffer_m, " m), dotted = outer (", outer_buffer_m, " m)")
    ) +
    theme_tracks() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# ------------------------------ SAVE ALL PLOTS --------------------------------
save_all_visualisations <- function(track_data, trips_data, tripcheck_points, fi) {
  if (!dir.exists(viz_dir)) dir.create(viz_dir, recursive = TRUE)

  cat("\nCreating visualisations in:", viz_dir, "\n")

  dep_ids <- sort(unique(track_data$deploymentid))
  birds <- track_data %>% distinct(deploymentid, tagnumber) %>% arrange(deploymentid, tagnumber)

  n_root_plots <- 1
  n_dep_plots <- length(dep_ids) * 2
  n_bird_plots <- nrow(birds)

  n_tripcheck_plots <- 0
  if (!is.null(trips_data) && nrow(trips_data) > 0 && !is.null(tripcheck_points) && nrow(tripcheck_points) > 0) {
    birds2 <- trips_data %>% distinct(deploymentid, tag) %>% arrange(deploymentid, tag)
    n_tripcheck_plots <- nrow(birds2) * 2
  }

  total_steps <- n_root_plots + n_dep_plots + n_bird_plots + n_tripcheck_plots
  step_i <- 0

  pb <- txtProgressBar(min = 0, max = total_steps, style = 3)
  update_pb <- function(msg = NULL) {
    step_i <<- step_i + 1
    setTxtProgressBar(pb, step_i)
    if (!is.null(msg)) {
      cat(sprintf("\n[%d/%d] %s\n", step_i, total_steps, msg))
    }
  }

  on.exit(close(pb), add = TRUE)

  p_all <- plot_all_birds_map(track_data, fi)
  ggsave(file.path(viz_dir, "all_birds_tracks.png"), p_all, width = 12, height = 8, dpi = 300)
  update_pb("Saved all_birds_tracks.png")

  for (dep in dep_ids) {
    dep_dir <- file.path(viz_dir, dep)
    if (!dir.exists(dep_dir)) dir.create(dep_dir, recursive = TRUE)

    p_dep <- plot_deployment_map_zoom(
      track_data, fi, dep,
      pad_lon = pad_lon_dep, pad_lat = pad_lat_dep,
      buffer_m = outer_buffer_m
    )
    if (!is.null(p_dep)) {
      ggsave(file.path(dep_dir, paste0("deployment_", dep, "_tracks.png")),
             p_dep, width = 12, height = 8, dpi = 300)
    }
    update_pb(paste("Deployment map:", dep))

    p_dep_dist <- plot_distance_time_deployment(track_data, dep, inner_buffer_m, outer_buffer_m)
    if (!is.null(p_dep_dist)) {
      ggsave(file.path(dep_dir, paste0("deployment_", dep, "_distance_time.png")),
             p_dep_dist, width = 12, height = 6, dpi = 300)
    }
    update_pb(paste("Deployment distance plot:", dep))
  }

  for (i in seq_len(nrow(birds))) {
    dep <- birds$deploymentid[i]
    tg  <- birds$tagnumber[i]
    dep_dir <- file.path(viz_dir, dep)

    p_bird <- plot_bird_map_zoom(
      track_data, fi, dep, tg,
      buffer_m = outer_buffer_m,
      pad_lon = pad_lon_bird, pad_lat = pad_lat_bird
    )
    if (!is.null(p_bird)) {
      fname <- paste0("bird_", dep, "_tag_", tg, "_tracks.png")
      ggsave(file.path(dep_dir, fname), p_bird, width = 8, height = 6, dpi = 300)
    }
    update_pb(paste("Bird map:", dep, "tag", tg))
  }

  if (!is.null(trips_data) && nrow(trips_data) > 0 && !is.null(tripcheck_points) && nrow(tripcheck_points) > 0) {
    birds2 <- trips_data %>% distinct(deploymentid, tag) %>% arrange(deploymentid, tag)

    for (i in seq_len(nrow(birds2))) {
      dep <- birds2$deploymentid[i]
      tg  <- birds2$tag[i]
      dep_dir <- file.path(viz_dir, dep)

      p_fac_map <- plot_bird_trip_facets_map(tripcheck_points, fi, dep, tg, buffer_m = outer_buffer_m)
      if (!is.null(p_fac_map)) {
        ggsave(file.path(dep_dir, paste0("bird_", dep, "_tag_", tg, "_tripcheck_facets_map.png")),
               p_fac_map, width = 12, height = 8, dpi = 300)
      }
      update_pb(paste("Trip facet map:", dep, "tag", tg))

      p_fac_dist <- plot_bird_trip_facets_distance(tripcheck_points, dep, tg, inner_buffer_m, outer_buffer_m)
      if (!is.null(p_fac_dist)) {
        ggsave(file.path(dep_dir, paste0("bird_", dep, "_tag_", tg, "_tripcheck_facets_distance.png")),
               p_fac_dist, width = 12, height = 8, dpi = 300)
      }
      update_pb(paste("Trip facet distance:", dep, "tag", tg))
    }
  }

  cat("\nDone.\n")
}

# ------------------------------ RUN EVERYTHING --------------------------------
cat("Input data dir:", data_dir, "\n")
cat("Output dir:", viz_dir, "\n")

if (!dir.exists(viz_dir)) dir.create(viz_dir, recursive = TRUE)

results <- process_all_RH_files(data_dir, colonies)

if (!is.null(results$tracks) && nrow(results$tracks) > 0) {
  track_data <- results$tracks %>%
    mutate(
      bird_id = paste(deploymentid, tagnumber, sep = "__"),
      date = as.Date(datetime)
    ) %>%
    arrange(deploymentid, tagnumber, datetime)

  trips_data <- results$trips
  tripcheck_points <- make_tripcheck_facet_points(track_data, trips_data, n_pad = qa_pad_n)

  cat("\nUnique birds (deployment__tag):", length(unique(track_data$bird_id)), "\n")
  cat("Unique tags:", length(unique(track_data$tagnumber)), "\n")
  cat("Unique deploymentid:", length(unique(track_data$deploymentid)), "\n")

  save_all_visualisations(track_data, trips_data, tripcheck_points, fi)

  if (!is.null(trips_data) && nrow(trips_data) > 0) {
    cat("\nANALYSIS COMPLETE\n")
    cat("Total trips:", nrow(trips_data), "\n")
    cat("Unique birds with trips:", length(unique(trips_data$bird_id)), "\n")
  } else {
    cat("\nANALYSIS COMPLETE (no trips detected with current thresholds)\n")
  }
}

#############################################################################
#############################################################################
# Example static QA map:
create_trip_qa_leaflet(
  tracks_df = results$tracks,
  qa_tbl = results$qa,
  deployment_id = "2025-01-Hummock",
  tag_id = "45778",
  output_html = NULL
  #output_html = file.path(viz_dir, "QA_2025-01-Hummock_tag45778.html")
)

# Example time-slider map:
create_trip_time_slider_leaflet(
  tracks_df = results$tracks,
  deployment_id = "2025-01-Hummock",
  tag_id = "45778",
  output_html = NULL
  #output_html = file.path(viz_dir, "QA_timeslider_2025-01-Hummock_tag45778.html")
)
