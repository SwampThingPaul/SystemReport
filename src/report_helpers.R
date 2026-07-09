## report_helpers.R
## Defensive helpers for the Everglades System Conditions Report.
## Source this at the top of REPORT.Rmd (setup chunk) or in scripts/:
##   source("R/report_helpers.R")

# latest_obs ------------------------------------------------------------------
#' Most recent usable observation at or before a target date.
#'
#' Replaces the fragile `subset(df, Date == YEST)` pattern, which returns a
#' zero-row data frame whenever yesterday's value hasn't posted yet (data
#' latency) or the download for that gauge silently failed. Instead, walk
#' back to the most recent row where the required columns are non-NA, up to
#' `max_lag_days` behind the target.
#'
#' Returns a 1-row data frame (with attribute "lag_days"), or a 0-row data
#' frame if nothing usable exists within the window. Downstream code must
#' check nrow() -- see stage_status_text(), which does this for you.
latest_obs <- function(df, target_date, date_col = "Date",
                       req_cols = NULL, max_lag_days = 10) {
  if (is.null(df) || nrow(df) == 0) return(df[0, , drop = FALSE])

  x <- df[!is.na(df[[date_col]]) & df[[date_col]] <= target_date, , drop = FALSE]
  if (!is.null(req_cols)) {
    req_cols <- intersect(req_cols, names(x))
    if (length(req_cols)) {
      keep <- stats::complete.cases(x[, req_cols, drop = FALSE])
      x <- x[keep, , drop = FALSE]
    }
  }
  if (nrow(x) == 0) return(x)

  x <- x[order(x[[date_col]]), , drop = FALSE]
  out <- x[nrow(x), , drop = FALSE]

  lag_days <- as.numeric(difftime(target_date, out[[date_col]], units = "days"))
  if (is.finite(lag_days) && lag_days > max_lag_days) return(x[0, , drop = FALSE])

  attr(out, "lag_days") <- lag_days
  out
}

# stage_status_text ------------------------------------------------------------
#' Build the "Stage is X ft above/below the Y line" sentence safely.
#'
#' Replaces the four near-identical reg.delta chunks (LOK, WCA1, WCA2, WCA3)
#' and their inline `r paste0(...)` calls. Handles:
#'   * zero rows (no recent data)  -> graceful sentence instead of a crash
#'   * NA stage or schedule values -> graceful sentence
#'   * delta exactly 0             -> "at the ... line" (the old
#'     `ifelse(sign(x)>0,"above ")` errored here: missing `no=` argument)
#'   * stale data                  -> appends "(as of <date>)"
#'
#' @param obs        1-row (or 0-row) data frame, e.g. from latest_obs()
#' @param stage_col  column holding the observed stage
#' @param zone_cols  *named* character vector: names = columns in `obs`
#'                   holding zone/schedule elevations, values = display names.
#'                   e.g. c(A1 = "A1 line", A2 = "A2 line", Floor = "Floor")
#' @param label      subject of the sentence, e.g. "WCA-1 stage"
#' @param datum      datum text, default "Ft NGVD29"
#' @param target_date the date you wanted (YEST); used for the "(as of)" note
stage_status_text <- function(obs, stage_col, zone_cols,
                              label = "Stage", datum = "Ft NGVD29",
                              target_date = NULL, date_col = "Date",
                              digits = 2) {
  no_data <- paste0(label, " relative to the regulation schedule is not ",
                    "available (no recent data retrieved).")
  if (is.null(obs) || nrow(obs) == 0) return(no_data)

  obs <- obs[nrow(obs), , drop = FALSE]
  stg <- suppressWarnings(as.numeric(obs[[stage_col]]))

  zone_cols <- zone_cols[names(zone_cols) %in% names(obs)]
  if (!length(zone_cols)) return(no_data)
  zvals <- suppressWarnings(
    vapply(names(zone_cols), function(z) as.numeric(obs[[z]][1]), numeric(1))
  )

  if (is.na(stg) || all(is.na(zvals))) return(no_data)

  # nearest zone among non-NA schedule values
  ok    <- which(!is.na(zvals))
  zi    <- ok[which.min(abs(zvals[ok] - stg))]
  delta <- stg - zvals[zi]

  asof <- ""
  if (!is.null(target_date) && !is.null(obs[[date_col]]) &&
      as.Date(obs[[date_col]][1]) < as.Date(target_date)) {
    asof <- paste0(" (as of ", format(obs[[date_col]][1], "%b %d, %Y"), ")")
  }

  if (isTRUE(all.equal(unname(delta), 0))) {
    return(paste0(label, " is at the ", zone_cols[zi], asof, "."))
  }
  dir_txt <- if (delta < 0) "below" else "above"
  paste0(label, " is ", format(round(abs(delta), digits), nsmall = digits),
         " ", datum, " ", dir_txt, " the ", zone_cols[zi], asof, ".")
}

# fetch_daily_safe --------------------------------------------------------------
#' One DBKEY/site fetch with retry + structured result (no silent NULLs).
#'
#' The current pattern -- tryCatch(..., error = function(e) NULL) followed by
#' Filter(Negate(is.null), ...) -- makes every failure invisible. You only find
#' out three chunks later when a subset comes back empty. This wrapper retries
#' with backoff and returns a status record either way, so a manifest can be
#' written and inspected (locally or as a GitHub Actions artifact).
#'
#' @param fetch_fun a function taking (sdate, edate, dbkey, ...) -- e.g.
#'        AnalystHelper::insight_fetch_daily, or an anonymous wrapper around
#'        dataRetrieval::read_waterdata_daily.
fetch_daily_safe <- function(fetch_fun, sdate, edate, dbkey, ...,
                             tries = 3, backoff = 2, pause = 0.5) {
  last_err <- NA_character_
  for (k in seq_len(tries)) {
    out <- tryCatch(fetch_fun(sdate, edate, dbkey, ...),
                    error = function(e) structure(conditionMessage(e),
                                                  class = "fetch_err"))
    if (!inherits(out, "fetch_err")) {
      Sys.sleep(pause)  # be polite to the API between calls
      return(list(data = out, dbkey = as.character(dbkey), status = "ok",
                  n = if (is.data.frame(out)) nrow(out) else NA_integer_,
                  max_date = if (is.data.frame(out) && "Date" %in% names(out) && nrow(out))
                    as.character(max(out$Date, na.rm = TRUE)) else NA_character_,
                  message = NA_character_))
    }
    last_err <- unclass(out)
    if (k < tries) Sys.sleep(backoff^k)   # 2s, 4s, ...
  }
  list(data = NULL, dbkey = as.character(dbkey), status = "failed",
       n = 0L, max_date = NA_character_, message = last_err)
}

# fetch_many ---------------------------------------------------------------------
#' Loop fetch_daily_safe over a key table; return data + manifest.
#'
#' @param keys data frame with at least a `DBKEY` column; extra columns
#'        (loc, group, ...) are carried into the manifest.
#' @param sdate_fun function(row_i) -> start date (lets you keep the
#'        "LOK goes back to 2007, others to CurWY-4" logic).
fetch_many <- function(keys, fetch_fun, sdate_fun, edate, key_col = "DBKEY", ...) {
  res <- lapply(seq_len(nrow(keys)), function(i) {
    fetch_daily_safe(fetch_fun,
                     sdate = sdate_fun(i),
                     edate = edate,
                     dbkey = keys[[key_col]][i],
                     ...)
  })
  manifest <- cbind(
    keys,
    status   = vapply(res, `[[`, character(1), "status"),
    n_rows   = vapply(res, `[[`, integer(1),  "n"),
    max_date = vapply(res, `[[`, character(1), "max_date"),
    message  = vapply(res, `[[`, character(1), "message")
  )
  # attach DBKEY to each successful result, drop failures/empties
  dat_list <- Map(function(r, k) {
    d <- r$data
    if (is.data.frame(d) && nrow(d)) { d$DBKEY <- k; d } else NULL
  }, res, as.character(keys[[key_col]]))
  dat_list <- Filter(Negate(is.null), dat_list)
  data <- if (length(dat_list)) do.call(rbind, dat_list) else NULL
  list(data = data, manifest = manifest)
}

# check_freshness -----------------------------------------------------------------
#' Fail (or warn) when critical series are stale.
#'
#' @param manifest  a manifest (or rbind of manifests) from fetch_many()
#' @param critical  character vector of `loc` values that must be fresh
#' @param max_lag_days allowed staleness
#' @param strict    TRUE -> stop(); FALSE -> warning() and return invisibly
check_freshness <- function(manifest, critical, target_date,
                            max_lag_days = 3, strict = FALSE,
                            loc_col = "loc") {
  m <- manifest[manifest[[loc_col]] %in% critical, , drop = FALSE]
  m$max_date_d <- as.Date(m$max_date)
  m$lag <- as.numeric(as.Date(target_date) - m$max_date_d)
  bad <- m[m$status != "ok" | is.na(m$lag) | m$lag > max_lag_days, , drop = FALSE]
  if (nrow(bad)) {
    msg <- paste0("Stale/failed critical series:\n",
                  paste0("  ", bad[[loc_col]], " [", bad$DBKEY, "] status=",
                         bad$status, " max_date=", bad$max_date,
                         collapse = "\n"))
    if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
  invisible(bad)
}
