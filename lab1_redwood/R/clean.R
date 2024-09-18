#' Clean dates data
#'
#' @param dates_data the dates data to clean
#'
#' @returns A data frame containing the cleaned dates data
clean_dates_data <- function(dates_data) {

  dates_data <- dates_data |>
    dplyr::mutate(
      # extract day of week
      day_of_week = stringr::str_extract(date, "Mon|Tue|Wed|Thu|Fri|Sat|Sun") |>
        factor(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
      # extract time of day
      time_chr = stringr::str_extract(date, "\\w+:\\w+:\\w+"),
      time = lubridate::hms(time_chr),
      # extract date
      date_chr = stringr::str_remove(date, "\\w+:\\w+:\\w+ "),
      date = stringr::str_remove(date_chr, "(Mon|Tue|Wed|Thu|Fri|Sat|Sun) ") |>
        lubridate::mdy(),
      # combine date and time in lubridate format
      datetime = lubridate::mdy_hms(paste(date_chr, time_chr))
    )

  return(dates_data)
}


#' Clean redwood data
#'
#' @param redwood_data the redwood data to clean
#'
#' @returns A data frame containing the cleaned redwood data
clean_redwood_data <- function(redwood_data) {

  redwood_data <- redwood_data |>
    # rename columns
    dplyr::rename(
      temp = humid_temp,
      iPAR = hamatop,
      rPAR = hamabot
    ) |>
    # remove irrelevant column; not sure what this variable is
    dplyr::select(-humid_adj) |>
    # remove na observations (do with caution!)
    na.omit() |>
    # remove duplicated rows
    dplyr::distinct()

  return(redwood_data)
}


#' Clean mote location data
#'
#' @param mote_data the mote location data to clean
#'
#' @returns A data frame containing the cleaned mote location data
clean_mote_location_data <- function(mote_data) {
  return(mote_data)
}


#' Merge data
#'
#' @param dates_data the dates data
#' @param motes_data the mote location data
#' @param redwood_net_data the redwood network data
#' @param redwood_log_data the redwood log data
#'
#' @returns A data frame containing the merged redwood data
merge_redwood_data <- function(dates_data, motes_data,
                               redwood_net_data, redwood_log_data) {

  redwood_data <- dplyr::bind_rows(
    redwood_log_data |>
      dplyr::mutate(source = "log"),
    redwood_net_data |>
      dplyr::mutate(source = "net")
  ) |>
    # remove duplicate entries (i.e., a copy on both the local log and network)
    dplyr::distinct(
      epoch, nodeid, humidity, temp, iPAR, rPAR, .keep_all = TRUE
    ) |>
    # sort in time order
    dplyr::arrange(epoch, nodeid) |>
    # result time is weird so let's merge with the epoch data
    dplyr::left_join(dates_data, by = c("epoch" = "number")) |>
    # merge with mote location data
    dplyr::left_join(motes_data, by = c("nodeid" = "ID")) |>
    # remove edge tree nodes (don't have data past week 2 for these nodes)
    dplyr::filter(Tree != "edge") |>
    # voltages from network and log are measured on different scales
    # so let's transform voltage from network data to match log data
    dplyr::mutate(
      voltage = dplyr::case_when(
        source == "net" ~ 1 / (0.001683729 * voltage),
        TRUE ~ voltage
      )
    )

  return(redwood_data)
}


#' Remove outliers in redwood data
#'
#' @param redwood_data the (merged) redwood data to remove outliers from
#'
#' @returns A data frame containing the cleaned redwood data
remove_redwood_outliers <- function(redwood_data) {

  # remove obvious errors values
  redwood_data <- redwood_data |>
    dplyr::filter(
      temp > 0,
      humidity < 110
    )

  thrs <- seq(2.3, 2.5, by = 0.01)
  nodes <- unique(redwood_data$nodeid)
  epoch_cutoffs <- list()
  for (node in nodes) {
    for (thr in rev(thrs)) {
      # find epoch where the node's voltage dips below threshold
      epoch_cutoff <- redwood_data |>
        dplyr::filter(
          nodeid == !!node,
          voltage < thr
        ) |>
        dplyr::slice_min(epoch, n = 1)
      if (nrow(epoch_cutoff) == 0) {
        # if voltage does not dip below threshold, do nothing
        next
      }
      redwood_subset <- redwood_data |>
        dplyr::filter(
          nodeid == !!node,
          epoch >= epoch_cutoff$epoch[[1]],
          epoch <= epoch_cutoff$epoch[[1]] + 100
        ) |>
        dplyr::arrange(epoch)
      autocor <- as.numeric(acf(redwood_subset$temp, plot = FALSE)[1]$acf)
      if (autocor < 0.8) {
        # if temperature measurements after identified epoch are not highly
        # correlated, then assume node failure and remove data after epoch
        epoch_cutoffs[[as.character(node)]] <- epoch_cutoff$epoch[[1]]
        break
      }
    }
  }

  # do same for nodes 141 and 145 but use epoch threshold directly since
  # voltages are strange
  for (node in c(141, 145)) {
    # get all possible epoch cutoffs
    thrs <- redwood_data |>
      dplyr::filter(
        nodeid == !!node,
        source == "net"
      ) |>
      dplyr::arrange(epoch) |>
      dplyr::pull(epoch) |>
      unique()
    for (thr in thrs) {
      redwood_subset <- redwood_data |>
        dplyr::filter(
          nodeid == !!node,
          epoch >= !!thr,
          epoch <= !!thr + 100
        ) |>
        dplyr::arrange(epoch)
      autocor <- as.numeric(acf(redwood_subset$temp, plot = FALSE)[1]$acf)
      if (autocor < 0.8) {
        # if temperature measurements after identified epoch are not highly
        # correlated, then assume node failure and remove data after epoch
        epoch_cutoffs[[as.character(node)]] <- thr
        break
      }
    }
  }

  redwood_data <- redwood_data |>
    dplyr::mutate(
      outlier = FALSE
    )
  for (node in names(epoch_cutoffs)) {
    # mark outliers
    redwood_data <- redwood_data |>
      dplyr::mutate(
        outlier = dplyr::case_when(
          (nodeid == !!node) & (epoch >= epoch_cutoffs[[node]]) ~ T,
          TRUE ~ outlier
        )
      )
  }
  # remove outliers
  redwood_data_cleaned <- redwood_data |>
    dplyr::filter(
      !outlier
    )

  return(redwood_data_cleaned)
}
