#' Loads or downloads the necessary data for all modules
#'
#' Either downloads the necessary data for all modules from Eurostat using
#' dataset ids or loads the files provided in the user-specified input directory
#' and selects the required variables.
#'
#' @param specification A tibble or data.frame as returned by
#'   \code{\link{translate_variables}}.
#' @param filter_list A named list with one entry per Eurostat code that
#'   specifies the filter that is applied to choose the correct data series,
#'   e.g. geographical restrictions or whether seasonally adjusted or not. See
#'   Details.
#' @param download A logical value whether the data should be downloaded from
#'   Eurostat or will be provided directly via \code{inputdata_directory}.
#' @param dictionary A tibble or data.frame storing the Eurostat variable code
#'   in column 'eurostat_code'. If \code{download == TRUE} then the dictionary
#'   also requires column named 'dataset_id' that stores the Eurostat dataset
#'   id. When \code{NULL}, the \link[=dict]{default dictionary} is used.
#' @param inputdata_directory A path to .rds input files in which the data is
#'   stored. Can be \code{NULL} if \code{download == TRUE}.
#' @param save_to_disk A path to a directory where the final dataset will be
#'   saved, including file name and ending. Not saved when \code{NULL}.
#'
#' @note Currently does not support mixed inputs, i.e. downloading some data and
#'   reading the rest from files.
#'
#' @details Argument \code{filter_list} is not fully functional yet. It allows
#'   the user to specify which type of data they want. Current implementation
#'   mostly for minimum viable product. Once the
#'   \code{\link[eurostat]{get_eurostat}} API functions properly, can use their
#'   \code{filter} argument specification to make the download faster by
#'   restricting it to the necessary subset only.
#'
#' @export


load_or_download_variables <- function(specification,
                                       filter_list = NULL,
                                       download = TRUE,
                                       dictionary = NULL,
                                       inputdata_directory = NULL,
                                       save_to_disk = NULL) {

  # input check
  if (is.null(dictionary)) {
    dictionary <- aggregate.model::dict
  }
  if (download == FALSE && is.null(inputdata_directory)) {
    stop("Must specify 'inputdata_directory' when load files locally instead of download from Eurostat.")
  }
  if (download == TRUE && !("dataset_id" %in% colnames(dictionary))) {
    stop("Dictionary must have a column 'dataset_id' for download from Eurostat.")
  }

  if (isTRUE(download)) {

    # note:
    # ideally wanted to set filters, so don't need to download all data series and all countries
    # eurostat::get_eurostat(id = "namq_10_gdp", filters = list(na_item = "B1GQ", geo = "AT"), cache = FALSE)
    # but eurostat JSON API cannot handle this, says "too many categories" (there are issues about this on GitHub)
    # so for now, need to download whole dataset and only then select subset

    # initialise variables
    ids <- determine_datacodes(specification = specification, dictionary = dictionary)
    codes.download <- ids$var.ids
    codes.remain <- codes.download
    full <- data.frame()

    # loop through required datasets
    for (i in 1:length(ids$data.ids)) {
      tmp <- eurostat::get_eurostat(id = ids$data.ids[i])
      codes.found <- codes.download[which(codes.download %in% unique(tmp$na_item))]
      codes.remain <- setdiff(codes.remain, codes.found)
      for (j in 1:length(codes.found)) {
        # which filter should be applied for that variable
        filter <- filter_list[[codes.found[j]]]
        # choose subset according to filter
        sub <- tmp %>%
          filter(na_item == codes.found[j] & geo == filter$geo & s_adj == filter$s_adj & unit == filter$unit)
        # add subset to full, final dataset
        full <- rbind(full, sub)
      }
    }

    # check whether all Eurostat codes were found
    if (!identical(length(codes.remain), 0L)) {
      stop("Not all Eurostat codes were found in the provided dataset ids.")
    }

  } else { # not download but local directory

    files <- list.files(path = inputdata_directory, pattern = "\\.(Rds|RDS|rds)$")

    codes.find <- determine_eurocodes(specification = specification, dictionary = dictionary)$var.ids
    codes.remain <- codes.find
    full <- data.frame()

    # loop through required datasets
    for (i in 1:length(files)) {
      pth <- file.path(inputdata_directory, files[i])
      tmp <- readRDS(file = pth)
      codes.found <- codes.find[which(codes.find %in% unique(tmp$na_item))]
      codes.remain <- setdiff(codes.remain, codes.found)
      for (j in 1:length(codes.found)) {
        # which filter should be applied for that variable
        filter <- filter_list[[codes.found[j]]]
        # choose subset according to filter
        sub <- tmp %>%
          filter(na_item == codes.found[j] & geo == filter$geo & s_adj == filter$s_adj & unit == filter$unit)
        # add subset to full, final dataset
        full <- rbind(full, sub)
      }
    }

    # check whether all Eurostat codes were found
    if (!identical(length(codes.remain), 0L)) {
      stop("Not all Eurostat codes were found in the provided dataset ids.")
    }
  } # end local directory

  # might have to deal with unbalanced data (though arx/isat might deal with it?)
  # quick solution for our present case, might not work for all cases
  availability <- full %>%
    group_by(na_item) %>%
    summarise(
      min_date = min(time),
      max_date = max(time),
      n = n()
    ) %>%
    ungroup()
  if (max(dist(availability$n, method = "maximum") / max(availability$n)) > 0.2) {
    warning("Unbalanced panel, will lose more than 20\\% of data when making balanced")
  }
  min_date <- max(availability$min_date) # highest minimum date
  max_date <- min(availability$max_date) # lowest maximum date
  full <- full %>%
    filter(time >= min_date & time <= max_date)
  # might still not be balanced but beginning- & end-points are balanced
  # I believe zoo in gets deals with unbalanced inside time period (could be wrong)

  if (!is.null(save_to_disk)) {

    # which file ending was chosen
    ending <- str_extract(string = save_to_disk, pattern = "\\.[:alpha:]+$")

    if (ending %in% c(".RDS", ".rds", ".Rds")) {
      saveRDS(object = full, file = save_to_disk)
    }
    if (ending == ".csv") {
      write_csv(x = full, file = save_to_disk)
    }
    if (ending %in% c(".xls", ".xlsx")) {
      write_excel_csv(x = full, file = save_to_disk)
    }

  }

  return(full)



  # Moritz' original notes, check whether fulfilled
  # check the inputdata_directory for any files

  # open files and check if variables available

  # if not, download via eurostat package

  # return finalised data

}
