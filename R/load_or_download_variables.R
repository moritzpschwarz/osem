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
#' @param quiet Logical with default = FALSE. Should messages be displayed?
#' These messages are intended to give more information about the estimation
#' and data retrieval process.
#' @param constrain.to.minimum.sample Logical. Should all data series be constrained to the minimum data series? Default is FALSE.
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
                                       save_to_disk = NULL,
                                       quiet = FALSE,
                                       constrain.to.minimum.sample = TRUE) {

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

    # Save original timeout setting
    tmpmc <- options("timeout")
    on.exit(options(tmpmc)) # set the old mc warning on exit
    options(timeout = 1000)

    # note:
    # ideally wanted to set filters, so don't need to download all data series and all countries
    # eurostat::get_eurostat(id = "namq_10_gdp", filters = list(na_item = "B1GQ", geo = "AT"), cache = FALSE)
    # but eurostat JSON API cannot handle this, says "too many categories" (there are issues about this on GitHub)
    # so for now, need to download whole dataset and only then select subset

    # initialise variables
    dictionary %>%
      dplyr::distinct(dataset_id, var_col) -> var_col_list

    ids <- determine_datacodes(specification = specification, dictionary = dictionary)

    id_to_dataset <- dictionary %>%
      dplyr::rowwise() %>%
      dplyr::mutate(var.ids = dplyr::case_when(!is.na(nace_r2)~paste0(eurostat_code,"*",nace_r2),
                                        TRUE ~eurostat_code)) %>%
      dplyr::select(var.ids, dataset_id, var_col, model_varname, cpa2_1)

    codes.download <- dplyr::tibble(var.ids = ids$var.ids,
                                    model.ids = ids$model.ids) %>%
      dplyr::left_join(id_to_dataset, by = "var.ids") %>%
      tidyr::separate(var.ids, into = c("var","nace_r2"), sep = "\\*", fill = "right", remove = FALSE)
    codes.remain <- codes.download$var.ids
    full <- data.frame()

    # loop through required datasets
    for (i in 1:length(ids$data.ids)) {

      if(quiet){
        suppressMessages(tmp <- eurostat::get_eurostat(id = ids$data.ids[i]))
      } else {
        tmp <- eurostat::get_eurostat(id = ids$data.ids[i])
      }



      if(is.null(tmp)) {stop("Issue with automatic EUROSTAT download. Likely cause is a lack of internet connection. Check your internet connection. Also consider saving the downloaded data to disk using 'save_to_disk' and 'inputdata_directory'.")}
      varcolname <- codes.download %>% dplyr::filter(dataset_id == ids$data.ids[i]) %>% dplyr::distinct(var_col) %>% dplyr::pull(var_col)
      codes.in.tmp <- tmp %>% dplyr::pull(varcolname) %>% unique
      codes.found <- codes.download %>% dplyr::filter(dataset_id == ids$data.ids[i], var %in% codes.in.tmp)
      codes.remain <- dplyr::setdiff(codes.remain, codes.found$var.ids)

      for (j in 1:nrow(codes.found)) {
        # which filter should be applied for that variable
        filter <- filter_list[[codes.found$var[j]]]

        if (is.null(filter)) {stop(paste0("For variable '",codes.found$var[j],"' no entry in filter list found. Check filter list."))}

        # choose subset according to filter
        #if(ids$data.ids[i] == "ei_lmlc_q"){browser()}
        sub <- tmp %>%
          dplyr::filter(dplyr::if_all(dplyr::all_of(varcolname), ~ . == codes.found$var[j])) %>%
          dplyr::filter(geo == filter$geo & unit == filter$unit) %>%
          {if(dplyr::select(., dplyr::any_of("s_adj")) %>% ncol == 1){dplyr::filter(.,s_adj == filter$s_adj)} else {.}} %>%
          {if(dplyr::select(., dplyr::any_of("nace_r2")) %>% ncol == 1){dplyr::filter(.,nace_r2 == codes.found$nace_r2[j])} else {.}} %>%
          {if(dplyr::select(., dplyr::any_of("cpa2_1")) %>% ncol == 1){dplyr::filter(.,cpa2_1 == codes.found$cpa2_1[j])} else {.}} %>%
          dplyr::rename_with(.cols = dplyr::all_of(varcolname), .fn = ~paste0("na_item")) %>%
          dplyr::mutate(na_item = codes.found$model_varname[j])

        # add subset to full, final dataset
        full <- dplyr::bind_rows(full, sub)
      }
    }

    full$time <- as.Date(full$time)

    # check whether all Eurostat codes were found
    if (!identical(length(codes.remain), 0L)) {
      stop("Not all Eurostat codes were found in the provided dataset ids.")
    }

  } else if(is.character(inputdata_directory)){ # not download but local directory


    if(file.exists(inputdata_directory) & !dir.exists(inputdata_directory)){
      stop("The variable 'inputdata_directory' must be a character path to a directory, not to a file.")}

    files <- list.files(path = inputdata_directory, pattern = "\\.(Rds|RDS|rds|csv|xlsx|xls)$")

    if(!quiet){
      cat("Local files are used.\n")
      cat("The following files are opened and scanned for relevant data for the model.\n")
      cat(paste0(files, collapse = " "))
      cat("\n")
      cat("Note: If these include non-data files (with a likely different structure and hence likely errors), it is recommended to move all data files to a dedicated directory or to save the there using the 'save_to_disk' argument in the first place:\n")
      cat("\n")
      cat("You can quiet this message with quiet = FALSE.")
      cat("\n")
    }

    codes.find <- determine_eurocodes(specification = specification, dictionary = dictionary)$model_varname
    codes.remain <- codes.find
    full <- data.frame()

    # loop through required datasets
    for (i in seq_along(files)) {
      pth <- file.path(inputdata_directory, files[i])

      if(grepl("\\.(Rds|RDS|rds)$",pth)){
        tmp <- readRDS(file = pth)
      } else if (grepl("\\.(csv)$",pth)){
        tmp <- readr::read_csv(pth, show_col_types = FALSE, guess_max = 1000000)
      } else if (grepl("\\.(xls|xlsx)$",pth)){
        tmp <- readxl::read_excel(path = pth, guess_max = 1000000)
        #stop("Loading from xls or xlsx file not yet implemented.)
      }

      codes.found <- codes.find[which(codes.find %in% unique(tmp$na_item))]
      codes.remain <- setdiff(codes.remain, codes.found)

      # Commented out by Moritz because all of these will be applied when first downloading
      #   for (j in 1:length(codes.found)) {
      #     # which filter should be applied for that variable
      #     curfilter <- filter_list[[codes.found[j]]]
      #     # choose subset according to filter
      #     sub <- tmp %>%
      #       dplyr::filter(dplyr::if_all(na_item, ~ . == codes.found[j])) %>%
      #       dplyr::filter(geo == curfilter$geo & unit == curfilter$unit) %>%
      #       {if(dplyr::select(., dplyr::any_of("s_adj")) %>% ncol == 1){dplyr::filter(.,s_adj == curfilter$s_adj)} else {.}} %>%
      #       {if(dplyr::select(., dplyr::any_of("nace_r2")) %>% ncol == 1){dplyr::filter(.,nace_r2 == codes.found$nace_r2[j])} else {.}} %>%
      #       dplyr::mutate(na_item = codes.found$var.ids[j])
      #
      #     # add subset to full, final dataset
      full <- dplyr::bind_rows(full, tmp)
      #   }
    }

    full$time <- as.Date(full$time)

    # check whether all Eurostat codes were found
    if (!identical(length(codes.remain), 0L)) {
      stop("Not all Eurostat codes were found in the provided dataset ids.")
    }
  } else if(is.data.frame(inputdata_directory)){ # end local directory

    if(!quiet){
      cat("Variable provided as 'inputdata_directory' seems to be a data.frame type. Used as data source.\n")
    }

    full <- inputdata_directory
  } else {

    stop("Check the variable 'inputdata_directory'! You can specify either NULL to download data (with download = TRUE), a file path to an existing folder with files or an already loaded variable as a data.frame with the right dimensions.")
  }

  # might have to deal with unbalanced data (though arx/isat might deal with it?)
  # quick solution for our present case, might not work for all cases
  availability <- full %>%
    dplyr::group_by(na_item) %>%
    dplyr::summarise(
      min_date = min(time),
      max_date = max(time),
      n = dplyr::n()
    ) %>%
    dplyr::ungroup()

  if(constrain.to.minimum.sample){
    if (max(stats::dist(availability$n, method = "maximum") / max(availability$n)) > 0.2) {
      warning("Unbalanced panel, will lose more than 20\\% of data when making balanced")
    }
    min_date <- max(availability$min_date) # highest minimum date
    max_date <- min(availability$max_date) # lowest maximum date
    full <- full %>%
      dplyr::filter(time >= min_date & time <= max_date)
    # might still not be balanced but beginning- & end-points are balanced
    # I believe zoo in gets deals with unbalanced inside time period (could be wrong)
  }


  if (!is.null(save_to_disk)) {
    if(!is.character(save_to_disk)){stop("'save_to_disk' must be a character file path.")}

    if(!dir.exists(gsub("/[^/]*$","",save_to_disk))){
      dir.create(gsub("/[^/]*$","",save_to_disk))

    }

    # which file ending was chosen
    ending <- stringr::str_extract(string = save_to_disk, pattern = "\\.[:alpha:]+$")

    if (ending %in% c(".RDS", ".rds", ".Rds")) {
      saveRDS(object = full, file = save_to_disk)
    } else if (ending == ".csv") {
      readr::write_csv(x = full, file = save_to_disk)
    } else if (ending %in% c(".xls", ".xlsx")) {
      writexl::write_xlsx(x = full, path = save_to_disk)
    } else {
      warning(paste0("File ending currently chosen in 'save_to_disk' is ",ending,", which is not yet implemented. Please choose one of RDS, rds, Rds, csv, xls, xlsx."))
    }

  }

  return(full)



  # Moritz' original notes, check whether fulfilled
  # check the inputdata_directory for any files

  # open files and check if variables available

  # if not, download via eurostat package

  # return finalised data

}
