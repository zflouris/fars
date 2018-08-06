#' fars_read Function
#'
#' This is a function that reads the cvs file with the data.
#'
#' @param filename A character with the name of the file you want to study
#' @importFrom readr read_csv
#' @return data.table object with all the data
#' @note An error occur when the file does not exist'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
#' make_filename Functions
#'
#' This is adunction that creates the filename from which you will access the data with the help of the fars read function
#' @param year A character, numeric or integer with the year you are interested in
#' @return A character vector with the name of the file you want to access.
#' @export


make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years Function
#'
#' This is a function that gathers the data cancerning all the years you give as input.
#'
#' @param years A vector with all the years you want to study
#' @importFrom tidyr spread
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @return a data list with objects the data from all the years you are interested in.
#' @note An error occur when there are no data for a year that you asked'
#' @export

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' fars_summarize_years Function
#'
#' This is a function that gathers the data cancerning all the years you give as input.
#'
#' @param years A vector with all the years you want to study
#' @importFrom tidyr spread
#' @importFrom dplyr summarize bind_rows
#' @return a data list with objects the data from all the years you are interested in.
#' @note An error occur when there are no data for a year that you asked
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' fars_map_state Function
#'
#' This is a function that takes as input the state you are interested in and the state you want to study
#' and gives as output a map with dots in all the places were a fatal accident took place.
#'
#' @param state.num code number of a state and the year
#' @param year the year
#' @importFrom maps map
#' @importFrom dplyr filter
#' @return a plot that depicts a map of the state
#' @note A message appears when an invalid state number is given when there are no accidents.
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
#### these are all
