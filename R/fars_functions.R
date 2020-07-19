#' Import dataset
#'
#' This function can be used to import a dataset in .csv format. The file name
#' needs to be provided via the \code{filename} argument.
#'
#' @param filename A character string giving the name of the input file
#'
#' @return This function returns a tibble (a data frame with class \code{tbl_df}.
#' If the file does not exist, a message error is produced instead.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2015")
#' }
#'
fars_read <- function(filename) {
  data <- data(filename)
  dplyr::tbl_df(data)
  if(is.null(data))
    stop("file '", filename, "' does not exist")
}

#' Produce name of a dataset given an year
#'
#' This function can be used to obtain the name of one of the datasets contained
#' in this package, providing an year via the \code{year} argument. The according
#' file name is produced.
#'
#' @param year An integer representing an year
#'
#' @return This function returns a string (a file name). An error will be produced
#' if the input cannot be coerced to an integer.
#'
#' @examples
#' \dontrun{
#' make_filename(2015)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read month data from a dataset
#'
#' This function can be used to extract the MONTH column from the datasets provided
#' in the package. The packages to which the function will be applied are selected via
#' the the argument \code{years}.
#'
#' @param years A list of integers representing years
#'
#' @return This function returns a list of dataframes. Each dataset contains two columns:
#' the column \code{MONTH}, extracted from the datasets contained in the package, and the
#' column \code{year}, containing the year to which the dataset refers.
#' An error is thrown in case an invalid year is inputed.
#'
#' @importFrom dplyr mutate select
#'
#' @examples
#' \dontrun{
#' fars_read_year(list(2014,2015))
#' }
#'
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

#' Summarize monthly data
#'
#' This function can be used to obtain the number of accidents per month for several
#' years, selected via the \code{years} parameter.
#'
#' @inheritParams fars_read_years
#'
#' @return This function returns a dataframe with the number of accident per months for
#' every given year. Each year is represented as a column in the dataframe.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(list(2014,2015))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Map distribution of accidents for a given state and year
#'
#' This function can be used to draw a map of a state, with a dot for each registered
#' accident in a given year. The state is selected via the \code{state.num} argument,
#' the year through the \code{year} aergument.
#'
#' @param state.num An integer between 1 and 56 representing the state number
#'
#' @inheritParams make_filename
#'
#' @return This function plots a map of a state with accident data represented by point.
#' It does not return anything. Not inputing an integer in \code{state.num} will result
#' in an error.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(1,2015)
#' }
#'
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
