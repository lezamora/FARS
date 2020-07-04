#' Read .csv file whic contains FARS data
#'
#' This function reads data from .csv file. The data is from the \strong{US
#' National Highway Traffic Safety Administration's} \emph{Fatality Analysis
#' Reporting System (FARS)}. For more information enter hear:
#' \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'
#' @param filename A character string with the name of either a path to a file or file.
#'
#' @return A data frame with data readed from the csv file, or an error if the
#' file does not exists.
#'
#' @export
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' accident_2015 <- fars_read("./data/accident_2015.csv.bz2")
#' }
#'
#' @note To generate file name use: \code{\link{make_filename}}
#'
#' @seealso \link{make_filename}
#'
fars_read <- function(filename) {
    if(!file.exists(filename))
      stop("file '", filename, "' does not exist")
    data <- suppressMessages({
      readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}



#' Make file name to feed to fars_read function
#'
#' Make .csv data file name related to the given \code{year}
#' The function does not check if the file is available.
#'
#' @param year A string or an integer with the input \code{year}.
#'
#' @return This function returns a string with the data file name for a given
#' \code{year}, or a coercion error if the parameter cannot be parsed to integer.
#'
#' @export
#'
#' @examples
#' make_filename(2013)
#'
#' @seealso \link{make_filename}
#'
make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}



#' Read FARS years
#'
#' Helper function used by \code{fars_summarize_years}.
#' The function accepts a vector or list of years and returns a list of data
#' frames with \code{month} and \code{year} columns based on data in \code{"accident_<year>.csv.bz2"}
#' files. The files need to be located in the working directory.
#'
#' @param years A vector with a list of years.
#'
#' @return A data.frame including entries in data by \code{month}, or \code{NULL} if the
#' year is not valid.
#'
#' @export
#'
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
#' @importFrom magrittr "%>%"
#'
#' @examples
#' fars_read_years(2013)
#'
#' @seealso \link{fars_read}
#' @seealso \link{make_filename}
#' @seealso \link{fars_summarize_years}
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



#' Summarize FARS data by years
#'
#' This function summarizes yearly accidents data, by month
#'
#' @param years A vector with a list of years to summarize by.
#'
#' @return A data.frame with number of accidents by years summarized by month,
#' or an error in the case that \code{fars_read_years} function return \code{NULL}.
#'
#' @export
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarize_
#' @importFrom tidyr spread_
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' plot(fars_summarize_years(2014))
#' fars_summarize_years(c(2014, 2013))
#' }
#'
#' @seealso \link{fars_read_years}
#'
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
      dplyr::group_by(year, MONTH) %>%
      dplyr::summarize(n = n()) %>%
      tidyr::spread(year, n)
}



#' Display accidents map by state and year
#'
#' Draw a geographical map of accident sites for a specific US state and year.
#'
#' @param state.num An Integer with the \code{State Code}
#' @param year A string, or an integer, with the input \code{year}
#'
#' @return Returns a map, or an error if the \code{state.num} is invalid the
#' function shows an error.
#'
#' @export
#'
#' @importFrom maps map
#' @importFrom dplyr filter_
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(49, 2015)
#' }
#'
#' @seealso \link{fars_read}
#' @seealso \link{make_filename}
#'
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
