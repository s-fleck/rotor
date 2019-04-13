#' @section Supported Datetime Formats
#'
#' **rotor** only supports a limited number of
#' [ISO_8601][https://en.wikipedia.org/wiki/ISO_8601]
#' inspired formats for Dates and Datetimes.
#' To work with lgr dateimtes have to be lexically sortable (`2018-12-04`` is
#' ok, `12-04-2018` is not) and can only contain characters that are valid in
#' filenames (`2018-12-04T12-34-52` is ok, `2018-12-04T12:34:52`) is not.
#' The formatting string syntax is as with [strftime()].
#'
#' For Dates, the recommended format is `"%Y-%m-%d"`. The `%m` and `%d` are
#' optional, as are the dashes (`-`).
#'
#' For Datettimes, the recommended format is `""%Y%m%dT%H%M%S"`. `%M` and
#' `%S` are optional, as is the `T`. You can also add dashes (`-`) between the
#' date/time components if you like.
#'
#' Examples:
#'
#' ```
#' `2018-12-04`
#' `201810`  # same as `2018-10-01`
#'
#' `20181204T123452`
#' `2018120412`  # same as `2018-12-04T12-00-00`
#' ```
#'
#' These formats are all based on
#' .
#' To work with
#'
#'
#'
#'
#' @family abstract classes
