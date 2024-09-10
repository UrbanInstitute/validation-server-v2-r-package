#' Combine table and model analyses to submit to the validation server
#'
#' @param ... Data frames returned from the get_table_output() and/or get_model_output() functions
#'
#' @return A combined data frame formatted for the validation server
#'
#' @export
submit_output <- function(...) {
    analysis_list <- list(...)

    # Check for duplicated analysis names
    names_ <- sapply(analysis_list, function(a) return(a$analysis_name[1]))
    dupes_ <- names_[duplicated(names_)]
    if (length(dupes_) > 0) {
        stop('Analysis names must be unique.\nDuplicated analysis name(s): ',
             paste(dupes_, collapse = ', '),
             call. = FALSE)
    }

    output <- dplyr::bind_rows(analysis_list)

    # TODO: # Should get_model_output() and get_table_output() be classes?
    required_cols <- c("analysis_type", "analysis_name", "statistic", "n", "value")
    if (!all(required_cols %in% names(output))) {
        stop('Only objects from the `get_table_output()` and `get_model_output()` functions can be submitted.',
             call. = FALSE)
    }

    # reorder output
    output <- output %>%
        dplyr::select(analysis_type,
               analysis_name,
               matches("var"),
               matches("group"),
               statistic,
               value,
               n,
               everything())

    return(output)
}
