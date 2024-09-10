#' Specify a fit model to submit to the validation server
#'
#' @param fit A model object. Currently only `lm` and `glm` are supported.
#' @param model_name Arbitrary name for the model
#'
#' @return A data frame formatted for the validation server
#'
#' @importFrom magrittr %>%
#' @export
get_model_output <- function(fit = NULL,
                             model_name = NULL,
                             validate = TRUE) {
    if (validate) {
        validate_model_args(fit, model_name)
    }

    td <- broom::tidy(fit)
    gl <- broom::glance(fit)

    output <- dplyr::bind_rows(
        gl %>%
            tidyr::pivot_longer(everything(), names_to = "statistic", values_to = "value"),
        td %>%
            tidyr::pivot_longer(!term, names_to = "statistic", values_to = "value") %>%
            dplyr::select(var = term, statistic, value)
    ) %>%
        dplyr::mutate(n = gl[["nobs"]],
                      analysis_type = "model",
                      analysis_name = model_name)

    return(output)
}

#' Validate get_model_output() arguments with basic error handling
validate_model_args <- function(fit, model_name) {
    # Validate `fit`
    valid_methods <- list("lm", "glm")
    if (!(class(fit)[1] %in% valid_methods)) {
        stop('`fit` object of type "', class(fit)[1], '" is invalid.\n',
             'Only lm and glm methods are currently supported.',
             call. = FALSE)
    }

    # Validate `model_name`
    if (is.null(model_name)) {
        stop('`model_name` argument must be specified.\n',
             'This will be the name for this analysis in the validation server output.',
             call. = FALSE)
    }
}
