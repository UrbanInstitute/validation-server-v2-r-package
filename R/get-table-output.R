#' Specify a tabular analysis to submit to the validation server
#'
#' @param data Data frame object
#' @param table_name Arbitrary name for the analysis
#' @param stat Summary statistic(s) to calculate (e.g. mean, median, sum, sd, var, min, max, n, n_distinct)
#' @param var Variable(s) to summarize
#' @param by Optional variable(s) to group by
#' @param na.rm Optional NA behavior (defaults to FALSE)
#'
#' @return A data frame formatted for the validation server
#'
#' @importFrom magrittr %>%
#' @export
get_table_output <- function(data = NULL,
                             table_name = NULL,
                             stat = NULL,
                             var = NULL,
                             by = NULL,
                             na.rm = FALSE,
                             validate = TRUE) {

    if (validate) {
        validate_table_args(data, table_name, stat, var, by)
    }

    # split out quantile metrics from non-quantile metrics
    my_quantile_stat <- stat[stringr::str_detect(stat, "quantile")]
    stat <- stat[!stringr::str_detect(stat, "quantile")]

    # instantiate outputs for a later bind_rows call
    output <- NULL
    output_quantile <- NULL

    # only run if `quantile` isn't the only stat
    if (!rlang::is_empty(stat)) {
        # set names for use in .fns argument of dplyr:: across
        stat_fn <- setNames(vector("list", length(stat)), stat)

        for (s in stat) {
            if (s == "n") {
                stat_fn[[s]] <- rlang::as_closure(~ dplyr::n())
            } else {
                stat_fn[[s]] <- rlang::as_closure(s)
            }
        }

        # summarise non-quantile data by `by` variables
        output_ <- data %>%
            dplyr::group_by(across(all_of(by))) %>%
            dplyr::summarise(
                dplyr::across(
                    .cols = all_of(var),
                    .fns = stat_fn,
                    .names = "{.col}||{.fn}",
                    na.rm = na.rm
                ),
                n = dplyr::n(),
                .groups = "drop"
            )

        # clean non-quantile data
        output <- output_ %>%
            tidyr::pivot_longer(!c(all_of(by), n), names_to = "varstat", values_to = "value") %>%
            tidyr::separate(varstat, into = c("var", "statistic"), sep = "\\|\\|") %>%
            dplyr::select(all_of(by), var, statistic, value, n) %>%
            dplyr::mutate(analysis_type = "table",
                          analysis_name = table_name)

    }

    # run if there's a quantile argument present
    if (!rlang::is_empty(my_quantile_stat)) {


        stat_quantile <- setNames(vector("list", length(my_quantile_stat)), my_quantile_stat)

        # evaluate what's in between `quantile(` and `)`.
        my_probs <- my_quantile_stat %>%
            stringr::str_remove("probs = ") %>%
            stringr::str_remove("quantile\\(") %>%
            stringr::str_remove("\\)$") %>%
            parse(text = .) %>%
            eval()


        stat_quantile[[my_quantile_stat]] <- rlang::as_closure(~quantile_df(., probs = my_probs, na.rm = na.rm ))

        # create dataframe with multiple rows if there are multiple arguments of probs, summarised by `by` variables
        output_quantile_ <- data %>%
            dplyr::group_by(across(all_of(by))) %>%
            dplyr::reframe(
                dplyr::across(
                    .cols = all_of(var),
                    .fns = stat_quantile,
                    .names = "{.col}||q",
                    .unpack = TRUE,
                    na.rm = na.rm
                ),
                n = dplyr::n()
            )


        quant_col <- dplyr::select(output_quantile_, dplyr::contains("_quant")) %>%
            dplyr::select(1) %>%
            names()

        output_quantile <- output_quantile_ %>%
            rename(quantile_num = all_of(quant_col)) %>%
            dplyr::select(-contains("_quant")) %>%
            tidyr::pivot_longer(!c(all_of(by), n, quantile_num), names_to = "varstat", values_to = "value") %>%
            dplyr::mutate(statistic = stringr::str_c("quantile_",stringr::str_pad(quantile_num*100, 3, "left", "0"))) %>%
            tidyr::separate(varstat, into = c("var", "to_remove"), sep = "\\|\\|") %>%
            dplyr::select(all_of(by), var, statistic, value, n) %>%
            dplyr::mutate(analysis_type = "table",
                          analysis_name = table_name)
    }


    final_output <- bind_rows(output, output_quantile) %>% # bind non-quantile and quantile summary stats together
        dplyr::arrange(across(dplyr::all_of(c(by, "var", "statistic")), .fns = list(~ as.character(.)))) %>% # arrange data
        # we originally create quantile variable with padded 3 digits for sorting purposes
        # this code rewrites the format back to `quantile_[0-100]`
        dplyr::mutate(statistic = dplyr::if_else(
            stringr::str_detect(statistic, "quantile"),
            stringr::str_c("quantile_", stringr::str_extract(statistic, "[0-9]+") %>%
                    as.numeric()),
            statistic
        ))

    return(final_output)

}

#' Validate get_table_output() arguments with basic error handling
validate_table_args <- function(data, table_name, stat, var, by, stat_fn) {
    # Validate `data`
    if (is.null(data)) {
        stop('`data` argument must be specified.\n',
             'To use the full, untransformed confidential dataset, set `data = conf_data`.',
             call. = FALSE)
    }

    # Validate `table_name`
    if (is.null(table_name)) {
        stop('`table_name` argument must be specified.\n',
             'This will be the name for this analysis in the validation server output.',
             call. = FALSE)
    }

    # Validate `stat`
    if (is.null(stat)) {
        stop('`stat` argument must be specified.\n',
             'This will be the summary statistic(s) for this analysis.',
             call. = FALSE)
    }
    valid_stats <- c("mean", "median", "sum", "sd", "var", "min", "max", "n", "n_distinct")

    # Validate `quantile` stat
    if (any(stringr::str_detect(stat, "quantile"))) {
        my_quantile <- stat[stringr::str_detect(stat, "quantile")] %>%
            stringr::str_remove("probs = ")

        if (length(my_quantile) > 1) {
            stop("Only 1 `quantile` stat may be inputted in each run, \n",
                 "with the syntax `quantile(c(0, .2, .4, .6, .8. 1))",
                 call. = FALSE)
        }

        # TODO: create error handling for the `eval` step here
        quantile_nums <-my_quantile %>%
            stringr::str_remove("quantile\\(") %>%
            stringr::str_remove("\\)$") %>%
            parse(text = .) %>%
            eval()

        if (is.null(quantile_nums) | !is.numeric(quantile_nums)) {
            stop('`quantile` stat must be specified with the syntax `quantile(`numeric_vector`)`, \n',
                 'where `numeric_vector` is a vector of values between 0 and 1, as in `quantile(0.25)`, or `quantile(c(0.5, 0.7, .9))`',
                 call. = FALSE)
        }

        if(any(quantile_nums < 0 |
               quantile_nums > 1 )) {
            stop('`quantile` stat must be specified with the syntax `quantile(`numeric_vector`)`, \n',
                 'where `numeric_vector` is a vector of values between 0 and 1, as in `quantile(0.25)`, or `quantile(c(0.5, 0.7, .9))`',
                 call. = FALSE)
        }
    }


    temp_stat <- stat[!stringr::str_detect(stat, "quantile")]


    invalid_stats <- setdiff(temp_stat, valid_stats)
    if (length(invalid_stats) > 0) {

        stop('`stat` argument is invalid.\nInvalid stat(s) specified: ',
             paste(invalid_stats, collapse = ', '),
             '\nValid stats are: ',
             paste(valid_stats, collapse = ', '),
             call. = FALSE)
    }

    # Validate `var`
    if (is.null(var)) {
        stop('`var` argument must be specified.\n',
             call. = FALSE)
    }
    invalid_var_cols <- setdiff(var, names(data))
    if (length(invalid_var_cols) > 0) {
        stop('`var` argument is invalid.\nVariable(s) not in data: ',
             paste(invalid_var_cols, collapse = ', '),
             call. = FALSE)
    }

    # Validate `by`
    invalid_by_cols <- setdiff(by, names(data))
    if (length(invalid_by_cols) > 0) {
        stop('`by` argument is invalid.\nVariable(s) not in data: ',
             paste(invalid_by_cols, collapse = ', '),
             call. = FALSE)
    }
}



# helper function to create a tibble to implement the `quantile` function in a `reframe` function
quantile_df <- function(x, probs, na.rm) {

    # the quantile function throws an error if any NAs exist.
    # this code will instead return NA if na.rm is false and there are missing values.
    if (any(is.na(x)) & !na.rm) {
        out_tibble<- tibble(val = as.numeric(NA),
                            quant = probs)
    } else {

        out_tibble <-

            tibble(
                val = quantile(x, probs, na.rm = na.rm),
                quant = probs
            )
    }
    out_tibble
}
