#' Summarize exact differences from `compare_table_across_dbs()`
#'
#' Makes a quick summary of the outputs of compare_table_across_dbs(), giving
#' min, max, 0.01 and 0.99 quantiles of the absolute differences of values by run id. A perfect match would have values that are all 0.
#'
#' @param tab tibble produced from `compare_table_across_dbs()`.
#'
#' @return gt with summary information.
#' @export
#'
summarize_exact = function(tab){
  tab |>
    dplyr::summarize(min = min(.data$diff_exact),
              q01 = quantile(.data$diff_exact, probs = 0.01),
              q99 = quantile(.data$diff_exact, probs = 0.99),
              max = max(.data$diff_exact),
              .by = .data$run_id)|>
    gt::gt() |>
    gt::cols_label(
      run_id = gt::md("Run ID"),
      min = gt::md("Minimum"),
      q01 = gt::md("1%"),
      q99 = gt::md("99%"),
      max = gt::md("Maximum")
    ) |>
    gt::fmt_number(decimals = 2) |>
    gt::fmt_number(columns = "run_id", decimals = 0)
}


#' Summarize ratio differences from `compare_table_across_dbs()`
#'
#' Makes a quick summary of the outputs of compare_table_across_dbs(), giving
#' min, max, 0.01 and 0.99 quantiles of the ratio of values from db2 / db1, by run id.
#' Ignore entries with values <=1 to db1 to avoid divide-by-zero issues and inflation of small differences.
#'
#' @inheritParams summarize_exact
#'
#' @return gt with summary information.
#' @export
#'
summarize_ratio = function(tab){
  tab |>
    dplyr::filter(.data$db1>1) |>
    dplyr::summarize(min = min(.data$diff_ratio),
              q01 = quantile(.data$diff_ratio, probs = 0.01),
              q99 = quantile(.data$diff_ratio, probs = 0.99),
              max = max(.data$diff_ratio),
              .by = .data$run_id) |>
    gt::gt() |>
    gt::cols_label(
      run_id = gt::md("Run ID"),
      min = gt::md("Minimum"),
      q01 = gt::md("1%"),
      q99 = gt::md("99%"),
      max = gt::md("Maximum")
    ) |>
    gt::fmt_number(decimals = 2) |>
    gt::fmt_number(columns = .data$run_id, decimals = 0)
}
