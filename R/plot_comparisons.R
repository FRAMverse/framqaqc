#' Plot the values of the two databases against each other
#'
#' Creates faceted plots based on the output of `compare_table_across_dbs()`, with the values from one database plotted against the corresponding values from the other. If two databases are exactly the same, all points should fall on the 1:1 line; dashed 1:1 line included as reference. To improve performance, avoids plotting duplicate and near-duplicate entries. "near-duplicate" is controlled by argument `round_digits`.
#'
#' @inheritParams summarize_exact
#' @param round_digits Digits to round to before identifying entries as "near-duplicate" entries. Defaults to 0 (e.g., round to the nearest fish). Appropriate to reduce when comparing tables in which values are much smaller.
#'
#' @return ggplot object
#' @export
#'
plot_comparisons_exact = function(tab, round_digits = 0){
  tab |>
    dplyr::mutate(db1 = round(.data$db1, round_digits),
           db2 = round(.data$db2, round_digits),
           diff_exact = .data$db1-.data$db2
    ) |>
    dplyr::select("metric", "db1", "db2") |>
    dplyr::distinct() |>
    ggplot2::ggplot(ggplot2::aes(x = .data$db1, y = .data$db2))+
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2)+
    ggplot2::geom_point()+
    ggplot2::facet_wrap(. ~ .data$metric, scales = "free")
}

#' Plot the ratios of database comparisons
#'
#' Creates faceted plots based on the output of `compare_table_across_dbs()`, with the ratio of db2/db1 plotted against the values of db1. If two databases are exactly the same, all points should have a ratio of exactly 1; dashed horizontal line at 1 is included as reference. To improve performance, avoids plotting duplicate and near-duplicate entries. "near-duplicate" values of db1 is controlled by argument `round_digits`; ratio values within 0.001 are treated as near-duplicate.
#'
#' @inheritParams summarize_exact
#' @param round_digits digits to round db1 to in order to avoid plotting near-duplicate points.
#'
#' @return ggplot object
#' @export
#'
plot_comparisons_ratio = function(tab, round_digits = 0){
  tab |>
    dplyr::mutate(db1 = round(.data$db1, round_digits),
           diff_ratio = round(.data$diff_ratio, 3)) |>
    dplyr::select(.data$metric, .data$db1, .data$diff_ratio) |>
    dplyr::distinct() |>
    ggplot2::ggplot(ggplot2::aes(x = .data$db1, y = .data$diff_ratio))+
    ggplot2::geom_abline(intercept = 1, slope = 0, linetype = 2)+
    ggplot2::geom_point()+
    ggplot2::facet_wrap(. ~ .data$metric, scales = "free")
}
