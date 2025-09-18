#' Compare tables between two databases
#'
#' @param fram_db_1 fram database connection to first database (used as reference for ratio calculations)
#' @param fram_db_2 fram database connection to second database
#' @param table_name Name of table to compare
#'
#' @return tibble of comparisons, including run_id, as appropriate stock_id, age, fishery_id, time_step. `$db1` and `$db2` give the values in the database for column `$metric`; `$diff_exact` has db1-db2, and `$diff_ratio` has db2/db1.
#' @export
#'
#' @examples
#'\dontrun{
#' library(framrsquared)
#'fram_a = connect_fram_db(
#' "C:/Repos/fram multirun testing/Formal testing/coho_preseason_notamm_a.mdb",
#'read_only = TRUE,
#'quiet = TRUE)
#'fram_c = connect_fram_db(
#' "C:/Repos/fram multirun testing/Formal testing/coho_preseason_notamm_c.mdb",
#'                         read_only = TRUE,
#'                        quiet = TRUE)
#' compare_table_across_dbs(fram_a, fram_c, "Mortality")
#' disconnect_fram_db(fram_a)
#' disconnect_fram_db(fram_b)
#'}

compare_table_across_dbs = function(fram_db_1, fram_db_2,
                                   table_name){
  ## all possible names to use when joining
  names_for_joining = c("run_id", "stock_id", "age", "fishery_id", "time_step")

  tab1 = fram_db_1 |>
    framrsquared::fetch_table(table_name) |>
    dplyr::select(-.data$primary_key)
  tab2 = fram_db_2 |>
    framrsquared::fetch_table(table_name)|>
    dplyr::select(-.data$primary_key)

  ## identify names to join by for this table
  join_by = names_for_joining[names_for_joining %in% names(tab1)]

    tab1 |>
      dplyr::full_join(tab2,
              by = join_by,
              suffix = c(".db1", ".db2")
    ) |>
      tidyr::pivot_longer(-tidyselect::any_of(join_by),
                 names_to = c("metric", "db"),
                 names_sep = "[.]") |>
      tidyr::pivot_wider(names_from = .data$db, values_from = .data$value) |>
      dplyr::mutate(diff_exact = .data$db1-.data$db2,
           diff_ratio = .data$db2/.data$db1)

    }
