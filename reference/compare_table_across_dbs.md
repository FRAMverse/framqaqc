# Compare tables between two databases

Compare tables between two databases

## Usage

``` r
compare_table_across_dbs(fram_db_1, fram_db_2, table_name)
```

## Arguments

- fram_db_1:

  fram database connection to first database (used as reference for
  ratio calculations)

- fram_db_2:

  fram database connection to second database

- table_name:

  Name of table to compare

## Value

tibble of comparisons, including run_id, as appropriate stock_id, age,
fishery_id, time_step. `$db1` and `$db2` give the values in the database
for column `$metric`; `$diff_exact` has db1-db2, and `$diff_ratio` has
db2/db1.

## Examples

``` r
if (FALSE) { # \dontrun{
library(framrsquared)
fram_a = connect_fram_db(
"C:/Repos/fram multirun testing/Formal testing/coho_preseason_notamm_a.mdb",
read_only = TRUE,
quiet = TRUE)
fram_c = connect_fram_db(
"C:/Repos/fram multirun testing/Formal testing/coho_preseason_notamm_c.mdb",
                        read_only = TRUE,
                       quiet = TRUE)
compare_table_across_dbs(fram_a, fram_c, "Mortality")
disconnect_fram_db(fram_a)
disconnect_fram_db(fram_b)
} # }
```
