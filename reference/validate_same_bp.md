# Compare base periods of two runs

Compares the base periods of two or more run ids. If `strict = TRUE`,
errors if the base periods are not the same. If `strict = FALSE`, does
not error and invisibly returns a list of comparison information.

## Usage

``` r
validate_same_bp(fram_db, run_ids, strict = TRUE, call = rlang::caller_env())
```

## Arguments

- fram_db:

  Fram database connection

- run_ids:

  vector of two or more run ids to check

- strict:

  Should this function error out if the runs don't have the same base
  period (`TRUE`) or just return a list with information about which
  aspects of the base periods do / don't match (`FALSE`)?

## Value

a list with the following elements:

- `same_bp`:

  Is the BP identical for each run? Logical.

- `same_fishery_version`:

  Is the fishery version number the same? Logical.

- `same_stock_version`:

  Is the stock version the same? Logical.

- `same_time_step_version`:

  Is the time step version the same? Logical.

- `base_periods_df`:

  Data frame with the run_id and run_name, joined with the info from the
  BaseID table.
