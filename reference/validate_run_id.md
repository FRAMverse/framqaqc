# Convenience function to check run_id input

Convenience function to check run_id input

## Usage

``` r
validate_run_id(
  fram_db,
  run_id,
  n = NULL,
  allow_null = FALSE,
  call = rlang::caller_env()
)
```

## Arguments

- fram_db:

  FRAM database object

- run_id:

  one or more run_ids

- n:

  Required number of run ids

- allow_null:

  Should this function treat a `NULL` input as valid? Logical, defaults
  to TRUE

- call:

  internal use: identify name of function that called this function (for
  informative error message)
