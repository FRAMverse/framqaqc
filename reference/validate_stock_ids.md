# Convenience function to check stock input

No error checking for transfer databases.

## Usage

``` r
validate_stock_ids(
  fram_db,
  stock_id,
  n = NULL,
  allow_null = FALSE,
  call = rlang::caller_env()
)
```

## Arguments

- fram_db:

  FRAM database object

- stock_id:

  one or more stock_ids

- n:

  Required number of run ids

- allow_null:

  Should this function treat a `NULL` input as valid? Logical, defaults
  to TRUE

- call:

  internal use: identify name of function that called this function (for
  informative error message)
