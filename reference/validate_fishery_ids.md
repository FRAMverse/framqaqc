# Convenience function to check fishery input

No error checking for transfer databases

## Usage

``` r
validate_fishery_ids(
  fram_db,
  fishery_id,
  n = NULL,
  allow_null = FALSE,
  call = rlang::caller_env()
)
```

## Arguments

- fram_db:

  FRAM database object

- fishery_id:

  one or more fishery_ids

- n:

  Required number of run ids

- allow_null:

  Should this function treat a `NULL` input as valid? Logical, defaults
  to TRUE

- call:

  internal use: identify name of function that called this function (for
  informative error message)
