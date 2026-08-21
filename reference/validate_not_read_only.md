# Check that a database connection isn't read only

Validation in preparation for modifying a database.

## Usage

``` r
validate_not_read_only(fram_db, call = rlang::caller_env())
```

## Arguments

- fram_db:

  Fram database connection

- call:

  internal use: identify name of function that called this function (for
  informative error message)
