# Check that input is a numeric atomic or vector

Check that input is a numeric atomic or vector

## Usage

``` r
validate_numeric(
  x,
  n = NULL,
  allow_null = FALSE,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)
```

## Arguments

- x:

  object that must be numeric atomic or vector.

- n:

  If provided, exact x must be exactly this length

- allow_null:

  If provided, treat a `NULL` value of x as valid. Logical, defaults to
  FALSE

- arg:

  caller argument name used for error message. Defaults to caller of
  this function

- call:

  caller environment used for error messages. Defaults to caller
  environment of this function
