# Check that input is a dataframe

Check that input is a dataframe

## Usage

``` r
validate_data_frame(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)
```

## Arguments

- x:

  object that must be dataframe.

- arg:

  caller argument name used for error message. Defaults to caller of
  this function

- call:

  caller environment used for error messages. Defaults to caller
  environment of this function
