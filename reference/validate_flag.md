# Check that input is logical of length 1

Check that input is logical of length 1

## Usage

``` r
validate_flag(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env())
```

## Arguments

- x:

  object that must be a logical of length 1.

- arg:

  caller argument name used for error message. Defaults to caller of
  this function

- call:

  caller environment used for error messages. Defaults to caller
  environment of this function
