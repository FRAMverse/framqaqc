# Custom package error function

Custom wrapper for
[`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html),
which passes the class "framrsquared_error" to the error message. This
means that `testthat::expect_error()` and similar message checking can
specifically check for framrsquared error messages.

## Usage

``` r
fram_abort(
  message,
  internal_error = FALSE,
  class = NULL,
  call = rlang::caller_env(),
  .envir = parent.frame(),
  ...
)
```

## Arguments

- message:

  Error message, handled the same as cli::cli_abort `message` argument

- internal_error:

  Is this errror likely reflecting a problem with FRAM (TRUE) or a user
  input error (FALSE)? If TRUE, message will include suggestion to file
  an issue on github. Logical, defaults to FALSE.

- class:

  Additional error classes to add. Character vector. Defaults to NULL

- call:

  calling function, used to label the function that errored.

- .envir:

  execution environment, used to populate info in error messages.

- ...:

  arguments passed to cli::cli_abort
