# One-stop shop for validation of filter\_\*() functions

Wrapper function for all the input validation done by `filter_*()`
functions (specifically those that filter by fishery_id).

## Usage

``` r
validate_fishery_filter_inputs(
  .data,
  species,
  return_ids,
  call = rlang::caller_env()
)
```
