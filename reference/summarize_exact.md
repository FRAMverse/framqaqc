# Summarize exact differences from `compare_table_across_dbs()`

Makes a quick summary of the outputs of compare_table_across_dbs(),
giving min, max, 0.01 and 0.99 quantiles of the absolute differences of
values by run id. A perfect match would have values that are all 0.

## Usage

``` r
summarize_exact(tab)
```

## Arguments

- tab:

  tibble produced from
  [`compare_table_across_dbs()`](https://framverse.github.io/framqaqc/reference/compare_table_across_dbs.md).

## Value

gt with summary information.
