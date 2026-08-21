# Summarize ratio differences from `compare_table_across_dbs()`

Makes a quick summary of the outputs of compare_table_across_dbs(),
giving min, max, 0.01 and 0.99 quantiles of the ratio of values from db2
/ db1, by run id. Ignore entries with values \<=1 to db1 to avoid
divide-by-zero issues and inflation of small differences.

## Usage

``` r
summarize_ratio(tab)
```

## Arguments

- tab:

  tibble produced from
  [`compare_table_across_dbs()`](https://framverse.github.io/framqaqc/reference/compare_table_across_dbs.md).

## Value

gt with summary information.
