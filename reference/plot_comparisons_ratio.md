# Plot the ratios of database comparisons

Creates faceted plots based on the output of
[`compare_table_across_dbs()`](https://framverse.github.io/framqaqc/reference/compare_table_across_dbs.md),
with the ratio of db2/db1 plotted against the values of db1. If two
databases are exactly the same, all points should have a ratio of
exactly 1; dashed horizontal line at 1 is included as reference. To
improve performance, avoids plotting duplicate and near-duplicate
entries. "near-duplicate" values of db1 is controlled by argument
`round_digits`; ratio values within 0.001 are treated as near-duplicate.

## Usage

``` r
plot_comparisons_ratio(tab, round_digits = 0)
```

## Arguments

- tab:

  tibble produced from
  [`compare_table_across_dbs()`](https://framverse.github.io/framqaqc/reference/compare_table_across_dbs.md).

- round_digits:

  digits to round db1 to in order to avoid plotting near-duplicate
  points.

## Value

ggplot object
