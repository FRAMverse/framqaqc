# Plot the values of the two databases against each other

Creates faceted plots based on the output of
[`compare_table_across_dbs()`](https://framverse.github.io/framqaqc/reference/compare_table_across_dbs.md),
with the values from one database plotted against the corresponding
values from the other. If two databases are exactly the same, all points
should fall on the 1:1 line; dashed 1:1 line included as reference. To
improve performance, avoids plotting duplicate and near-duplicate
entries. "near-duplicate" is controlled by argument `round_digits`.

## Usage

``` r
plot_comparisons_exact(tab, round_digits = 0)
```

## Arguments

- tab:

  tibble produced from
  [`compare_table_across_dbs()`](https://framverse.github.io/framqaqc/reference/compare_table_across_dbs.md).

- round_digits:

  Digits to round to before identifying entries as "near-duplicate"
  entries. Defaults to 0 (e.g., round to the nearest fish). Appropriate
  to reduce when comparing tables in which values are much smaller.

## Value

ggplot object
