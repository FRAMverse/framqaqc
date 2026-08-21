# Handle species identification for filters

Convenience function to condense code. `filter_*` either uses the
"species" attr, or the optional `species` argument, and must provide
informative errors when both are missing or both are present and
mismatch. Uses
[`standardize_species()`](https://framverse.github.io/framqaqc/reference/standardize_species.md)
to allow multiple specifications for species (e.g., "COHO", "Coho", or
"coho").

## Usage

``` r
validate_species(.data, species = NULL)
```

## Arguments

- .data:

  Dataframe

- species:

  Optional, either "COHO" or "CHINOOK". Character vector, defaults to
  NULL

## Value

Character vector "species"
