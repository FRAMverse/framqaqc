# Gets all tables of FRAM database

Skips the internal tables access creates for administration, which all
start with `"MSys"`.

## Usage

``` r
get_tables(fram_db)
```

## Arguments

- fram_db:

  Fram database object

## Examples

``` r
if (FALSE) fram_dataframe |> get_tables() # \dontrun{}
```
