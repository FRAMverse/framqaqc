# Finds tables that contain a specific column name

Finds tables that contain a specific column name

## Usage

``` r
find_tables_by_column_(fram_db, column_name)
```

## Arguments

- fram_db:

  FRAM database object

- column_name:

  Name of a column; character atomic or vector

## Examples

``` r
if (FALSE) fram_db |> find_tables_by_column_('RunID') # \dontrun{}
```
