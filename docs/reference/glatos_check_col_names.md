# Check column names and classes of a list or data.frame against requirements

Check column names and classes of a list or data.frame against
requirements

Check column classes of a list or data.frame against requirements

## Usage

``` r
glatos_check_col_names(x, req_cols)

glatos_check_col_classes(x, req_cols)
```

## Arguments

- x:

  a data.frame, or object that inherits from data.frame, to check

- req_cols:

  a named list containing a character string with the class of each
  required column; each element name is a required column name
