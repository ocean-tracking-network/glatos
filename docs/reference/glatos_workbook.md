# Constructor function for the class glatos_workbook

Constructor function for the class glatos_workbook. Currently barebones
and only used inside read_glatos_workbook.

## Usage

``` r
glatos_workbook(x)
```

## Arguments

- x:

  A list containing data from a standard GLATOS data workbook (\*.xlsm)
  file.

## Value

A list of class `glatos_workbook` created from a standard GLATOS data
workbook (\*.xlsx or \*.xlsm) file with three elements:

- metadata:

  A list with data about the project.

- animals:

  A data frame with data about tagged animals.

- receivers:

  A data frame with data about receivers.

## Note

This function may be developed in the future to dictate conversion
construction from a data frame.
