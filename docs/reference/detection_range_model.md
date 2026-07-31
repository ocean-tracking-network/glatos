# Detection Range Probability Model

Uses a third-order polynomial, logit, or, probit model to determine
detection range based on a expected percentage of detections for
acoustic telemetry receivers. This function is to be used after a
preliminary range test which uses multiple distances away from
representative receiver. Preliminary detection efficiency is to be
determined in Vemco's Range Testing software and exported as a csv.

## Usage

``` r
detection_range_model(
  formula,
  data,
  percentage = NULL,
  link = NULL,
  subset = NULL,
  summary_stats = TRUE,
  model_frame = NULL
)
```

## Arguments

- formula:

  an object of class `formula` or one that can be coerced to that
  class): a symbolic description of the model to be fitted. The details
  of model specification are given under Details.

- data:

  an optional data frame, list or environment (or object coercible by
  as.data.frame to a data frame) containing the variables in the model.
  If not found in data, the variables are taken from
  environment(formula), typically the environment from which
  `detection_range_model()` is called.

- percentage:

  a given percentage of expected detections to be heard by
  representative receiver. For example a value of 50 will calculate the
  distance at which 50% of a range tag is expected to be heard by a
  representative receiver. If more than one percentage value is wanted,
  specify by creating a vector. Percentages can be calculated down to
  the 1e-16 of a percentage (e.g. 99.9). However, the closer you
  approach 0 or 100 the less accurate the model becomes. The resulting
  dataframe may round the display, if it does just call the specific
  column desired.

- link:

  default is `"polynomial"` which will use and return a third order
  polynomial model. However, if a logit or probit model is desired the
  argument can be supplied with `"logit"` or `"probit"` and will return
  a logit or probit model. See Details about y variable format depending
  on which link is chosen.

- subset:

  allows for the data to be subsetted if desired. Default set to `NULL`
  and does not need to be supplied.

- summary_stats:

  default is set to `TRUE` resulting in all summary statistics of the
  model to be returned. If set to `FALSE`, summary statistics of the
  model will not be displayed. Summary statistics should always be
  evaluated, however the argument exists solely to shorten the dataframe
  in the need to quickly look at predicted distances. Use with caution.

- model_frame:

  argument call is only required when link is set to `"polynomial"`
  default is `"data_frame"`. See details about polynomial `formula` call
  and when it is appropriate to use `"data_frame"` or `"matrix"`.

## Value

A tibble object that consists of the percentage of interest, the
predicted distance from the model, and model summary statistics:

If a third order polynomial is selected the following summary statistics
are displayed: degrees of freedom, chi-square test, person's goodness of
fit test, slopes, slopes' standard error, slopes' p-value, residual
standard error, r squared and adjusted r squared values, and aic value.

If a logit or probit model is selected the following summary statistics
are displayed: degrees of freedom, chi-square (deviance), person's
goodness of fit test, slope, slope's standard error, slope's p-value,
z-value, null deviance, and aic value.

## Details

If a third order polynomial model is selected, the formula call can be
in two different formats. The preferred and default format is
`y ~ -1 + x + I(x ^ 2) + I(x ^ 3) + offset(y-intercept)`. `model_frame`
needs to be set `"data_frame"`to properly extract parameters and
determine distances away from a receiver given a percentage of interest.
If using the `base::poly()` within the formula as such
`y ~ -1 + poly(x, 3, raw = TRUE) + offset(y-intercept)`, then
`model_frame` argument needs to be set to `"matrix"`. Both formula
formats have [`offset()`](https://rdrr.io/r/stats/offset.html) which
sets the y-intercept. The y-intercept needs to be set to 100, as x = 0 m
away from a receiver you expect to hear a tag 100\\

A third order polynomial will handle preliminary detection efficiency
percentages (y variable) as whole numbers as the model is not bound by 0
and 1. While both logit and probit models have to use percentages as
decimals as the models are bound by 0 and 1.

Additionally, its been noticed that with fewer data points a third order
polynomial often fits the data better however this does not mean that
neither a logit or probit model should not be assessed as well.

## References

This function was developed for an ongoing study which followed
detection range efficiency methods similar to:

Brownscombe, J.W., L.P. Griffin, J.M. Chapman, D. Morley, A. Acosta,
G.T. Crossin, S.J. Iverson, A.J. Adams, S.J. Cooke, and A.J. Danylchuk.
2020. A practical method to account for variation in detection range in
acoustic telemetry arrays to accurately quantify the spatial ecology of
aquatic animals. Methods in Ecology and Evolution 11(1):82–94.

## Author

Benjamin L. Hlina

## Examples

``` r
sample_detection_efficiency
#> # A tibble: 7 × 5
#>   distance_m avg_percent std_dev avg_percent_d intercept
#>        <dbl>       <dbl>   <dbl>         <dbl>     <dbl>
#> 1       62.9        95.3    12.3         0.953       100
#> 2      181.         89.3    14.3         0.893       100
#> 3      346.         76.2    23.0         0.762       100
#> 4      724.          0       0           0           100
#> 5      348.         70.0    28.0         0.700       100
#> 6      464.          0       0           0           100
#> 7      752.          0       0           0           100

# third order polynomial: # ave_percent is a whole number

m <- detection_range_model(
  avg_percent ~ -1 + distance_m + I(distance_m^2) +
    I(distance_m^3) + offset(intercept),
  data = sample_detection_efficiency,
  percentage = c(10, 50, 90),
  link = "polynomial",
  model_frame = "data_frame"
)
#> Warning: Check if your formula is correct for the model_frame argument

# logit model: aver percent is in decimal form

m1 <- detection_range_model(avg_percent_d ~ distance_m,
  data = sample_detection_efficiency,
  percentage = c(10, 50, 90),
  link = "logit",
  summary_stats = TRUE
)
#> Warning: non-integer #successes in a binomial glm!

# probit model: aver percent is in decimal form

m2 <- detection_range_model(avg_percent_d ~ distance_m,
  data = sample_detection_efficiency,
  percentage = c(10, 50, 90),
  link = "probit",
  summary_stats = TRUE
)
#> Warning: non-integer #successes in a binomial glm!

m
#> # A tibble: 3 × 19
#>       p distance    df chi_square      pgof     a  a_se a_sig        b     b_se
#>   <dbl>    <dbl> <int>      <dbl>     <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1    10      634     4      1304. 4.20e-281 0.202 0.183 0.332 -0.00136 0.000753
#> 2    50      371     4      1304. 4.20e-281 0.202 0.183 0.332 -0.00136 0.000753
#> 3    90      217     4      1304. 4.20e-281 0.202 0.183 0.332 -0.00136 0.000753
#> # ℹ 9 more variables: b_sig <dbl>, d <dbl>, d_se <dbl>, d_sig <dbl>,
#> #   offset <dbl>, resid_se <dbl>, r2 <dbl>, adj_r2 <dbl>, aic <dbl>
m1
#> # A tibble: 3 × 14
#>       p distance    df chi_square  pgof   slope slope_se slope_sig intercept
#>   <dbl>    <dbl> <int>      <dbl> <dbl>   <dbl>    <dbl>     <dbl>     <dbl>
#> 1    10     503.     5      0.751 0.980 -0.0165   0.0155     0.287      6.08
#> 2    50     369.     5      0.751 0.980 -0.0165   0.0155     0.287      6.08
#> 3    90     236.     5      0.751 0.980 -0.0165   0.0155     0.287      6.08
#> # ℹ 5 more variables: intercept_se <dbl>, intercept_sig <dbl>, z_value <dbl>,
#> #   null_deviance <dbl>, aic <dbl>
m2
#> # A tibble: 3 × 14
#>       p distance    df chi_square  pgof    slope slope_se slope_sig intercept
#>   <dbl>    <dbl> <int>      <dbl> <dbl>    <dbl>    <dbl>     <dbl>     <dbl>
#> 1    10     521.     5      0.864 0.973 -0.00815  0.00664     0.220      2.97
#> 2    50     364.     5      0.864 0.973 -0.00815  0.00664     0.220      2.97
#> 3    90     207.     5      0.864 0.973 -0.00815  0.00664     0.220      2.97
#> # ℹ 5 more variables: intercept_se <dbl>, intercept_sig <dbl>, z_value <dbl>,
#> #   null_deviance <dbl>, aic <dbl>

# for further instruction see vignettes
```
