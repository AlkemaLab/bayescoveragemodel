# Get Standard Error on invProbit Scale from Proportion

Transforms standard error from proportion scale to invprobit scale using
the delta method. Uses the derivative of the inverse probit function:
SE_probit = SE_prop / dnorm(qnorm(prop))

## Usage

``` r
get_se_invprobitprop(prop, se_prop)
```

## Arguments

- prop:

  Proportion value (between 0 and 1)

- se_prop:

  Standard error on proportion scale

## Value

Standard error on probit scale
