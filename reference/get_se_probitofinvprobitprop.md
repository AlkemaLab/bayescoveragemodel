# Get Standard Error on Proportion Scale from invProbit proportion

Transforms standard error from invprobit scale back to proportion scale
using the delta method. Uses the derivative of the probit function:
SE_prop = SE_invprobit \* dnorm(invprobit_value)

## Usage

``` r
get_se_probitofinvprobitprop(invprobitprop, se_invprobitprop)
```

## Arguments

- invprobitprop:

  Value on invprobit scale (i.e., qnorm of proportion)

- se_invprobitprop:

  Standard error on invprobit scale

## Value

Standard error on proportion scale
