# Load a saved model fit

This helper function loads a previously saved model fit based on the
indicator name, run step, and folder suffix.

## Usage

``` r
get_fit(indicator, runstep, folder_suffix)
```

## Arguments

- indicator:

  Character string specifying the indicator name (e.g., `"ideliv"`,
  `"anc4"`).

- runstep:

  Character string indicating the model step (e.g., `"step1a"`,
  `"step1b"`).

- folder_suffix:

  Character or numeric. The suffix at the end of the folder name.

## Value

A model fit object read from an RDS file.
