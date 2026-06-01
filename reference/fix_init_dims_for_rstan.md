# Fix initialization dimensions for rstan backend

RStan (using older Stan 2.21) requires explicit array dimensions, while
cmdstanr (using newer Stan 2.35+) is more flexible. This function
ensures that all vector parameters in the initialization have proper
dimensions for rstan compatibility.

## Usage

``` r
fix_init_dims_for_rstan(init_list, backend = "cmdstanr")
```

## Arguments

- init_list:

  List returned by init_fun

- backend:

  Character string: "cmdstanr" or "rstan"

## Value

Modified init_list with proper dimensions for rstan

## Details

The function uses a systematic approach to classify parameters:

- Known scalars (single values that should stay as scalars)

- Known matrices (2D arrays)

- Everything else is treated as a vector and gets explicit dimensions
