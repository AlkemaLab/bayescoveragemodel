# Rename output folder and directory in fit object

Renames an existing folder in `bayestransition_output`and updates the
`output_dir` in the model fit object.

## Usage

``` r
rename_output_folder(indicator, run_step, old_folder_name, new_folder_name)
```

## Arguments

- indicator:

  Character. Indicator name.

- run_step:

  Character. Run step - either 1a, 1b, local_national.

- old_folder_name:

  Character. The current folder name (inside `bayestransition_output`).

- new_folder_name:

  Character. The new name to rename the folder to.
