
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BayesCoverageModel

To fit a Bayesian transition model to coverage indicators.

# Installation

To load the package, use

``` r
devtools::load_all(here::here())
```

# What is where?

## Private files

Any files that you don’t want to push to the repository can be stored in
the `private` folder. We have set up Git to ignore the `private` folder,
so any data you add there will not be tracked by Git or uploaded to the
repository. Routine data are also included in that folder (see next)

## Input data and estimates

Publicly available input data are stored in the `data_raw` and `data`
folders. `data_raw` currently includes

- national survey data (national as dta)

- subnational survey data (processed in the repo `submeta`)

- estimates of denominator weights for subnational estimation (processed
  in the repo `submeta`)

- regional meta information

- meta info on cd countries

- toy data set for testing the inclusion of routine data

- info on indicator definitions (will probably move elsewhere)

The `data` folder currently includes parameter estimates:

- parameter estimates from 1b and global subnational fits, for local
  fitting

- parameter estimates for inclusion of routine data

Routine data are not included in the repository. We recommend storing
the routine data locally in the private folder (see template for reading
in of routine data).

## Outputs

When doing a run, by default, results are stored locally in a folder
called `bayestransitionoutput`, one folder up from the root directory. A
subfolder is created with the runname and all results associated with
that model fit can be stored there. We found this to work well for FPET,
to have all results in one place. The function `get_out_dir` can be used
to get the output directory for a specific run.

For sharing many larger fit objects (with posterior samples), and
perhaps results too, we cannot use github. Instead, we should keep those
locally and share via some other way.

## Templates, scripts, analysis files

As of 2025/9/26, we have two folders

- model fitting workflow
  - This contains scripts and templates to explore survey data, fit
    global models, and do for local fiting (for all countries)
- analysis
  - This contains script for analyses, eg specific outputs we created
    for the workflow paper, example plots, and a folder to generate
    country reports we used at cam2025

This repo had a lot more (outdated) files prior to the Sep 26 clean up.
The previous version is on the `cam2025` branch. We can delete that
branch once we’ve confirmed we are not missing anything useful.

Suggested approach to do analyses:

- If analyses/updates to existing files are relevant to all repo users,
  please add/update in the respective folders. For new files, please use
  an informative name.

- For files that do not need to be shared, you can save them in your
  private folder.

# Overview of repositories

In the Bayescoverage organization, we have a number of repositories,
each with a different purpose. As per 2025/9/25, we have:

- BayesCoverageIndicators (this repo): for model fitting and analysis

- SubMeta: for processing of subnational survey data, calculating
  denominator weights, and (I believe) cleaning up regional
  names/dealing with inconsistencies in names between survey and routine
  data.

- cdroutine: for everything routine data related: processing, EDA,
  modeling to estimate the parameters related to data quality. Estimates
  of model parameters are saved into this BayescoverageIndicators repo
  for use in modeling. Clean routine data are saved in the `private`
  folder for use in local model fitting.

The plan is to add an additional repo that is focused just on
communication of results (produce summaries, make graphs, produce
tables).

# GitHub Issues

We use GitHub Issues to track bugs, feature requests, and other tasks.
Here’s how to use them effectively:

### Creating an Issue

1.  **Search First**: Before creating a new issue, search the [issue
    tracker](https://github.com/AlkemaLab/BayesCoverageIndicators/issues)
    to see if a similar issue already exists.
2.  **Choose a Descriptive Title**: Use a clear, concise title that
    summarizes the issue.
3.  **Provide Details**:
    - **For bugs**: Include steps to reproduce, expected behavior, and
      actual behavior.
    - **For feature requests**: Explain the problem your idea solves and
      describe the proposed solution.
    - Add any supporting details, such as screenshots, logs, or code
      snippets.
4.  **Add Labels**: When creating an issue, please **use both a type
    label and a priority label** to help organize and prioritize tasks
    (see below).

------------------------------------------------------------------------

### Adding Labels

Using labels ensures that issues are categorized and prioritized
effectively. When creating or reviewing an issue, assign the following:

#### **Type Labels**

- **`Type: Bug`**: An issue that needs fixing.
- **`Type: Feature Request`**: A suggestion for new functionality.
- **`Type: Documentation`**: Issues related to missing or unclear
  documentation.
- **`Type: Question`**: General inquiries or requests for clarification.

#### **Priority Labels**

- **`Priority: Critical`**: Blocks progress or requires immediate
  attention (e.g. major bugs).
- **`Priority: High`**: Important but not blocking (e.g., key feature
  bugs, significant enhancements).
- **`Priority: Medium`**: Useful but not urgent (e.g., minor bugs,
  general improvements).
- **`Priority: Low`**: Nice-to-have improvements (e.g., minor changes,
  non-critical updates).

------------------------------------------------------------------------

Thank you for contributing to BayesCoverageIndicators!
