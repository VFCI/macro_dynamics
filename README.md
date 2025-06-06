# macro_dynamics

<!-- badges: start -->
<!-- badges: end -->

An [R](http://r-project.org) project that replicates [The Market Price of Risk and Macro-Financial Dynamics](https://drive.google.com/file/d/1Hr_lrsj5WpqpBeSySCgboDiox0azblID/view?usp=sharing), by Tobias Adrian, Fernando Duarte and Tara Iyer.

## Installation

You can install the project from [GitHub](https://github.com/VFCI/macro_dynamics) with:

``` r
# install.packages("devtools")
devtools::install_github("VFCI/macro_dynamics")
```

## Running this Project
Open the project `macro_dynamics.Rproj` in [R](http://r-project.org).

Run `0_main.R` to recreate all output of the project relying only upon the raw data files in the `data/` folder (and packages).

The folder `output/baseline/` has the figures and tables in [The Market Price of Risk and Macro-Financial Dynamics](https://drive.google.com/file/d/1Hr_lrsj5WpqpBeSySCgboDiox0azblID/view?usp=sharing). The folder `output/appendix/` has figures and tables in the Appendix.

This project uses the R package [renv](https://rstudio.github.io/renv/articles/renv.html) for package version control. Running `main.R` automatically reinstantiates the project with the correct packages, but this can also be done manually with `renv::restore()`.

`renv` also records the version of R used, but does not enforce that specific version of R is used.  In order to install older versions of R, consider using [rig](https://github.com/r-lib/rig).

#### GSL Requirement
Some of the R packages require that the numerical C and C++ library `gsl` is installed on your system.
Please install [gsl](https://www.gnu.org/software/gsl/) if it is not already.

Note: `0_main.R` runs Monte Carlo simulations, so it takes a few minutes
to run to completion. To recreate all the results that do not require
Monte Carlo, run `0_main.R` up to [this line](https://github.com/VFCI/macro_dynamics/blob/e06107f35180c4d9e0cb92a1abe717f5ee30bece/0_main.R#L57).

## Data
Running `0_main.R` pulls the latest data from [FRED](https://fred.stlouisfed.org) and other sources. These latest data may not be the exact vintage used in the paper. If you want to see the exact data used in the paper, run

``` r
base::load("variables.RData")
```

before running `0_main.R`.

### Vintage data

[ALFRED](https://alfred.stlouisfed.org), the archival version of FRED, can be used to retreive different vintages of FRED data.

## Github Codespaces
You can also run this project using Github [codespaces](https://github.com/features/codespaces).  which sets up an environment that is pre-configured to runs this repository's code (has the
right version of R, R studio, VSCode, etc.).

To create a codespace, see [Creating a codespace for a repository](https://docs.github.com/en/codespaces/developing-in-a-codespace/creating-a-codespace-for-a-repository). Then [open the codespace](https://docs.github.com/en/codespaces/developing-in-a-codespace/opening-an-existing-codespace). Both creating and opening take a few minutes the first time.

Upon opening the codespace, VSCode should open up in one of your browser's tabs. Wait until the setup code finishes execution and all VSCode extensions are loaded.

Close any ports if any are open using the [ports tab](https://code.visualstudio.com/docs/editor/port-forwarding) in VSCode. Then in Terminal, run

``` bash
docker run --rm -ti -e PASSWORD=yourpassword -p 8787:8787 rocker/rstudio
```

Make sure you replace `yourpassword` by a suitable password of your choice. After some
initial setting up, VSCode should prompt you to open RStudio in a new tab in your browser. Use the username `rstudio` and the password you chose to log in to RStudio.

Now that you are in RStudio, you should be able to follow the steps outlined above to install and run the repository.
