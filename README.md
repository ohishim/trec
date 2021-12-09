
# trec

<!-- badges: start -->
<!-- badges: end -->

This package is for fit cubic trends to multivariate time series observations and classify the trends.
When observations are inputted, you can obtain estimated trends and icons assigned to the trends.

## Installation

You can install the development version of trec like so:

``` r
# install.packages("devtools")
devtools::install_github("ohishim/trec")
```

## Example

This is a basic example which shows you how to solve a common problem:

### the first step

You can use an example dataset "exData" which has 9 variables with 20 time steps.
This step is executed as follows:

``` r
library(trec)

#execute this step
res1 <- TREC1(exData)

#figure of estimated trends for standardized observations
plot(res1$fig.ctrend)

#figure of all trends
plot(res1$fig.trend)
```

### the second step

First, you have to select two representative trends.
This step is executed as follows:

``` r
#representative trends
pnum <- c(2, 7)

#execute this step
TREC2(pnum, res1$argTREC)
```

The output is a dendrogram for the trends.

### the last step

First, you have to assign the trends into three groups and select target trends.
This step is executed as follows:

``` r
#groups
trn <- list(
  c(1, 2, 3, 4, 6, 9),
  c(),
  c(5, 7, 8)
)

#target trends
tnum <- c(4, 1, 2, 8)

#execute this step
res3 <- TREC3(trn, tnum, res1$argTREC)
```

Then, you can obtain a figure which shows the target trends and their assigned icons.
