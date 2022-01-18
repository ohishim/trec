
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

First, you have to select two representative trends for two-categorical discrimination to get rough common trend classification.
This step is executed as follows:

``` r
#input two representative trends 
pnum <- c(2, 7)

#execute this step
TREC2(pnum, res1$argTREC)
```

A dendrogram for the trend groups obatined by two-categorical discrimination based on the representative trends (pnum) is illustrated.
Then, you have the following message:  
`Do you want to more concrete classification? Please enter yes or no: `.  


1. If "yes":  
  you have another message: 
  `Do you need some modifications? Please enter yes or no: `.  
    1. If "yes":   
      you can redefine groups and execute `TREC2.1`.
      Here, you can use three objects `trn`, `trn1`, and `trn2` to modify the groups, like this:

        ``` r
        trn[[1]] <- c(trn1, 2)  # variable numbers for group 1
        trn[[2]] <- trn2[-1]    # variable numbers for group 2
        
        # draw trends for each group
        TREC2.1(trn, res1$argTREC)
        ```
        Then, you can proceed the next step.
    1. If "no": 
      you select target trends as `tnum` and then, can proceed the next step.
      
1. If "no":  
  trec procedure terminates and you have group numbers as `trn` object.

### the last step

First, you have to select target trends.
This step is executed as follows:

``` r
#target trends
tnum <- c(4, 1, 2, 8)

#execute this step
res3 <- TREC3(tnum, res1$argTREC)
```

Then, you can obtain a figure which shows the target trends and their assigned icons.
