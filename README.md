
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

res1 <- TREC1(exData)
```
Then, you have a figure of all trends.
This figure can be reconstructed by `plot(res1$fig.trend)`.
The trend fits are visible by `plot(res1$fig.ctrend)` where each variable is standardized.

### the second step

After the first step, you have an object `pnum` which has two variable numbers of representative trends.
If you have no problem, the second step can be executed with the `pnum` as follows:

``` r
#classification into two groups
res2 <- TREC2(pnum, res1$argTREC)

#classification into three groups
res2 <- TREC2(pnum, res1$argTREC, groups=3)
```

If `pnum` cannot be automatically defined, `pnum` has the following message:  
`"'pnum' cannot be defined."`  
In such a case, or when you want to modify `pnum`, you can manually define `pnum`. 
Here you can get a correspondence between variable names and variable numbers as `Vnames`.

A dendrogram for the trend groups obatined by two- or three-categorical discrimination based on the representative trends `pnum` is illustrated.
This dendrobram can be reconstructed by `plot(res2$dend)`.
Then, you have the following message:  
`Do you want to more concrete classification? Please enter yes or no: `.  


1. If "yes":  
  you have a figure of trends for each group and have another message: 
  `Do you need some modifications? Please enter yes or no: `.  
    1. If "yes" (in the case of classification into two groups):   
      you can redefine groups and execute `TREC2.1`.
      Here, you can use three objects `trn`, `trn1`, and `trn2` to modify the groups, like this:

        ``` r
        trn[[1]] <- c(trn1, 2)  # variable numbers for group 1
        trn[[2]] <- trn2[-1]    # variable numbers for group 2
        
        # draw trends for each group
        TREC2.1(trn, res1$argTREC)
        ```
        Then, you can proceed the next step.
    1. If "yes" (in the case of classification into three groups):  
      you can classify into three groups as follows:  

        ``` r
        trn[[3]] <- trn2[1]   # variable numbers for group 3
        trn[[2]] <- trn2[-1]  # variable numbers for group 2
        
        # draw trends for each group
        TREC2.1(trn, res1$argTREC, groups=3)
        ```
        Then, you can proceed the next step.
    1. If "no":   
      the figure of trends for each group can be reconstructed by `plot(res2$fig.trends)`.
      Then, you select target trends as `tnum` and can proceed the next step.
      
1. If "no":  
  trec procedure terminates and you have group numbers as `trn` object.

### the last step

First, you have to select target trends.
This step is executed as follows:

``` r
#target trends
tnum <- c(1, 2, 4, 8)

#execute this step
res3 <- TREC3(tnum, res1$argTREC)
```

Then, you can obtain a figure which shows the target trends and their assigned icons, and obtain variable numbers for each group classified based on the target trends.
These figure and group numbers are respectively reconstructed by `plot(res3$fig.icon)` and `res3$group`.
Moreover, you can obtain a figure of group-wise trends as

``` r
plot(res3$fig.tgtrend.G)
```
