
# trec (v1.0.0)

<!-- badges: start -->
<!-- badges: end -->

This package is for fit cubic trends to multivariate time series observations and classify the trends.
When observations are inputted, you can obtain estimated trends and icons assigned to the trends.  

**cite this package**:  
Ohishi, M. & Solvang, H. K. (2023). 
trec: Trend estimation and classification for time series data.
R package version 1.0.0. 
https://github.com/ohishim/trec

## Installation

You can install the R package trec like so:

``` r
# install.packages("devtools")
devtools::install_github("ohishim/trec")
```

## Example

This is a basic example which shows you how to solve a common problem:

### the first step

You can use an example dataset `exData` which has 10 variables with 20 time steps, where `year` is time points.
This step is executed as follows:

``` r
library(trec)

res1 <- TREC1(
  exData,  # dataset
  "year"   # variable name expressing time points if the dataset has it
)
```
If the dataset has a variable expressing time points, you must enter the variable name as the second parameter.
If not, you can ignore the second parameter.
In `TREC1`, the variable names of the dataset are automatically represented by V1, V2, ...
A relationship between the original names and the represented names is automatically output on R console with the message `The variable names are represented as follows:`.
In the TREC process, the following variables (corresponding to columns of input data) are removed:   

- Either or both the start and end points are missing.  
- The number of missing values is greater than "X", where "X" is a value given by the user.  

For "X", `TREC1` has the third parameter `remove.num` by describing, e.g., `TREC1(exData, "year", 5)` if "X" is 5.
The default of `remove.num` is set as half of row's number of input data. 
If there are removed variables, they are also displayed on R console.
You can obtain variable names of the represented and removed variables by `res1$Vnames` and `res1$remove`, respectively.

Moreover, you have a figure of all trends.
This figure can be reconstructed by `res1$fig.trend`.
The trend fits with prediction band are visible by `res1$fig.ctrend` where each variable is standardized.
If the number of variables is larger than 16, you should enter `res1$fig.ctrend[[1]]`, `res1$fig.ctrend[[2]]`, ... to output the figures.

### the second step

The second step performs rough classification of estimated trends into two or three groups, where the two groups are "Downward" and "Upward", and the three groups are the two groups and "Flat".
A classification into two groups, i.e., "Downward" and "Upward" groups can be executed as follows:

``` r
res2 <- TREC2(res1$argTREC)
```
If you want to classify into three groups, i.e., you also need "Flat" group, you can set `groups=3` as an argument of `TREC2`.
The classification by the above code is a clustering by a dendrogram (this is default).
The `TREC2` has the other option that is a discrimination by the criterion, which is performed by setting of `clustering=FALSE`.
Since the classification by `clustering=FALSE` is not clustering, "not applicable" groups may exist.
Actually for `exData`, "Flat" group is not applicable by the following classification.
```r
TREC2(res1$argTREC, clustering=FALSE, groups=3)
```
Moreover, in default, the classification is performed based on the two fixed (but data-dependent) linear trends.
You can also select the two trends like this:
``` r
TREC2(res1$argTREC, pvar=c("V2", "V7"))
```

The `TREC2` outputs a figure of group-wise trends obtained by the discrimination.
This figure can be reconstructed by `res2$fig.trends`.
Entering `res2$dend()` outputs a dendorogram.
You can also obtain variable names for each group by `res2$trn`.
The `res2$trn` is used in the next step.

### the last step

To more concrete classification, this step is performed.
First, you have to select target trends for each group like this:
```r 
tvar <- list(
  Downward = paste0("V", c(1, 3, 9)),  # selected from res2$trn[[1]]
  Upward = paste0("V", c(2, 5))        # selected from res2$trn[[2]]
)
```
where each element of `tvar` must be selected from the corresponding element of `res2$trn`.
If you classified in three groups, you must add the third element "Flat" in `tvar`.
Notice that not applicable groups are not need to define in `tvar`.
Then, this step is executed as follows:

``` r
res3 <- TREC3(tvar, res2$trn, res1$argTREC)
```

Then, you can obtain the target trends, more concrete groups, and assigned icons as a table.
This table can be reconstructed by `res3$fig.icon`.
Moreover, you can obtain trends of more concrete groups for each rough group by `res3$fig.down`, `res3$fig.up`, and `res3$fig.flat`, respectively.

## References

1. Solvang, H. K. & Ohishi, M. (2023). trec: An R package for trend estimation and classification to support integrated ecosystem assessment of the marine ecosystem and environmental factors. *SoftwareX*, **21**, 101309. doi: [10.1016/j.softx.2023.101309](https://doi.org/10.1016/j.softx.2023.101309)
1. Solvang, H. K. & Planque, B. (2020). Estimation and classification of temporal trends to support integrated ecosystem assessment. *ICES J. Marine Sci.*, **77**, 2529 - 2540. doi: [10.1093/icesjms/fsaa111](https://doi.org/10.1093/icesjms/fsaa111)  
