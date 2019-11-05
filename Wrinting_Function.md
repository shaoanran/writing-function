Writing\_Function
================

## get started

We’re going to write some functions.

``` r
x = rnorm(n = 30, mean = 4, sd = 2.3)
x_again = rnorm(n = 20, mean = 6, sd = 0.3)
y = rnorm(n = 30, mean = 24, sd = 2.3)

(x - mean(x)) / sd(x)
```

    ##  [1] -0.61280713 -1.13227282 -0.72287356  1.53460214 -0.70016380
    ##  [6]  0.65187090 -0.06295384  0.40816906  1.00028460  0.27877359
    ## [11]  1.25626864  0.30701681 -0.10480881 -0.21138599 -0.90769984
    ## [16] -0.57471656 -0.88513734  0.07176795  3.06063250  0.77558813
    ## [21]  0.58226855  0.82044897  0.41739169 -0.21876997 -0.46217506
    ## [26] -2.37867801 -0.59756228 -0.73161522 -0.04895151 -0.81251179

``` r
(x_again - mean(x_again)) / sd(x_again)
```

    ##  [1]  0.01161475  1.30578899  0.75499582  0.65799027 -0.63583711
    ##  [6] -1.90849762 -1.32029808  0.54683665 -0.97196416 -0.75543891
    ## [11] -0.16250688 -0.19863135 -0.38246049 -0.61754720 -0.92373097
    ## [16]  1.32926192  1.85986408  1.35627640 -0.26761699  0.32190086

Now a function.

``` r
z_score = function(x_arg) {
## condition execution
  if (!is.numeric(x_arg)){
    stop( "x should be numeric")
    } else if (length(x_arg) < 3){
    stop("x should be longer than 3")
    }
  
  (x - mean(x_arg)) /  sd(x_arg)
  
}
```

Try out the
    function.

``` r
z_score(x_arg = y)
```

    ##  [1]  -9.150922  -9.562452  -9.238119  -7.449708  -9.220128  -8.149023
    ##  [7]  -8.715319  -8.342087  -7.873003  -8.444597  -7.670208  -8.422222
    ## [13]  -8.748477  -8.832910  -9.384542  -9.120746  -9.366667  -8.608590
    ## [19]  -6.240760  -8.051012  -8.204163  -8.015472  -8.334781  -8.838759
    ## [25]  -9.031589 -10.549876  -9.138845  -9.245044  -8.704226  -9.309132

``` r
z_score(x = 3)
```

    ## Error in z_score(x = 3): x should be longer than 3

``` r
z_score(x = c(TRUE,TRUE,FALSE,TRUE))
```

    ## Error in z_score(x = c(TRUE, TRUE, FALSE, TRUE)): x should be numeric

``` r
## convert them to numeric
z_score(x = "my name is jeff")
```

    ## Error in z_score(x = "my name is jeff"): x should be numeric

``` r
z_score(x = iris)  ## iris is a dataframe 
```

    ## Error in z_score(x = iris): x should be numeric

## multiple outputs

``` r
mean_and_sd = function(input_x){
  if (!is.numeric(input_x)){
    stop( "x should be numeric")
    } else if (length(input_x) < 3){
    stop("x should be longer than 3")
    }
  ## into a data frame with tibble
    list(
    mean_input = mean(input_x),
    sd_input = sd(input_x),
    z_score = (input_x - mean(input_x))/sd(input_x)
    ) 
}
```

test this function

``` r
mean_and_sd(input_x = y)
```

    ## $mean_input
    ## [1] 24.18511
    ## 
    ## $sd_input
    ## [1] 2.39043
    ## 
    ## $z_score
    ##  [1] -0.33823216  0.07887925 -0.36672838 -0.51465733  1.97865708
    ##  [6] -1.08152078  0.57104300 -1.85935641 -0.43107620 -0.04000891
    ## [11] -0.26275307  1.98904142 -0.91667336  0.36445009 -0.72457617
    ## [16] -0.56901596 -0.34846204 -0.30409195 -0.59449995 -1.91457162
    ## [21]  0.93420959  1.01108122  1.64300749 -0.71937916  1.31308653
    ## [26] -0.54381793 -0.20907895  0.06415222  0.36584493  1.42504750

## Multiple inputs
