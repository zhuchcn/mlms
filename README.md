
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mlms

<!-- badges: start -->

<!-- badges: end -->

The mlms(mixed linear model selector) is an R package to build linear
models with adjustment of covariants. Each potential covariate in the
`Z` data frame is frist tested whether it iscorrelated with any variable
in X. For any variable in `X`, only the covariants in `Z` that has a
p-value \< 0.1 is used to adjust the variance in the complete model.
Next, the covariants are tested whether they are significantin the
complete model. Finally, the complete model is fit to test the effect of
the target coefficient.

## Installation

``` r
devtools::install_github("zhuchcn/mlms")
```

## Data

An example dataset `growth` is included in the package. The dataset has
80 observations of infant growth at 18 month from a nutritional
supplementation study.

``` r
library(mlms)
data(growth)
```

## Example

Create the data frame `X` for outcome variables

``` r
X = growth[,1:3]
head(X)
#>     waz   laz   hcz
#> 1 -3.00 -1.45 -2.35
#> 2 -0.88 -0.45 -1.65
#> 3 -0.34 -1.20 -0.21
#> 4 -0.86 -0.38 -1.13
#> 5 -1.82 -1.87 -2.19
#> 6 -0.40 -0.99 -0.42
```

Create the design matrix

``` r
design = model.matrix(~ treatment, data = growth)
head(design)
#>   (Intercept) treatmentLNS
#> 1           1            0
#> 2           1            0
#> 3           1            0
#> 4           1            1
#> 5           1            1
#> 6           1            0
```

Create the data frame `Z` for potential covariants

``` r
Z = growth[, 5:10]
```

Run model selection

``` r
fit_mlms(X, design, Z, coef = "treatmentLNS")
#> Categorical variables are converted to dummy variables
#> $bivars
#>          momht       mbhb    asset1    gaatdel totschyrs sex_updated
#> waz 0.16195149 0.73601858 0.3755778 0.59073138 0.8681225   0.3665692
#> laz 0.00192442 0.70791953 0.2433709 0.04841732 0.7863344   0.1273705
#> hcz 0.43447850 0.09449692 0.3143394 0.75104026 0.2396203   0.3651060
#> 
#> $covars
#>           momht       mbhb asset1   gaatdel totschyrs sex_updated
#> waz          NA         NA     NA        NA        NA          NA
#> laz 0.008589524         NA     NA 0.2328213        NA          NA
#> hcz          NA 0.09057698     NA        NA        NA          NA
#> 
#> $unadjusted.model
#>       Estimate Std. Error   t value  Pr(>|t|)
#> waz 0.24375000  0.2353540 1.0356738 0.3035546
#> laz 0.34000000  0.2270770 1.4972894 0.1383539
#> hcz 0.07858974  0.2159208 0.3639749 0.7168743
#> 
#> $adjusted.model
#>      Estimate Std. Error   t value  Pr(>|t|)
#> waz 0.2437500  0.2353540 1.0356738 0.3035546
#> laz 0.1700555  0.2310253 0.7360904 0.4640671
#> hcz 0.1039090  0.2137643 0.4860916 0.6283009
#> 
#> $df.residual
#>     unadjusted adjusted
#> waz         78       78
#> laz         78       72
#> hcz         77       76
#> 
#> $confint
#>     unadjusted CI 2.5 % unadjusted CI 97.5 % adjusted CI 2.5 %
#> waz          -0.2248039            0.7123039        -0.2248039
#> laz          -0.1120756            0.7920756        -0.2904851
#> hcz          -0.3513634            0.5085429        -0.3218394
#>     adjusted CI 97.5 %
#> waz          0.7123039
#> laz          0.6305961
#> hcz          0.5296574
#> 
#> attr(,"class")
#> [1] "mlms"
```
