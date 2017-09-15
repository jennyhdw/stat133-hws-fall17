up02-Jenny-Huang
================
Jenny Huang
9/9/2017

``` r
load("nba2017-salary-points.RData")
ls()
```

    ## [1] "experience" "player"     "points"     "points1"    "points2"   
    ## [6] "points3"    "position"   "salary"     "team"

``` r
# quantitative variable
summary(salary)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##     5145  1286160  3500000  6187014  9250000 30963450

``` r
summary(experience)
```

    ##    Length     Class      Mode 
    ##       441 character character

``` r
summary(points)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     0.0   156.0   432.0   546.6   780.0  2558.0

``` r
summary(points1)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.00   21.00   58.00   92.47  120.00  746.00

``` r
summary(points2)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     0.0    39.0   111.0   152.5   213.0   730.0

``` r
summary(points3)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.00    3.00   32.00   49.71   78.00  324.00

``` r
mean(salary)
```

    ## [1] 6187014

``` r
sd(salary)
```

    ## [1] 6571890

``` r
max(salary)
```

    ## [1] 30963450

``` r
min(salary)
```

    ## [1] 5145

``` r
median(salary)
```

    ## [1] 3500000

``` r
quantile(salary)
```

    ##       0%      25%      50%      75%     100% 
    ##     5145  1286160  3500000  9250000 30963450

``` r
hist(salary)
```

![](up02-Jenny-Huang_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

``` r
boxplot(salary)
```

![](up02-Jenny-Huang_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-2.png)

``` r
plot(density(salary))
```

![](up02-Jenny-Huang_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-3.png)

``` r
is.factor(position)
```

    ## [1] FALSE

``` r
position_fac <- factor(position)
table(position_fac)
```

    ## position_fac
    ##  C PF PG SF SG 
    ## 89 89 85 83 95

``` r
prop.table(table(position_fac))
```

    ## position_fac
    ##         C        PF        PG        SF        SG 
    ## 0.2018141 0.2018141 0.1927438 0.1882086 0.2154195

There is a pretty even distribution across all the positions.

``` r
barplot(prop.table(table(position_fac)))
```

![](up02-Jenny-Huang_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

Reflections
-----------

I feel like this assignment is easier than the last one in the sense that it is much more intuitive/mathematical. I think the hard part is the difference between vector and factor, and really understand why we are using them.
