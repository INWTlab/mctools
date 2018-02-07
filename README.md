The problem
-----------

Here is the problem we'd like to solve.

-   We would like to have all warnings to be errors so that we can deal with them.
-   We use third party packages which sometimes throw warnings. These warnings do not make us happy, but they should not break in production. We know them and want to accept them.
-   We use `parallel::mclapply` because we have some long running tasks and realize `suppressWarnings` and `options(warn = 2)` do not work as expected in parallel! Also, interactive sessions behave differently than batch mode. All this is very tricky to resolve.

``` r
options(warn = 0)
productionFunction <- function(x) {
  if (x == 1) suppressWarnings(warning("this may be okay"))
  ## just like *real* production code...
  else if (x == 2) stop("We should never end up in this branch...")
  else cbind(1:2, 1:3) # this is not okay and should be an error!
}

res <- parallel::mclapply(1:3, productionFunction, mc.cores = 2)
#> Warning in parallel::mclapply(1:3, productionFunction, mc.cores = 2):
#> scheduled cores 2 encountered errors in user code, all values of the jobs
#> will be affected
str(res)
#> List of 3
#>  $ : chr "this may be okay"
#>  $ :Class 'try-error'  atomic [1:1] Error in FUN(X[[i]], ...) : We should never end up in this branch...
#> 
#>   .. ..- attr(*, "condition")=List of 2
#>   .. .. ..$ message: chr "We should never end up in this branch..."
#>   .. .. ..$ call   : language FUN(X[[i]], ...)
#>   .. .. ..- attr(*, "class")= chr [1:3] "simpleError" "error" "condition"
#>  $ : int [1:3, 1:2] 1 2 1 1 2 3
options(warn = 2)
res1 <- parallel::mclapply(c(1, 3), productionFunction, mc.cores = 2)
str(res1)
#> List of 2
#>  $ : chr "this may be okay"
#>  $ : int [1:3, 1:2] 1 2 1 1 2 3
```

Do you see the problem? Where was that error again? (Please notice, that if you try to reproduce these results in an interactive session, you won't even get a result back from `mclapply`.) Terrible indeed! I hear you. Fear not, there is a solution...

The salvation
-------------

Use `mcMap` as a drop in replacement for `mclapply` if you need more control over errors and warnings. Be aware that `mcMap` is a simple wrapper around `mclapply` and is not a novel parallel computing interface.

``` r
library(mctools)
options(warn = 2)
```

This is how I expected `mclapply` to behave in the first place:

``` r
mcMap(1:3, productionFunction)
#> ERROR [2018-02-07 20:38:49] We should never end up in this branch...
#> ERROR [2018-02-07 20:38:49] Escalated warning: number of rows of result is not a multiple of vector length (arg 1)
#> [[1]]
#> [1] "this may be okay"
#> 
#> [[2]]
#> <simpleError in fun(...): We should never end up in this branch...>
#> 
#> [[3]]
#> <simpleError in cbind(1:2, 1:3): Escalated warning: number of rows of result is not a multiple of vector length (arg 1)>
```

White-list specific warnings if that is what you want:

``` r
mcMap(1:3, productionFunction, warningsWhitelist = "multiple")
#> ERROR [2018-02-07 20:38:49] We should never end up in this branch...
#> WARN [2018-02-07 20:38:49] number of rows of result is not a multiple of vector length (arg 1)
#> [[1]]
#> [1] "this may be okay"
#> 
#> [[2]]
#> <simpleError in fun(...): We should never end up in this branch...>
#> 
#> [[3]]
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    2    2
#> [3,]    1    3
```

Or fail when any of the processes encounter warnings:

``` r
mcMap(1:3, productionFunction, finallyStop = TRUE)
#> ERROR [2018-02-07 20:38:49] We should never end up in this branch...
#> ERROR [2018-02-07 20:38:49] Escalated warning: number of rows of result is not a multiple of vector length (arg 1)
#> Error in handleErrors(res, finallyStop): #overall/#errors: 3/2
```
