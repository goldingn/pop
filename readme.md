pop
===

### A Flexible Syntax for Population Dynamic Modelling

[![Build Status](https://travis-ci.org/goldingn/pop.svg)](https://travis-ci.org/goldingn/pop) [![codecov.io](https://codecov.io/github/goldingn/pop/coverage.svg?branch=master)](https://codecov.io/github/goldingn/pop?branch=master) [![cran version](http://www.r-pkg.org/badges/version/pop)](https://cran.rstudio.com/web/packages/pop)

Population dynamic models underpin a range of analyses and applications in ecology and epidemiology. The various approaches for fitting and analysing these models (MPMs, IPMs, ODEs, POMPs, PVA, with and without metapopulation structure) are generally fitted using different software, each with a different interface. This makes it difficult to combine various modelling approaches and data types to solve a given problem. pop aims to provide a flexible and easy to use common interface for constructing population dynamic models and enabling to them to be fitted and analysed in various ways.

This is a work in progress - so far you can only create simple dynamical models and not do very much with them. More will follow.

#### Installation

`pop` is not yet on CRAN, so the easiest way to install it is directly from GitHub using the `devtools` package (Windows users may need to download RTools first)

``` r
devtools::install_github('goldingn/pop')
library(pop)
```

#### Creating a simple population dynamic model

`dynamic` objects (created with the function `dynamic()`) are the core of `pop` since they encode dynamical systems, can be updated by fitting to data, and can be analysed in a variety of ways. `dynamic` objects are composed of a series of `transitions` encoding the links between different states in the dynamic. That's all a bit generic, so here's an example for a simple ecological population with three life stages (states).

First we define the different types of transitions between the states:

``` r
# probability of staying in the same step between timesteps
stasis_egg <- tr(eggs ~ eggs, p(0.4))
stasis_larva <- tr(larvae ~ larvae, p(0.3))
stasis_adult <- tr(adults ~ adults, p(0.8))

# probability of moving to the next state
hatching <- tr(larvae ~ eggs, p(0.5))
pupation <- tr(adults ~ larvae, p(0.2))

# number of eggs laid
fecundity <- tr(eggs ~ adults, r(20))
```

Next we can combine these to create dynamics (systems of transitions). We'll make these three different dynamics to start with:

``` r
stasis <- dynamic(stasis_egg,
                  stasis_larva,
                  stasis_adult)

growth <- dynamic(hatching,
                  pupation)

reproduction <- dynamic(fecundity)
```

We can plot each of these to see what they look like schematically:

``` r
par(mfrow = c(1, 3))
plot(stasis); title(main = 'stasis')
plot(growth); title(main = 'growth')
plot(reproduction); title(main = 'reproduction')
```

![](readme_files/figure-markdown_github/plot_dynamics-1.png)<!-- -->

These components aren't particularly useful on their own though, so we should combine them into one overall population dynamic:

``` r
all <- stasis + growth + reproduction
plot(all)
```

![](readme_files/figure-markdown_github/all_dynamics-1.png)<!-- -->

#### Doing things with models

We can convert any of these objects into transition matrices, and analyse them deterministically using functions from other matrix population model packages, like `popbio` and `popdemo`:

``` r
A <- as.matrix(all)

# estimate the intrinsic growth rate & stable stage distribution
popbio::lambda(A)
```

    ## [1] 1.105935

``` r
(ss <- popbio::stable.stage(A))
```

    ##       eggs     larvae     adults 
    ## 0.78353880 0.18096945 0.03549176

``` r
# plot predicted deterministic trajectory
plot(popdemo::project(A, ss * 1000, time = 50))
```

![](readme_files/figure-markdown_github/popdemo-1.png)<!-- -->

We can also use the function `simulation` to carry out discrete-time stochastic simulations from dynamic objects:

``` r
# define the starting population as a named integer vector
population <- round(ss * 1000)

# simulate 30 times for 50 generations each
sim <- simulation(dynamic = all,
           population = population,
           timesteps = 50,
           replicates = 30)

# plot abundance of the three life stages
par(mfrow = c(3, 1))
plot(sim)
```

![](readme_files/figure-markdown_github/simulation-1.png)<!-- -->
