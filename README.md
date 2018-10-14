---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introduction

This is `nprotreg`, an `R` package that exploits
nonparametric rotations in the analysis
of Sphere-Sphere regression models.

The package implements methods proposed by 
[Di Marzio, Panzera & Taylor (2018)](https://doi.org/10.1080/01621459.2017.1421542).

Thanks to package `nprotreg`,
regressing data represented as points on a hypersphere you can
* simulate a very flexible regression model 
  where, for each location of the manifold,
  a specific rotation matrix is applied to
  obtain a spherical response;
* fit Sphere-Sphere regression models by
  allowing for approximations of rotation 
  matrices based on a series expansion; 
* reduce estimation bias applying iterative 
  estimation procedures within a Newton-Raphson 
  learning scheme;
* use cross-validation to select smoothing parameters.


# Getting Started

The following script shows how to fit a Sphere-Sphere
regression model using simulated data via package `nprotreg`.


```r
library(nprotreg)

# Define a matrix of explanatory points.

number_of_explanatory_points <- 50

explanatory_points <- get_equally_spaced_points(
  number_of_explanatory_points)

### Define a matrix of response points by simulation.

# define the reponse _local_ rotation model (eg Model 2 in Table 1 of [Di Marzio, Panzera & Taylor (2018)])

local_rotation_composer <- function(point) {
  independent_components <- (1 / 2) *
    c(exp(2.0 * point[3]), - exp(2.0 * point[2]), exp(2.0 * point[1]))
}

# define rotation (error) perturbation model using random skew symmetric matrix:

local_error_sampler <- function(point) {
  rnorm(3,mean=0,sd=.25)
}

response_points <- simulate_regression(explanatory_points,
                                       local_rotation_composer,
                                       local_error_sampler)

# Define a matrix of evaluation points for prediction.

evaluation_points <- rbind(
  cbind(.5, 0, .8660254),
  cbind(-.5, 0, .8660254),
  cbind(1, 0, 0),
  cbind(0, 1, 0),
  cbind(-1, 0, 0),
  cbind(0, -1, 0),
  cbind(.5, 0, -.8660254),
  cbind(-.5, 0, -.8660254)
)

# Use a default weights generator.

weights_generator <- weight_explanatory_points

# Set the concentration parameter (kappa).

concentration <- 5

# Fit regression.

fit_info <- fit_regression(
  evaluation_points,
  explanatory_points,
  response_points,
  concentration,
  weights_generator,
  number_of_expansion_terms = 1,
  number_of_iterations = 2
)
```

See the documentation for addressing additional scenarios.

# Installation
To download and install the package from the
CRAN repository, execute the
following command:
```
install.packages("nprotreg")
```
