#
# BF10_tTest_Informed.R
# Simple function to calculate a Bayes Factor for a t-Test with a
# specified, informed Normal distribution as alternative.
# Adapted from JASP-Engine (http://jasp-stats.org)
# 
# Copyright (C) 2018  Christopher Harms, University of Bonn
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
# 
library(hypergeo)

BayesFactor_InformedNormal <- function(t, ny, nx,
                                       prior.mean, prior.variance) {
  # This function is adapted from the code used in the JASP statistical package
  # Original code is available at the JASP GitHub repository
  # (https://goo.gl/QyjQB3).
  # Code is based on the formula given here: https://arxiv.org/abs/1704.02479
  # 
  # Changes made by: Christopher Harms <christopher.harms@uni-bonn.de>
  # Changes made on: 21.03.2018
  # Changes: simplified formula to simply calculate the Bayes Factor for the
  #   example case only, thus changed function and variables names and removed
  #   code for one-sided and paired samples tests.
  
  .A <- function(t, n, nu, mu.delta, g) {
    
    Re(hypergeo::genhypergeo(U = (nu + 1)/2, L = 1/2,
                             z = mu.delta^2*t^2/
                               (2*(1/n + g)*((1 + n*g)*nu + t^2))))
    
  }
  
  .B <- function(t, n, nu, mu.delta, g) {
    
    out <- mu.delta*t/sqrt(1/2*(1/n + g)*((1 + n*g)*nu + t^2)) *
      exp(lgamma((nu + 2)/2) - lgamma((nu + 1)/2)) *
      Re(hypergeo::genhypergeo(U = (nu + 2)/2, L = 3/2,
                               z = mu.delta^2*t^2/
                                 (2*(1/n + g)*((1 + n*g)*nu + t^2))))
    
    return(out)
    
  }

  .term_normalprior <- function(t, n, nu, mu.delta, g) {
    
    (1 + n*g)^(-1/2) * exp(-mu.delta^2/(2*(1/n + g))) *
      (1 + t^2/(nu*(1 + n*g)))^(-(nu + 1)/2) *
      (.A(t, n, nu, mu.delta, g) + .B(t, n, nu, mu.delta, g))
    
  }
    
  neff <- ny*nx/(ny + nx)
  nu <- ny + nx - 2
  
  mu.delta <- prior.mean
  g <- prior.variance
  numerator <- .term_normalprior(t = t, n = neff, nu  = nu,
                                 mu.delta = mu.delta, g = g)
  denominator <- (1 + t^2/nu)^(-(nu + 1)/2)
  
  BF10 <- numerator/denominator
  
  return(BF10)
}
