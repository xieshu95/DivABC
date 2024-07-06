DivABC: Diversification rates estimation using approximate Bayesian computation 
(ABC) 

`DivABC` is an `R` package which aim to provide an ABC tool to estimate diversification rates in multiple diversification models: 
    1) DAISIE (Dynamic Assembly of Island biota through Speciation, Immigration and Extinction)
    2) SecSSE (Several Examined and Concealed States-Dependent Speciation and Extinction)
    3) TraiSIE (trait-dependent DAISIE).

This package can achieve the simulation, maximum likelihood estimation (MLE) and markov chain Monte Carlo (MCMC) and approximate Bayesian computation (ABC) inference for both DAISIE and SecSSE model, and ABC inference for the TraiSIE model.



# Installation

DivABC can be installed from GitHub by running the following code:
``` r
install.packages("remotes")
remotes::install_github("xieshu95/TraisieABC")
```
