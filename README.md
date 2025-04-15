# HPRU_RZV

Vaccine impact evaluation for the Shingles vaccination programme in the UK


## To run the model:

The analyses were implemented using *targets. To replicate the full analyses, do run*

```{r}
library(targets)
tar_make()
```

## Input data

-   [Demography](references/data_demography.md)
-   [Epidemiology](references/data_epidemiology.md)
-   [Medical costs](references/data_cost.md)
-   [Health-related quality of life](references/data_qol.md)

## List of pre-modelling

-   **Vaccines:** We utilised Bayesian models implemented in rstan to determine the input parameters for vaccine efficacy/effectiveness and vaccine uptake.

-   **Epidemiological indices:** We applied Gaussian process regression to capture the incidence of HZ and PHN, as well as the hospitalisation rate of HZ.

-   **Health-related quality of life for the shingles patients:** The QALY estimates were sourced from a systematic review with a data synthesis. See [TimeWz667/HPRU_HZQoL](https://github.com/TimeWz667/HPRU_HZQoL) for details.

# License

See [LICENSE](LICENSE)
