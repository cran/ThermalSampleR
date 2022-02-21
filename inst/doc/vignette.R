## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github("clarkevansteenderen/ThermalSampleR")

## ----eval=FALSE---------------------------------------------------------------
#  library(ThermalSampleR)

## ----eval=FALSE---------------------------------------------------------------
#  shiny::runUrl("https://github.com/clarkevansteenderen/ThermalSampleR_Shiny/archive/main.tar.gz")

## -----------------------------------------------------------------------------
head(ThermalSampleR::coreid_data)

## ----eval=FALSE---------------------------------------------------------------
#  # Set a seed to make the results reproducible, for illustrative purposes.
#  set.seed(2012)
#  
#  # Perform simulations
#  bt_one = boot_one(
#    # Which dataframe does the data come from?
#    data = coreid_data,
#    # Provide the column name containing the taxon ID
#    groups_col = col,
#    # Provide the name of the taxon to be tested
#    groups_which = "Catorhintha schaffneri_APM",
#    # Provide the name of the column containing the response variable (e.g CTmin data)
#    response = response,
#    # Maximum sample sample to extrapolate to
#    n_max = 49,
#    # How many bootstrap resamples should be drawn?
#    iter = 299)
#  dplyr::glimpse(bt_one)

## ----eval=FALSE---------------------------------------------------------------
#  plot_one_group(
#    # Variable containing the output from running `boot_one` function
#    x = bt_one,
#    # Minimum sample size to plot
#    n_min = 3,
#    # Actual size of your existing dataset
#    n_max = 15,
#    # Colour for your experimental data
#    colour_exp = "forestgreen",
#    # Colour for the extrapolated predictions
#    colour_extrap = "orange",
#    # Position of the legend
#    legend.position = "right",
#    # Change the degree of shading on the graph
#    alpha_val = 0.25)

## ----eval=FALSE---------------------------------------------------------------
#  # Set a seed to make the results reproducible, for illustrative purposes.
#  set.seed(2012)
#  
#  # Perform simulations
#  bt_two <- boot_two(
#    # Which dataframe does the data come from?
#    data = coreid_data,
#    # Provide the column name containing the taxon ID
#    groups_col = col,
#    # Provide the name of the column containing the response variable (e.g CTmin data)
#    response = response,
#    # Provide the name of the first taxon to be compared
#    group1 = "Catorhintha schaffneri_APM",
#    # Provide the name of the second taxon to be compared
#    group2 = "Catorhintha schaffneri_NPM",
#    # Maximum sample sample to extrapolate to
#    n_max = 49,
#    # How many bootstrap resamples should be drawn?
#    iter = 299)
#  dplyr::glimpse(bt_two)

## ----eval=FALSE---------------------------------------------------------------
#  plot_two_groups(
#    # Variable containing the output from running `boot_two` function
#    x = bt_two,
#    # Minimum sample size to plot
#    n_min = 3,
#    # Actual size of your existing dataset
#    n_max = 30,
#    # Colour for your experimental data
#    colour_exp = "blue",
#    # Colour for the extrapolated predictions
#    colour_extrap = "red",
#    # Position of the legend
#    legend.position = "right",
#    # Change the degree of shading on the graph
#    alpha_val = 0.25)

## ----eval=FALSE---------------------------------------------------------------
#  tte = equiv_tost(
#      # Which dataframe does the data come from?
#      data = coreid_data,
#      # Provide the column name containing the taxon ID
#      groups_col = col,
#      # Provide the name of the taxon to be tested
#      groups_which = "Catorhintha schaffneri_APM",
#      # Provide the name of the column containing the response variable (e.g CTmin data)
#      response = response,
#      # Define the skewness parameters
#      skews = c(1,10),
#      # Define the equivalence of subsets to full population CT estimate (unit = degree Celcius)
#      equiv_margin = 1,
#      # Size of the population to sample (will test subsamples of size pop_n - x against pop_n for equivalence). Defaults to population size = 30
#      pop_n = 30
#  )
#  
#  # Inspect ouput
#  tte

