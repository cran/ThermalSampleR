library(ThermalSampleR)
library(testthat)
coreid = ThermalSampleR::coreid_data

bt_one = boot_one(data=coreid, groups_col=col,
                  groups_which="Catorhintha schaffneri_APM",
                  n_max=49, iter=15, response=response)

#############################
# test bt_one plotting
#############################

plot_bt_one = plot_one_group(
  # Variable containing the output from running `boot_one` function
  x = bt_one,
  # Minimum sample size to plot
  n_min = 3,
  # Actual size of your existing dataset
  n_max = 15,
  # Colour for your experimental data
  colour_exp = "forestgreen",
  # Colour for the extrapolated predictions
  colour_extrap = "orange",
  # Position of the legend
  legend.position = "right",
  # Change the degree of shading on the graph
  alpha_val = 0.25)

########################################
# Test for the class of plot_one_group
########################################
testthat::test_that("plot_one_group output is the ggplot class", {

  testthat::expect_true(inherits(plot_bt_one, "ggplot"))
})

##############################################
# Test for the correctness of user input file
##############################################

testthat::test_that("Error: incorrect input data", {

  # Call the function and check for errors
  testthat::expect_error(plot_one_group(
    # Variable containing the output from running `boot_one` function
    x = bt_oneee, # incorrect name here -> should throw an error
    # Minimum sample size to plot
    n_min = 3,
    # Actual size of your existing dataset
    n_max = 15,
    # Colour for your experimental data
    colour_exp = "forestgreen",
    # Colour for the extrapolated predictions
    colour_extrap = "orange",
    # Position of the legend
    legend.position = "right",
    # Change the degree of shading on the graph
    alpha_val = 0.25))
})
