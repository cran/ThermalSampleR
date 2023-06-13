library(ThermalSampleR)
library(testthat)
coreid = ThermalSampleR::coreid_data

bt_two = boot_two(data=coreid, groups_col=col,
         group1="Catorhintha schaffneri_APM",
         group2="Catorhintha schaffneri_NPM",
         n_max=49, iter=15, response=response)

#############################
# test bt_two plotting
#############################

plot_bt_two = plot_two_groups(
  # Variable containing the output from running `boot_two` function
  x = bt_two,
  # Minimum sample size to plot
  n_min = 3,
  # Actual size of your existing dataset
  n_max = 30,
  # Colour for your experimental data
  colour_exp = "blue",
  # Colour for the extrapolated predictions
  colour_extrap = "red",
  # Position of the legend
  legend.position = "right",
  # Change the degree of shading on the graph
  alpha_val = 0.25)

########################################
# Test for the class of plot_one_group
########################################
testthat::test_that("plot_two_groups output is the ggplot class", {

  testthat::expect_true(inherits(plot_bt_two, "ggplot"))
})
