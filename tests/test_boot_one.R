library(ThermalSampleR)
library(testthat)
# run test_file("test_ThermalSampleR.R") to test everything in this R file

coreid = ThermalSampleR::coreid_data

############################################################################################
# BOOT_ONE and PLOT_ONE_GROUP TESTS
############################################################################################

########################################
# Test for general errors: boot_one
########################################

bt_one = boot_one(data=coreid, groups_col=col,
                  groups_which="Catorhintha schaffneri_APM",
                  n_max=49, iter=15, response=response)

testthat::test_that("No error is thrown in boot_one function", {

  # Call the function and check for errors
  testthat::expect_no_error(boot_one(data=coreid, groups_col=col,
                        groups_which="Catorhintha schaffneri_APM",
                        n_max=49, iter=15, response=response))
})

##################################################
# Test: user puts in the incorrect groups_col
##################################################

testthat::test_that("Error is thrown in boot_one function", {

  # Call the function and check for errors
  testthat::expect_error(boot_one(data=coreid, groups_col="Catorhintha schaffneri_APM",
                           groups_which="Catorhintha schaffneri_APM",
                           n_max=49, iter=15, response=response))
})

########################################
# check that the col column consists of names
########################################

testthat::test_that("Incorrect columns in data", {

  testthat::expect_type(coreid$col, "character")
})

########################################
# check that the response column contains integers
########################################

testthat::test_that("Incorrect columns in data", {

  testthat::expect_type(coreid$response, "integer")
})

########################################
# Test for the class of bt_one output
########################################
testthat::test_that("boot_one output is the list class", {

  testthat::expect_type(bt_one, "list")
  testthat::expect_type(bt_one$mean_low_ci[1], "double")
  testthat::expect_equal(ncol(bt_one), 14)
})

########################################
# Test for NA values in bt_one
########################################

bt_one_buggy = bt_one
bt_one_buggy$mean_low_ci[1] = NA

testthat::test_that("No NA values in the dataframe", {
  testthat::expect_false(any(is.na(bt_one)), info = "The dataframe contains NA values.")
})

testthat::test_that("NA values present in the dataframe", {
  testthat::expect_true(any(is.na(bt_one_buggy)), info = "The dataframe does not contain NA values.")
})
