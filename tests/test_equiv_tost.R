# library(ThermalSampleR)
# library(testthat)
# coreid = ThermalSampleR::coreid_data
#
# ############################################################################################
# # EQUIV_TOST TESTS
# ############################################################################################
#
# tte = equiv_tost(
#   # Which dataframe does the data come from?
#   data = coreid,
#   # Provide the column name containing the taxon ID
#   groups_col = col,
#   # Provide the name of the taxon to be tested
#   groups_which = "Catorhintha schaffneri_APM",
#   # Provide the name of the column containing the response variable (e.g CTmin data)
#   response = response,
#   # Define the skewness parameters
#   skews = c(1,10),
#   # Define the equivalence of subsets to full population CT estimate (unit = degree Celcius)
#   equiv_margin = 1,
#   # Size of the population to sample (will test subsamples of size pop_n - x against pop_n for equivalence). Defaults to population size = 30
#   pop_n = 5
# )
#
# ########################################
# # Test for the class of tte
# ########################################
# testthat::test_that("tte output is the ggplot class", {
#
#   testthat::expect_true(inherits(tte, "ggplot"))
# })
#
#
# testthat::test_that("No error is thrown in equiv_tost function", {
#
#   # Call the function and check for errors
#   testthat::expect_no_error(equiv_tost(
#     # Which dataframe does the data come from?
#     data = coreid,
#     # Provide the column name containing the taxon ID
#     groups_col = col,
#     # Provide the name of the taxon to be tested
#     groups_which = "Catorhintha schaffneri_APM",
#     # Provide the name of the column containing the response variable (e.g CTmin data)
#     response = response,
#     # Define the skewness parameters
#     skews = c(1,10),
#     # Define the equivalence of subsets to full population CT estimate (unit = degree Celcius)
#     equiv_margin = 1,
#     # Size of the population to sample (will test subsamples of size pop_n - x against pop_n for equivalence). Defaults to population size = 30
#     pop_n = 5
#   ))
# })
