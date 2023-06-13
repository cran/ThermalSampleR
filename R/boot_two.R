##########################################################################
##########################################################################
##########################################################################
# Function - boot_two_groups
#          - Write function to perform bootstrap resampling for two
#            groups comparisons and calculate differences between
#            groups and CI's across a range of sample sizes
##########################################################################
##########################################################################
##########################################################################

# Define bootstrap sampling function ---------------------------------

#' Bootstrap sampling for difference in means between two groups
#'
#'
#' @name boot_two
#' @description Calculate difference in mean CT limits between two groups.
#' @param data Data frame contain raw data.
#' @param groups_col Factor. Column containing names of two populations to compare
#' @param n_max Numeric. Maximum sample size to extrapolate simulations.
#' @param n_min Numeric. Minimum sample size to extrapolate simulations. Defaults to 3.
#' @param iter Numeric. Number of bootstrap samples. Defaults to 29.
#' @param response Numeric. Column containing thermal limit data for individual samples.
#' @param group1 String. Name of first population to compare.
#' @param group2 String. Name of second population to compare.
#' @param colour_exp Colour of the experimental data. Defaults to "blue".
#' @param colour_extrap Colour of the extrapolated data. Defaults to "red".
#' @param legend.position Position of the legend. Defaults to "top". Can be "bottom", "left", "right", or "none".
#' @param ggtheme The theme for the ggplot created. See ggplot2 themes for options. Default set to theme_classic().
#'
#' @return A data frame of bootstrap resamples
#' @importFrom magrittr %>%
#' @examples
#' \donttest{
#' head(coreid_data)
#' sims <- boot_two(data = coreid_data,
#'                         groups_col = col,
#'                         response = response,
#'                         group1 = "Catorhintha schaffneri_APM",
#'                         group2 = "Catorhintha schaffneri_NPM",
#'                         n_max = 49,
#'                         iter = 99)
#' }
#' @export


utils::globalVariables(c("sample_size", 'sample_data', 'calc', 'mean_val','median_pop_val', 'sd_val', 'std_error', 'error', 'ci_falls',
                         'lower_ci', 'upper_ci','mean_upp_ci','mean_low_ci','width_ci','sd_width'))

# Define function
boot_two <- function(data,
                            groups_col,
                            n_max,
                            n_min = 3,
                            iter = 29,
                            response,
                            group1,
                            group2){

  # Filter which species to plot
  df <- dplyr::filter({{ data }},
                      {{ groups_col }} %in% c( {{group1}}, {{ group2 }} ))

  # Perform boostrap sampling
  boot_data <- df %>%
    dplyr::group_by( {{ groups_col }} ) %>%
    tidyr::nest() %>%
    tidyr::crossing(sample_size = c({{ n_min }} : {{ n_max }}),
                    iter = seq(1: {{ iter }} )) %>%
    # Added sampling with replacement to code below
    dplyr::mutate(sample_data = purrr::map2(data,
                                            sample_size,
                                            ~ dplyr::sample_n(.x, .y,
                                                              # Sample with replacement
                                                              replace = TRUE))) %>%
    dplyr::mutate(calc = purrr::map(sample_data,
                                    ~ dplyr::reframe(.,
                                                       mean_val = mean( {{ response }} ),
                                                       sd_val = stats::sd(( {{ response }} ))))) %>%
    dplyr::select({{ groups_col }}, sample_size, iter, calc) %>%
    tidyr::unnest(cols = calc)

  # Add CI's to raw bootstrap samples
  boot_data <- boot_data %>%
    # Add standard errors
    dplyr::mutate(std_error = sd_val/sqrt(sample_size)) %>%
    # Now find error
    dplyr::mutate(error = stats::qt(0.975, df = sample_size - 1) * std_error) %>%
    # Calculate lower and upper 95% CI limits
    dplyr::mutate(lower_ci  = mean_val - error,
                  upper_ci  = mean_val + error) %>%
    dplyr::ungroup()

  # Split data from two groups
  sp1_data <- boot_data %>%
    dplyr::filter({{ groups_col }} == {{ group1 }})
  sp2_data <- boot_data %>%
    dplyr::filter({{ groups_col }} == {{ group2 }})

  # Convert data into wide format
  comb_data <- dplyr::left_join(sp1_data,
                                sp2_data,
                                by = c("sample_size", "iter"))

 # print(colnames(comb_data))

  # Let's keep only the columns we need, as things are about to get real
  # comb_data <- comb_data %>%
  #   dplyr::select(sample_size, #2
  #                 iter, # 3
  #                 groups_col.x, #1
  #                 mean_val.x, #4
  #                 sd_val.x,#5
  #                 groups_col.y,#10
  #                 mean_val.y,#11
  #                 sd_val.y)#12
  #
  comb_data <- comb_data %>%
    dplyr::select(1,2,3,4,5,10,11,12)

  # Add student t CI's
  comb_data <- comb_data %>%
    dplyr::mutate(mean_diff    = mean_val.x - mean_val.y,
                  first_term   = (sample_size - 1) * sd_val.x^2,
                  second_term  = (sample_size - 1) * sd_val.y^2,
                  pooled_df    = (sample_size * 2) - 2,
                  s_pooled     = sqrt((first_term + second_term) / pooled_df),
                  se_diff      = s_pooled * sqrt((1 / sample_size) + (1 / sample_size)),
                  error        = stats::qt(0.975, df = pooled_df) * se_diff,
                  lower_ci     = mean_diff - error,
                  upper_ci     = mean_diff + error)

  # Calculate summary statistics
  comb_data_sum <- comb_data %>%
    dplyr::group_by(sample_size) %>%
    dplyr::reframe(mean_low_ci = mean(lower_ci),
                     mean_upp_ci    = mean(upper_ci),
                     mean_diff      = mean(mean_diff),
                     width_ci       = mean_upp_ci - mean_low_ci,
                     sd_width       = stats::sd(upper_ci - lower_ci),
                     sd_width_lower = width_ci - sd_width,
                     sd_width_upper = width_ci + sd_width,
                     iter=iter,
                     lower_ci=lower_ci,
                     upper_ci=upper_ci)

  return(comb_data_sum)

}

##########################################################################
##########################################################################
##########################################################################
