##########################################################################
##########################################################################
##########################################################################
# Function - boot_sample
#          - Write function to perform bootstrap resampling for one
#            population and calculate summary statistics for CTL values
#            across a range of sample sizes
##########################################################################
##########################################################################
##########################################################################

# Define bootstrap sampling function ---------------------------------

#' Bootstrap sampling to calculate summary statistics of CTL values
#'
#'
#'
#' @name boot_one
#' @description Calculate mean and CI's of CTL for a single population
#' @param data Data frame contains raw data. Must contain a column with a population
#'             identifier (e.g. population ID), and a column containing critical
#'             thermal limit data (e.g. temperatures at which critical limits are reached).
#' @param groups_col Factor. Column containing name(s) of population(s) of interest
#' @param groups_which Character. Which population should be analysed?
#' @param n_max Numeric. Maximum sample size to extrapolate simulations.
#' @param n_min Numeric. Minimum sample size to extrapolate simulations. Defaults to 3.
#' @param iter Numeric. Number of bootstrap samples. Defaults to 29.
#' @param response Numeric. Column containing thermal limit data for individual samples
#' @return A data frame of CTL summary statistics from bootstrap resamples
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \donttest{
#' head(coreid_data)
#' sims <- boot_one(data = coreid_data,
#'                     groups_col = col,
#'                     groups_which = "Catorhintha schaffneri_APM",
#'                     response = response,
#'                     n_max = 49,
#'                     iter = 99)
#' }
#' @export


# Define function to perform bootstrap resampling for a single population

utils::globalVariables(c("sample_size", 'sample_data', 'calc', 'mean_val','median_pop_val', 'sd_val', 'std_error', 'error', 'ci_falls',
                         'lower_ci', 'upper_ci','mean_upp_ci','mean_low_ci','width_ci','sd_width', 'col.x', 'mean_val.x', 'sd_val.x', 'col.y',
                         'mean_val.y', 'sd_val.y', 'first_term', 'second_term', 'pooled_df', 's_pooled', 'se_diff', 'mean_diff'))


boot_one <- function(data,
                        groups_col,
                        groups_which,
                        n_max,
                        n_min = 3,
                        iter = 29,
                        response) {

  # Perform boostrap sampling
  boot_data <- {{ data }} %>%
    dplyr::group_by( {{ groups_col }} ) %>%
    dplyr::filter({{ groups_col }} %in% c( {{ groups_which }} )) %>%
    tidyr::nest() %>%
    tidyr::crossing(sample_size = c({{ n_min }} : {{ n_max }}),
                    iter = seq(1: {{ iter }} )) %>%
    # Added sampling with replacement to code below
    dplyr::mutate(sample_data = purrr::map2(data,
                                            sample_size,
                                            ~dplyr::sample_n(.x, .y,
                                                             # Sample with replacement
                                                             replace = TRUE))) %>%
    dplyr::mutate(calc = purrr::map(sample_data,
                                    ~dplyr::summarize(.,
                                                      mean_val = mean( {{ response }} ),
                                                      sd_val = stats::sd(( {{ response }} ))))) %>%
    dplyr::select({{ groups_col }}, sample_size, iter, calc) %>%
    tidyr::unnest(cols = calc)

  # Estimate population CT value per species
  median_vals <- boot_data %>%
    dplyr::group_by({{ groups_col }}) %>%
    dplyr::filter(sample_size == max(sample_size)) %>%
    dplyr::mutate(median_pop_val = mean_val) %>%
    dplyr::slice(1) %>%
    dplyr::select({{ groups_col }}, median_pop_val)
  median_vals

  # Add median value per species to the bootstrapped dataset
  # Join the two datsets by the {{ groups_col }} column
  boot_proc <- suppressMessages(dplyr::full_join(boot_data,
                                median_vals))
  boot_proc

  # Add CI's to raw bootstrap samples
  boot_comb <- boot_proc %>%
    dplyr::group_by({{ groups_col }}) %>%
    # Add standard errors
    dplyr::mutate(std_error = sd_val/sqrt(sample_size)) %>%
    # Now calculate error
    dplyr::mutate(error = stats::qt(0.975, df = sample_size - 1) * std_error) %>%
    # Calculate lower and upper 95% CI limits
    dplyr::mutate(lower_ci = mean_val - error,
                  upper_ci  = mean_val + error) %>%
    dplyr::ungroup()

  # Calculate proportion of bootstrap samples containing median CT value
  boot_comb <- boot_comb %>%
    dplyr::group_by({{ groups_col }}, sample_size) %>%
    dplyr::mutate(ci_falls = dplyr::case_when(
      median_pop_val < lower_ci ~ 0,
      median_pop_val > upper_ci ~ 0,
      median_pop_val > lower_ci & median_pop_val < upper_ci ~ 1,
      median_pop_val < upper_ci & median_pop_val > lower_ci ~ 1)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ groups_col }}, sample_size) %>%
    dplyr::mutate(prop_correct = sum(ci_falls)/max( {{ iter }} )) %>%
    dplyr::ungroup()

  # Calculate summary statistics
  data_sum <- boot_comb %>%
    dplyr::group_by({{ groups_col }}, sample_size) %>%
    dplyr::summarise(mean_low_ci    = mean(lower_ci),
                     mean_upp_ci    = mean(upper_ci),
                     mean_ct        = mean(mean_val),
                     width_ci       = mean_upp_ci - mean_low_ci,
                     sd_width       = stats::sd(upper_ci - lower_ci),
                     sd_width_lower = width_ci - sd_width,
                     sd_width_upper = width_ci + sd_width,
                     median_pop_val = max(median_pop_val),
                     prop_ci_contain = sum(ci_falls)/ {{ iter}},
                     iter=iter,
                     lower_ci=lower_ci,
                     upper_ci=upper_ci)


  # Return this dataframe
  return(data_sum)

}

##########################################################################
##########################################################################
##########################################################################
