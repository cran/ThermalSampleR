##########################################################################
##########################################################################
##########################################################################
# Function - plot_two_groups
#          - Write function to plot output from bootstrap resampling for two
#            groups comparisons across a range of sample sizes
##########################################################################
##########################################################################
##########################################################################

# Define plotting function ---------------------------------

#' Plot output from boot_two_groups
#'
#'
#' @name plot_two_groups
#' @description  Plot output from boot_two.
#' @param x Output from boot_two_groups function. Defaults to 'sims'.
#' @param n_max Numeric. Maximum sample size to extrapolate simulations.
#' @param n_min Numeric. Minimum sample size to extrapolate simulations.
#' Defaults to 3.
#' @param colour_exp Colour of the experimental data. Defaults to "blue".
#' @param colour_extrap Colour of the extrapolated data. Defaults to "red".
#' @param legend.position Position of the legend. Defaults to "top". Can be "bottom", "left", "right", or "none".
#' @param alpha_val Change the degree of shading of the graphs. Default is 0.2.
#' @param ggtheme The theme for the ggplot created. See ggplot2 themes for options. Default set to theme_classic().
#' @return Two plots: (a) the precision of the estimates for the difference in CTmin
#' between the two selected groups across sample sizes; (b)
#' the 95% confidence interval of the mean difference in CTmin between the two selected groups.
#' @import ggplot2
#' @importFrom cowplot plot_grid
#' @importFrom dplyr bind_rows
#' @importFrom dplyr between
#' @export
#' @examples
#' \donttest{
#' sims <- boot_two(data = coreid_data,
#'                         groups_col = col,
#'                         response = response,
#'                         group1 = "Catorhintha schaffneri_APM",
#'                         group2 = "Catorhintha schaffneri_NPM",
#'                         n_max = 30,
#'                         iter = 99)
#'
#' plots <- plot_two_groups(x = sims,
#'                          n_min = 3,
#'                          n_max = 30,
#'                          colour_exp = "gold",
#'                          colour_extrap = "darkgreen",
#'                          legend.position = "right")
#' }

utils::globalVariables(c("id", 'sd_width_lower', 'sd_width_upper',  'prop_ci_contain'))

plot_two_groups <- function(x, n_min = 3, n_max, colour_exp = "blue", colour_extrap = "red", legend.position = "top", ggtheme = theme_classic(), point_size = 1, point_shape = 16, alpha_val = 0.2){

  ggplot2::theme_set(ggtheme)

  # Create dataframe for experimental data
  exp_data <- {{ x }} %>%
    dplyr::filter(dplyr::between(sample_size, {{ n_min }}, {{ n_max }}))

  # Create dataframe for extrapolations from data
  ext_data <- {{ x }} %>%
    dplyr::filter(dplyr::between(sample_size, {{ n_max }}, max(sample_size)))

  # Make a combined datafram with id included to colour-code ribbon
  both_data <- dplyr::bind_rows(exp_data, ext_data, .id = "id")

  # Plot the width of the 95% CI
  width_plot <- ggplot2::ggplot(data = {{ x }}, aes(x = sample_size,
                                                    y = width_ci)) +
    geom_line(data = both_data, aes(x = sample_size,
                                    y = width_ci,
                                    colour = id),
              alpha = 0.8) +
    scale_colour_manual(values = c(colour_exp, colour_extrap),
                        labels = c("Experimental", "Extrapolation")) +
    geom_ribbon(data = both_data, aes(ymin = sd_width_lower,
                                      ymax = sd_width_upper,
                                      fill = id),
                linetype = 3,
                alpha = alpha_val) +
    scale_fill_manual(values = c(colour_exp, colour_extrap),
                      labels = c("Experimental", "Extrapolation")) +
    theme_classic() +
    geom_hline(yintercept = 0,
               linetype = "dashed") +
    labs(x = "Sample size (n)",
         y = "Width of confidence interval (95% CI)",
         subtitle = "(a)") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(colour = "black"),
          axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
          axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
          legend.position = "none")

  # Plot the 95% CI of mean difference

  ci_plot <- ggplot2::ggplot(data = {{ x }}, aes(x = sample_size,
                                                 y = mean_diff)) +
    geom_line(data = {{ x }}, aes(x = sample_size,
                                  y = mean_low_ci),
              linetype = "dashed") +
    geom_line(data = {{ x }}, aes(x = sample_size,
                                  y = mean_upp_ci),
              linetype = "dashed") +
    geom_ribbon(data = both_data, aes(ymin = mean_low_ci,
                                      ymax = mean_upp_ci,
                                      fill = id),
                linetype = 3,
                alpha = alpha_val) +
    scale_fill_manual(values = c(colour_exp, colour_extrap),
                      labels = c("Experimental", "Extrapolation")) +
    geom_point(data = both_data, size = point_size, shape = point_shape, aes(x = sample_size,
                                     y = mean_diff,
                                     colour = id),
               alpha = 0.8) +
    scale_colour_manual(values = c(colour_exp, colour_extrap),
                        labels = c("Experimental", "Extrapolation")) +
    theme_classic() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Sample size (n)",
         y = "Mean difference between groups (95% CI)",
         subtitle = "(b)",
         fill = "Data") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(colour = "black"),
          axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
          axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
          legend.position = legend.position,
          legend.key = element_rect(linetype = "dashed")) +
    guides(colour = "none")

  # Combine the two plots

  two_groups_output = cowplot::plot_grid(width_plot,
                     ci_plot,
                     ncol = 2)

  return(two_groups_output)
}

##########################################################################
##########################################################################
##########################################################################
