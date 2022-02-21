##########################################################################
##########################################################################
##########################################################################
# Function - plot_one_group
#          - Write function to plot output from bootstrap resampling for
#            single population estimates across a range of sample sizes
##########################################################################
##########################################################################
##########################################################################

# Define plotting function ---------------------------------

#' Plot output from boot_sample
#'
#'
#' @name plot_one_group
#' @description Plot output from boot_one.
#' @param x Output from boot_one function.
#' @param n_max Numeric. Maximum sample size to extrapolate simulations.
#' @param n_min Numeric. Minimum sample size to extrapolate simulations.
#' Defaults to 3.
#' @param colour_exp Colour of the experimental data. Defaults to "blue".
#' @param colour_extrap Colour of the extrapolated data. Defaults to "red".
#' @param legend.position Position of the legend. Defaults to "top". Can be "bottom", "left", "right", or "none".
#' @param alpha_val Change the degree of shading of the graphs. Default is 0.2.
#' @param ggtheme The theme for the ggplot created. See ggplot2 themes for options. Default set to theme_classic().
#' @return Two plots; (a) precision of the CTmin estimate across experimental and extrapolated sample sizes; (b)
#' the sampling distribution (range of plausible CTmin values) across experimental and extrapolated sample sizes.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr between
#' @export
#' @examples
#' \donttest{
#' sims <- boot_one(coreid_data,
#'                     groups_col = col,
#'                     groups_which = "Catorhintha schaffneri_APM",
#'                     n_max = 30,
#'                     response = response)
#' plot_one_group(x = sims,
#'                n_min = 3,
#'                n_max = 15,
#'                colour_exp = "darkblue",
#'                colour_extrap = "green",
#'                legend.position = "right")
#'}

utils::globalVariables(c('id', 'sd_width_lower', 'sd_width_upper', 'sims', 'prop_ci_contain'))

plot_one_group <- function(x = sims, n_max, n_min = 3, colour_exp = "blue", colour_extrap = "red", legend.position = "top", alpha_val = 0.2, ggtheme = theme_classic()){

  ggplot2::theme_set(ggtheme)

  # Create dataframe for experimental data
  exp_data <- {{ x }} %>%
    dplyr::filter(dplyr::between(sample_size, {{ n_min }}, {{ n_max }}))

  # Create dataframe for extrapolations from data
  ext_data <- {{ x }} %>%
    dplyr::filter(dplyr::between(sample_size, {{ n_max }}, max(sample_size)))

  # Make a combined dataframe with id included to colour-code ribbon
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
    #ggtheme +

    geom_hline(yintercept = 0,
               linetype = "dashed") +
    labs(x = "Sample size (n)",
         y = "Width of confidence interval (95% CI)",
         fill = "Data",
         subtitle = "(a)") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(colour = "black"),
          axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
          axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
          legend.position = legend.position) +
    guides(colour = "none")

  # Plot the width of the 95% CI
  contain_plot <- ggplot2::ggplot(data = {{ x }}, aes(x = sample_size,
                                                      y = prop_ci_contain)) +
    geom_line(data = both_data, aes(x = sample_size,
                                    y = prop_ci_contain,
                                    colour = id),

              alpha = 1) +
    scale_colour_manual(values = c(colour_exp, colour_extrap),
                        labels = c("Experimental", "Extrapolation")) +
    #ggtheme +
    geom_hline(yintercept = 0.90,
               linetype = "dashed") +
    labs(x = "Sample size (n)",
         y = "Proportion of 95%'s \ncontaining median CTL",
         colour = "Data",
         subtitle = "(b)") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(colour = "black"),
          axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
          axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
          legend.position = legend.position)

  # Return the plots
     one_group_output = cowplot::plot_grid(width_plot,
                     contain_plot,
                     ncol = 2)

     return(one_group_output)

}

##########################################################################
##########################################################################
##########################################################################
