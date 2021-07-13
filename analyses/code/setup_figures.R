# ==========================================================================
# Setup for all figures
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(ggplot2, patchwork, ggsci, tidybayes, bayesplot)
pacman::p_load_gh("janajarecki/themejj")
theme_set(themejj(base_size = 14))

.w <- 7 # width of all plots
.h <- 7 # height of all plots

# Color-blind friendly colors --------------------------------------------
colors <- c(
  perc_risk = "#373F51",
  seek_risk = "#008DD5",
  social = "#DFBBB1"
)

# from lightest to darkest, darkest is the value in colors
color_schemes <- list(
  perc_risk=c("#d7d8dc", "#c3c5ca", "#afb2b9", "#878b96", "#5e6573","#373f51"),
  seek_risk=c("#cce8f6", "#99d1ee", "#66bae5", "#32a3dd", "#1998d9","#008DD5"),
  social =c("#f2e3df", "#efddd8", "#ebd6d0", "#e5c8c0", "#DFBBB1", "#c8a89f"),
  other = tolower(color_scheme_get("teal"))
)

# Define labels for the predictors for all plots --------------------------
variable_labels <- c(
  perc_risk_health = "Health-risk Perception",
  perc_risk_data = "Data-security Risk Perception",
  perc_risk_econ = "Economic Risk Perception",
  seek_risk_general = "General Risk Seeking",
  seek_risk_health = "Health Risk Seeking",
  seek_risk_data = "Data-Security Risk Seeking",
  seek_risk_econ = "Economic Risk Seeking",
  honhum_score = "Honesty-Humility Personality",
  svo_angle = "Social-Value-Orientation",
  iwah_diff_score = "Identification-World-over-Community",
  compreh_score = "Comprehension",
  policy_score = "Policy-Support",
  tech_score = "Technology-Affinity",
  age = "Age"
)


# Plotting functions --------------------------------------------------------
plot_group <- function(vars, title, ylabels = variable_labels, text_nudge = -0.33) {
  names <- grep(paste(vars, collapse="|"),
    gsub("b_", "", parnames(fit_reduced_no_social)), value=T)
  # get color scheme based on vars
  if (any(grepl("hon", vars)))    { scheme <- "social"
    } else if (any(grepl("compr", vars))) { scheme <- "other"
      } else { scheme <- grep(vars, names(color_schemes), value=T) }
  color_scheme_set(color_schemes[[scheme]]) # set color scheme
  ylabels <- ylabels[names] # y-axis labels
  ylabels <- tstrsplit(ylabels, " ")[[1]] # only first word
  ylabels <- gsub("-", " ", ylabels) # no hyphens
  xlim <- if (grepl("Major", title)) c(-.8,.8) else c(-0.4,0.4)
  asp_ratio = force(max(xlim)/2)

  v <- gather_draws(model=fit_reduced_no_social, !!sym(paste0(".*", gsub("\\|", ".*|.*", vars), ".*")), regex=TRUE)
  v <- as.data.table(v)
  v[, .variable := gsub("b_", "", .variable)]
  v[, .variable := factor(.variable, names, ylabels)]
  

  mhdi <- v[, median_hdi(.value), by = .variable]
  mhdi[, label := paste0(
    sprintf("%.2f", y),
    "\n[", paste0(sprintf("%.2f", ymin)),
      "; ", paste0(sprintf("%.2f", ymax)),
      "]")]

    ggplot(v, aes(y = .variable, x = .value)) +
      geom_vline(xintercept = 0, color="lightgrey") +
      stat_halfeye(
        point_interval = median_hdi,
        .width = c(.95, .89),
        slab_fill = color_schemes[[scheme]][1],
        interval_colour = color_schemes[[scheme]][5],
        slab_alpha = .7,
        point_colour = color_schemes[[scheme]][6],
        point_fill = color_schemes[[scheme]][6],
        point_size = 2
    ) +
    geom_text(data=mhdi, aes(x = y, label = label),
      nudge_y = text_nudge,
      family = .default_font,
      size = 2.8,
      lineheight = .85,
      color = color_schemes[[scheme]][6]) +
    geom_label(data=bfs[grepl(vars, .variable)], aes(x = max(xlim) + 0.3, label = BF, y = factor(.variable, names, ylabels)),
        family = .default_font,
        nudge_x = -0.14,
        fill = "white",
        label.size = 0,
        label.padding  = unit(0.2, "lines")) +
    # geom_text(
    #   aes(label = "B", x = max(xlim), y = nrow(mqi) + .5),
    #    # parse=TRUE,
    #    # family = .default_font,
    #    # inherit.aes = FALSE
    #  ) +
    #scale_y_discrete(labels = ylabels) +
    scale_x_continuous(breaks = round(seq(xlim[1], xlim[2], round(max(xlim)/2, 1)), 1)) +
    coord_fixed(ratio = asp_ratio, xlim = xlim + c(0,0.2), clip = "off") +
    ggtitle(title) +
    theme(
      plot.margin = unit(c(.5,.5,.5,.5), "lines"),
      axis.title = element_blank(),
      panel.grid.major.y = element_line(),
      axis.ticks = element_blank()
    )
}

message("Defined vector 'colors', `color_schemes` and 'variable_labels', plot_groups().")