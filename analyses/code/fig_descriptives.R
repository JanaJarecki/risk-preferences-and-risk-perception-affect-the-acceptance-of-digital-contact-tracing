pacman::p_load(data.table, ggplot2, themejj, ggthemes, patchwork)


# Load data -------------------------------------------------------------------
d <- fread("../../data/processed/data.csv")

# Prepare --------------------------------------------------------------------
d[, gender := fcase(
  female == 0, " Male",
  female == 1, "Female",
  female == 2, NA_character_)]
d[education <= 2, education := 2]
d[, education_f := factor(education, labels = c("No/Obligatory", "Vocational", "Higher", "College", "University"), ordered = T)]

d[, safety_mask := safety_mask/100]
to_plot <- c("mhealth_score", "compreh_score", "policy_score", "accept_index", "comply_index")
labels <- c("Mental Health", "Comprehension", "Policy Support", "Acceptance", "Compliance")

plot_it <- function(xstring, xtitle) {
  p1 <- ggplot(melt(d, id = c("id", xstring), measure = to_plot, na.rm=TRUE)[!is.na(get(xstring))],
    aes_string(xstring, "value")) +
    geom_tufteboxplot(median.type = "line", hoffset = 0, width = 5, fatten = 10) +
    stat_summary(geom = "crossbar", width = 1, fatten=1.3, color="white", fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))}) +
    facet_wrap(~factor(variable, levels = to_plot, label = labels), nrow=1, scales = "free_y") +
    guides(fill="none") +
    theme(
      aspect.ratio = .9,
      axis.text.x = element_text(angle = 65, vjust = 1, hjust = .95),
      axis.ticks.x = element_blank(),
      plot.margin = margin(c(0,0,0,0), unit = "lines")) +
    xlab(xtitle) +
    ylab("Value")

  p2 <- ggplot(d[!is.na(get(xstring)), .N, by = xstring], aes_string(x = xstring, y = "N")) +
    geom_bar(stat = "identity", fill = "grey80", color = NA, width = .5, position = position_dodge(width=0.6)) +
    scale_x_discrete(name = "", expand = c(.5,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(
      aspect.ratio = .9,
      axis.text.x = element_text(angle = 65, vjust = 1, hjust = .95),
      axis.ticks.x = element_blank(),
      plot.margin = margin(c(b=2,0,0,0), unit = "lines"))
  (p1 | p2) 
}


(plot_it("age_bin", "Age bin") + plot_layout(tag_level = "new")) /
(plot_it("gender", "") + plot_layout(tag_level = 'new')) /
(plot_it("education_f", "Education") + plot_layout(tag_level = "new")) +
plot_layout(nrow = 3) +
plot_annotation(tag_levels = c("A", "1"))

ggsave("../figures/fig_descriptive.pdf", w = 8, h = 6.9)