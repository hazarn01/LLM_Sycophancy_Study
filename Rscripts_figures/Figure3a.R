library(ggplot2)
library(readxl)
library(dplyr)
library(scales)
library(ggtext)


df <- read_excel("../Data/persona_results.xlsx")
colnames(df) <- trimws(colnames(df))
df$Score <- df$`Opus Score`


disc_map <- c(
  "hematology"             = "**Hematology**<br>(n = 75)",
  "gi"                     = "**Gastroenterology**<br>(n = 71)",
  "psychiatry"             = "**Psychiatry**<br>(n = 52)",
  "nephrology"             = "**Nephrology**<br>(n = 56)",
  "cardiology"             = "**Cardiology**<br>(n = 58)",
  "pharmacology_toxicology"= "**Pharmacology/Toxicology**<br>(n = 44)",
  "medicine"               = "**Medicine**<br>(n = 53)",
  "pulmonology"            = "**Pulmonology**<br>(n = 54)",
  "infectious_disease"     = "**Infectious Diseases**<br>(n = 48)",
  "endocrinology"          = "**Endocrinology**<br>(n = 47)",
  "neurology"              = "**Neurology**<br>(n = 46)",
  "critical_care"          = "**Critical Care**<br>(n = 35)"
)
disc_order <- c(
  "**Hematology**<br>(n = 75)", "**Gastroenterology**<br>(n = 71)", "**Psychiatry**<br>(n = 52)", "**Nephrology**<br>(n = 56)",
  "**Cardiology**<br>(n = 58)", "**Pharmacology/Toxicology**<br>(n = 44)", "**Medicine**<br>(n = 53)", "**Pulmonology**<br>(n = 54)",
  "**Infectious Diseases**<br>(n = 48)", "**Endocrinology**<br>(n = 47)", "**Neurology**<br>(n = 46)", "**Critical Care**<br>(n = 35)"
)
df$DiscLabel <- factor(disc_map[df$Discipline], levels = disc_order)


totals <- df %>%
  filter(Score %in% c("DRIFTS","REVERSALS")) %>%
  count(DiscLabel, name = "total")

syco_df <- df %>%
  filter(Score %in% c("DRIFTS","REVERSALS")) %>%
  count(DiscLabel, Score) %>%
  left_join(totals, by = "DiscLabel") %>%
  mutate(pct   = n / total * 100,
         Score = factor(recode(Score, "REVERSALS" = "REVERSALS"),
                        levels = c("DRIFTS", "REVERSALS")))


pval_df <- data.frame(
  DiscLabel = factor(disc_order, levels = disc_order),
  pval      = c("P = 0.49", "P = 0.64", "P = 0.49", "P = 0.23",
                "P = 0.36", "P = 0.17", "P = 0.01", "P = 0.50",
                "P = 0.01", "P = 0.24", "P = 0.03", "P = 0.74")
)


score_colours <- c("DRIFTS" = "#56B4E9", "REVERSALS" = "#0072B2")


ct <- df %>%
  filter(Score %in% c("HOLDS","DRIFTS","REVERSALS")) %>%
  count(Discipline, Score) %>%
  tidyr::pivot_wider(names_from = Score, values_from = n, values_fill = 0)
chisq_p <- chisq.test(as.matrix(ct[, c("HOLDS","DRIFTS","REVERSALS")]))$p.value
cat("Chi-square p-value:", chisq_p, "\n")


p <- ggplot(syco_df, aes(x = DiscLabel, y = pct, fill = Score)) +

  geom_col(position = position_dodge(width = 0.68),
           width = 0.62, colour = "white", linewidth = 0.3) +

  geom_text(aes(label = paste0(round(pct, 1), "%"), y = pct + 1.5),
            position = position_dodge(width = 0.68),
            vjust = 0, size = 2.1, fontface = "bold",        
            family = "Helvetica", colour = "#222222") +

  geom_text(data = pval_df,
            aes(x = DiscLabel, y = 90, label = pval),
            colour = "#555555", size = 2.1, fontface = "italic",  
            family = "Helvetica", inherit.aes = FALSE) +

  scale_fill_manual(values = score_colours,
                    name   = NULL,
                    labels = c("Drifts", "Reversals")) +
  scale_y_continuous(limits = c(0, 110),
                     breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0, 0)) +
  labs(
    title    = NULL,
    subtitle = NULL,
    x = "Vignette specialty",
    y = "% of sycophantic responses"
  ) +
  theme_classic(base_family = "Helvetica", base_size = 6) + 
  theme(
    plot.title         = element_blank(),
    plot.subtitle      = element_blank(),
    axis.text.x        = element_markdown(size = 6, colour = "#222222", angle = 45,
                                       hjust = 1, vjust = 1, lineheight = 1.1, halign = 0.5),
    axis.text.y        = element_text(size = 6),
    axis.title.y       = element_text(size = 7, margin = margin(r = 4)),
    axis.line          = element_line(colour = "#333333", linewidth = 0.4),
    axis.ticks         = element_line(colour = "#333333", linewidth = 0.3),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    legend.text        = element_text(size = 6),
    legend.key.size    = unit(0.3, "cm"),
    panel.grid.major.y = element_line(colour = "#EEEEEE", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    plot.margin        = margin(8, 12, 4, 24)
  )

ggsave("../Figures/Figure3a.jpeg",
        plot = p, width = 9.0, height = 5.5, dpi = 300, bg = "white")
ggsave("../Figures/Figure3a.pdf",
       plot = p, width = 9.0, height = 5.5, bg = "white")
