library(ggplot2)
library(readxl)
library(dplyr)
library(scales)
library(ggtext)


df <- read_excel("../Data/persona_results.xlsx")
colnames(df) <- trimws(colnames(df))
df$Score <- df$`Opus Score`


acuity_map <- c(
  "outpatient"  = "**Outpatient**<br>(n = 165)",
  "urgent_care" = "**Urgent<br>Care**<br>(n = 132)",
  "emergency"   = "**Emergency**<br>(n = 100)",
  "inpatient"   = "**Inpatient**<br>(n = 153)",
  "icu"         = "**ICU**<br>(n = 89)"
)
acuity_order <- c("**Outpatient**<br>(n = 165)", "**Urgent<br>Care**<br>(n = 132)", "**Emergency**<br>(n = 100)", "**Inpatient**<br>(n = 153)", "**ICU**<br>(n = 89)")
df$SettingLabel <- factor(acuity_map[df$Acuity], levels = acuity_order)


totals <- df %>%
  filter(Score %in% c("DRIFTS","REVERSALS")) %>%
  count(SettingLabel, name = "total")

syco_df <- df %>%
  filter(Score %in% c("DRIFTS","REVERSALS")) %>%
  count(SettingLabel, Score) %>%
  left_join(totals, by = "SettingLabel") %>%
  mutate(pct   = n / total * 100,
         Score = factor(recode(Score, "REVERSALS" = "REVERSALS"),
                        levels = c("DRIFTS", "REVERSALS")))


pval_df <- data.frame(
  SettingLabel = factor(acuity_order, levels = acuity_order),
  pval         = c("P < 0.001", "P = 0.26", "P = 0.37", "P = 0.02", "P = 0.06")
)


score_colours <- c("DRIFTS" = "#56B4E9", "REVERSALS" = "#0072B2")


p <- ggplot(syco_df, aes(x = SettingLabel, y = pct, fill = Score)) +

  geom_col(position = position_dodge(width = 0.68),
           width = 0.62, colour = "white", linewidth = 0.3) +

  geom_text(aes(label = paste0(round(pct, 1), "%"), y = pct + 1.5),
            position = position_dodge(width = 0.68),
            vjust = 0, size = 2.1, fontface = "bold",        
            family = "Helvetica", colour = "#222222") +

  geom_text(data = pval_df,
            aes(x = SettingLabel, y = 98, label = pval),
            colour = "#555555", size = 2.1, fontface = "italic", 
            family = "Helvetica", inherit.aes = FALSE) +

  scale_fill_manual(values = score_colours,
                    name   = NULL,
                    labels = c("Drifts", "Reversals")) +
  scale_y_continuous(limits = c(0, 108),
                     breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0, 0)) +
  labs(
    title    = NULL,
    subtitle = NULL,
    x = "Clinical setting",
    y = "% of sycophantic responses"
  ) +
  theme_classic(base_family = "Helvetica", base_size = 6) +   
  theme(
    plot.title         = element_blank(),
    plot.subtitle      = element_blank(),
    axis.text.x        = element_markdown(size = 6, colour = "#222222", angle = 0,
                                       hjust = 0.5, vjust = 1, lineheight = 1.1),
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

ggsave("../Figures/Figure3b.jpeg",
       plot = p, width = 7.0, height = 5.0, dpi = 300, bg = "white")
ggsave("../Figures/Figure3b.pdf",
       plot = p, width = 7.0, height = 5.0, bg = "white")
