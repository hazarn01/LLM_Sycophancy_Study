library(ggplot2)
library(readxl)
library(dplyr)
library(scales)
library(ggtext)


df <- read_excel("../Data/persona_results.xlsx")
colnames(df) <- trimws(colnames(df))
df$Score <- df$`Opus Score`

model_labels <- c(
  "claude-opus-4-6"   = "**Claude<br>Opus 4.6**<br>(n = 95)",
  "claude-sonnet-4-6" = "**Claude<br>Sonnet 4.6**<br>(n = 67)",
  "gpt-5.4"           = "**GPT-5.4**<br>(n = 158)",
  "grok-4.1"          = "**Grok-4.1**<br>(n = 43)",
  "gemini-3-flash"    = "**Gemini-3-Flash**<br>(n = 276)"
)
model_order <- c("**Gemini-3-Flash**<br>(n = 276)", "**GPT-5.4**<br>(n = 158)", "**Claude<br>Opus 4.6**<br>(n = 95)", "**Claude<br>Sonnet 4.6**<br>(n = 67)", "**Grok-4.1**<br>(n = 43)")
df$ModelLabel <- factor(model_labels[df$Model], levels = model_order)


totals <- df %>%
  filter(Score %in% c("DRIFTS","REVERSALS")) %>%
  count(ModelLabel, name = "total")

syco_df <- df %>%
  filter(Score %in% c("DRIFTS","REVERSALS")) %>%
  count(ModelLabel, Score) %>%
  left_join(totals, by = "ModelLabel") %>%
  mutate(pct   = n / total * 100,
         Score = factor(recode(Score, "REVERSALS" = "REVERSALS"),
                        levels = c("DRIFTS", "REVERSALS")))


pval_df <- data.frame(
  ModelLabel = factor(model_order, levels = model_order),
  pval       = c("P < 0.001", "P < 0.001", "P < 0.001", "P = 0.46", "P < 0.001")
)


score_colours <- c("DRIFTS" = "#56B4E9", "REVERSALS" = "#0072B2")


p <- ggplot(syco_df, aes(x = ModelLabel, y = pct, fill = Score)) +
  geom_col(position = position_dodge(width = 0.68),
           width = 0.62, colour = "white", linewidth = 0.3) +
  geom_text(aes(label = paste0(round(pct, 1), "%"), y = pct + 0.25),
            position = position_dodge(width = 0.68),
            vjust = 0, size = 2.1, fontface = "bold",        
            family = "Helvetica", colour = "#222222") +
  geom_text(data = pval_df,
            aes(x = ModelLabel, y = 98, label = pval),
            colour = "#555555", size = 2.1, fontface = "italic",  
            family = "Helvetica", inherit.aes = FALSE) +
  scale_fill_manual(values = score_colours,
                    name   = NULL,
                    labels = c("Drifts", "Reversals")) +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0, 0)) +
  labs(
    title    = NULL,
    subtitle = NULL,
    x = "LLMs",
    y = "% of sycophantic responses"
  ) +
  theme_classic(base_family = "Helvetica", base_size = 6) + 
  theme(
    plot.title         = element_blank(),
    plot.subtitle      = element_blank(),
    axis.text.x        = element_markdown(size = 6, colour = "#222222",
                                       hjust = 0.5, vjust = 1, lineheight = 1.1),
    axis.text.y        = element_text(size = 6),
    axis.title.x       = element_text(size = 7, margin = margin(t = 4, b = -4)),
    axis.title.y       = element_text(size = 7, margin = margin(r = 4)),
    axis.line          = element_line(colour = "#333333", linewidth = 0.4),
    axis.ticks         = element_line(colour = "#333333", linewidth = 0.3),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    legend.text        = element_text(size = 6),
    legend.key.size    = unit(0.3, "cm"),
    panel.grid.major.y = element_line(colour = "#EEEEEE", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    plot.margin        = margin(8, 10, 4, 6)
  )

ggsave("../Figures/Figure2.jpeg",
       plot = p, width = 6.7, height = 4.5, dpi = 300, bg = "white")
ggsave("../Figures/Figure2.pdf",
       plot = p, width = 6.7, height = 4.5, bg = "white")
