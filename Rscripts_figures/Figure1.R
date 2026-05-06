library(ggplot2)
library(readxl)
library(dplyr)
library(scales)
library(ggtext)


df <- read_excel("../Data/persona_results.xlsx")
colnames(df) <- trimws(colnames(df))
df$Score <- df$`Opus Score`

persona_map <- c(
  "medical_student"            = "**Medical<br>Student**<br>(n = 193)",
  "patient_vulnerable"         = "**Patient**<br>(n = 127)",
  "new_nurse"                  = "**New<br>Nurse**<br>(n = 117)",
  "family_vulnerable"          = "**Family<br>Member**<br>(n = 74)",
  "lawyer_authority"           = "**Lawyer**<br>(n = 37)",
  "world_expert_authority"     = "**World<br>Expert**<br>(n = 29)",
  "pharmacist_authority"       = "**Pharmacist**<br>(n = 27)",
  "senior_physician_authority" = "**Senior<br>Physician**<br>(n = 18)",
  "senior_nurse_incharge"      = "**Senior Nurse<br>in Charge**<br>(n = 17)"
)
persona_order <- c(
  "**Medical<br>Student**<br>(n = 193)", "**Patient**<br>(n = 127)", "**New<br>Nurse**<br>(n = 117)", "**Family<br>Member**<br>(n = 74)",
  "**Lawyer**<br>(n = 37)", "**World<br>Expert**<br>(n = 29)", "**Pharmacist**<br>(n = 27)", "**Senior<br>Physician**<br>(n = 18)", "**Senior Nurse<br>in Charge**<br>(n = 17)"
)
df$PersonaLabel <- factor(persona_map[df$Persona], levels = persona_order)


totals <- df %>%
  filter(Score %in% c("DRIFTS","REVERSALS")) %>%
  count(PersonaLabel, name = "total")

syco_df <- df %>%
  filter(Score %in% c("DRIFTS","REVERSALS")) %>%
  count(PersonaLabel, Score) %>%
  left_join(totals, by = "PersonaLabel") %>%
  mutate(pct   = n / total * 100,
         Score = factor(recode(Score, "REVERSALS" = "REVERSALS"),
                        levels = c("DRIFTS", "REVERSALS")))


syco_df <- bind_rows(
  syco_df,
  data.frame(
    PersonaLabel = factor("**Family<br>Member**<br>(n = 74)", levels = persona_order),
    Score        = factor("Reversals", levels = c("Drifts","Reversals")),
    n = 0L, total = 74L, pct = 0
  )
)


cat_df <- data.frame(
  PersonaLabel = factor(c("**New<br>Nurse**<br>(n = 117)", "**Pharmacist**<br>(n = 27)"), levels = persona_order),
  y            = c(114, 114),
  label        = c("Vulnerable", "Authority"),
  col          = c("#333333", "#333333")
)


pval_df <- data.frame(
  PersonaLabel = factor(persona_order, levels = persona_order),
  pval         = c("P < 0.001", "P < 0.001", "P = 0.27", "P < 0.001",
                   "P < 0.001", "P < 0.001", "P = 0.25", "P = 0.81", "P = 0.14")
)


score_colours <- c("DRIFTS" = "#56B4E9", "REVERSALS" = "#0072B2")


p <- ggplot(syco_df, aes(x = PersonaLabel, y = pct, fill = Score)) +

  geom_vline(xintercept = 4.5, linetype = "dotted",
             colour = "#888888", linewidth = 0.6) +

  geom_col(position = position_dodge(width = 0.68),
           width = 0.62, colour = "white", linewidth = 0.3) +

  geom_text(aes(label = paste0(round(pct, 1), "%"), y = pct + 1.5),
            position = position_dodge(width = 0.68),
            vjust = 0, size = 2.1, fontface = "bold",        
            family = "Helvetica", colour = "#222222") +

  geom_text(data = pval_df,
            aes(x = PersonaLabel, y = 108, label = pval),
            colour = "#555555", size = 2.1, fontface = "italic", 
            family = "Helvetica", inherit.aes = FALSE) +

  geom_text(data = cat_df,
            aes(x = PersonaLabel, y = y, label = label, colour = col),
            size = 2.1, fontface = "bold.italic",
            family = "Helvetica", inherit.aes = FALSE) +
  scale_colour_identity() +

  scale_fill_manual(values = score_colours,
                    name   = NULL,
                    labels = c("Drifts", "Reversals")) +
  scale_y_continuous(limits = c(0, 122),
                     breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0, 0)) +
  labs(
    title    = NULL,
    subtitle = NULL,
    x = "Persona injections",
    y = "% of sycophantic responses"
  ) +
  theme_classic(base_family = "Helvetica", base_size = 6) +   
  theme(
    plot.title         = element_blank(),
    plot.subtitle      = element_blank(),
    axis.text.x        = element_markdown(size = 6, colour = "#222222", angle = 0,
                                       hjust = 0.5, vjust = 1, lineheight = 1.1),
    axis.text.y        = element_text(size = 6),
    axis.title.x       = element_text(size = 7),
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

ggsave("../Figures/Figure1.jpeg",
       plot = p, width = 7.083, height = 5.0, dpi = 300, bg = "white")
ggsave("../Figures/Figure1.pdf",
       plot = p, width = 7.083, height = 5.0, bg = "white")
