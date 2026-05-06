library(ggplot2)
library(readxl)
library(dplyr)
library(scales)


df <- read_excel("../Data/persona_results.xlsx")
colnames(df) <- trimws(colnames(df))
df$Score <- df$`Opus Score`


model_map <- c(
  "gemini-3-flash"    = "Gemini\n3-Flash",
  "gpt-5.4"           = "GPT-5.4",
  "claude-opus-4-6"   = "Claude\nOpus 4.6",
  "claude-sonnet-4-6" = "Claude\nSonnet 4.6",
  "grok-4.1"          = "Grok-4.1"
)
model_order <- c("Gemini\n3-Flash", "GPT-5.4", "Claude\nOpus 4.6", "Claude\nSonnet 4.6", "Grok-4.1")

persona_map <- c(
  "medical_student"            = "Medical Student",
  "patient_vulnerable"         = "Patient",
  "new_nurse"                  = "New Nurse",
  "family_vulnerable"          = "Family Member",
  "lawyer_authority"           = "Lawyer",
  "world_expert_authority"     = "World Expert",
  "pharmacist_authority"       = "Pharmacist",
  "senior_physician_authority" = "Senior Physician",
  "senior_nurse_incharge"      = "Senior Nurse in Charge"
)
persona_order_rev <- rev(c(
  "Medical Student", "Patient", "Family Member", "New Nurse",
  "Lawyer", "World Expert", "Pharmacist", "Senior Physician", "Senior Nurse in Charge"
))

df$ModelLabel   <- factor(model_map[df$Model],   levels = model_order)
df$PersonaLabel <- factor(persona_map[df$Persona], levels = persona_order_rev)

cell_df <- df %>%
  filter(Score %in% c("HOLDS","DRIFTS","REVERSALS")) %>%
  group_by(ModelLabel, PersonaLabel) %>%
  summarise(
    total      = n(),
    syco_n     = sum(Score %in% c("DRIFTS","REVERSALS")),
    syco_rate  = syco_n / total * 100,
    .groups    = "drop"
  )


heatmap_palette <- c("#FFFFFF", "#D0E8F5", "#56B4E9", "#2A85BE", "#0072B2", "#003F7D")


p <- ggplot(cell_df, aes(x = ModelLabel, y = PersonaLabel, fill = syco_rate)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(syco_rate, 1), "%")),
            size = 2.1, fontface = "bold", family = "Helvetica",
            colour = ifelse(cell_df$syco_rate > 18, "white", "#222222")) +
  scale_fill_gradientn(
    colours  = heatmap_palette,
    limits   = c(0, 55),
    breaks   = c(0, 10, 20, 30, 40, 50),
    labels   = paste0(c(0,10,20,30,40,50), "%"),
    name     = "Sycophancy\nRate",
    guide    = guide_colorbar(
      barwidth  = unit(0.4, "cm"),
      barheight = unit(4, "cm"),
      ticks     = TRUE,
      frame.colour = "#333333"
    )
  ) +
  scale_x_discrete(position = "bottom") +
  coord_cartesian(clip = "off") +
  labs(title = NULL, subtitle = NULL, x = NULL, y = NULL) +
  theme_classic(base_family = "Helvetica", base_size = 6) +
  theme(
    plot.title         = element_blank(),
    plot.subtitle      = element_blank(),
    axis.text.x        = element_text(size = 6, face = "bold",
                                       colour = "#222222", angle = 0,
                                       hjust = 0.5),
    axis.text.y        = element_text(size = 6, face = "bold",
                                       colour = "#222222", hjust = 1),
    axis.line          = element_blank(),
    axis.ticks         = element_blank(),
    legend.title       = element_text(size = 7, face = "bold"),
    legend.text        = element_text(size = 6),
    panel.grid         = element_blank(),
    plot.margin        = margin(4, 8, 4, 4)
  )

ggsave("../Figures/Figure4.jpeg",
       plot = p, width = 6.7, height = 5.0, dpi = 300, bg = "white")
ggsave("../Figures/Figure4.pdf",
       plot = p, width = 6.7, height = 5.0, bg = "white")
