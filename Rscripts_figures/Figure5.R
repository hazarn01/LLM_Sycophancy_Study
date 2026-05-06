library(ggplot2)
library(readxl)
library(dplyr)
library(geepack)
library(scales)
library(openxlsx)


df <- read_excel("../Data/persona_results.xlsx")
colnames(df) <- trimws(colnames(df)) 
df <- df %>% filter(Output %in% c("HOLDS","COMBINED"))
df$syco <- as.integer(df$Output == "COMBINED")

model_map <- c(
  "gemini-3-flash"    = "Gemini-3-Flash",
  "gpt-5.4"           = "GPT-5.4",
  "claude-opus-4-6"   = "Claude Opus 4.6",
  "claude-sonnet-4-6" = "Claude Sonnet 4.6",
  "grok-4.1"          = "Grok-4.1"
)
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
acut_map <- c(
  "icu"         = "ICU",
  "emergency"   = "Emergency",
  "urgent_care" = "Urgent Care",
  "inpatient"   = "Inpatient",
  "outpatient"  = "Outpatient"
)
disc_map <- c(
  "cardiology"              = "Cardiology",
  "pulmonology"             = "Pulmonology",
  "medicine"                = "Medicine",
  "nephrology"              = "Nephrology",
  "neurology"               = "Neurology",
  "infectious_disease"      = "Infectious Diseases",
  "endocrinology"           = "Endocrinology",
  "gi"                      = "Gastroenterology",
  "hematology"              = "Hematology",
  "critical_care"           = "Critical Care",
  "psychiatry"              = "Psychiatry",
  "pharmacology_toxicology" = "Pharmacology/Toxicology"
)

df$ModelLabel      <- relevel(factor(model_map[df$Model]),       ref = "Claude Opus 4.6")
df$PersonaLabel    <- relevel(factor(persona_map[df$Persona]),   ref = "Lawyer")
df$AcuityLabel     <- relevel(factor(acut_map[df$Acuity]),       ref = "Urgent Care")
df$DisciplineLabel <- relevel(factor(disc_map[df$Discipline]),   ref = "Medicine")
df <- df %>% arrange(Case_ID)

# ── Fit GEE - main effects model ────
gee_fit <- geeglm(
  syco ~ ModelLabel + PersonaLabel + AcuityLabel + DisciplineLabel,
  family = binomial(link = "logit"),
  id     = Case_ID,
  corstr = "exchangeable",
  data   = df
)

# ── Extract coefficients: OR and 95% CI ────
coef_df <- as.data.frame(summary(gee_fit)$coefficients)
coef_df$term <- rownames(coef_df)
names(coef_df)[1:4] <- c("estimate","se","wald","p")
coef_df <- coef_df[coef_df$term != "(Intercept)", ]

coef_df$OR    <- exp(coef_df$estimate)
coef_df$CI_lo <- exp(coef_df$estimate - 1.96 * coef_df$se)
coef_df$CI_hi <- exp(coef_df$estimate + 1.96 * coef_df$se)

coef_df$term_clean <- gsub("^ModelLabel",       "", coef_df$term)
coef_df$term_clean <- gsub("^PersonaLabel",     "", coef_df$term_clean)
coef_df$term_clean <- gsub("^AcuityLabel",      "", coef_df$term_clean)
coef_df$term_clean <- gsub("^DisciplineLabel",  "", coef_df$term_clean)

coef_df$group <- ifelse(grepl("^ModelLabel",      coef_df$term), "LLM",
                 ifelse(grepl("^PersonaLabel",     coef_df$term), "Persona Injections",
                 ifelse(grepl("^AcuityLabel",      coef_df$term), "Clinical Setting",
                                                                  "Vignette Specialty")))

# ── Plot ──
model_rows   <- coef_df %>% filter(group == "LLM")                %>% arrange(OR)
persona_rows <- coef_df %>% filter(group == "Persona Injections") %>% arrange(OR)
acut_rows    <- coef_df %>% filter(group == "Clinical Setting")   %>% arrange(OR)
disc_rows    <- coef_df %>% filter(group == "Vignette Specialty") %>% arrange(OR)

ref_model   <- data.frame(term_clean="Claude Opus 4.6", OR=1, CI_lo=1, CI_hi=1,
                           group="LLM", p=NA, estimate=NA, se=NA, wald=NA)
ref_persona <- data.frame(term_clean="Lawyer",         OR=1, CI_lo=1, CI_hi=1,
                           group="Persona Injections", p=NA, estimate=NA, se=NA, wald=NA)
ref_acut    <- data.frame(term_clean="Urgent Care",    OR=1, CI_lo=1, CI_hi=1,
                           group="Clinical Setting", p=NA, estimate=NA, se=NA, wald=NA)
ref_disc    <- data.frame(term_clean="Medicine",       OR=1, CI_lo=1, CI_hi=1,
                           group="Vignette Specialty", p=NA, estimate=NA, se=NA, wald=NA)

plot_df <- bind_rows(model_rows, ref_model,
                     persona_rows, ref_persona,
                     acut_rows, ref_acut,
                     disc_rows, ref_disc)
plot_df$term_clean <- factor(plot_df$term_clean, levels = rev(unique(plot_df$term_clean)))


plot_df$group <- factor(plot_df$group,
                         levels = c("Persona Injections", "LLM", "Vignette Specialty", "Clinical Setting"))

group_colours <- c("LLM"                = "#0072B2",
                   "Persona Injections" = "#0072B2",
                   "Vignette Specialty" = "#0072B2",
                   "Clinical Setting"   = "#0072B2")



p <- ggplot(plot_df, aes(y = term_clean, x = OR, colour = group)) +

  geom_vline(xintercept = 1, linetype = "dashed",
             colour = "#666666", linewidth = 0.5) +

  geom_errorbarh(aes(xmin = CI_lo, xmax = CI_hi),
                 height = 0, linewidth = 0.7, na.rm = TRUE) +

  geom_point(aes(shape = ifelse(is.na(p), "ref", "est"),
                 size  = ifelse(is.na(p), 2.0, 3.0)), na.rm = TRUE) +
  scale_shape_manual(values = c("est" = 15, "ref" = 18), guide = "none") +
  scale_size_identity() +

  coord_cartesian(clip = "off") +

  facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +

  scale_colour_manual(values = group_colours, guide = "none") +
  scale_x_continuous(
    trans  = "log",
    breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 8),
    labels = c("0.1","0.25","0.5","1","2","4","8"),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(
    title    = NULL,
    subtitle = NULL,
    x       = "Odds ratio (95% CI, log scale)",
    y       = NULL,
    caption = "<--- Less Sycophantic                                 More Sycophantic --->"
  ) +
  theme_classic(base_family = "Helvetica", base_size = 6) +
  theme(
    plot.title             = element_blank(),
    plot.title.position    = "plot",
    plot.subtitle          = element_blank(),
    plot.subtitle.position = "plot",
    axis.text.y            = element_text(size = 6, colour = "#222222"),
    axis.text.x            = element_text(size = 6),
    axis.title.x           = element_text(size = 7, margin = margin(t = 4), hjust = 0.5),
    axis.line.y            = element_blank(),
    axis.ticks.y           = element_blank(),
    strip.text             = element_text(size = 7, face = "bold", colour = "white",
                                          vjust = 0.5),
    strip.text.y.left      = element_text(size = 7, face = "bold", colour = "white",
                                          angle = 90, vjust = 0.5, hjust = 0.5),
    strip.background       = element_rect(fill = "#2C3E50", colour = NA),
    strip.placement        = "outside",
    panel.grid.major.y     = element_line(colour = "#F5F5F5", linewidth = 0.3),
    panel.grid.major.x     = element_line(colour = "#EEEEEE", linewidth = 0.3),
    panel.spacing          = unit(0.5, "cm"),
    plot.caption           = element_text(size = 6, colour = "#0072B2", face = "italic",
                                          hjust = 0.5, margin = margin(t = 6)),
    plot.margin            = margin(6, 6, 6, 6)
  )

ggsave("../Figures/Figure5.jpeg",
       plot = p, width = 5.0, height = 10.5, dpi = 300, bg = "white")
ggsave("../Figures/Figure5.pdf",
       plot = p, width = 5.0, height = 10.5, bg = "white")
cat("Saved: Figure5.jpeg + .pdf\n")
