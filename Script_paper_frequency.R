setwd("/Users/mgonline/Library/CloudStorage/GoogleDrive-guglielmo.inglese@unito.it/Il mio Drive/Anticausatives_project_UNITO_KULeuven/Anticausative_Romance/Dati_totale_final")

install.packages("tidyverse")

library(openxlsx)
library(tidyverse)
library(ggplot2)

data_antic <- read.xlsx("romall_new_VNC_pub.xlsx")

##Script for Figure 2

result_noncaus_meaning <- data_antic %>%
  group_by(language, meaning) %>%
  summarise(noncaus = sum(semantics == 'noncaus') * 100 / n())

plot_noncaus_tot <- ggplot(result_noncaus_meaning, aes(x = meaning, y = noncaus, color = language, group = language)) +
  geom_smooth(aes(y = noncaus, color = language), method = 'loess', se = FALSE) +
  labs(y = "% noncausal usage", x = "Causative prominence") +
  scale_color_manual(
    values = c("italian" = "gray70", "spanish" = "gray20"),
    labels = c("italian" = "Italian", "spanish" = "Spanish")) +
  coord_cartesian(ylim = c(1, 100)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)  # Rotate x-axis labels
  )

custom_order <- c("split", "close", "break", "open", "gather", "connect", "rock", "improve", "rise", "fill", "burn", "turn", "stop", "melt", "sink", "go out", "wake up", "dry", "freeze", "boil")
plot_noncaus_tot <- plot_noncaus_tot + scale_x_discrete(limits = custom_order)
print(plot_noncaus_tot)


##Calculate causalness degree

caus_degree_ita <- data_antic %>%
  filter (language == 'italian') %>%
  mutate(caus_degree = causalness_degree_apr25 * 100) %>% 
  select(meaning, vnc_period_apr25, caus_degree) %>% 
  group_by(meaning, vnc_period_apr25, caus_degree) %>%  
  mutate(caus_degree = round(caus_degree, 2))
  
caus_degree_es <- data_antic %>%
  filter (language == 'spanish') %>%
  mutate(caus_degree = causalness_degree_apr25 * 100) %>% 
  select(meaning, vnc_period_apr25, caus_degree) %>% 
  group_by(meaning, vnc_period_apr25, caus_degree) %>% 
  mutate(caus_degree = round(caus_degree, 2))


##Calculate percentage of anticausative marking

coding_percentages_ita <-  data_antic %>%
  group_by(
    meaning, semantics, coding, vnc_period_apr25) %>%
  filter (
    language == 'italian') %>%
  summarise(count = n()) %>%
  spread(
    coding, count, fill = 0) %>%
  mutate(
    caus_degree_vs_zero = caus * 100 / (caus + zero),
    prop_antic_vs_zero = antic * 100 / (antic + zero)) %>% 
  mutate(
    caus_degree_vs_zero = round(caus_degree_vs_zero, 2),
    prop_antic_vs_zero = round(prop_antic_vs_zero, 2))

coding_percentages_es <- data_antic %>%
  group_by(meaning, semantics, coding, vnc_period_apr25) %>%
  filter (language == 'spanish') %>%
  summarise(count = n()) %>%
  spread(coding, count, fill = 0) %>%
  mutate(caus_degree_vs_zero = caus * 100 / (caus + zero),
         prop_antic_vs_zero = antic * 100 / (antic + zero))%>% 
  mutate(
    caus_degree_vs_zero = round(caus_degree_vs_zero, 2),
    prop_antic_vs_zero = round(prop_antic_vs_zero, 2))

##Scatter Plot

merged_data_ita <- merge(caus_degree_ita, coding_percentages_ita, by = c("meaning", "vnc_period_apr25"))

merged_data_ita_noncaus <- merged_data_ita %>%
  filter(semantics == 'noncaus')

merged_data_ita_noncaus_1 <- merged_data_ita_noncaus %>%
  filter(vnc_period_apr25 == '1200-1409')

merged_data_ita_noncaus_2 <- merged_data_ita_noncaus %>%
  filter(vnc_period_apr25 == '1410-1549')

merged_data_ita_noncaus_3 <- merged_data_ita_noncaus %>%
  filter(vnc_period_apr25 == '1550-1619')

merged_data_ita_noncaus_4 <- merged_data_ita_noncaus %>%
  filter(vnc_period_apr25 == '1620-1968')

merged_data_es <- merge(caus_degree_es, coding_percentages_es, by = c("meaning", "vnc_period_apr25"))

merged_data_es_noncaus <- merged_data_es %>%
  filter(semantics == 'noncaus')

merged_data_es_noncaus_1 <- merged_data_es_noncaus %>%
  filter(vnc_period_apr25 == '1140-1349')

merged_data_es_noncaus_2 <- merged_data_es_noncaus %>%
  filter(vnc_period_apr25 == '1350-1419')

merged_data_es_noncaus_3 <- merged_data_es_noncaus %>%
  filter(vnc_period_apr25 == '1420-1699')

merged_data_es_noncaus_4 <- merged_data_es_noncaus %>%
  filter(vnc_period_apr25 == '1700-2001')

##Script for Figure 6

library(ggplot2)
library(ggrepel)

plot_ita_1 <- ggplot(merged_data_ita_noncaus_1, aes(x = caus_degree, y = prop_antic_vs_zero, label = meaning)) +
  geom_point() +
  labs(x = "Causalness",
       y = "%anticausative", title = "1200-1409") +
  theme_minimal() +
  theme(text = element_text(size = 14))

plot_ita_2 <- ggplot(merged_data_ita_noncaus_2, aes(x = caus_degree, y = prop_antic_vs_zero, label = meaning)) +
  geom_point() +
  labs(x = "Causalness",
       y = "%anticausative", title = "1410-1549") +
  theme_minimal() +
  theme(text = element_text(size = 14))

plot_ita_3 <- ggplot(merged_data_ita_noncaus_3, aes(x = caus_degree, y = prop_antic_vs_zero, label = meaning)) +
  geom_point() +
  labs(x = "Causalness",
       y = "%anticausative", title = "1420-1699") +
  theme_minimal() +
  theme(text = element_text(size = 14))

plot_ita_4 <- ggplot(merged_data_ita_noncaus_4, aes(x = caus_degree, y = prop_antic_vs_zero, label = meaning)) +
  geom_point() +
  labs(x = "Causalness",
       y = "%anticausative", title = "1700-2001") +
  theme_minimal() +
  theme(text = element_text(size = 14))

library(gridExtra)
grid.arrange(plot_ita_1, plot_ita_2, plot_ita_3, plot_ita_4, nrow = 2, ncol = 2)

##Script for Figure 7
plot_es_1 <- ggplot(merged_data_es_noncaus_1, aes(x = caus_degree, y = prop_antic_vs_zero, label = meaning)) +
  geom_point() +
  labs(x = "Causalness",
       y = "%anticausative", title = "1140-1349") +
  theme_minimal() +
  theme(text = element_text(size = 14))

plot_es_2 <- ggplot(merged_data_es_noncaus_2, aes(x = caus_degree, y = prop_antic_vs_zero, label = meaning)) +
  geom_point() +
  labs(x = "Causalness",
       y = "%anticausative", title = "1350-1419") +
  theme_minimal() +
  theme(text = element_text(size = 14))

plot_es_3 <- ggplot(merged_data_es_noncaus_3, aes(x = caus_degree, y = prop_antic_vs_zero, label = meaning)) +
  geom_point() +
  labs(x = "Causalness",
       y = "%anticausative", title = "1420-1699") +
  theme_minimal() +
  theme(text = element_text(size = 14))

plot_es_4 <- ggplot(merged_data_es_noncaus_4, aes(x = caus_degree, y = prop_antic_vs_zero, label = meaning)) +
  geom_point() +
  labs(x = "Causalness",
       y = "%anticausative", title = "1700-2001") +
  theme_minimal() +
  theme(text = element_text(size = 14))

library(gridExtra)
grid.arrange(plot_es_1, plot_es_2, plot_es_3, plot_es_4, nrow = 2, ncol = 2)

##Correlation for Italian
library(dplyr)
library(knitr)
library(kableExtra)

# Funzione per formattare i p-value con asterischi
format_pval <- function(p) {
  case_when(
    p < 0.001 ~ "< 0.001***",
    p < 0.01  ~ "< 0.01**",
    p < 0.05  ~ "< 0.05*",
    TRUE      ~ paste0("= ", round(p, 3))
  )
}

# Lista dei dataset per ciascun periodo
datasets <- list(
  Period1 = merged_data_ita_noncaus_1,
  Period2 = merged_data_ita_noncaus_2,
  Period3 = merged_data_ita_noncaus_3,
  Period4 = merged_data_ita_noncaus_4
)

# Costruzione della tabella
cor_table_ita <- purrr::imap_dfr(datasets, function(df, period) {
  test_result <- cor.test(df$caus_degree, df$prop_antic_vs_zero, method = "spearman", exact = FALSE)
  
  tibble(
    Period = period,
    Spearman_Correlation = round(test_result$estimate, 3),
    P_value = format_pval(test_result$p.value)
  )
})

# Creazione della tabella con kableExtra
cor_table_ita %>%
  kable("html", caption = "Spearman Correlation Between Causalness and Anticausative Usage by Period (Italian)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))




# Gu
cor_test_result_1_ita <- cor.test(merged_data_ita_noncaus_1$caus_degree, merged_data_ita_noncaus_1$prop_antic_vs_zero, method = "spearman", exact = FALSE)
cat("Spearman Correlation Coefficient:", cor_test_result_1_ita$estimate, "\n")
cat("P-value:", cor_test_result_1_ita$p.value, "\n")

cor_test_result_2_ita <- cor.test(merged_data_ita_noncaus_2$caus_degree, merged_data_ita_noncaus_2$prop_antic_vs_zero, method = "spearman", exact = FALSE)
cat("Spearman Correlation Coefficient:", cor_test_result_2_ita$estimate, "\n")
cat("P-value:", cor_test_result_2_ita$p.value, "\n")

cor_test_result_3_ita <- cor.test(merged_data_ita_noncaus_3$caus_degree, merged_data_ita_noncaus_3$prop_antic_vs_zero, method = "spearman", exact = FALSE)
cat("Spearman Correlation Coefficient:", cor_test_result_3_ita$estimate, "\n")
cat("P-value:", cor_test_result_3_ita$p.value, "\n")

cor_test_result_4_ita <- cor.test(merged_data_ita_noncaus_4$caus_degree, merged_data_ita_noncaus_4$prop_antic_vs_zero, method = "spearman", exact = FALSE)
cat("Spearman Correlation Coefficient:", cor_test_result_4_ita$estimate, "\n")
cat("P-value:", cor_test_result_4_ita$p.value, "\n")

##Correlation for Spanish
library(dplyr)
library(knitr)
library(kableExtra)

# Funzione per formattare i p-value con asterischi
format_pval <- function(p) {
  case_when(
    p < 0.001 ~ "< 0.001***",
    p < 0.01  ~ "< 0.01**",
    p < 0.05  ~ "< 0.05*",
    TRUE      ~ paste0("= ", round(p, 3))
  )
}

# Lista dei dataset per ciascun periodo (Spagnolo)
datasets_es <- list(
  Period1 = merged_data_es_noncaus_1,
  Period2 = merged_data_es_noncaus_2,
  Period3 = merged_data_es_noncaus_3,
  Period4 = merged_data_es_noncaus_4
)

# Costruzione della tabella con i risultati del test di correlazione
cor_table_es <- purrr::imap_dfr(datasets_es, function(df, period) {
  test_result <- cor.test(df$caus_degree, df$prop_antic_vs_zero, method = "spearman", exact = FALSE)
  
  tibble(
    Period = period,
    Spearman_Correlation = round(test_result$estimate, 3),
    P_value = format_pval(test_result$p.value)
  )
})

# Creazione della tabella formattata
cor_table_es %>%
  kable("html", caption = "Spearman Correlation Between Causalness and Anticausative Usage by Period (Spanish)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))

# Gu

cor_test_result_1_es <- cor.test(merged_data_es_noncaus_1$caus_degree, merged_data_es_noncaus_1$prop_antic_vs_zero, method = "spearman", exact = FALSE)
cat("Spearman Correlation Coefficient:", cor_test_result_1_es$estimate, "\n")
cat("P-value:", cor_test_result_1_es$p.value, "\n")

cor_test_result_2_es <- cor.test(merged_data_es_noncaus_2$caus_degree, merged_data_es_noncaus_2$prop_antic_vs_zero, method = "spearman", exact = FALSE)
cat("Spearman Correlation Coefficient:", cor_test_result_2_es$estimate, "\n")
cat("P-value:", cor_test_result_2_es$p.value, "\n")

cor_test_result_3_es <- cor.test(merged_data_es_noncaus_3$caus_degree, merged_data_es_noncaus_3$prop_antic_vs_zero, method = "spearman", exact = FALSE)
cat("Spearman Correlation Coefficient:", cor_test_result_3_es$estimate, "\n")
cat("P-value:", cor_test_result_3_es$p.value, "\n")

cor_test_result_4_es <- cor.test(merged_data_es_noncaus_4$caus_degree, merged_data_es_noncaus_4$prop_antic_vs_zero, method = "spearman", exact = FALSE)
cat("Spearman Correlation Coefficient:", cor_test_result_4_es$estimate, "\n")
cat("P-value:", cor_test_result_4_es$p.value, "\n")

##Script for Figure 8

caus_degree_ita_lemma <- data_antic %>%
  filter (language == 'italian') %>%
  group_by(lemma, vnc_period_apr25) %>%
  summarise(caus_degree = sum(semantics == 'caus') * 100 / n())

coding_percentages_ita_lemma <- data_antic %>%
  group_by(lemma, semantics, coding, vnc_period_apr25) %>%
  filter (language == 'italian') %>%
  summarise(count = n()) %>%
  spread(coding, count, fill = 0) %>%
  mutate(caus_degree_vs_zero = caus * 100 / (caus + zero),
         prop_antic_vs_zero = antic * 100 / (antic + zero))

merged_data_ita_lemma <- merge(caus_degree_ita_lemma, coding_percentages_ita_lemma, by = c("lemma", "vnc_period_apr25"))

merged_data_ita_lemma_noncaus <- merged_data_ita_lemma %>%
  filter(semantics == 'noncaus')

merged_data_ita_noncaus_4_lemma <- merged_data_ita_lemma_noncaus %>%
  filter(vnc_period_apr25 == '1620-1968')

library(ggplot2)
library(ggrepel)

burn <- c("ardere", "bruciare")
fill <- c("empiere", "riempire")
melt <- c("sciogliere", "fondere")
rise <- c("alzare", "sollevare")
rock <- c("dondolare", "oscillare")
split <- c("dividere", "separare")
stop <- c("arrestare", "fermare")
freeze <-c("gelare", "ghiacciare")

merged_data_ita_noncaus_4_lemma$group <- factor(NA, levels = c("burn", "fill", "freeze", "gather", "melt", "rise", "rock", "split", "stop"))

merged_data_ita_noncaus_4_lemma$group[merged_data_ita_noncaus_4_lemma$lemma %in% burn] <- "burn"
merged_data_ita_noncaus_4_lemma$group[merged_data_ita_noncaus_4_lemma$lemma %in% fill] <- "fill"
merged_data_ita_noncaus_4_lemma$group[merged_data_ita_noncaus_4_lemma$lemma %in% melt] <- "melt"
merged_data_ita_noncaus_4_lemma$group[merged_data_ita_noncaus_4_lemma$lemma %in% rise] <- "rise"
merged_data_ita_noncaus_4_lemma$group[merged_data_ita_noncaus_4_lemma$lemma %in% rock] <- "rock"
merged_data_ita_noncaus_4_lemma$group[merged_data_ita_noncaus_4_lemma$lemma %in% split] <- "split"
merged_data_ita_noncaus_4_lemma$group[merged_data_ita_noncaus_4_lemma$lemma %in% stop] <- "stop"
merged_data_ita_noncaus_4_lemma$group[merged_data_ita_noncaus_4_lemma$lemma %in% freeze] <- "freeze"



merged_data_ita_noncaus_4_lemma <- merged_data_ita_noncaus_4_lemma[!is.na(merged_data_ita_noncaus_4_lemma$group), ]

ggplot(merged_data_ita_noncaus_4_lemma, aes(x = caus_degree, y = prop_antic_vs_zero, label = lemma, color = group, fill = group)) +
  geom_point() +
  geom_text_repel(data = subset(merged_data_ita_noncaus_4_lemma, lemma %in% c(burn, fill, freeze, gather, melt, rise, rock, split, stop)),
                  aes(color = group),
                  hjust = 0.5, vjust = -0.5, size = 4) +
  labs(
    x = "Causalness",
    y = "%anticausative") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  scale_color_manual(name = "Meaning", values = c(burn = "red", fill = "blue", melt = "purple", rise = "brown", rock = "green", split = "cyan", stop = "orange", freeze="darkgrey" )) +
  scale_fill_manual(name = "Meaning", values = c(burn = "red", fill = "blue", melt = "purple", rise = "brown", rock = "green", split = "cyan", stop = "orange", freeze="darkgrey")) +
  guides(color = guide_legend(override.aes = list(shape = 16, size = 4)))













## New

library(tidyverse)
library(ggrepel)

# Step 1: Calculate causal degree for Italian lemmas
caus_degree_ita_lemma <- data_antic %>%
  filter(language == "italian") %>%
  group_by(lemma, vnc_period_apr25) %>%
  summarise(caus_degree = sum(semantics == "caus") * 100 / n(), .groups = "drop")

# Step 2: Calculate coding percentages
coding_percentages_ita_lemma <- data_antic %>%
  filter(language == "italian") %>%
  group_by(lemma, semantics, coding, vnc_period_apr25) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = coding, values_from = count, values_fill = 0) %>%
  mutate(
    caus_degree_vs_zero = caus * 100 / (caus + zero),
    prop_antic_vs_zero = antic * 100 / (antic + zero)
  )

# Step 3: Merge the datasets
merged_data_ita_lemma <- left_join(
  caus_degree_ita_lemma,
  coding_percentages_ita_lemma,
  by = c("lemma", "vnc_period_apr25")
)

# Step 4: Filter for noncausative semantics and specific time period
merged_data_ita_noncaus_4_lemma <- merged_data_ita_lemma %>%
  filter(semantics == "noncaus", vnc_period_apr25 == "1620-1968")

# Step 5: Create verb meaning groups
groupings <- list(
  burn = c("ardere", "bruciare"),
  fill = c("empiere", "riempire"),
  melt = c("sciogliere", "fondere"),
  rise = c("alzare", "sollevare"),
  rock = c("dondolare", "oscillare"),
  split = c("dividere", "separare"),
  stop = c("arrestare", "fermare"),
  freeze = c("gelare", "ghiacciare")
)

# Assign group labels
merged_data_ita_noncaus_4_lemma <- merged_data_ita_noncaus_4_lemma %>%
  mutate(group = map_chr(lemma, function(l) {
    matched_group <- names(keep(groupings, ~ l %in% .x))
    if (length(matched_group) > 0) matched_group else NA_character_
  })) %>%
  filter(!is.na(group)) %>%
  mutate(group = factor(group, levels = names(groupings)))

# Step 6: Plot
ggplot(merged_data_ita_noncaus_4_lemma, aes(
  x = caus_degree,
  y = prop_antic_vs_zero,
  label = lemma,
  color = group,
  fill = group
)) +
  geom_point() +
  geom_text_repel(
    data = merged_data_ita_noncaus_4_lemma %>%
      filter(lemma %in% unlist(groupings)),
    aes(color = group),
    hjust = 0.5,
    vjust = -0.5,
    size = 4
  ) +
  labs(
    x = "Causalness",
    y = "% Anticausative"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  scale_color_manual(
    name = "Meaning",
    values = c(
      burn = "red", fill = "blue", melt = "purple", rise = "brown",
      rock = "green", split = "cyan", stop = "orange", freeze = "darkgrey"
    )
  ) +
  scale_fill_manual(
    name = "Meaning",
    values = c(
      burn = "red", fill = "blue", melt = "purple", rise = "brown",
      rock = "green", split = "cyan", stop = "orange", freeze = "darkgrey"
    )
  ) +
  guides(color = guide_legend(override.aes = list(shape = 16, size = 4)))

