suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lme4)
})

# Загрузка данных
df <- read.csv("cleaned_data_dermatology_Final.csv", stringsAsFactors = FALSE)

df$Price_Group <- ifelse(df[[30]] == "Четная", "Low", "High")

# Перекодируем вопросы о доходе в шкалу
recode_q1 <- function(x) {
  case_when(
    grepl("Испытываю серьёзные трудности", x) ~ 1,
    grepl("Хватает, но приходится на многом экономить", x) ~ 2,
    grepl("Вполне хватает", x) ~ 3,
    TRUE ~ NA_real_
  )
}

recode_q2 <- function(x) {
  case_when(
    grepl("Не сможете оплатить без заимствований", x) ~ 1,
    grepl("Сможете оплатить, но это будет ощутимо", x) ~ 2,
    grepl("Сможете оплатить без серьёзных проблем", x) ~ 3,
    TRUE ~ NA_real_
  )
}

recode_q3 <- function(x) {
  case_when(
    grepl("Часто испытываю нехватку средств", x) ~ 1,
    grepl("Хватает на базовые нужды, крупные покупки затруднительны", x) ~ 2,
    grepl("Хватает на всё необходимое и значительную часть желаемого", x) ~ 3,
    TRUE ~ NA_real_
  )
}

# Подготовка данных респондентов
df <- df %>%
  mutate(
    Income_1 = recode_q1(df[[8]]),
    Income_2 = recode_q2(df[[9]]),
    Income_3 = recode_q3(df[[10]]),
    Respondent_ID = row_number()
  ) %>%
  filter(!is.na(Income_1) & !is.na(Income_2) & !is.na(Income_3))

df$Income_Score <- df$Income_1 + df$Income_2 + df$Income_3
median_income <- median(df$Income_Score, na.rm = TRUE)
df$Income_Group <- ifelse(df$Income_Score <= median_income, "Low", "High")

# Преобразуем в длинный формат (DCE)
dce_start <- 31
dce_end <- 46
dce_col_names <- names(df)[dce_start:dce_end]

suppressWarnings({
  df_long <- df %>%
    select(Respondent_ID, Price_Group, Income_Score, Income_Group, all_of(dce_col_names)) %>%
    pivot_longer(
      cols = all_of(dce_col_names),
      names_to = "Card_Number",
      values_to = "Choice"
    ) %>%
    mutate(
      Card_Number = as.numeric(gsub("X", "", gsub("\\.", "", Card_Number))) - 30
    ) %>%
    filter(!is.na(Choice) & Choice != "" & Choice != "NA")
})

# Подготовка факторов и бинарной переменной (1 - только ИИ, 0 - иначе)
df_long <- df_long %>%
  mutate(
    AI_Solo = ifelse(Choice == "ИИ", 1, 0),
    Choice_Factor = factor(Choice, levels = c("ИИ", "Доктор", "Доктор и ИИ")),
    Price_Group = factor(Price_Group, levels = c("Low", "High")),
    Income_Group = factor(Income_Group, levels = c("Low", "High"))
  )

cat("\n==========================================================\n")
cat("ОПИСАТЕЛЬНАЯ СТАТИСТИКА\n")
cat("==========================================================\n")

cat("Размер датасета в длинном формате:", nrow(df_long), "наблюдений\n")
cat("Количество уникальных респондентов:", length(unique(df_long$Respondent_ID)), "\n")
cat("\nРаспределение выборов (все карточки):\n")
print(table(df_long$Choice_Factor))

cat("\n--- Доля выбора ИИ-соло по ценовым группам ---\n")
ai_by_price <- df_long %>%
  group_by(Price_Group) %>%
  summarise(
    Total_Choices = n(),
    AI_Solo_Count = sum(AI_Solo),
    AI_Solo_Prop = mean(AI_Solo),
    SE = sqrt(AI_Solo_Prop * (1 - AI_Solo_Prop) / Total_Choices)
  )
print(as.data.frame(ai_by_price))

cat("\n==========================================================\n")
cat("ПРОВЕРКА ГИПОТЕЗЫ: ТОЧНЫЙ ТЕСТ ФИШЕРА\n")
cat("==========================================================\n")

# Формируем таблицу сопряженности цена vs выбор
table_fisher <- table(df_long$Price_Group, df_long$AI_Solo)

# Переименовываем и упорядочиваем для удобства интерпретации 
rownames(table_fisher) <- c("Низкая цена (Low)", "Высокая цена (High)")
colnames(table_fisher) <- c("С участием врача (0)", "Только ИИ (1)")
table_fisher <- table_fisher[, c("Только ИИ (1)", "С участием врача (0)")]

cat("\n--- Таблица сопряженности (Количество выборов) ---\n")
print(table_fisher)

# alternative = "greater" проверяет, что шансы выбрать ИИ в группе Low выше, чем в High
fisher_res <- fisher.test(table_fisher, alternative = "greater")
cat("\n--- Результаты точного теста Фишера ---\n")
print(fisher_res)

cat("\n==========================================================\n")
cat("ПРОВЕРКА ГИПОТЕЗЫ: РЕГРЕССИЯ СО СМЕШАННЫМИ ЭФФЕКТАМИ\n")
cat("==========================================================\n")

# Модель 1: Только цена
cat("--- Модель 1: Влияние цены на вероятность выбора соло ИИ ---\n")
model1 <- glmer(
  AI_Solo ~ Price_Group + (1 | Respondent_ID),
  data = df_long,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
print(summary(model1))

# Odds Ratio для Модели 1
or1 <- exp(fixef(model1))
ci1 <- exp(suppressMessages(confint(model1, parm = "beta_", method = "Wald")))

cat("\nОтношение шансов (Odds Ratios) - Модель 1:\n")
cat("Intercept (Low Price):", round(or1[1], 3), "\n")
cat("High Price vs Low Price:", round(or1[2], 3), "\n")
cat("95% CI: [", round(ci1[2,1], 3), ",", round(ci1[2,2], 3), "]\n")

if (or1[2] < 1) {
  cat("   -> При высокой цене шансы выбрать ИИ-соло СНИЖАЮТСЯ на", round((1 - or1[2]) * 100, 1), "%\n")
} else {
  cat("   -> При высокой цене шансы выбрать ИИ-соло ПОВЫШАЮТСЯ на", round((or1[2] - 1) * 100, 1), "%\n")
}

# Модель 2: Цена + Доход
cat("\n--- Модель 2: Влияние цены с контролем на доход ---\n")
model2 <- glmer(
  AI_Solo ~ Price_Group + Income_Group + (1 | Respondent_ID),
  data = df_long,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
print(summary(model2))

# Модель 3: Со взаимодействием
cat("\n--- Модель 3: Со взаимодействием Цена х Доход ---\n")
model3 <- glmer(
  AI_Solo ~ Price_Group * Income_Group + (1 | Respondent_ID),
  data = df_long,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
print(summary(model3))

# Сравнение моделей
cat("\n--- Сравнение моделей ---\n")
cat("Информационный критерий Акаике (AIC):\n")
cat("Модель 1 (только цена):", round(AIC(model1), 2), "\n")
cat("Модель 2 (цена + доход):", round(AIC(model2), 2), "\n")
cat("Модель 3 (с взаимодействием):", round(AIC(model3), 2), "\n")

cat("\nLikelihood Ratio Test:\n")
anova_result <- anova(model1, model2, model3)
print(anova_result)

best_model_idx <- which.min(c(AIC(model1), AIC(model2), AIC(model3)))
best_model_name <- c("Модель 1", "Модель 2", "Модель 3")[best_model_idx]
cat("\nЛучшая модель по метрике AIC:", best_model_name, "\n")



# ==========================================================
# Визуализации
# ==========================================================

source("visuals.R") 

# Подключаем файл с настройками стилей и цветов
source("visuals.R") 

# Необходимые библиотеки
library(ggplot2)
library(dplyr)
library(scales)

# ==============================================================================
# СТОЛБЧАТАЯ ДИАГРАММА С ДОВЕРИТЕЛЬНЫМИ ИНТЕРВАЛАМИ (95% ДИ)
# ==============================================================================

# Расчет долей и 95% доверительных интервалов
df_bar_ci <- df_long %>%
  group_by(Price_Group) %>%
  summarise(
    Prop = mean(AI_Solo),
    SE = sqrt(Prop * (1 - Prop) / n()),
    .groups = 'drop'
  ) %>%
  mutate(
    Lower = Prop - 1.96 * SE,
    Upper = Prop + 1.96 * SE,
    color_group = as.character(Price_Group)
  )

p_bar_ci <- ggplot(df_bar_ci, aes(x = Price_Group, y = Prop, fill = color_group)) +
  # Столбчатая диаграмма (средние значения)
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) +
  
  # Доверительные интервалы
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1, linewidth = 0.8, color = "#1B263B") +
  
  # Текстовые подписи с процентами
  geom_text(aes(y = Upper, label = format_perc(Prop)), vjust = -0.8, size = 3.5, family = "serif") +
  
  # Палитра
  scale_fill_temp() + 
  scale_x_discrete(labels = c("Low" = "Низкая цена", "High" = "Высокая цена")) +
  
  # Форматирование оси Y
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(df_bar_ci$Upper) * 1.2),
    expand = expansion(mult = c(0, 0))
  ) +
  
  labs(
    x = labels["Price_Group"],
    y = "Доля выбора «Только ИИ» с 95% ДИ"
  )

# Отображаем и сохраняем
print(p_bar_ci)
ggsave_temp("bar_with_ci.png", p_bar_ci)