# Загрузка необходимых библиотек
suppressPackageStartupMessages({
  library(dplyr)
  library(psych)
  library(nnet)
  library(tidyr)
})

# Загрузка данных
df <- read.csv("cleaned_data_dermatology_Final.csv", stringsAsFactors = FALSE)

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

df <- df %>%
  mutate(
    Income_1 = recode_q1(df[[8]]),
    Income_2 = recode_q2(df[[9]]),
    Income_3 = recode_q3(df[[10]])
  )

# Очистка от пропущенных значений
df_finance <- df %>% filter(!is.na(Income_1) & !is.na(Income_2) & !is.na(Income_3))

cat("\n==========================================================\n")
cat("НАДЕЖНОСТЬ ШКАЛЫ ОЦЕНКИ ДОХОДА\n")
cat("==========================================================\n")

cat("\n--- Корреляционная матрица Пирсона ---\n")
print(cor(df_finance[, c("Income_1", "Income_2", "Income_3")]))

cat("\n--- Коэффициент Альфа Кронбаха ---\n")
alpha_res <- psych::alpha(df_finance[, c("Income_1", "Income_2", "Income_3")], check.keys = FALSE)
print(alpha_res$total)

cat("\n==========================================================\n")
cat("РАСПРЕДЕЛЕНИЕ ВЫБОРКИ ПО ДОХОДУ\n")
cat("==========================================================\n")

# Расчет индекса дохода
df_finance$Income_Score <- df_finance$Income_1 + df_finance$Income_2 + df_finance$Income_3
median_income <- median(df_finance$Income_Score, na.rm = TRUE)

# Разделение на группы
df_finance$Income_Group <- ifelse(df_finance$Income_Score <= median_income, "Low", "High")

cat("\n--- Размеры групп ---\n")
print(table(df_finance$Income_Group))

cat("\n--- Описательная статистика индекса дохода по группам ---\n")
print(tapply(df_finance$Income_Score, df_finance$Income_Group, summary))

# Функция для определения преобладающего выбора для каждого респондента
get_majority_choice <- function(row_data) {
  choices <- as.character(row_data)
  choices <- choices[!is.na(choices) & choices != "NA" & choices != ""]
  if(length(choices) == 0) return(NA)
  tbl <- table(choices)
  return(names(tbl)[which.max(tbl)])
}

# Применяем к столбцам DCE
df_finance$Majority_Choice <- apply(df_finance[, 31:46], 1, get_majority_choice)

# Удаляем респондентов без определенного выбора
df_finance <- df_finance %>% filter(!is.na(Majority_Choice))

# Определяем ценовую группу
df_finance$Price_Group <- as.factor(df_finance[[30]])

cat("\n==========================================================\n")
cat("ПРОВЕРКА БАЛАНСА\n")
cat("==========================================================\n")

price_income_table <- table(df_finance$Income_Group, df_finance$Price_Group)
cat("\n--- Таблица сопряженности ---\n")
print(price_income_table)

cat("\n--- Тест Хи-квадрат на независимость ---\n")
print(chisq.test(price_income_table))

cat("\n==========================================================\n")
cat("ПРОВЕРКА ГИПОТЕЗЫ: ТОЧНЫЙ ТЕСТ ФИШЕРА\n")
cat("==========================================================\n")

# Убираем гибрид
df_binary <- df_finance %>% filter(Majority_Choice %in% c("Доктор", "ИИ"))

# Формируем таблицу сопряженности
table_fisher <- table(df_binary$Income_Group, df_binary$Majority_Choice)
rownames(table_fisher) <- c("Высокий доход (High)", "Низкий доход (Low)")

# Упорядочиваем столбцы: ИИ, затем Доктор
table_fisher <- table_fisher[, c("ИИ", "Доктор")]

cat("\n--- Таблица сопряженности для теста Фишера ---\n")
print(table_fisher)

# Применяем точный тест Фишера
fisher_res <- fisher.test(table_fisher, alternative = "greater")
cat("\n--- Результаты теста Фишера ---\n")
print(fisher_res)

cat("\n==========================================================\n")
cat("ПРОВЕРКА ГИПОТЕЗЫ: МУЛЬТИНОМИАЛЬНАЯ РЕГРЕССИЯ\n")
cat("==========================================================\n")

# Возвращаем гибридный формат
df_multi <- df_finance

# База для зависимой переменной - "ИИ", база для предиктора - "Low"
df_multi$Majority_Choice <- as.factor(df_multi$Majority_Choice)
df_multi$Majority_Choice <- relevel(df_multi$Majority_Choice, ref = "ИИ")

df_multi$Income_Group <- as.factor(df_multi$Income_Group)
df_multi$Income_Group <- relevel(df_multi$Income_Group, ref = "Low")

# Построение модели
multi_model <- multinom(Majority_Choice ~ Income_Group + Price_Group, 
                        data = df_multi, 
                        trace = FALSE)

print_multinom <- function(model) {
  coefs <- summary(model)$coefficients
  stderrs <- summary(model)$standard.errors
  z_values <- coefs / stderrs
  p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
  
  get_signif_stars <- function(p) {
    sapply(p, function(x) {
      if (x < 0.001) return("***")
      if (x < 0.01)  return("**")
      if (x < 0.05)  return("*")
      if (x < 0.1)   return(".")
      return(" ")
    })
  }
  
  for (dv_level in rownames(coefs)) {
    cat(sprintf("\n--- Зависимая переменная: %s ---\n", dv_level))
    
    # Формируем датафрейм
    res_table <- data.frame(
      Estimate  = sprintf("%8.3f", coefs[dv_level, ]),
      Std.Error = sprintf("%8.3f", stderrs[dv_level, ]),
      z.value   = sprintf("%8.3f", z_values[dv_level, ]),
      p.value   = sprintf("%8.4f", p_values[dv_level, ]),
      Signif    = get_signif_stars(p_values[dv_level, ])
    )
    
    print(res_table, quote = FALSE)
  }
}

print_multinom(multi_model)



# ==========================================================
# Визуализации
# ==========================================================

source("visuals.R")

# ==========================================================
# РАСПРЕДЕЛЕНИЕ ИНДЕКСА ДОХОД
# ==========================================================

df_plot2 <- df_finance %>%
  mutate(
    # Привязываем цвета к Low и High
    Income_Group_Label = factor(Income_Group, 
                                levels = c("Low", "High"), 
                                labels = c("Низкий доход (Low)", "Высокий доход (High)"))
  )

p_income_dist <- ggplot(df_plot2, aes(x = factor(Income_Score), fill = Income_Group)) +
  geom_bar(width = 0.7, color = "black", linewidth = 0.1) +
  
  # Подписи количества респондентов над столбцами
  geom_text(
    stat = "count", 
    aes(label = after_stat(count)), 
    vjust = -0.5, 
    size = 3.5, 
    fontface = "bold"
  ) +
  
  scale_fill_temp() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    x = "Суммарный индекс финансового положения (баллы)",
    y = "Количество респондентов",
    fill = "Группа дохода"
  )

# Отображаем и сохраняем
print(p_income_dist)
ggsave_temp("income_distribution.png", p_income_dist)

# ==========================================================
# ГРАФИК ЭФФЕКТОВ
# ==========================================================

# Создаем координатную сетку из всех комбинаций предикторов
pred_grid <- expand.grid(
  Income_Group = factor(c("Low", "High"), levels = c("Low", "High")),
  Price_Group = factor(levels(df_multi$Price_Group))
)

# Вычисляем предсказанные вероятности
predicted_probs <- predict(multi_model, newdata = pred_grid, type = "probs")

# Объединяем предикторы с предсказаниями, переводим в длинный формат и считаем логиты
df_plot3 <- cbind(pred_grid, as.data.frame(predicted_probs)) %>%
  pivot_longer(
    cols = c("ИИ", "Доктор", "Доктор и ИИ"),
    names_to = "Choice",
    values_to = "Probability"
  ) %>%
  mutate(
    Prob_clamped = pmax(pmin(Probability, 0.999), 0.001),
    Logit = log(Prob_clamped / (1 - Prob_clamped)),
    
    Income_Group_Label = factor(Income_Group, 
                                levels = c("Low", "High"), 
                                labels = c("Низкий доход", "Высокий доход")),
    Choice = factor(Choice, levels = c("ИИ", "Доктор и ИИ", "Доктор"))
  )

# Построение графика
p_effects <- ggplot(df_plot3, aes(x = Income_Group_Label, y = Logit, color = Choice, group = Choice)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  
  geom_text(
    aes(label = format_perc(Probability)),
    vjust = -1.2,
    size = 3,
    show.legend = FALSE,
    fontface = "bold"
  ) +
  
  scale_color_temp() +
  
  # Разделяем на панели по ценовым группам
  facet_wrap(~ Price_Group, labeller = labeller(Price_Group = c("Нечетная" = "Опросник: Нечетный вариант", 
                                                                "Четная" = "Опросник: Четный вариант"))) +
  
  # Настраиваем ось Y
  scale_y_continuous(
    expand = expansion(mult = c(0.1, 0.2)) 
  ) +
  
  labs(
    x = "Уровень дохода",
    y = "Log-Odds предсказанной вероятности",
    color = "Формат консультации"
  )

# Отображаем и сохраняем
print(p_effects)
ggsave_temp("predicted_probabilities_logit.png", p_effects)