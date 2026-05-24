library(tidyverse)
library(writexl)

# Данная функция считает долю моды ответов
mode_share <- function(x) {
  x <- na.omit(x)
  if(length(x) == 0) return(NA)
  max(table(x)) / length(x)
}

# Загружаем данные
df <- read_csv("Отношение к использованию искусственного интеллекта в дерматологии.csv")

# Добавим ID
df <- df %>% mutate(ID = row_number(), .before = 1)

# Переименовываем колонки
df_prep <- df %>%
  rename(
    age = starts_with("Укажите Ваш возраст"),
    location = starts_with("Укажите тип населённого пункта"),
    fin_expenses = starts_with("Если у Вас возникнут непредвиденные"),
    fin_overall = starts_with("Как Вы оцениваете своё финансовое"),
    diagnoses = starts_with("Были ли у Вас диагностированы"),
    prob_current = starts_with("В настоящее время у меня есть"),                   # q1
    prob_past = starts_with("Ранее у меня были значимые"),                         # q2
    privacy_critical = starts_with("Для меня критична конфиденциальность"),        # q9
    privacy_worried = starts_with("Я обеспокоен(а) вопросами конфиденциальности"), # q18
    attention_check = starts_with("Для контроля внимательности"),                  # q15
    
    q3 = starts_with("Я регулярно"),
    q4 = starts_with("Состояние кожи"),
    q5 = starts_with("ИИ может быть"),
    q6 = starts_with("Я доверяю"),
    q7 = starts_with("ИИ должен"),
    q8 = starts_with("Я готов"),
    q10 = starts_with("Мне важно понимать"),
    q11 = starts_with("Я бы попробовал"),
    q12 = starts_with("Внедрение ИИ"),
    q13 = starts_with("Риск ошибки"),
    q14 = starts_with("Использование врачом"),
    q16 = starts_with("ИИ способен сократить"),
    q17 = starts_with("ИИ способен снизить")
  ) %>%
  # Переводим шкалы Ликерста и возраст в числовой формат
  mutate(across(c(age, prob_current, prob_past, privacy_critical, privacy_worried, attention_check), as.numeric))

likert_cols <- c(
  "prob_current", "prob_past", "q3", "q4", "q5", "q6", "q7", "q8",
  "privacy_critical", "q10", "q11", "q12", "q13", "q14",
  "attention_check", "q16", "q17", "privacy_worried"
)

# Фильтрация данных
df_cleaned <- df_prep %>%
  filter(
    attention_check == 4, # nolint
    age <= 27,
    !location %in% c("Город с населением менее 100 тыс человек", "Сельская местность / посёлок городского типа")
  ) %>%

# Штрафуем
  mutate(
    # Штраф 1
    penalty_finance = case_when(
      fin_overall == "Часто испытываю нехватку средств" & 
        fin_expenses == "Сможете оплатить без серьёзных проблем" ~ 1,
      
      fin_overall == "Хватает на всё необходимое и значительную часть желаемого" & 
        fin_expenses == "Не сможете оплатить без заимствований / помощи" ~ 1,
      
      TRUE ~ 0
    ),
    
    # Штраф 2
    penalty_diagnosis_logic = ifelse(
      str_detect(diagnoses, "Никогда не диагностировались") & str_detect(diagnoses, ";"),
      1, 0
    ),
    
    # Штраф 3
    penalty_past_problems = case_when(
      # 2 балла – диагноз есть, но человек говорит, что проблем нет ни сейчас, ни в прошлом (ответ 1-2)
      !str_detect(diagnoses, "Никогда не диагностировались") & 
        !str_detect(diagnoses, "^Затрудняюсь ответить$") &
        prob_current <= 2 & prob_past <= 2 ~ 2,
      
      TRUE ~ 0
    ),
    
    # Штраф 4
    penalty_current_vs_past = case_when(
      # 2 балла – оценки 5 и 5
      prob_current == 5 & prob_past == 5 ~ 2,
      # 1 балл – оценки 5 и 4
      (prob_current == 5 & prob_past == 4) | (prob_current == 4 & prob_past == 5) ~ 1,
      TRUE ~ 0
    ),
    
    # Штраф 5
    penalty_privacy = case_when(
      # 2 балла - разница 4
      abs(privacy_critical - privacy_worried) == 4 ~ 2,
      # 1 балл - разница 3
      abs(privacy_critical - privacy_worried) == 3 ~ 1,
      TRUE ~ 0
    ),
    
    # Сумма штрафов
    total_penalty = penalty_finance + penalty_diagnosis_logic + penalty_past_problems + penalty_current_vs_past + penalty_privacy,
    
    
    # Исключим straightlining 
    
    # Используя стандартное отклонение
    straightlining_sd = apply(
      select(., all_of(likert_cols)),
      1,
      sd,
      na.rm = TRUE
    ),
    
    # Используя долю моды ответов
    straightlining_mode = apply(
      select(., all_of(likert_cols)),
      1,
      mode_share
    ),
    
    # Флаг
    straightliner_flag = ifelse(
      (!is.na(straightlining_sd) & straightlining_sd < 0.5) |
      (!is.na(straightlining_mode) & straightlining_mode > 0.8),
      1,
      0
    )
  )

cat("\n==========================================================\n")
cat("СТАТИСТИКА СРАБАТЫВАНИЯ КАЖДОГО ШТРАФА\n")
cat("==========================================================\n")
penalty_stats <- df_cleaned %>%
  summarise(
    `Штр1` = sum(penalty_finance > 0),
    `Штр2` = sum(penalty_diagnosis_logic > 0),
    `Штр3` = sum(penalty_past_problems > 0),
    `Штр4` = sum(penalty_current_vs_past > 0),
    `Штр5` = sum(penalty_privacy > 0)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Тип штрафа", values_to = "Кол-во человек")

print(as.data.frame(penalty_stats))

cat("\n==========================================================\n")
cat("РАСПРЕДЕЛЕНИЕ ОБЩИХ ШТРАФНЫХ БАЛЛОВ\n")
cat("==========================================================\n")
penalty_distribution <- as.data.frame(table(df_cleaned$total_penalty))
colnames(penalty_distribution) <- c("Штрафной балл", "Количество анкет")
print(penalty_distribution)

cat("\n==========================================================\n")
cat("АНКЕТЫ СО ШТРАФОМ БОЛЕЕ ТРЕХ И ИХ НАРУШЕНИЯ\n")
cat("==========================================================\n")
removed_respondents <- df_cleaned %>%
  filter(total_penalty >= 3) %>%
  select(ID, starts_with("penalty_"), total_penalty) %>%
  rename(
    `Штр1` = penalty_finance,
    `Штр2` = penalty_diagnosis_logic,
    `Штр3` = penalty_past_problems,
    `Штр4` = penalty_current_vs_past,
    `Штр5` = penalty_privacy,
    `пенальти` = total_penalty
  )

print(as.data.frame(removed_respondents))
cat("==========================================================\n\n")

cat("\n==========================================================\n")
cat("STRAIGHTLINING\n")
cat("==========================================================\n")
print(table(df_cleaned$straightliner_flag))

# Удаляем анкеты со штрафом >= 3
df_final <- df_cleaned %>%
  filter(total_penalty < 3, straightliner_flag == 0) %>%
  # Уберем служебные колонки, ID оставляем
  select(-starts_with("penalty_"), -total_penalty, -attention_check)

cat("\n==========================================================\n")
cat("ИТОГОВЫЙ РЕЗУЛЬТАТ\n")
cat("==========================================================\n")
cat("Всего строк до чистки:", nrow(df), "\n")
cat("Осталось чистых анкет:", nrow(df_final), "\n")

write_csv(df_final, "cleaned_data_dermatology.csv")
write_xlsx(df_final, "cleaned_data_dermatology.xlsx")
print(nrow(df_final))