
#ETAP 1 - PRZYGOTOWANIE DANYCH (WCZYTANIE PACZEK, ZALADOWANIE DANYCH, ZMIANA DANYCH )
#* " ..count.. " trzeba w przyszlosci zamienic bo ggplot nie bedzie juz obslugiwaj tego fragmentu
#* #przez co wykresy sie nie stworzą 
#################################################################################################################
#################################################################################################################

#wczytywanie potrzebnych paczek do R'a
# Lista pakietów
packages <- c(
  "dplyr", "ggplot2", "scorecard", "stringi", "rpart", "rpart.plot",
  "ROCR", "corrplot", "ggcorrplot", "lsr", "reshape2", "randomForest", "ranger"
)


# Instalacja brakujących pakietów
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
lapply(packages, install_if_missing)

# Załadowanie pakietów
lapply(packages, library, character.only = TRUE)

#Wczytanie danych 
gym <- read.csv("C:/Users/PC/Desktop/IRD/gym_members_exercise_tracking.csv")
#Przygotowanie danych do dalszej pracy (zamiana na zmienne kategoryczne i grupy, żeby
#nie było np. 800 słupków - tak będzie to czytelniejsze)
calories_burned_median <- median(gym$Calories_Burned)

clean_gym <- gym %>%
  mutate( IlośćSpalonychKalorii = factor(ifelse(Calories_Burned <= calories_burned_median, "893 kalorie lub mniej", "894 kalorie lub więcej")),
    Calories_Burned = ifelse(Calories_Burned <= calories_burned_median, 0, 1))


#ustalanie poziomu zmiennej do macierzy bledow
clean_gym$Calories_Burned <- factor(clean_gym$Calories_Burned, levels = c("1", "0"))

#Tworzenie grup dla zmiennych
clean_gym$Age <- cut(clean_gym$Age,
                     breaks = c(18, 27, 37, 47, 59),
                     labels = c("18-27", "28-37", "38-47", "48-59"),
                     right = TRUE,
                     include.lowest = TRUE)

clean_gym$Weight..kg. <- cut(clean_gym$Weight..kg.,
                    breaks = seq(40, 130, length.out = 5),  
                    include.lowest = TRUE, right = TRUE,
                    labels = c("40–62.5", "62.5–85", "85–107.5", "107.5–130"))

clean_gym$Height..m. <- cut(clean_gym$Height..m.,
                       breaks = seq(1.5, 2, length.out = 5),
                       include.lowest = TRUE,
                       labels = c("1.5–1.625", "1.625–1.75", "1.75–1.875", "1.875–2"))

clean_gym$Max_BPM <- cut(clean_gym$Max_BPM,
                               breaks = seq(160, 200, length.out = 5),
                               include.lowest = TRUE,
                               labels = c("160–170", "170–180", "180–190", "190–200"))

clean_gym$Avg_BPM <- cut(clean_gym$Avg_BPM,
                               breaks = seq(120, 170, length.out = 5),
                               include.lowest = TRUE,
                               labels = c("120–132.5", "132.5–145", "145–157.5", "157.5–170"))

clean_gym$Resting_BPM <- cut(clean_gym$Resting_BPM,
                                   breaks = seq(50, 74, length.out = 5),
                                   include.lowest = TRUE,
                                   labels = c("50–56", "56–62", "62–68", "68–74"))

clean_gym$Session_Duration..hours. <- cut(clean_gym$Session_Duration..hours.,
                                        breaks = seq(0.5, 2, length.out = 5),
                                        include.lowest = TRUE,
                                        labels = c("0.5–0.875", "0.875–1.25", "1.25–1.625", "1.625–2"))

clean_gym$Fat_Percentage <- cut(clean_gym$Fat_Percentage,
                                      breaks = seq(10, 36, length.out = 5),
                                      include.lowest = TRUE,
                                      labels = c("10–16.5", "16.5–23", "23–29.5", "29.5–36"))


clean_gym$Water_Intake..liters. <- cut(clean_gym$Water_Intake..liters.,
                                    breaks = seq(1.4, 3.8, length.out = 5),
                                    include.lowest = TRUE,
                                    labels = c("1.4–1.975", "1.975–2.55", "2.55–3.125", "3.125–3.8"))

clean_gym$BMI<- cut(clean_gym$BMI,
                           breaks = seq(10, 50, length.out = 5),
                           include.lowest = TRUE,
                           labels = c("10–20", "20–30", "30–40", "40–50"))



# ETAP2 : TWORZENIE WYKRESOW ( WYKRESY POKAZUJACE ROZKLADY )
#################################################################################################################
#################################################################################################################


#Wykres zmiennej objaśnianej do raportu
ggplot(clean_gym, aes(
  x = factor(IlośćSpalonychKalorii, levels = c("894 kalorie lub więcej", "893 kalorie lub mniej")),
  fill = IlośćSpalonychKalorii)) +
  geom_bar() +  
  labs(title = "Rozkład zmiennej objaśnianej",
       y = "Ilość osób",
       x = NULL,
       fill = "Ilość spalonych kalorii") +  
  scale_y_continuous(limits = c(0, 973)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4) +  
  scale_fill_manual(
    values = c(
      "894 kalorie lub więcej" = "#1F77B4",
      "893 kalorie lub mniej" = "#E63946"
    ),
    breaks = c("894 kalorie lub więcej", "893 kalorie lub mniej")  # kolejność w legendzie
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold", margin = ggplot2::margin(t = 10)),  
    axis.title.y = element_text(size = 12, face = "bold", margin = ggplot2::margin(r = 10)), 
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = ggplot2::margin(b = 10)),
    axis.text.x = element_blank()
  )


#lista, żeby nie było bałaganu
distribution_list <- list()

#Wykresy zmiennych objaśniających

#1 Age
distribution_list$Age <- clean_gym %>%
  select(Age, Calories_Burned) %>%
  group_by(Age) %>%
  count(Calories_Burned)

ggplot(distribution_list$Age, aes(x = Age, y = n, fill = factor(Calories_Burned))) + 
  geom_col(position = position_dodge()) +  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +                     
  scale_fill_manual(
    values = c("0" = "#E63946",  
               "1" = "#1F77B4"),
    name = "Ilość spalonych kalorii",
    labels = c("0" = "893 kalorie lub mniej", "1" = "894 kalorie lub więcej")
  ) +
  ylim(0, 300) + 
  theme_minimal() +
  labs(x = "Grupa wiekowa", y = "Ilość wystąpień", title = "Rozkład ilości spalonych kalorii wg grup wiekowych") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 15)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15))  
  )

#2 Gender

distribution_list$Gender <- clean_gym %>%
  select(Gender, Calories_Burned) %>%
  group_by(Gender) %>%
  count(Calories_Burned)

ggplot(distribution_list$Gender,  aes(x = Gender, y = n, fill = factor(Calories_Burned))) + 
  geom_col(position = position_dodge()) +  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +                     
  scale_fill_manual(
    values = c("0" = "#E63946",  
               "1" = "#1F77B4"),
    name = "Ilość spalonych kalorii",
    labels = c("0" = "893 kalorie lub mniej", "1" = "894 kalorie lub więcej")
  ) +
  ylim(0, 300) + 
  theme_minimal() +
  labs(x = "Płeć", y = "Ilość wystąpień", title = "Rozkład ilości spalonych kalorii wg płci") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 15)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15))  
  )


#3 Weight

distribution_list$Weight <- clean_gym %>%
  select(Weight..kg., Calories_Burned) %>%
  group_by(Weight..kg.) %>%
  count(Calories_Burned)

ggplot(distribution_list$Weight,  aes(x = Weight..kg., y = n, fill = factor(Calories_Burned))) + 
  geom_col(position = position_dodge()) +  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +                     
  scale_fill_manual(
    values = c("0" = "#E63946",  
               "1" = "#1F77B4"),
    name = "Ilość spalonych kalorii",
    labels = c("0" = "893 kalorie lub mniej", "1" = "894 kalorie lub więcej")
  ) +
  ylim(0, 300) + 
  theme_minimal() +
  labs(x = "Waga", y = "Ilość wystąpień", title = "Rozkład ilości spalonych kalorii wg wagi") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold",margin = ggplot2::margin(b = 15)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15))  
  )

#4 Height 

distribution_list$Height <- clean_gym %>%
  select(Height..m., Calories_Burned) %>%
  group_by(Height..m.) %>%
  count(Calories_Burned)

ggplot(distribution_list$Height,  aes(x = Height..m., y = n, fill = factor(Calories_Burned))) + 
  geom_col(position = position_dodge()) +  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +                     
  scale_fill_manual(
    values = c("0" = "#E63946",  
               "1" = "#1F77B4"),
    name = "Ilość spalonych kalorii",
    labels = c("0" = "893 kalorie lub mniej", "1" = "894 kalorie lub więcej")
  ) +
  ylim(0, 300) + 
  theme_minimal() +
  labs(x = "Wzrost (m)", y = "Ilość wystąpień", title = "Rozkład ilości spalonych kalorii wg wzrostu (m)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 15)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15))  
  )

# 5) Max_BPM 
distribution_list$Max_BPM <- clean_gym %>%
  select(Max_BPM, Calories_Burned) %>%
  group_by(Max_BPM) %>%
  count(Calories_Burned)

ggplot(distribution_list$Max_BPM,  aes(x = Max_BPM, y = n, fill = factor(Calories_Burned))) + 
  geom_col(position = position_dodge()) +  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +                     
  scale_fill_manual(
    values = c("0" = "#E63946",  
               "1" = "#1F77B4"),
    name = "Ilość spalonych kalorii",
    labels = c("0" = "893 kalorie lub mniej", "1" = "894 kalorie lub więcej")
  ) +
  ylim(0, 300) + 
  theme_minimal() +
  labs(x = "Maksymalne tętno [BPM]", y = "Ilość wystąpień", title = "Rozkład ilości spalonych kalorii wg maksymalnego tętna [BPM]") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 15)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15))  
  )

# 6) Avg_BPM 
distribution_list$Avg_BPM <- clean_gym %>%
  select(Avg_BPM, Calories_Burned) %>%
  group_by(Avg_BPM) %>%
  count(Calories_Burned)

ggplot(distribution_list$Avg_BPM,  aes(x = Avg_BPM, y = n, fill = factor(Calories_Burned))) + 
  geom_col(position = position_dodge()) +  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +                     
  scale_fill_manual(
    values = c("0" = "#E63946",  
               "1" = "#1F77B4"),
    name = "Ilość spalonych kalorii",
    labels = c("0" = "893 kalorie lub mniej", "1" = "894 kalorie lub więcej")
  ) +
  ylim(0, 300) + 
  theme_minimal() +
  labs(x = "Średnie tętno [BPM]", y = "Ilość wystąpień", title = "Rozkład ilości spalonych kalorii wg średniego tętna [BPM]") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 15)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15))  
  )

# 7) Resting_BPM 
distribution_list$Resting_BPM <- clean_gym %>%
  select(Resting_BPM, Calories_Burned) %>%
  group_by(Resting_BPM) %>%
  count(Calories_Burned)

ggplot(distribution_list$Resting_BPM,  aes(x = Resting_BPM, y = n, fill = factor(Calories_Burned))) + 
  geom_col(position = position_dodge()) +  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +                     
  scale_fill_manual(
    values = c("0" = "#E63946",  
               "1" = "#1F77B4"),
    name = "Ilość spalonych kalorii",
    labels = c("0" = "893 kalorie lub mniej", "1" = "894 kalorie lub więcej")
  ) +
  ylim(0, 300) + 
  theme_minimal() +
  labs(x = "Tętno w spoczynku [BPM]", y = "Ilość wystąpień", title = "Rozkład ilości spalonych kalorii wg tętna w spoczynku [BPM]") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 15)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15))  
  )

# 8) Session_Duration_Hours
distribution_list$Session_Duration..hours. <- clean_gym %>%
  select(Session_Duration..hours., Calories_Burned) %>%
  group_by(Session_Duration..hours.) %>%
  count(Calories_Burned)

ggplot(distribution_list$Session_Duration..hours.,  aes(x = Session_Duration..hours., y = n, fill = factor(Calories_Burned))) + 
  geom_col(position = position_dodge()) +  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +                     
  scale_fill_manual(
    values = c("0" = "#E63946",  
               "1" = "#1F77B4"),
    name = "Ilość spalonych kalorii",
    labels = c("0" = "893 kalorie lub mniej", "1" = "894 kalorie lub więcej")
  ) +
  ylim(0, 300) + 
  theme_minimal() +
  labs(x = "Czas trwania sesji (godz.)", y = "Ilość wystąpień", title = "Rozkład ilości spalonych kalorii wg czasu trwania sesji (godz.)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 15)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15))  
  )


# 9) Workout Type
distribution_list$Workout_Type <- clean_gym %>%
  select(Workout_Type, Calories_Burned) %>%
  group_by(Workout_Type) %>%
  count(Calories_Burned)

ggplot(distribution_list$Workout_Type,  aes(x = Workout_Type, y = n, fill = factor(Calories_Burned))) + 
  geom_col(position = position_dodge()) +  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +                     
  scale_fill_manual(
    values = c("0" = "#E63946",  
               "1" = "#1F77B4"),
    name = "Ilość spalonych kalorii",
    labels = c("0" = "893 kalorie lub mniej", "1" = "894 kalorie lub więcej")
  ) +
  ylim(0, 300) + 
  theme_minimal() +
  labs(x = "Typ ćwiczeń", y = "Ilość wystąpień", title = "Rozkład ilości spalonych kalorii wg typu ćwiczeń") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 15)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15))  
  )


# 10) Fat_percentage
distribution_list$Fat_Percentage <- clean_gym %>%
  select(Fat_Percentage, Calories_Burned) %>%
  group_by(Fat_Percentage) %>%
  count(Calories_Burned)

ggplot(distribution_list$Fat_Percentage,  aes(x = Fat_Percentage, y = n, fill = factor(Calories_Burned))) + 
  geom_col(position = position_dodge()) +  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +                     
  scale_fill_manual(
    values = c("0" = "#E63946",  
               "1" = "#1F77B4"),
    name = "Ilość spalonych kalorii",
    labels = c("0" = "893 kalorie lub mniej", "1" = "894 kalorie lub więcej")
  ) +
  ylim(0, 300) + 
  theme_minimal() +
  labs(x = "% tkanki tłuszczowej", y = "Ilość wystąpień", title = "Rozkład ilości spalonych kalorii wg % tkanki tłuszczowej") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 15)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15))  
  )



# 11) Water Intake Liters 
distribution_list$Water_Intake..liters. <- clean_gym %>%
  select(Water_Intake..liters., Calories_Burned) %>%
  group_by(Water_Intake..liters.) %>%
  count(Calories_Burned)

ggplot(distribution_list$Water_Intake..liters.,  aes(x = Water_Intake..liters., y = n, fill = factor(Calories_Burned))) + 
  geom_col(position = position_dodge()) +  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +                     
  scale_fill_manual(
    values = c("0" = "#E63946",  
               "1" = "#1F77B4"),
    name = "Ilość spalonych kalorii",
    labels = c("0" = "893 kalorie lub mniej", "1" = "894 kalorie lub więcej")
  ) +
  ylim(0, 300) + 
  theme_minimal() +
  labs(x = "Ilość spożywanej wody (L)", y = "Ilość wystąpień", title = "Rozkład ilości spalonych kalorii wg ilości spożywanej wody (L)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 15)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15))  
  )

# 12) Workout Frequency
distribution_list$Workout_Frequency..days.week. <- clean_gym %>%
  select(Workout_Frequency..days.week., Calories_Burned) %>%
  group_by(Workout_Frequency..days.week.) %>%
  count(Calories_Burned)

ggplot(distribution_list$Workout_Frequency..days.week.,  aes(x = Workout_Frequency..days.week., y = n, fill = factor(Calories_Burned))) + 
  geom_col(position = position_dodge()) +  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +                     
  scale_fill_manual(
    values = c("0" = "#E63946",  
               "1" = "#1F77B4"),
    name = "Ilość spalonych kalorii",
    labels = c("0" = "893 kalorie lub mniej", "1" = "894 kalorie lub więcej")
  ) +
  ylim(0, 300) + 
  theme_minimal() +
  labs(x = "Częstotliwość ćwiczeń (dni w tygodniu)", y = "Ilość wystąpień", title = "Rozkład ilości spalonych kalorii wg częstotliwości ćwiczeń (dni w tygodniu)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 15)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15))  
  )

# 13) Experience Level
distribution_list$Experience_Level <- clean_gym %>%
  select(Experience_Level, Calories_Burned) %>%
  group_by(Experience_Level) %>%
  count(Calories_Burned)

ggplot(distribution_list$Experience_Level,  aes(x = Experience_Level, y = n, fill = factor(Calories_Burned))) + 
  geom_col(position = position_dodge()) +  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +                     
  scale_fill_manual(
    values = c("0" = "#E63946",  
               "1" = "#1F77B4"),
    name = "Ilość spalonych kalorii",
    labels = c("0" = "893 kalorie lub mniej", "1" = "894 kalorie lub więcej")
  ) +
  ylim(0, 300) + 
  theme_minimal() +
  labs(x = "Poziom doświadczenia", y = "Ilość wystąpień", title = "Rozkład ilości spalonych kalorii wg poziomu doświadczenia") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 15)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15))  
  )

# 14) BMI
distribution_list$BMI <- clean_gym %>%
  select(BMI, Calories_Burned) %>%
  group_by(BMI) %>%
  count(Calories_Burned)

ggplot(distribution_list$BMI,  aes(x = BMI, y = n, fill = factor(Calories_Burned))) + 
  geom_col(position = position_dodge()) +  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3.5) +                     
  scale_fill_manual(
    values = c("0" = "#E63946",  
               "1" = "#1F77B4"),
    name = "Ilość spalonych kalorii",
    labels = c("0" = "893 kalorie lub mniej", "1" = "894 kalorie lub więcej")
  ) +
  ylim(0, 300) + 
  theme_minimal() +
  labs(x = "BMI", y = "Ilość wystąpień", title = "Rozkład ilości spalonych kalorii wg BMI") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 15)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15))  
  )

#Usunięcie zmiennej, która była tylko stworzona do lepszej wizualicji wyniku
clean_gym <- clean_gym %>%
  select(-IlośćSpalonychKalorii)


#Sprawdzenie typu zmiennych
str(clean_gym)
#pętla, która zamienia wszystkie kolumny na factory 
clean_gym[] <- lapply(clean_gym, function(col) {
  if (is.character(col) || is.numeric(col) || is.integer(col)) {
    as.factor(col)
  } else {
    col
  }
})



# ETAP3: LICZENIE INFORMATION VALUE 
#################################################################################################################
#################################################################################################################


#Liczenie Information Value 
IV <- iv(clean_gym, y="Calories_Burned") 
IV$info_value <- round(IV$info_value, 2)


# ETAP4: ANALIZA KORELACJI MIEDZY ZMIENNYMI (TWORZENIE MACIERZY + USUNIECIE ZBYT SKORELOWANYCH ZMIENNYCH)
#################################################################################################################
#################################################################################################################

#Tworzenie macierzy korelacji 

vars <- names(clean_gym)
n <- length(vars)

cramer_matrix <- matrix(NA, nrow = n, ncol = n)
colnames(cramer_matrix) <- vars
rownames(cramer_matrix) <- vars

for (i in 1:n) {
  for (j in i:n) {
    cram_val <- cramersV(clean_gym[[i]], clean_gym[[j]])
    cramer_matrix[i, j] <- cram_val
    cramer_matrix[j, i] <- cram_val
  }
}

# Wyświetl macierz
cramer_df <- melt(cramer_matrix, na.rm = TRUE)

ggplot(cramer_df, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "grey70") +
  geom_text(aes(label = round(value, 2)), size = 3) +   
  scale_fill_gradient2(low = "white", high = "blue", midpoint = 0.5) +
  theme_minimal() +
  labs(title = "Macierz Korelacji", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))

#Usunięcie zbyt skorelowanych zmiennych 

clean_gym <- clean_gym %>%
  select(-Gender, -Height..m.,-Experience_Level, -Weight..kg. )


# ETAP5 : USTAWIENIE ZIARNA + STWORZENIE ZBIOROW : TRENINGOWE I TESTOWEGO + STWORZENIE DRZEW +
# WIZUALIZACJA DRZEW 
#################################################################################################################
#################################################################################################################

#Ustawianie ziarna
set.seed(1)

# Dzielenie zbioru na uczacy i testowy losowo w proporcji 0.8 : 0.2 
train_proportion <- 0.8
train_index <- runif(nrow(clean_gym)) < train_proportion  #Losowanie dla każdego wierszu 1 liczby i wybieranie tam gdzie jest wiekszę niż 0.8 i mniejsze
train <- clean_gym[train_index,]
test <- clean_gym[!train_index,]


#Ustawianie leveli zeby miec dobrą macierz bledow tak jak na wikipedii 
train$Calories_Burned <- factor(train$Calories_Burned, levels = c("1", "0"))
test$Calories_Burned <- factor(test$Calories_Burned, levels = c("1", "0"))

# Trenowanie modeli
tree_train <- rpart(Calories_Burned ~ ., data = train, method = "class", cp = 0.02)
tree_deep_train <- rpart(Calories_Burned ~ ., data = train, method = "class",cp = 0.0075)
Random_Forest_train <- randomForest( #Random Forest - ograniczony aby uniknąć przeuczenia
  Calories_Burned ~ ., 
  data = train,
  mtry = 4,
  max.depth = 3,
  nodesize = 20)

#WIZUALIZACJA DREZW
#Drzewo klasyczne
rpart.plot(tree_train, main = "Drzewo klasyczne", extra = 106)

#Drzewo glebokie
rpart.plot(tree_deep_train, main = "Drzewo głębokie", extra = 106)
  
#Las losowy
varImpPlot(Random_Forest_train, main = "Ważność zmiennych (Mean Decrease in Gini)")
# ETAP6 : MACIERZ BLEDOW + WYNIKI NP. ACCURACY/PRECISION ITP. 
#################################################################################################################
#################################################################################################################


# Dla zbioru treningowego
cm_train_tree <- table(Actual = train$Calories_Burned, Predicted = predict(tree_train, newdata = train, type = "class"))
cm_train_deep_tree <- table(Actual = train$Calories_Burned, Predicted = predict(tree_deep_train, newdata = train, type = "class"))
cm_train_rf <- table(Actual = train$Calories_Burned, Predicted = predict(Random_Forest_train, newdata = train))

# Dla zbioru testowego
cm_test_tree <- table(Actual = test$Calories_Burned, Predicted = predict(tree_train, newdata = test, type = "class"))
cm_test_deep_tree <- table(Actual = test$Calories_Burned, Predicted = predict(tree_deep_train, newdata = test, type = "class"))
cm_test_rf <- table(Actual = test$Calories_Burned, Predicted = predict(Random_Forest_train, newdata = test))

# Funkcja do oceny modeli
EvaluateModel <- function(cm){
  TP <- cm[1,1]
  FN <- cm[1,2]
  FP <- cm[2,1]
  TN <- cm[2,2]
  
  accuracy <- (TP + TN) / sum(cm)
  MER <- 1 - accuracy
  precision <- TP / (TP + FP)
  sensitivity <- TP / (TP + FN)  # recall
  specificity <- TN / (TN + FP)
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  
  return(list(
    accuracy = accuracy,
    MER = MER,
    precision = precision,
    sensitivity = sensitivity,
    specificity = specificity,
    F1 = F1
  ))
}

# Tworzenie list z macierzami błędów
CM_train <- list(tree = cm_train_tree, tree_deep = cm_train_deep_tree, rf = cm_train_rf)
CM_test <- list(tree = cm_test_tree, tree_deep = cm_test_deep_tree, rf = cm_test_rf)

# Obliczanie metryk
results_train <- lapply(CM_train, EvaluateModel)
results_test <- lapply(CM_test, EvaluateModel)

# Tworzenie ramek danych z wynikami

df_train <- bind_rows(
  lapply(names(results_train), function(m) as.data.frame(results_train[[m]]) %>% mutate(Model = m, Set = "Train"))
)

df_test <- bind_rows(
  lapply(names(results_test), function(m) as.data.frame(results_test[[m]]) %>% mutate(Model = m, Set = "Test"))
)

results_all <- bind_rows(df_train, df_test) %>%
  select(Model, Set, everything())

# Wyświetlanie wyników
results_all <- results_all %>%
  mutate_if(is.numeric, round, 4)

print(results_all)


# ETAP7: Krzywe ROC, LIFT i POLE AUC POD KRZYWA ROC 
#################################################################################################################
#################################################################################################################


######### KRZYWE ROC ##########

# Predykcje prawdopodobieństw (wybieraj kolumnę "1" czyli pozytywną klasę)
pred_tree <- predict(tree_train, newdata = test, type = "prob")[, "1"]
pred_deep <- predict(tree_deep_train, newdata = test, type = "prob")[, "1"]
pred_rf <- predict(Random_Forest_train, newdata = test, type = "prob")[, "1"]

# Obiekty prediction
pred_obj_tree <- prediction(pred_tree, test$Calories_Burned)
pred_obj_deep <- prediction(pred_deep, test$Calories_Burned)
pred_obj_rf <- prediction(pred_rf, test$Calories_Burned)

# Obiekty performance
perf_tree <- performance(pred_obj_tree, "tpr", "fpr")
perf_deep <- performance(pred_obj_deep, "tpr", "fpr")
perf_rf <- performance(pred_obj_rf, "tpr", "fpr")

# Rysowanie pierwszej krzywej
plot(perf_tree, col = "green", lwd = 2, main = "Porównanie krzywych ROC")
# Dodawanie kolejnych krzywych
plot(perf_deep, col = "blue", lwd = 2, add = TRUE)
plot(perf_rf, col = "red", lwd = 2, add = TRUE)

# Dodanie legendy
legend("bottomright", legend = c("Drzewo", "Drzewo głębokie", "Las losowy"),
       col = c("green", "blue", "red"), lwd = 2)



######### WYNIKI AUC ##########
auc_tree <- performance(pred_obj_tree, measure = "auc")@y.values[[1]]
auc_deep <- performance(pred_obj_deep, measure = "auc")@y.values[[1]]
auc_rf <- performance(pred_obj_rf, measure = "auc")@y.values[[1]]


cat("AUC Drzewo klasyczne: ", round(auc_tree, 4), "\n")
cat("AUC Drzewo głębokie: ", round(auc_deep, 4), "\n")
cat("AUC Las losowy: ", round(auc_rf, 4), "\n")


######### KRZYWE LIFT ##########

# Predykcje prawdopodobieństw (klasa "1")
pred_tree <- predict(tree_train, newdata = test, type = "prob")[, "1"]
pred_deep <- predict(tree_deep_train, newdata = test, type = "prob")[, "1"]
pred_rf <- predict(Random_Forest_train, newdata = test, type = "prob")[, "1"]

# Obiekty prediction
pred_obj_tree <- prediction(pred_tree, test$Calories_Burned)
pred_obj_deep <- prediction(pred_deep, test$Calories_Burned)
pred_obj_rf <- prediction(pred_rf, test$Calories_Burned)

# Obiekty performance dla LIFT (lift względem rate of positive predictions - rpp)
perf_lift_tree <- performance(pred_obj_tree, measure = "lift", x.measure = "rpp")
perf_lift_deep <- performance(pred_obj_deep, measure = "lift", x.measure = "rpp")
perf_lift_rf <- performance(pred_obj_rf, measure = "lift", x.measure = "rpp")

# Rysowanie pierwszej krzywej LIFT
plot(perf_lift_tree, col = "green", lwd = 2, main = "Porównanie krzywych LIFT")
# Dodawanie kolejnych krzywych
plot(perf_lift_deep, col = "blue", lwd = 2, add = TRUE)
plot(perf_lift_rf, col = "red", lwd = 2, add = TRUE)

# Dodanie legendy z dobrymi nazwami
legend("topright", legend = c("Drzewo klasyczne", "Drzewo głębokie", "Las losowy"),
       col = c("green", "blue", "red"), lwd = 2)

