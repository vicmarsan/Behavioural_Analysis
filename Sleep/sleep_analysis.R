# Packages

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(ggpubr)

# Data

control_sleep_data <- read_excel("bouts Control (empty UAS) - TH-Gal4.xls")
aSyn_A53T_sleep_data <- read_excel("bouts UAS_alphaSyn-A53T - TH-Gal4.xls")
LRRK2_G2019S_sleep_data <- read_excel("bouts UAS_hLRRK2-G2019S - TH-Gal4.xls")

############################ Sleep percentage per hour #########################

# Calculate sleep percentage per hour
calculate_sleep_percentage <- function(data) {
  data %>%
    group_by(`hour`) %>%
    summarise(across(starts_with("animal"), ~sum(. == 0,na.rm = T) / length(.) * 100))
}

control_sleep_pct <- calculate_sleep_percentage(control_sleep_data)
control_sleep_pct <- control_sleep_pct[-13,]
control_sleep_pct_long <- control_sleep_pct %>%
  pivot_longer(cols = starts_with("animal"), 
               names_to = "animal", 
               values_to = "sleep_pct")

aSyn_A53T_sleep_pct <- calculate_sleep_percentage(aSyn_A53T_sleep_data)
aSyn_A53T_sleep_pct <- aSyn_A53T_sleep_pct[-13,]
aSyn_A53T_sleep_pct_long <- aSyn_A53T_sleep_pct %>%
  pivot_longer(cols = starts_with("animal"), 
               names_to = "animal", 
               values_to = "sleep_pct")

LRRK2_G2019S_sleep_pct <- calculate_sleep_percentage(LRRK2_G2019S_sleep_data)
LRRK2_G2019S_sleep_pct <- LRRK2_G2019S_sleep_pct[-13,]
LRRK2_G2019S_sleep_pct_long <- LRRK2_G2019S_sleep_pct %>%
  pivot_longer(cols = starts_with("animal"), 
               names_to = "animal", 
               values_to = "sleep_pct")
all_sleep_pct <- bind_rows(mutate(control_sleep_pct_long,population="control"),
                           mutate(aSyn_A53T_sleep_pct_long,population="aSyn-A53T"),
                           mutate(LRRK2_G2019S_sleep_pct_long,population="LRRK2-G2019S"))

#statistic
resultados_kruskal <- list()
for (hora in 1:12) {
  resultado <- kruskal.test(sleep_pct ~ population, data = all_sleep_pct[all_sleep_pct$hour == hora, ])
  resultados_kruskal[[hora]] <- resultado
}
# Dunn-test
mc_results <- list()
for (hour in 1:12) {
  data_actual_hour <- all_sleep_pct[all_sleep_pct$hour == hour, ]
  comparisons_dunn <- dunn.test::dunn.test(data_actual_hour$sleep_pct, 
                                           g = data_actual_hour$population, 
                                           method = "bonferroni")
  mc_results[[as.character(hour)]] <- comparisons_dunn
}

#plot
all_sleep_pct <- all_sleep_pct %>%
  group_by(hour, population) %>%
  summarize(Mean = mean(sleep_pct),
            SD = sd(sleep_pct),
            N = n()) %>%
  ungroup()
ggplot(all_sleep_pct, aes(x = factor(hour), y = Mean,fill=population)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.8, preserve = "single"),color="gray20") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.25, position = position_dodge(width = 0.9),color="gray20") +
  labs(x = "Hour", y = "Sleep percentage", title = "Sleep percentage per hour") +
  scale_x_discrete(labels = 1:12) +
  scale_fill_manual(values = c("royalblue1", "palegreen3", "yellow1")) + 
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey")) 

######################## Sleep episodes length per hour ########################

result_sleep_length_control <- data.frame(Animal_Hora = character(0), 
                                          Episodio = numeric(0),
                                          Duración = numeric(0))
result_sleep_length_aSyn <- data.frame(Animal_Hora = character(0), 
                                       Episodio = numeric(0), 
                                       Duración = numeric(0))
result_sleep_length_LRRK2 <- data.frame(Animal_Hora = character(0), 
                                        Episodio = numeric(0), 
                                        Duración = numeric(0))

for (col in names(control_sleep_data[, -1])) {
  for (h in unique(control_sleep_data$hour)) {
    grupo <- control_sleep_data[, col][control_sleep_data[,"hour"] == h]  
    grupo <- na.exclude(grupo)
    episodio <- 0
    longitud_episodio <- 0
    episodios <- c()  
    duraciones <- c()  
    for (valor in grupo) {
      if (valor == 0) {
        longitud_episodio <- longitud_episodio + 1
      } else {
        if (longitud_episodio > 0) {
          episodio <- episodio + 1
          episodios <- c(episodios, episodio)
          duraciones <- c(duraciones, longitud_episodio)
        }
        longitud_episodio <- 0
      }
    }
    if (length(episodios) > 0 && length(duraciones) > 0) {
      columna_resultados <- data.frame(Animal_Hora = paste0(col, "_", h), 
                                       Episodio = episodios, Duración = duraciones * 6)
      result_sleep_length_control <- rbind(result_sleep_length_control, columna_resultados)
    }
  }
}

colnames(result_sleep_length_control) <- c("Animal_Hora", "Episodio", "Duración")

mean_sleep_lengths_control <- data.frame(Animal = character(0), 
                                         Hora = integer(0), Media_Duracion = numeric(0))

for (i in 1:nrow(result_sleep_length_control)) {
  fila <- result_sleep_length_control[i, ]
  animal_hora <- fila$Animal_Hora
  episodio <- fila$Episodio
  duracion <- fila$Duración
  animal_hora_parts <- strsplit(animal_hora, "_")[[1]]
  animal <- animal_hora_parts[1]
  hora <- as.integer(animal_hora_parts[2])
  media_duracion <- mean_sleep_lengths_control$Media_Duracion[mean_sleep_lengths_control$Animal == animal & mean_sleep_lengths_control$Hora == hora]
  if (length(media_duracion) == 0) {
    media_duracion <- mean(result_sleep_length_control$Duración[result_sleep_length_control$Animal_Hora == animal_hora])
    mean_sleep_lengths_control <- rbind(mean_sleep_lengths_control, data.frame(Animal = animal, Hora = hora, Media_Duracion = media_duracion))
  } else {
    mean_sleep_lengths_control$Media_Duracion[mean_sleep_lengths_control$Animal == animal & mean_sleep_lengths_control$Hora == hora] <- media_duracion
  }
}

colnames(mean_sleep_lengths_control) <- c("Animal", "Hora", "Media_Duracion")


for (col in names(aSyn_A53T_sleep_data[, -1])) {
  for (h in unique(aSyn_A53T_sleep_data$hour)) {
    grupo <- aSyn_A53T_sleep_data[, col][aSyn_A53T_sleep_data[,"hour"] == h]  
    grupo <- na.exclude(grupo)
    episodio <- 0
    longitud_episodio <- 0
    episodios <- c() 
    duraciones <- c()  
    for (valor in grupo) {
      if (valor == 0) {
        longitud_episodio <- longitud_episodio + 1
      } else {
        if (longitud_episodio > 0) {
          episodio <- episodio + 1
          episodios <- c(episodios, episodio)
          duraciones <- c(duraciones, longitud_episodio)
        }
        longitud_episodio <- 0
      }
    }
    if (length(episodios) > 0 && length(duraciones) > 0) {
      columna_resultados <- data.frame(Animal_Hora = paste0(col, "_", h), Episodio = episodios, Duración = duraciones * 6)
      result_sleep_length_aSyn <- rbind(result_sleep_length_aSyn, columna_resultados)
    }
  }
}

colnames(result_sleep_length_aSyn) <- c("Animal_Hora", "Episodio", "Duración")

mean_sleep_lengths_aSyn <- data.frame(Animal = character(0), Hora = integer(0), Media_Duracion = numeric(0))

for (i in 1:nrow(result_sleep_length_aSyn)) {
  fila <- result_sleep_length_aSyn[i, ]
  animal_hora <- fila$Animal_Hora
  episodio <- fila$Episodio
  duracion <- fila$Duración
  animal_hora_parts <- strsplit(animal_hora, "_")[[1]]
  animal <- animal_hora_parts[1]
  hora <- as.integer(animal_hora_parts[2])
  media_duracion <- mean_sleep_lengths_aSyn$Media_Duracion[mean_sleep_lengths_aSyn$Animal == animal & mean_sleep_lengths_aSyn$Hora == hora]
  if (length(media_duracion) == 0) {
    media_duracion <- mean(result_sleep_length_aSyn$Duración[result_sleep_length_aSyn$Animal_Hora == animal_hora])
    mean_sleep_lengths_aSyn <- rbind(mean_sleep_lengths_aSyn, data.frame(Animal = animal, Hora = hora, Media_Duracion = media_duracion))
  } else {
    mean_sleep_lengths_aSyn$Media_Duracion[mean_sleep_lengths_aSyn$Animal == animal & mean_sleep_lengths_aSyn$Hora == hora] <- media_duracion
  }
}
colnames(mean_sleep_lengths_aSyn) <- c("Animal", "Hora", "Media_Duracion")


for (col in names(LRRK2_G2019S_sleep_data[, -1])) {
  for (h in unique(LRRK2_G2019S_sleep_data$hour)) {
    grupo <- LRRK2_G2019S_sleep_data[, col][LRRK2_G2019S_sleep_data[,"hour"] == h]  
    grupo <- na.exclude(grupo)
    episodio <- 0
    longitud_episodio <- 0
    
    episodios <- c()  
    duraciones <- c() 
    for (valor in grupo) {
      if (valor == 0) {
        longitud_episodio <- longitud_episodio + 1
      } else {
        if (longitud_episodio > 0) {
          episodio <- episodio + 1
          episodios <- c(episodios, episodio)
          duraciones <- c(duraciones, longitud_episodio)
        }
        longitud_episodio <- 0
      }
    }
    if (length(episodios) > 0 && length(duraciones) > 0) {
      columna_resultados <- data.frame(Animal_Hora = paste0(col, "_", h), Episodio = episodios, Duración = duraciones * 6)
      result_sleep_length_LRRK2 <- rbind(result_sleep_length_LRRK2, columna_resultados)
    }
  }
}

colnames(result_sleep_length_LRRK2) <- c("Animal_Hora", "Episodio", "Duración")

mean_sleep_lengths_LRRK2 <- data.frame(Animal = character(0), Hora = integer(0), Media_Duracion = numeric(0))

for (i in 1:nrow(result_sleep_length_LRRK2)) {
  fila <- result_sleep_length_LRRK2[i, ]
  animal_hora <- fila$Animal_Hora
  episodio <- fila$Episodio
  duracion <- fila$Duración
  animal_hora_parts <- strsplit(animal_hora, "_")[[1]]
  animal <- animal_hora_parts[1]
  hora <- as.integer(animal_hora_parts[2])
  media_duracion <- mean_sleep_lengths_LRRK2$Media_Duracion[mean_sleep_lengths_LRRK2$Animal == animal & mean_sleep_lengths_LRRK2$Hora == hora]
  if (length(media_duracion) == 0) {
    media_duracion <- mean(result_sleep_length_LRRK2$Duración[result_sleep_length_LRRK2$Animal_Hora == animal_hora])
    mean_sleep_lengths_LRRK2 <- rbind(mean_sleep_lengths_LRRK2, data.frame(Animal = animal, Hora = hora, Media_Duracion = media_duracion))
  } else {
    mean_sleep_lengths_LRRK2$Media_Duracion[mean_sleep_lengths_LRRK2$Animal == animal & mean_sleep_lengths_LRRK2$Hora == hora] <- media_duracion
  }
}

colnames(mean_sleep_lengths_LRRK2) <- c("Animal", "Hora", "Media_Duracion")
all_avg_sleep_lengths <- bind_rows(mutate(mean_sleep_lengths_control,population="control"),
                                   mutate(mean_sleep_lengths_aSyn,population="aSyn-A53T"),
                                   mutate(mean_sleep_lengths_LRRK2,population="LRRK2-G2019S"))

# Outliers:
Q1 <- quantile(all_avg_sleep_lengths$Media_Duracion, 0.25)
Q3 <- quantile(all_avg_sleep_lengths$Media_Duracion, 0.75)

# IQR
IQR <- Q3 - Q1
limite_superior <- Q3 + 1.5 * IQR
limite_inferior <- Q1 - 1.5 * IQR

# data filter
all_avg_sleep_lengths_filt <- all_avg_sleep_lengths[all_avg_sleep_lengths$Media_Duracion >= limite_inferior & all_avg_sleep_lengths$Media_Duracion <= limite_superior, ]
# KW
resultados_kruskal <- list()
for (hora in 1:12) {
  resultado <- kruskal.test(Media_Duracion ~ population, data = all_avg_sleep_lengths_filt[all_avg_sleep_lengths_filt$Hora == hora, ])
  resultados_kruskal[[hora]] <- resultado
}
# Dunn-test
mc_results <- list()
for (hour in c(1,6,8:12)) {
  data_actual_hour <- all_avg_sleep_lengths_filt[all_avg_sleep_lengths_filt$Hora == hour, ]
  comparisons_dunn <- dunn.test::dunn.test(data_actual_hour$Media_Duracion, 
                                           g = data_actual_hour$population, 
                                           method = "bonferroni")
  mc_results[[as.character(hour)]] <- comparisons_dunn
}

#plot
all_avg_sleep_lengths_filt <- all_avg_sleep_lengths_filt %>%
  group_by(Hora, population) %>%
  summarize(Mean = mean(Media_Duracion),
            SD = sd(Media_Duracion),
            N = n()) %>%
  ungroup()
ggplot(all_avg_sleep_lengths_filt, aes(x = factor(Hora), y = Mean,fill=population)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.8, preserve = "single"),color="gray20") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.25, position = position_dodge(width = 0.9),color="gray20") +
  labs(x = "Hour", y = "Sleep episodes length (sec)", title = "Sleep episodes length per hour") +
  scale_x_discrete(labels = 1:12) +
  scale_fill_manual(values = c("royalblue1", "palegreen3", "yellow1")) + 
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"))

####################### Number of sleep episodes per hour ######################

nse <- read.csv("sleep_episodes.csv",sep = ";")
colnames(nse) <- c("hour",rep("control",150),rep("LRRK2-G2019S",150),rep("aSyn-A53T",150))
nse <- pivot_longer(nse,cols = -hour, names_to = "population", values_to = "sleep_episodes")
# KW
resultados_kruskal <- list()
for (hora in 1:12) {
  resultado <- kruskal.test(sleep_episodes ~ population, data = nse[nse$hour == hora, ])
  resultados_kruskal[[hora]] <- resultado
}
# Dunn-test
mc_hours <- 1:7
mc_results <- list()
for (hour in mc_hours) {
  data_actual_hour <- nse[nse$hour == hour, ]
  comparisons_dunn <- dunn.test::dunn.test(data_actual_hour$sleep_episodes, 
                                           g = data_actual_hour$population, 
                                           method = "bonferroni")
  mc_results[[as.character(hour)]] <- comparisons_dunn
}

# plot
nse <- nse %>%
  group_by(hour, population) %>%
  summarize(Mean = mean(sleep_episodes),
            SD = sd(sleep_episodes),
            N = n()) %>%
  ungroup()
ggplot(nse, aes(x = factor(hour), y = Mean,fill=population)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.8, preserve = "single"),color="gray20") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.25, position = position_dodge(width = 0.9),color="gray20") +
  labs(x = "Hour", y = "Average sleep episodes", title = "Average sleep episodes per hour") +
  scale_x_discrete(labels = 1:12) +
  scale_fill_manual(values = c("royalblue1", "palegreen3", "yellow1")) + 
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"))
