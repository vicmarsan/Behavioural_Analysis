library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

# SPEED

# process file function
process_excel_file <- function(file_path) {
  length_data <- read_excel(file_path, sheet = 1)
  mean_length <- length_data$`avg_LarvaeLength (mm)`
  df <- read_excel(file_path, sheet = 2)
  df$`time (s)` <- round(df$`time (s)`)
  df <- df %>% filter(`time (s)` >= 0 & `time (s)` <= 90)
  df <- df %>%
    mutate(`speed (mm/s)` = `speed (mm/s)` / mean_length)
  
  return(df)
}

population_directories <- c("C:/Users/viky/Desktop/TFM/blue light stimulus/Response - BlueLight stimulus/thG@Uempty/n_blueLightAvoidance_10s_30s_10s_2pulses@n",
                            "C:/Users/viky/Desktop/TFM/blue light stimulus/Response - BlueLight stimulus/thG@UaSynA53T/n_blueLightAvoidance_10s_30s_10s_2pulses@n",
                            "C:/Users/viky/Desktop/TFM/blue light stimulus/Response - BlueLight stimulus/thG@UG2019S/n_blueLightAvoidance_10s_30s_10s_2pulses@n")

results_list <- list()

# Loop a través de las poblaciones
for (population_dir in population_directories) {
  # Obtener la lista de archivos en el directorio de la población actual
  excel_files <- list.files(population_dir, full.names = TRUE, pattern = "\\.xlsx$")
  
  # Lista para almacenar los resultados de todos los experimentos de esta población
  population_results <- list()
  
  # Loop a través de los archivos Excel de la población actual
  for (file_path in excel_files) {
    # Procesar el archivo y agregarlo a la lista de resultados de la población
    population_results[[length(population_results) + 1]] <- process_excel_file(file_path)
  }
  
  # Combinar los resultados de todos los experimentos de esta población en una única tabla
  combined_results <- bind_rows(population_results)
  
  # Calcular la velocidad media para cada segundo
  avg_velocity <- combined_results %>%
    group_by(`time (s)`) %>%
    summarise(mean_velocity = mean(`speed (mm/s)`, na.rm = TRUE))
  
  # Almacenar los resultados de esta población en la lista general
  results_list[[population_dir]] <- avg_velocity
}

# plot

population_names <- c("control","aSyn-A53T","LRRK2-G2019S")
names(results_list) <- population_names

combined_results <- bind_rows(results_list, .id = "Population")

plot_speed <- ggplot(combined_results, aes(x = `time (s)`, y = mean_velocity, color = Population)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(1, 90)) +
  labs(x = "Time (s)", y = "Speed (1/s)",title = "Populations speeds") +
  theme(legend.title = element_blank()) +
  geom_rect(aes(xmin = 10, xmax = 40, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01,color=NA) +
  geom_rect(aes(xmin = 50, xmax = 80, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01,color=NA) +
  scale_color_manual(values = c("control" = "palegreen3", "aSyn-A53T" = "royalblue", "LRRK2-G2019S" = "yellow3"))

print(plot_speed)

# # Divide data in 4 categories: 3 secs before bl, 3 secs after bl, 3 final secs of
# bl and 3 secs after turning off bl

process_excel_file <- function(file_path) {
  length_data <- read_excel(file_path, sheet = 1)
  mean_length <- length_data$`avg_LarvaeLength (mm)`
  df <- read_excel(file_path, sheet = 2)
  df$`time (s)` <- floor(df$`time (s)`)
  df <- df %>% filter(`time (s)` >= 0 & `time (s)` <= 90)
  df <- df %>%
    mutate(`speed (mm/s)` = `speed (mm/s)` / mean_length)
  df1 <- df %>% filter((`time (s)` >= 7 & `time (s)` < 10) | (`time (s)` >= 47 & `time (s)` < 50))
  df2 <- df %>% filter((`time (s)` >= 10 & `time (s)` < 13) | (`time (s)` >= 50 & `time (s)` < 53))
  df3 <- df %>% filter((`time (s)` >= 37 & `time (s)` < 40) | (`time (s)` >= 77 & `time (s)` < 80))
  df4 <- df %>% filter((`time (s)` >= 40 & `time (s)` < 43) | (`time (s)` >= 80 & `time (s)` < 83))
  df1$category <- "Category 1"
  df2$category <- "Category 2"
  df3$category <- "Category 3"
  df4$category <- "Category 4"
  
  final_df <- bind_rows(df1, df2, df3, df4)
  
  return(final_df)
}

vel_results_list <- list()
combined_results_list <- list()

for (population_dir in population_directories) {
  excel_files <- list.files(population_dir, full.names = TRUE, pattern = "\\.xlsx$")
  population_results <- list()
  for (file_path in excel_files) {
    population_results[[length(population_results) + 1]] <- process_excel_file(file_path)
  }
  combined_results <- bind_rows(population_results)
  combined_results_list[[population_dir]] <- combined_results
  # average vel for each category
  avg_velocity <- combined_results %>%
    group_by(category) %>%
    summarise(mean_velocity = mean(`speed (mm/s)`, na.rm = TRUE))
  vel_results_list[[population_dir]] <- avg_velocity
}

names(vel_results_list) <- c("Control","aSyn-A53T","LRRK2-G2019S")
names(combined_results_list) <- c("Control","aSyn-A53T","LRRK2-G2019S")

add_population_column <- function(data, population) {
  data %>%
    mutate(population = population)
}
combined_results_df <- lapply(names(combined_results_list), function(population) {
  add_population_column(combined_results_list[[population]], population)
}) %>%
  bind_rows()

# Statistics
kruskal.test(`speed (mm/s)` ~ population, combined_results_df)
mc <- dunn.test::dunn.test(x = combined_results_df$`speed (mm/s)`, g = combined_results_df$population, method = "bonferroni")

# Per category
resultados_kruskal <- list()
for (cat in unique(combined_results_df$category)) {
  resultado <- kruskal.test(`speed (mm/s)` ~ population, data = combined_results_df[combined_results_df$category == cat, ])
  resultados_kruskal[[cat]] <- resultado
}
mc_results <- list()
for (cat in unique(combined_results_df$category)) {
  data_actual_cat <- combined_results_df[combined_results_df$category == cat, ]
  comparisons_dunn <- dunn.test::dunn.test(combined_results_df$`speed (mm/s)`, 
                                           g = combined_results_df$population, 
                                           method = "bonferroni")
  mc_results[[as.character(cat)]] <- comparisons_dunn
}

# diferences between categories
mc_results_2 <- list()
for (pop in unique(combined_results_df$population)) {
  data_actual_cat <- combined_results_df[combined_results_df$population == pop, ]
  comparisons_dunn <- dunn.test::dunn.test(combined_results_df$`speed (mm/s)`, 
                                           g = combined_results_df$category, 
                                           method = "bonferroni")
  mc_results_2[[as.character(pop)]] <- comparisons_dunn
}

combined_results_df_plot <- combined_results_df %>%
  group_by(category,population) %>%
  summarize(Mean = mean(`speed (mm/s)`),
            SD = sd(`speed (mm/s)`),
            N = n()) %>%
  ungroup()

# ACCELERATION

# Calcular la aceleración a partir de los datos de velocidad y tiempo
combined_results <- combined_results %>%
  group_by(Population) %>%
  mutate(acceleration = (`mean_velocity` - lag(`mean_velocity`, default = first(`mean_velocity`))) / 
           (`time (s)` - lag(`time (s)`, default = first(`time (s)`))))

plot_acceleration <- ggplot(combined_results, aes(x = `time (s)`, y = `acceleration`, color = Population)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(1, 90)) +
  labs(x = "Time (s)", y = "Acceleration (1/s2)",title = "Populations accelerations") +
  theme(legend.title = element_blank()) +
  geom_rect(aes(xmin = 10, xmax = 40, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01,color=NA) +
  geom_rect(aes(xmin = 50, xmax = 80, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01,color=NA) +
  scale_color_manual(values = c("control" = "palegreen3", "aSyn-A53T" = "royalblue", "LRRK2-G2019S" = "yellow3"))

print(plot_acceleration)

# Initial and min max values

# categories

process_excel_file <- function(file_path) {
  length_data <- read_excel(file_path, sheet = 1)
  mean_length <- length_data$`avg_LarvaeLength (mm)`
  df <- read_excel(file_path, sheet = 2)
  df$`time (s)` <- floor(df$`time (s)`)
  df <- df %>% filter(`time (s)` >= 0 & `time (s)` <= 90)
  df <- df %>%
    mutate(`speed (mm/s)` = `speed (mm/s)` / mean_length)
  df1_1 <- df %>% filter((`time (s)` = 10)) 
  df1_2 <- df %>% filter((`time (s)` = 50))
  df2_1 <- df %>% filter((`time (s)` >= 10 & `time (s)` < 13)) 
  df2_2 <- df %>% filter((`time (s)` >= 50 & `time (s)` < 53))
  df3_1 <- df %>% filter((`time (s)` >= 37 & `time (s)` < 40)) 
  df3_2 <- df %>% filter((`time (s)` >= 77 & `time (s)` < 80))
  df4_1 <- df %>% filter((`time (s)` >= 40 & `time (s)` < 43))
  df4_2 <- df %>% filter((`time (s)` >= 80 & `time (s)` < 83))
  df1_1$subcategory <- "Category 1.1"
  df1_2$subcategory <- "Category 1.2"
  df2_1$subcategory <- "Category 2.1"
  df2_2$subcategory <- "Category 2.2"
  df3_1$subcategory <- "Category 3.1"
  df3_2$subcategory <- "Category 3.2"
  df4_1$subcategory <- "Category 4.1"
  df4_2$subcategory <- "Category 4.2"
  
  final_df <- bind_rows(df1_1,df1_2, df2_1,df2_2, df3_1,df3_2, df4_1,df4_2)
  
  return(final_df)
}

vel_results_list <- list()
combined_results_list_acc <- list()

for (population_dir in population_directories) {
  excel_files <- list.files(population_dir, full.names = TRUE, pattern = "\\.xlsx$")
  population_results <- list()
  for (file_path in excel_files) {
    population_results[[length(population_results) + 1]] <- process_excel_file(file_path)
  }
  combined_results <- bind_rows(population_results)
  combined_results_list_acc[[population_dir]] <- combined_results
  # average vel for each category
  avg_velocity <- combined_results %>%
    group_by(subcategory) %>%
    summarise(mean_velocity = mean(`speed (mm/s)`, na.rm = TRUE))
  
  # Almacenar los resultados de esta población en la lista general
  vel_results_list[[population_dir]] <- avg_velocity
}

names(vel_results_list) <- c("Control","aSyn-A53T","LRRK2-G2019S")
names(combined_results_list_acc) <- c("Control","aSyn-A53T","LRRK2-G2019S")

add_population_column <- function(data, population) {
  data %>%
    mutate(population = population)
}
combined_results_df_acc <- lapply(names(combined_results_list_acc), function(population) {
  add_population_column(combined_results_list_acc[[population]], population)
}) %>%
  bind_rows()

# one vel per subcategory and time per population:

sub_ve <- combined_results_df_acc %>%
  group_by(subcategory,`time (s)`,population) %>%
  mutate(sub_vel=mean(`speed (mm/s)`,na.rm = T))

sub_vel <- sub_ve %>%
  group_by(sub_vel) %>%
  slice(1) %>%
  ungroup()

result_acc <- sub_vel %>%
  group_by(subcategory,population) %>%
  arrange(`time (s)`) %>%
  mutate(acceleration = (last(sub_vel)-first(sub_vel)) / 3)

result_acc <- result_acc %>%
  mutate(new_category = case_when(
    subcategory %in% c("Category 1.1", "Category 1.2") ~ "Category 1",
    subcategory %in% c("Category 2.1", "Category 2.2") ~ "Category 2",
    subcategory %in% c("Category 3.1", "Category 3.2") ~ "Category 3",
    subcategory %in% c("Category 4.1", "Category 4.2") ~ "Category 4",
    TRUE ~ subcategory
  )) 


results_acc_df <- result_acc %>%
  group_by(new_category,population) %>%
  summarize(Mean_acc = mean(acceleration),
            SD_acc = sd(acceleration),
            N = n())

# Statistics
kruskal.test(acceleration ~ population, result_acc)
mc <- dunn.test::dunn.test(x = result_acc$acceleration, g = result_acc$population, method = "bonferroni")

# Per category
resultados_kruskal <- list()
for (cat in unique(combined_results_df$category)) {
  resultado <- kruskal.test(acceleration ~ population, data = combined_results_df[combined_results_df$category == cat, ])
  resultados_kruskal[[cat]] <- resultado
}
mc_results <- list()
for (cat in unique(combined_results_df$category)) {
  data_actual_cat <- combined_results_df[combined_results_df$category == cat, ]
  comparisons_dunn <- dunn.test::dunn.test(combined_results_df$acceleration, 
                                           g = combined_results_df$population, 
                                           method = "bonferroni")
  mc_results[[as.character(cat)]] <- comparisons_dunn
}
# differences between categories
mc_results_2 <- list()
for (pop in unique(combined_results_df$population)) {
  data_actual_cat <- combined_results_df[combined_results_df$population == pop, ]
  comparisons_dunn <- dunn.test::dunn.test(combined_results_df$acceleration, 
                                           g = combined_results_df$category, 
                                           method = "bonferroni")
  mc_results_2[[as.character(pop)]] <- comparisons_dunn
}


# ANGULAR VELOCITY

# process file function
process_excel_file2 <- function(file_path) {
  df <- read_excel(file_path, sheet = 5)
  return(df)
}

results_list_angular_velocity <- list()

for (population_dir in population_directories) {
  excel_files <- list.files(population_dir, full.names = TRUE, pattern = "\\.xlsx$")
  population_results <- list()
  for (file_path in excel_files) {
    population_results[[length(population_results) + 1]] <- process_excel_file2(file_path)
  }
  combined_results_angular_velocity <- bind_rows(population_results)
  avg_angular_velocity <- combined_results_angular_velocity %>%
    group_by(`time (s)`) %>%
    summarise(mean_angle = mean(`angle (degrees)`, na.rm = TRUE))
  results_list_angular_velocity[[population_dir]] <- avg_angular_velocity
}

names(results_list_angular_velocity) <- population_names

combined_results_angular_velocity <- bind_rows(results_list_angular_velocity, .id = "Population")
combined_results_angular_velocity <- combined_results_angular_velocity %>%
  group_by(Population) %>%
  mutate(angular_velocity = (`mean_angle` - lag(`mean_angle`, default = first(`mean_angle`))) / 
           (`time (s)` - lag(`time (s)`, default = first(`time (s)`))))

plot_angular_velocity <- ggplot(combined_results_angular_velocity, 
                                aes(x = `time (s)`, y = abs(`angular_velocity`), color = Population)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(1, 90)) +
  labs(x = "Time (s)", y = "Angular speed (º/s)",title = "Populations angular speeds") +
  theme(legend.title = element_blank()) +
  geom_rect(aes(xmin = 10, xmax = 40, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01,color=NA) +
  geom_rect(aes(xmin = 50, xmax = 80, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01,color=NA) +
  scale_color_manual(values = c("control" = "palegreen3", "aSyn-A53T" = "royalblue", "LRRK2-G2019S" = "yellow3"))


print(plot_angular_velocity)

df1 <- combined_results_angular_velocity %>% filter((`time (s)` >= 7 & `time (s)` < 10) | (`time (s)` >= 47 & `time (s)` < 50))
df2 <- combined_results_angular_velocity %>% filter((`time (s)` >= 10 & `time (s)` < 13) | (`time (s)` >= 50 & `time (s)` < 53))
df3 <- combined_results_angular_velocity %>% filter((`time (s)` >= 37 & `time (s)` < 40) | (`time (s)` >= 77 & `time (s)` < 80))
df4 <- combined_results_angular_velocity %>% filter((`time (s)` >= 40 & `time (s)` < 43) | (`time (s)` >= 80 & `time (s)` < 83))
df1$category <- "Category 1"
df2$category <- "Category 2"
df3$category <- "Category 3"
df4$category <- "Category 4"

final_df <- bind_rows(df1, df2, df3, df4)
avg_ang_velocity <- final_df %>%
  group_by(category,Population) %>%
  summarise(mean_ang_velocity = mean(`angular_velocity`, na.rm = TRUE),sd=sd(angular_velocity))

# ANGULAR ACCELERATION

combined_results_angular_velocity <- combined_results_angular_velocity %>%
  group_by(Population) %>%
  mutate(angular_acceleration = (angular_velocity - lag(angular_velocity, default = first(angular_velocity)))
           / (`time (s)` - lag(`time (s)`, default = first(`time (s)`))))

plot_angular_acceleration <- ggplot(combined_results_angular_velocity, aes(x = `time (s)`, y = `angular_acceleration`, color = Population)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(1, 90)) +
  labs(x = "Time (s)", y = "Angular acceleration (º/s2)",title = "Populations angular accelerations") +
  theme(legend.title = element_blank()) +
  geom_rect(aes(xmin = 10, xmax = 40, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01,color=NA) +
  geom_rect(aes(xmin = 50, xmax = 80, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01,color=NA)+
  scale_color_manual(values = c("control" = "palegreen3", "aSyn-A53T" = "royalblue", "LRRK2-G2019S" = "yellow3"))


print(plot_angular_acceleration)



# BEHAVIOUR CLASS

process_file_behaviour <- function(file, population) {
  sheet <- readxl::read_excel(file, sheet = 6)
  sheet$`time (s)` <- floor(sheet$`time (s)`)
  df1 <- sheet %>% filter((`time (s)` >= 7 & `time (s)` < 10) | (`time (s)` >= 47 & `time (s)` < 50))
  df2 <- sheet %>% filter((`time (s)` >= 10 & `time (s)` < 13) | (`time (s)` >= 50 & `time (s)` < 53))
  df3 <- sheet %>% filter((`time (s)` >= 37 & `time (s)` < 40) | (`time (s)` >= 77 & `time (s)` < 80))
  df4 <- sheet %>% filter((`time (s)` >= 40 & `time (s)` < 43) | (`time (s)` >= 80 & `time (s)` < 83))
  df1$category <- "1"
  df2$category <- "2"
  df3$category <- "3"
  df4$category <- "4"
  df <- bind_rows(df1, df2, df3, df4)
  df %>%
    group_by(category) %>%
    summarise(Running = mean(isRunning),Turning = mean(isTurning),
              Stopped=mean(isStopped),Casting=mean(isCasting),runsd=sd(isRunning),
              castsd=sd(isCasting),turnsd=sd(isTurning),stopsd=sd(isStopped)) %>%
    mutate(population = population)
}

df_list_behaviour <- lapply(1:3, function(i) {
  process_file_behaviour(excel_files[[i]], population = c("control", "aSyn-A53T", "LRRK2-G2019S")[i])
})

df_combined <- do.call(rbind, df_list_behaviour)
df_combined$category_population <- paste(df_combined$population, df_combined$category, sep = "-")
df_combined <- df_combined %>%
  pivot_longer(cols = 2:5, names_to = "behaviour") 

for (population_dir in population_directories) {
  excel_files <- list.files(population_dir, full.names = TRUE, pattern = "\\.xlsx$")
}

# RUNNING:

process_file_running <- function(file, population) {
  sheet <- readxl::read_excel(file, sheet = 6)
  sheet %>%
    select(`time (s)`, isRunning) %>%
    group_by(`time (s)`) %>%
    summarise(prop_isRunning = mean(isRunning)) %>%
    mutate(population = population)
}

# Crear una lista de data frames procesados
df_list_running <- lapply(1:3, function(i) {
  process_file_running(excel_files[[i]], population = c("control", "aSyn-A53T", "LRRK2-G2019S")[i])
})

# Combinar todos los data frames en uno solo
result_running <- do.call(rbind, df_list_running)

ggplot(result_running, 
       aes(x = `time (s)`, y = prop_isRunning, color = population)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(1, 90)) +
  labs(x = "Time (s)", y = "Proportion of larvae running",title = "Running") +
  geom_rect(aes(xmin = 10, xmax = 40, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01,color=NA) +
  geom_rect(aes(xmin = 50, xmax = 80, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01,color=NA) +
  scale_color_manual(values = c("control" = "palegreen3", "aSyn-A53T" = "royalblue", "LRRK2-G2019S" = "yellow3"))


# BENDING

process_file_casting <- function(file, population) {
  sheet <- readxl::read_excel(file, sheet = 6)
  sheet %>%
    select(`time (s)`, isCasting) %>%
    group_by(`time (s)`) %>%
    summarise(prop_isCasting = mean(isCasting)) %>%
    mutate(population = population)
}

# Crear una lista de data frames procesados
df_list_casting <- lapply(1:3, function(i) {
  process_file_casting(excel_files[[i]], population = c("control", "aSyn-A53T", "LRRK2-G2019S")[i])
})

# Combinar todos los data frames en uno solo
result_casting <- do.call(rbind, df_list_casting)

plot_isCasting <- ggplot(result_casting, aes(x = `time (s)`, y = prop_isCasting, color = population)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(1, 90)) +
  labs(x = "Time (s)", y = "Proportion of larvae bending",title = "Bending") +
  geom_rect(aes(xmin = 10, xmax = 40, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01,color=NA) +
  geom_rect(aes(xmin = 50, xmax = 80, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01,color=NA)+
  scale_color_manual(values = c("control" = "palegreen3", "aSyn-A53T" = "royalblue", "LRRK2-G2019S" = "yellow3"))

print(plot_isCasting)

# STOPPED

process_file_stopped <- function(file, population) {
  sheet <- readxl::read_excel(file, sheet = 6)
  sheet %>%
    select(`time (s)`, isStopped) %>%
    group_by(`time (s)`) %>%
    summarise(prop_isStopped = mean(isStopped)) %>%
    mutate(population = population)
}

# Crear una lista de data frames procesados
df_list_stopped <- lapply(1:3, function(i) {
  process_file_stopped(excel_files[[i]], population = c("empty", "aSynA53T", "G2019S")[i])
})

# Combinar todos los data frames en uno solo
result_stopped <- do.call(rbind, df_list_stopped)

plot_isStopped <- ggplot(result_stopped, aes(x = `time (s)`, y = result_stopped$prop_isStopped, color = population)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(1, 90)) +
  labs(x = "Time (s)", y = "Proportion of larvae stopped",title = "Stopped") +
  geom_rect(aes(xmin = 10, xmax = 40, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01) +
  geom_rect(aes(xmin = 50, xmax = 80, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01)

print(plot_isStopped)

# TURNING

process_file_turning <- function(file, population) {
  sheet <- readxl::read_excel(file, sheet = 6)
  sheet %>%
    select(`time (s)`, isTurning) %>%
    group_by(`time (s)`) %>%
    summarise(prop_isTurning = mean(isTurning)) %>%
    mutate(population = population)
}

# Crear una lista de data frames procesados
df_list_turning <- lapply(1:3, function(i) {
  process_file_turning(excel_files[[i]], population = c("empty", "aSynA53T", "G2019S")[i])
})

# Combinar todos los data frames en uno solo
result_turning <- do.call(rbind, df_list_turning)

plot_isTurning <- ggplot(result_turning, aes(x = `time (s)`, y = result_turning$prop_isTurning, color = population)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(1, 90)) +
  labs(x = "Time (s)", y = "Proportion of larvae turning",title = "Turning") +
  geom_rect(aes(xmin = 10, xmax = 40, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01) +
  geom_rect(aes(xmin = 50, xmax = 80, ymin = -Inf, ymax = Inf), fill = "lightblue", alpha = 0.01)

print(plot_isTurning)
