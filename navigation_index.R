library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
# Average speeds across X axis (Ycoord)
matrix_list_control_odor_Xspeed <- list()
matrix_list_control_free_Xspeed <- list()
matrix_list_asyn_odor_Xspeed <- list()
matrix_list_asyn_free_Xspeed <- list()
matrix_list_LRRK2_odor_Xspeed <- list()
matrix_list_LRRK2_free_Xspeed <- list()

file_names_control_odor <- c("20230517_152119.xlsx", "20230519_150636.xlsx", "20230519_151704.xlsx", 
                             "20230519_152911.xlsx", "20230519_153939.xlsx", "20230524_115654.xlsx", 
                             "20230524_182947.xlsx", "20230524_184045.xlsx", "20230526_130217.xlsx", 
                             "20230526_162454.xlsx")
file_names_control_free <- c("20221012_140638.xlsx", "20221012_141705.xlsx",  "20221012_142816.xlsx", 
                             "20221012_143901.xlsx", "20221012_145043.xlsx", "20221013_142405.xlsx", 
                             "20221013_143538.xlsx", "20221013_144706.xlsx", "20221014_144619.xlsx", 
                             "20221019_171801.xlsx")
file_names_asyn_odor <- c("20230517_160649.xlsx", "20230524_121955.xlsx", "20230524_181652.xlsx", 
                          "20230528_184900.xlsx", "20230530_173354.xlsx", "20230601_134033.xlsx", 
                          "20230601_135504.xlsx", "20230601_140814.xlsx", "20230601_171228.xlsx", 
                          "20230608_171938.xlsx")
file_names_asyn_free <- c("20221026_140512.xlsx", "20221026_152327.xlsx", "20221026_153358.xlsx", 
                          "20221026_154625.xlsx", "20221027_132328.xlsx", "20221027_144845.xlsx", 
                          "20221109_141511.xlsx", "20221109_143324.xlsx", "20221111_132812.xlsx", 
                          "20221117_151746.xlsx")
file_names_LRRK2_odor <- c("20230517_153304.xlsx", "20230517_154700.xlsx", "20230526_163749.xlsx", 
                           "20230528_183146.xlsx", "20230602_163643.xlsx", "20230602_164837.xlsx", 
                           "20230602_170012.xlsx", "20230607_103709.xlsx", "20230607_153041.xlsx", 
                           "20230607_154132.xlsx")
file_names_LRRK2_free <- c("20221007_141654.xlsx", "20221013_150346.xlsx", "20221014_143504.xlsx", 
                           "20221019_172855.xlsx", "20221019_174226.xlsx", "20221020_135957.xlsx", 
                           "20221020_141317.xlsx", "20221020_142430.xlsx", "20221021_135134.xlsx", 
                           "20221021_140636.xlsx")

# Round time (sec)
setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@Uempty/n_1ul1000EA_600s@n/")
for (file_name in file_names_control_odor) {
  matrix_list_control_odor_Xspeed[[file_name]] <- read_excel(file_name,sheet = 4)
  matrix_list_control_odor_Xspeed[[file_name]] <- matrix_list_control_odor_Xspeed[[file_name]] %>% 
    mutate(`time (s)` = floor(`time (s)`))
}
setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@Uempty/n_freeNavigation_600s@n")
for (file_name in file_names_control_free) {
  matrix_list_control_free_Xspeed[[file_name]] <- read_excel(file_name,sheet = 4)
  matrix_list_control_free_Xspeed[[file_name]] <- matrix_list_control_free_Xspeed[[file_name]] %>% 
    mutate(`time (s)` = floor(`time (s)`))
}
setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UaSynA53T/n_1ul1000EA_600s@n")
for (file_name in file_names_asyn_odor) {
  matrix_list_asyn_odor_Xspeed[[file_name]] <- read_excel(file_name,sheet = 4)
  matrix_list_asyn_odor_Xspeed[[file_name]] <- matrix_list_asyn_odor_Xspeed[[file_name]] %>% 
    mutate(`time (s)` = floor(`time (s)`))
}
setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UaSynA53T/n_freeNavigation_600s@n")
for (file_name in file_names_asyn_free) {
  matrix_list_asyn_free_Xspeed[[file_name]] <- read_excel(file_name,sheet = 4)
  matrix_list_asyn_free_Xspeed[[file_name]] <- matrix_list_asyn_free_Xspeed[[file_name]] %>% 
    mutate(`time (s)` = floor(`time (s)`))
}
setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UG2019S/n_1ul1000EA_600s@n")
for (file_name in file_names_LRRK2_odor) {
  matrix_list_LRRK2_odor_Xspeed[[file_name]] <- read_excel(file_name,sheet = 4)
  matrix_list_LRRK2_odor_Xspeed[[file_name]] <- matrix_list_LRRK2_odor_Xspeed[[file_name]] %>% 
    mutate(`time (s)` = floor(`time (s)`))
}
setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UG2019S/n_freeNavigation_600s@n")
for (file_name in file_names_LRRK2_free) {
  matrix_list_LRRK2_free_Xspeed[[file_name]] <- read_excel(file_name,sheet = 4)
  matrix_list_LRRK2_free_Xspeed[[file_name]] <- matrix_list_LRRK2_free_Xspeed[[file_name]] %>% 
    mutate(`time (s)` = floor(`time (s)`))
}

# Speed per larva per second

populations_ycoord_list <- list(matrix_list_control_odor_Xspeed, matrix_list_control_free_Xspeed,
                                matrix_list_asyn_odor_Xspeed, matrix_list_asyn_free_Xspeed,
                                matrix_list_LRRK2_odor_Xspeed, matrix_list_LRRK2_free_Xspeed)
X_speeds <- list()

for (population in populations_ycoord_list) {
  results_population <- list()
  for (file in population) {
    submatrix_df <- as.data.frame(file)
    colnames(submatrix_df) <- c("larvaID", "time (s)", "position")
    submatrix_df <- submatrix_df %>%
      arrange(larvaID, `time (s)`)
    position_dif <- submatrix_df %>%
      group_by(larvaID, `time (s)`) %>%
      summarize(dif = last(position) - first(position))
    results_population[[length(results_population) + 1]] <- position_dif
  }
  X_speeds[[length(X_speeds) + 1]] <- results_population
}

# Speed per larva

mean_vels_X <- list()

for (i in seq_along(X_speeds)) {
  population <- X_speeds[[i]]
  means_df_list <- list()
  for (j in seq_along(population)) {
    file <- population[[j]]
    means_df <- NULL
    unique_larva_ids <- unique(file$larvaID)
    for (larva_id in unique_larva_ids) {
      larva_velocities <- file[file$larvaID == larva_id, ]
      larvae_mean <- mean(larva_velocities$dif)
      means_df <- rbind(means_df, data.frame(larvaID = larva_id, mean_vel = larvae_mean))
    }
    means_df_list[[j]] <- means_df
  }
  mean_vels_X[[i]] <- means_df_list
}

# Normalize speeds by average larvae length

larvae_avg_length <- list(c(1.852366568, 1.896235619, 2.021237491,1.935850264, 1.954981966, 1.9660794, 1.799035934,1.861237015, 1.79106277, 1.841006218),
                               c(1.270373977,1.240402783,1.205876136,1.162587415,1.245212124,1.711308347,1.617508723,1.667652918,1.496879544,1.595711147),
                               c(1.883828311,1.924638912,2.00689007,2.092192452,2.040677092,1.946104666,1.921398984,1.920509885,1.787498003,1.828017683),
                               c(1.664062017, 1.588208567,1.594988147,1.607129348,1.840053803,1.623901144,1.663166184,1.558892223,1.578177557,1.723872424),
                               c(1.650845894,1.651100064,1.837324993,2.038861454,1.564580221,1.506040965,1.522741663,1.800651062,1.686677296,1.793310184),
                               c(1.727538032,2.012382609,1.976204447,1.975053859,1.954034484,1.996315397,1.952995429,1.871485412,1.839549746,1.775235987))

normalized_vel_X <- list()

for (i in seq_along(mean_vels_X)) {
  sublista <- mean_vels_X[[i]]
  vector <- larvae_avg_length[[i]]
  normalized_vel_X_sublista <- list()
  for (j in seq_along(sublista)) {
    data_frame <- sublista[[j]]
    valor <- vector[[j]]
    data_frame$vel_norm <- data_frame$mean_vel / valor
    normalized_vel_X_sublista[[j]] <- data_frame
  }
  normalized_vel_X[[i]] <- normalized_vel_X_sublista
}

# Average speeds

matrix_list_control_odor_speed <- list()
matrix_list_control_free_speed <- list()
matrix_list_asyn_odor_speed <- list()
matrix_list_asyn_free_speed <- list()
matrix_list_LRRK2_odor_speed <- list()
matrix_list_LRRK2_free_speed <- list()

# Round time (sec)
setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@Uempty/n_1ul1000EA_600s@n/")
for (file_name in file_names_control_odor) {
  matrix_list_control_odor_speed[[file_name]] <- read_excel(file_name,sheet = 2)
  matrix_list_control_odor_speed[[file_name]] <- matrix_list_control_odor_speed[[file_name]] %>% 
    mutate(`time (s)` = floor(`time (s)`))
}
setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@Uempty/n_freeNavigation_600s@n")
for (file_name in file_names_control_free) {
  matrix_list_control_free_speed[[file_name]] <- read_excel(file_name,sheet = 2)
  matrix_list_control_free_speed[[file_name]] <- matrix_list_control_free_speed[[file_name]] %>% 
    mutate(`time (s)` = floor(`time (s)`))
}
setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UaSynA53T/n_1ul1000EA_600s@n")
for (file_name in file_names_asyn_odor) {
  matrix_list_asyn_odor_speed[[file_name]] <- read_excel(file_name,sheet = 2)
  matrix_list_asyn_odor_speed[[file_name]] <- matrix_list_asyn_odor_speed[[file_name]] %>% 
    mutate(`time (s)` = floor(`time (s)`))
}
setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UaSynA53T/n_freeNavigation_600s@n")
for (file_name in file_names_asyn_free) {
  matrix_list_asyn_free_speed[[file_name]] <- read_excel(file_name,sheet = 2)
  matrix_list_asyn_free_speed[[file_name]] <- matrix_list_asyn_free_speed[[file_name]] %>% 
    mutate(`time (s)` = floor(`time (s)`))
}
setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UG2019S/n_1ul1000EA_600s@n")
for (file_name in file_names_LRRK2_odor) {
  matrix_list_LRRK2_odor_speed[[file_name]] <- read_excel(file_name,sheet = 2)
  matrix_list_LRRK2_odor_speed[[file_name]] <- matrix_list_LRRK2_odor_speed[[file_name]] %>% 
    mutate(`time (s)` = floor(`time (s)`))
}
setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UG2019S/n_freeNavigation_600s@n")
for (file_name in file_names_LRRK2_free) {
  matrix_list_LRRK2_free_speed[[file_name]] <- read_excel(file_name,sheet = 2)
  matrix_list_LRRK2_free_speed[[file_name]] <- matrix_list_LRRK2_free_speed[[file_name]] %>% 
    mutate(`time (s)` = floor(`time (s)`))
}

# Speed per larva per second

populations_speed_list <- list(matrix_list_control_odor_speed, matrix_list_control_free_speed,
                                matrix_list_asyn_odor_speed, matrix_list_asyn_free_speed,
                                matrix_list_LRRK2_odor_speed, matrix_list_LRRK2_free_speed)
speeds <- list()

for (population in populations_speed_list) {
  results_population <- list()
  for (file in population) {
    submatrix_df <- as.data.frame(file)
    submatrix_df <- submatrix_df %>%
      arrange(larvaID, `time (s)`)
    mean_speeds <- submatrix_df %>%
      group_by(larvaID, `time (s)`) %>%
      summarize(mean_speed = mean(`speed (mm/s)`))
    results_population[[length(results_population) + 1]] <- mean_speeds
  }
  speeds[[length(speeds) + 1]] <- results_population
}

# Speed per larva

mean_vels <- list()

for (i in seq_along(speeds)) {
  population <- speeds[[i]]
  means_df_list <- list()
  for (j in seq_along(population)) {
    file <- population[[j]]
    means_df <- NULL
    unique_larva_ids <- unique(file$larvaID)
    for (larva_id in unique_larva_ids) {
      larva_velocities <- file[file$larvaID == larva_id, ]
      larvae_mean <- mean(larva_velocities$mean_speed)
      means_df <- rbind(means_df, data.frame(larvaID = larva_id, mean_vel = larvae_mean))
    }
    means_df_list[[j]] <- means_df
  }
  mean_vels[[i]] <- means_df_list
}

# Normalize speeds by average larvae length

normalized_vel <- list()

for (i in seq_along(mean_vels)) {
  sublista <- mean_vels[[i]]
  vector <- larvae_avg_length[[i]]
  normalized_vel_sublista <- list()
  for (j in seq_along(sublista)) {
    data_frame <- sublista[[j]]
    valor <- vector[[j]]
    data_frame$vel_norm <- data_frame$mean_vel / valor
    normalized_vel_sublista[[j]] <- data_frame
  }
  normalized_vel[[i]] <- normalized_vel_sublista
}

# Navigation index across X axis = average speed across X axis / average speed

navigation_index_X <- list()

for (i in 1:length(normalized_vel)) {
  sublista1 <- normalized_vel_X[[i]]
  sublista2 <- normalized_vel[[i]]
  divisiones_sublista <- list()
  for (j in 1:length(sublista1)) {
    data_frame1 <- sublista1[[j]]
    data_frame2 <- sublista2[[j]]
    resultado <- data_frame1$vel_norm / data_frame2$vel_norm
    divisiones_sublista[[j]] <- resultado
  }
  navigation_index_X[[i]] <- divisiones_sublista
}

names(navigation_index_X) <- list("control_odor","control_free","aSyn-A53T_odor","aSyn-A53T_free",
                                  "LRRK2-G2019S_odor","LRRK2-G2019S_free")

# Statistics

navigation_indeces_X <- do.call(rbind, lapply(seq_along(navigation_index_X), function(i) {
  data.frame(population = names(navigation_index_X[i]), NI = unlist(navigation_index_X[[i]]))
}))

kruskal.test(NI ~ population, navigation_indeces_X)
mc <- dunn.test::dunn.test(x = navigation_indeces_X$NI, g = navigation_indeces_X$population, method = "bonferroni")

# Plot

navigation_indeces_X <- navigation_indeces_X %>%
  group_by(population) %>%
  summarize(Mean = mean(NI),
            SD = sd(NI),
            N = n()) %>%
  ungroup()

ggplot(navigation_indeces_X, aes(x = population, y = -Mean,fill=population)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.8, preserve = "single"),color="gray20") +
  geom_errorbar(aes(ymin = -Mean - SD, ymax = -Mean + SD), width = 0.25, position = position_dodge(width = 0.9),color="gray20") +
  labs(x = "Population", y = "Navigation index across X axis", title = "Navigation indices across X axis") +
  scale_fill_manual(values = c("royalblue","royalblue1", "palegreen","palegreen3", "yellow2","yellow1")) + 
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey")) +
  guides(fill=F)

# Plot normalized average speed

names(normalized_vel) <- c("Control odor","Control free","aSyn-A53T odor","aSyn-A53T free","LRRK2-G2019S odor","LRRK2-G2019S free")

normalized_vel_df <- normalized_vel %>%
  lapply(function(population) {
    population %>%
      bind_rows() %>%
      summarize(Mean = mean(vel_norm), SD = sd(vel_norm))
  }) %>%
  bind_rows() %>%
  mutate(population = names(normalized_vel))

ggplot(normalized_vel_df, aes(x = population, y = Mean, fill = population)) +
  geom_bar(stat = "identity",position = position_dodge2(width = 0.8, preserve = "single"),color="gray20") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.25, position = position_dodge(width = 0.9),color="gray20") +
  labs(title = "Average speed per population", y = "Average speed (mm/s)") +
  scale_fill_manual(values = c("royalblue","royalblue1", "palegreen","palegreen3", "yellow2","yellow1")) + 
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey")) +
  guides(fill=F)

# Statistic
normalized_vels <- data.frame(
  population = character(0),
  vel_norm = numeric(0)
)

for (i in 1:6) {
  population <- names(normalized_vel)[i]
  sublistas <- normalized_vel[[i]]
  for (j in 1:10) {
    if (!is.null(sublistas[[j]]$vel_norm)) {
      normalized_vels <- rbind(normalized_vels, data.frame(population = population, vel_norm = sublistas[[j]]$vel_norm))
    }
  }
}
rownames(normalized_vels) <- NULL

kruskal.test(vel_norm ~ population, normalized_vels)
mc <- dunn.test::dunn.test(x = normalized_vels$vel_norm, g = normalized_vels$population, method = "bonferroni")

