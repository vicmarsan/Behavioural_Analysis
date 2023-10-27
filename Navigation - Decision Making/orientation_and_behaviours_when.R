# ORIENTATIONS AND PROPORTIONS OF BEHAVIOUR EVENTS WHEN LARVAE ARE HEADING OR 
# OPPOSITE TO ODOUR

# Packages

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# File names for each population

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

# CONTROL ODOR

setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@Uempty/n_1ul1000EA_600s@n")

# Read angles from files

larvae_angle_list_control_odor <- lapply(file_names_control_odor, function(archivo) {
  datos <- readxl::read_excel(archivo, sheet = 5)
  return(datos)
})

combine_data_control_odor <- bind_rows(larvae_angle_list_control_odor)

# Asign orientations

asignar_orientacion <- function(angulo) {
  if (angulo >= 225 && angulo < 315) {
    return("heading odor")
  } else if (angulo >= 45 && angulo < 135) {
    return("opposite to odor")
  } else if (angulo >= 135 && angulo < 225) {
    return("heading top")
  } else {
    return("heading bottom")
  }
}

combine_data_control_odor$orientacion <- sapply(
  combine_data_control_odor$`angle (degrees)`, asignar_orientacion)

# Calculate  orientation proportions 

proporciones <- combine_data_control_odor %>% # Calculate total larvae per time instant
  group_by(`time (s)`, orientacion) %>%
  summarise(count = n())
total_larvae <- proporciones %>% 
  group_by(`time (s)`) %>%
  summarise(total_larvae = sum(count))
proporciones <- proporciones %>% # join proportions and total larvae
  left_join(total_larvae, by = "time (s)")
proporciones <- proporciones %>% # calculate normalized proportions
  mutate(proporcion = count / total_larvae)  
proporciones_pivot <- proporciones %>% # pivot table, one column per orientation
  pivot_wider(names_from = orientacion, values_from = proporcion, values_fill = 0)

# line plot, visualization through time

ggplot(proporciones_pivot, aes(x = proporciones_pivot$`time (s)`, group = 1)) +
  geom_line(aes(y = `heading odor`, color = "Heading Odor")) +
  geom_line(aes(y = `opposite to odor`, color = "Opposite to Odor")) +
  geom_line(aes(y = `heading top`, color = "Heading Top")) +
  geom_line(aes(y = `heading bottom`, color = "Heading Bottom")) +
  scale_color_manual(values = c("Heading Odor" = "red", "Opposite to Odor" = "blue", "Heading Top" = "green", "Heading Bottom" = "purple")) +
  labs(x = "Time (s)", y = "Larvae proportions",title = "Orientations control odor") +
  theme_minimal()

# Average orientations
mean(proporciones_pivot$`heading odor`[proporciones_pivot$`heading odor` != 0])*100
mean(proporciones_pivot$`heading bottom`[proporciones_pivot$`heading bottom` != 0])*100
mean(proporciones_pivot$`opposite to odor`[proporciones_pivot$`opposite to odor` != 0])*100
mean(proporciones_pivot$`heading top`[proporciones_pivot$`heading top` != 0])*100

# Read behaviour data from files

behaviour_data_control_odor <- lapply(file_names_control_odor, function(archivo) {
  datos <- readxl::read_excel(archivo, sheet = 6)
  return(datos)
})
behaviour_data_control_odor <- bind_rows(behaviour_data_control_odor)

control_odor <- full_join(combine_data_control_odor,
                          behaviour_data_control_odor,by=c("time (s)","larvaID"))

total_running_control_odor <- sum(control_odor$isRunning,na.rm = T)
total_turning_control_odor <- sum(control_odor$isTurning,na.rm = T)
total_casting_control_odor <- sum(control_odor$isCasting,na.rm = T)
total_stop_control_odor <- sum(control_odor$isStopped,na.rm = T)

control_odor_heading_odor <- subset(control_odor,orientacion=="heading odor")
control_odor_opposite_odor <- subset(control_odor,orientacion=="opposite to odor")

prop_run_Ho_co <- sum(control_odor_heading_odor$isRunning,na.rm = T) / total_running_control_odor
prop_run_Oo_co <- sum(control_odor_opposite_odor$isRunning,na.rm = T) / total_running_control_odor

prop_turn_Ho_co <- sum(control_odor_heading_odor$isTurning,na.rm = T) / total_turning_control_odor
prop_turn_Oo_co <- sum(control_odor_opposite_odor$isTurning,na.rm = T) / total_turning_control_odor

prop_cast_Ho_co <- sum(control_odor_heading_odor$isCasting,na.rm = T) / total_casting_control_odor
prop_cast_Oo_co <- sum(control_odor_opposite_odor$isCasting,na.rm = T) / total_casting_control_odor

prop_stop_Ho_co <- sum(control_odor_heading_odor$isStopped,na.rm = T) / total_stop_control_odor
prop_stop_Oo_co <- sum(control_odor_opposite_odor$isStopped,na.rm = T) / total_stop_control_odor


# CONTROL FREE

setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@Uempty/n_freeNavigation_600s@n")

larvae_angle_list_control_free <- lapply(file_names_control_free, function(archivo) {
  datos <- readxl::read_excel(archivo, sheet = 5)
  return(datos)
})

combine_data_control_free <- bind_rows(larvae_angle_list_control_free)

combine_data_control_free$orientacion <- sapply(
  combine_data_control_free$`angle (degrees)`, asignar_orientacion)

proporciones <- combine_data_control_free %>%
  group_by(`time (s)`, orientacion) %>%
  summarise(count = n())

total_larvae <- proporciones %>%
  group_by(`time (s)`) %>%
  summarise(total_larvae = sum(count))
proporciones <- proporciones %>%
  left_join(total_larvae, by = "time (s)")
proporciones <- proporciones %>%
  mutate(proporcion = count / total_larvae) 
proporciones_pivot <- proporciones %>%
  pivot_wider(names_from = orientacion, values_from = proporcion, values_fill = 0)


# line plot
ggplot(proporciones_pivot, aes(x = proporciones_pivot$`time (s)`, group = 1)) +
  geom_line(aes(y = `heading odor`, color = "Heading Odor")) +
  geom_line(aes(y = `opposite to odor`, color = "Opposite to Odor")) +
  geom_line(aes(y = `heading top`, color = "Heading Top")) +
  geom_line(aes(y = `heading bottom`, color = "Heading Bottom")) +
  scale_color_manual(values = c("Heading Odor" = "red", "Opposite to Odor" = "blue", "Heading Top" = "green", "Heading Bottom" = "purple")) +
  labs(x = "Time (s)", y = "Larvae proportions",title = "Orientation control free navigation") +
  theme_minimal()

# Average orientations
mean(proporciones_pivot$`heading odor`[proporciones_pivot$`heading odor` != 0])*100
mean(proporciones_pivot$`heading bottom`[proporciones_pivot$`heading bottom` != 0])*100
mean(proporciones_pivot$`opposite to odor`[proporciones_pivot$`opposite to odor` != 0])*100
mean(proporciones_pivot$`heading top`[proporciones_pivot$`heading top` != 0])*100

# Read behaviour data

behaviour_data_control_free <- lapply(file_names_control_free, function(archivo) {
  datos <- readxl::read_excel(archivo, sheet = 6)
  return(datos)
})
behaviour_data_control_free <- bind_rows(behaviour_data_control_free)

control_free <- full_join(combine_data_control_free,
                          behaviour_data_control_free,by=c("time (s)","larvaID"))

total_running_control_free <- sum(control_free$isRunning,na.rm = T)
total_turning_control_free <- sum(control_free$isTurning,na.rm = T)
total_casting_control_free <- sum(control_free$isCasting,na.rm = T)
total_stop_control_free <- sum(control_free$isStopped,na.rm = T)

control_free_heading_free <- subset(control_free,orientacion=="heading odor")
control_free_opposite_free <- subset(control_free,orientacion=="opposite to odor")

prop_run_Ho_cf <- sum(control_free_heading_free$isRunning,na.rm = T) / total_running_control_free
prop_run_Oo_cf <- sum(control_free_opposite_free$isRunning,na.rm = T) / total_running_control_free

prop_turn_Ho_cf <- sum(control_free_heading_free$isTurning,na.rm = T) / total_turning_control_free
prop_turn_Oo_cf <- sum(control_free_opposite_free$isTurning,na.rm = T) / total_turning_control_free

prop_cast_Ho_cf <- sum(control_free_heading_free$isCasting,na.rm = T) / total_casting_control_free
prop_cast_Oo_cf <- sum(control_free_opposite_free$isCasting,na.rm = T) / total_casting_control_free

prop_stop_Ho_cf <- sum(control_free_heading_free$isStopped,na.rm = T) / total_stop_control_free
prop_stop_Oo_cf <- sum(control_free_opposite_free$isStopped,na.rm = T) / total_stop_control_free

# ASYN ODOR

setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UaSynA53T/n_1ul1000EA_600s@n")

larvae_angle_list_asyn_odor <- lapply(file_names_asyn_odor, function(archivo) {
  datos <- readxl::read_excel(archivo, sheet = 5)
  return(datos)
})

combine_data_asyn_odor <- bind_rows(larvae_angle_list_asyn_odor)

combine_data_asyn_odor$orientacion <- sapply(
  combine_data_asyn_odor$`angle (degrees)`, asignar_orientacion)

proporciones <- combine_data_asyn_odor %>%
  group_by(`time (s)`, orientacion) %>%
  summarise(count = n())
total_larvae <- proporciones %>%
  group_by(`time (s)`) %>%
  summarise(total_larvae = sum(count))
proporciones <- proporciones %>%
  left_join(total_larvae, by = "time (s)")
proporciones <- proporciones %>%
  mutate(proporcion = count / total_larvae) 
proporciones_pivot <- proporciones %>%
  pivot_wider(names_from = orientacion, values_from = proporcion, values_fill = 0)

# line plot
ggplot(proporciones_pivot, aes(x = proporciones_pivot$`time (s)`, group = 1)) +
  geom_line(aes(y = `heading odor`, color = "Heading Odor")) +
  geom_line(aes(y = `opposite to odor`, color = "Opposite to Odor")) +
  geom_line(aes(y = `heading top`, color = "Heading Top")) +
  geom_line(aes(y = `heading bottom`, color = "Heading Bottom")) +
  scale_color_manual(values = c("Heading Odor" = "red", "Opposite to Odor" = "blue", "Heading Top" = "green", "Heading Bottom" = "purple")) +
  labs(x = "Time (s)", y = "Larvae proportions",title = "Orientation aSyn-A53T odor") +
  theme_minimal()

# Average orientations
mean(proporciones_pivot$`heading odor`[proporciones_pivot$`heading odor` != 0])*100
mean(proporciones_pivot$`heading bottom`[proporciones_pivot$`heading bottom` != 0])*100
mean(proporciones_pivot$`opposite to odor`[proporciones_pivot$`opposite to odor` != 0])*100
mean(proporciones_pivot$`heading top`[proporciones_pivot$`heading top` != 0])*100

# Read behaviour data

behaviour_data_asyn_odor <- lapply(file_names_asyn_odor, function(archivo) {
  datos <- readxl::read_excel(archivo, sheet = 6)
  return(datos)
})
behaviour_data_asyn_odor <- bind_rows(behaviour_data_asyn_odor)

asyn_odor <- full_join(combine_data_asyn_odor,
                          behaviour_data_asyn_odor,by=c("time (s)","larvaID"))

total_running_asyn_odor <- sum(asyn_odor$isRunning,na.rm = T)
total_turning_asyn_odor <- sum(asyn_odor$isTurning,na.rm = T)
total_casting_asyn_odor <- sum(asyn_odor$isCasting,na.rm = T)
total_stop_asyn_odor <- sum(asyn_odor$isStopped,na.rm = T)

asyn_odor_heading_odor <- subset(asyn_odor,orientacion=="heading odor")
asyn_odor_opposite_odor <- subset(asyn_odor,orientacion=="opposite to odor")

prop_run_Ho_ao <- sum(asyn_odor_heading_odor$isRunning,na.rm = T) / total_running_asyn_odor
prop_run_Oo_ao <- sum(asyn_odor_opposite_odor$isRunning,na.rm = T) / total_running_asyn_odor

prop_turn_Ho_ao <- sum(asyn_odor_heading_odor$isTurning,na.rm = T) / total_turning_asyn_odor
prop_turn_Oo_ao <- sum(asyn_odor_opposite_odor$isTurning,na.rm = T) / total_turning_asyn_odor

prop_cast_Ho_ao <- sum(asyn_odor_heading_odor$isCasting,na.rm = T) / total_casting_asyn_odor
prop_cast_Oo_ao <- sum(asyn_odor_opposite_odor$isCasting,na.rm = T) / total_casting_asyn_odor

prop_stop_Ho_ao <- sum(asyn_odor_heading_odor$isStopped,na.rm = T) / total_stop_asyn_odor
prop_stop_Oo_ao <- sum(asyn_odor_opposite_odor$isStopped,na.rm = T) / total_stop_asyn_odor

# ASYN FREE

setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UaSynA53T/n_freeNavigation_600s@n")

larvae_angle_list_asyn_free <- lapply(file_names_asyn_free, function(archivo) {
  datos <- readxl::read_excel(archivo, sheet = 5)
  return(datos)
})

combine_data_asyn_free <- bind_rows(larvae_angle_list_asyn_free)

combine_data_asyn_free$orientacion <- sapply(
  combine_data_asyn_free$`angle (degrees)`, asignar_orientacion)

proporciones <- combine_data_asyn_free %>%
  group_by(`time (s)`, orientacion) %>%
  summarise(count = n())
total_larvae <- proporciones %>%
  group_by(`time (s)`) %>%
  summarise(total_larvae = sum(count))
proporciones <- proporciones %>%
  left_join(total_larvae, by = "time (s)")
proporciones <- proporciones %>%
  mutate(proporcion = count / total_larvae) 
proporciones_pivot <- proporciones %>%
  pivot_wider(names_from = orientacion, values_from = proporcion, values_fill = 0)

# line plot
ggplot(proporciones_pivot, aes(x = proporciones_pivot$`time (s)`, group = 1)) +
  geom_line(aes(y = `heading odor`, color = "Heading Odor")) +
  geom_line(aes(y = `opposite to odor`, color = "Opposite to Odor")) +
  geom_line(aes(y = `heading top`, color = "Heading Top")) +
  geom_line(aes(y = `heading bottom`, color = "Heading Bottom")) +
  scale_color_manual(values = c("Heading Odor" = "red", "Opposite to Odor" = "blue", "Heading Top" = "green", "Heading Bottom" = "purple")) +
  labs(x = "Time (s)", y = "Larvae proportions",title = "Orientation aSyn-A53T free navigation") +
  theme_minimal()

# Average orientations
mean(proporciones_pivot$`heading odor`[proporciones_pivot$`heading odor` != 0])*100
mean(proporciones_pivot$`heading bottom`[proporciones_pivot$`heading bottom` != 0])*100
mean(proporciones_pivot$`opposite to odor`[proporciones_pivot$`opposite to odor` != 0])*100
mean(proporciones_pivot$`heading top`[proporciones_pivot$`heading top` != 0])*100

# Read behaviour data

behaviour_data_asyn_free <- lapply(file_names_asyn_free, function(archivo) {
  datos <- readxl::read_excel(archivo, sheet = 6)
  return(datos)
})
behaviour_data_asyn_free <- bind_rows(behaviour_data_asyn_free)

asyn_free <- full_join(combine_data_asyn_free,
                          behaviour_data_asyn_free,by=c("time (s)","larvaID"))

total_running_asyn_free <- sum(asyn_free$isRunning,na.rm = T)
total_turning_asyn_free <- sum(asyn_free$isTurning,na.rm = T)
total_casting_asyn_free <- sum(asyn_free$isCasting,na.rm = T)
total_stop_asyn_free <- sum(asyn_free$isStopped,na.rm = T)

asyn_free_heading_odor <- subset(asyn_free,orientacion=="heading odor")
asyn_free_opposite_odor <- subset(asyn_free,orientacion=="opposite to odor")

prop_run_Ho_af <- sum(asyn_free_heading_odor$isRunning,na.rm = T) / total_running_asyn_free
prop_run_Oo_af <- sum(asyn_free_opposite_odor$isRunning,na.rm = T) / total_running_asyn_free

prop_turn_Ho_af <- sum(asyn_free_heading_odor$isTurning,na.rm = T) / total_turning_asyn_free
prop_turn_Oo_af <- sum(asyn_free_opposite_odor$isTurning,na.rm = T) / total_turning_asyn_free

prop_cast_Ho_af <- sum(asyn_free_heading_odor$isCasting,na.rm = T) / total_casting_asyn_free
prop_cast_Oo_af <- sum(asyn_free_opposite_odor$isCasting,na.rm = T) / total_casting_asyn_free

prop_stop_Ho_af <- sum(asyn_free_heading_odor$isStopped,na.rm = T) / total_stop_asyn_free
prop_stop_Oo_af <- sum(asyn_free_opposite_odor$isStopped,na.rm = T) / total_stop_asyn_free

# G2019S ODOR

setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UG2019S/n_1ul1000EA_600s@n")

# Read angle data

larvae_angle_list_G2019S_odor <- lapply(file_names_LRRK2_odor, function(archivo) {
  datos <- readxl::read_excel(archivo, sheet = 5)
  return(datos)
})

combine_data_G2019S_odor <- bind_rows(larvae_angle_list_G2019S_odor)

combine_data_G2019S_odor$orientacion <- sapply(
  combine_data_G2019S_odor$`angle (degrees)`, asignar_orientacion)

proporciones <- combine_data_LRRK2_odor %>%
  group_by(`time (s)`, orientacion) %>%
  summarise(count = n())
total_larvae <- proporciones %>%
  group_by(`time (s)`) %>%
  summarise(total_larvae = sum(count))
proporciones <- proporciones %>%
  left_join(total_larvae, by = "time (s)")
proporciones <- proporciones %>%
  mutate(proporcion = count / total_larvae) 
proporciones_pivot <- proporciones %>%
  pivot_wider(names_from = orientacion, values_from = proporcion, values_fill = 0)

# line plot
ggplot(proporciones_pivot, aes(x = proporciones_pivot$`time (s)`, group = 1)) +
  geom_line(aes(y = `heading odor`, color = "Heading Odor")) +
  geom_line(aes(y = `opposite to odor`, color = "Opposite to Odor")) +
  geom_line(aes(y = `heading top`, color = "Heading Top")) +
  geom_line(aes(y = `heading bottom`, color = "Heading Bottom")) +
  scale_color_manual(values = c("Heading Odor" = "red", "Opposite to Odor" = "blue", "Heading Top" = "green", "Heading Bottom" = "purple")) +
  labs(x = "Time (s)", y = "Larvae proportions",title = "Orientation hLRRK2-G2019S odor") +
  theme_minimal()

# Average orientations
mean(proporciones_pivot$`heading odor`[proporciones_pivot$`heading odor` != 0])*100
mean(proporciones_pivot$`heading bottom`[proporciones_pivot$`heading bottom` != 0])*100
mean(proporciones_pivot$`opposite to odor`[proporciones_pivot$`opposite to odor` != 0])*100
mean(proporciones_pivot$`heading top`[proporciones_pivot$`heading top` != 0])*100

# Read behaviour

behaviour_data_G2019S_odor <- lapply(file_names_LRRK2_odor, function(archivo) {
  datos <- readxl::read_excel(archivo, sheet = 6)
  return(datos)
})
behaviour_data_G2019S_odor <- bind_rows(behaviour_data_G2019S_odor)

G2019S_odor <- full_join(combine_data_G2019S_odor,
                          behaviour_data_G2019S_odor,by=c("time (s)","larvaID"))

total_running_G2019S_odor <- sum(G2019S_odor$isRunning,na.rm = T)
total_turning_G2019S_odor <- sum(G2019S_odor$isTurning,na.rm = T)
total_casting_G2019S_odor <- sum(G2019S_odor$isCasting,na.rm = T)
total_stop_G2019S_odor <- sum(G2019S_odor$isStopped,na.rm = T)

G2019S_odor_heading_odor <- subset(G2019S_odor,orientacion=="heading odor")
G2019S_odor_opposite_odor <- subset(G2019S_odor,orientacion=="opposite to odor")

prop_run_Ho_lo <- sum(G2019S_odor_heading_odor$isRunning,na.rm = T) / total_running_G2019S_odor
prop_run_Oo_lo <- sum(G2019S_odor_opposite_odor$isRunning,na.rm = T) / total_running_G2019S_odor

prop_turn_Ho_lo <- sum(G2019S_odor_heading_odor$isTurning,na.rm = T) / total_turning_G2019S_odor
prop_turn_Oo_lo <- sum(G2019S_odor_opposite_odor$isTurning,na.rm = T) / total_turning_G2019S_odor

prop_cast_Ho_lo <- sum(G2019S_odor_heading_odor$isCasting,na.rm = T) / total_casting_G2019S_odor
prop_cast_Oo_lo <- sum(G2019S_odor_opposite_odor$isCasting,na.rm = T) / total_casting_G2019S_odor

prop_stop_Ho_lo <- sum(G2019S_odor_heading_odor$isStopped,na.rm = T) / total_stop_G2019S_odor
prop_stop_Oo_lo <- sum(G2019S_odor_opposite_odor$isStopped,na.rm = T) / total_stop_G2019S_odor

# G2019S FREE

setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UG2019S/n_freeNavigation_600s@n")

# Read angle data

larvae_angle_list_LRRK2_free <- lapply(file_names_LRRK2_free, function(archivo) {
  datos <- readxl::read_excel(archivo, sheet = 5)
  return(datos)
})

combine_data_LRRK2_free <- bind_rows(larvae_angle_list_LRRK2_free)

combine_data_LRRK2_free$orientacion <- sapply(
  combine_data_LRRK2_free$`angle (degrees)`, asignar_orientacion)

proporciones <- combine_data_LRRK2_free %>%
  group_by(`time (s)`, orientacion) %>%
  summarise(count = n())
total_larvae <- proporciones %>%
  group_by(`time (s)`) %>%
  summarise(total_larvae = sum(count))
proporciones <- proporciones %>%
  left_join(total_larvae, by = "time (s)")
proporciones <- proporciones %>%
  mutate(proporcion = count / total_larvae) 
proporciones_pivot <- proporciones %>%
  pivot_wider(names_from = orientacion, values_from = proporcion, values_fill = 0)

# line plot
ggplot(proporciones_pivot, aes(x = proporciones_pivot$`time (s)`, group = 1)) +
  geom_line(aes(y = `heading odor`, color = "Heading Odor")) +
  geom_line(aes(y = `opposite to odor`, color = "Opposite to Odor")) +
  geom_line(aes(y = `heading top`, color = "Heading Top")) +
  geom_line(aes(y = `heading bottom`, color = "Heading Bottom")) +
  scale_color_manual(values = c("Heading Odor" = "red", "Opposite to Odor" = "blue", "Heading Top" = "green", "Heading Bottom" = "purple")) +
  labs(x = "Time (s)", y = "Larvae proportions",title = "Orientation hLRRK2-G2019S free navigation") +
  theme_minimal()

# Average orientations
mean(proporciones_pivot$`heading odor`[proporciones_pivot$`heading odor` != 0])*100
mean(proporciones_pivot$`heading bottom`[proporciones_pivot$`heading bottom` != 0])*100
mean(proporciones_pivot$`opposite to odor`[proporciones_pivot$`opposite to odor` != 0])*100
mean(proporciones_pivot$`heading top`[proporciones_pivot$`heading top` != 0])*100

# Read behaviour data

behaviour_data_LRRK2_free <- lapply(file_names_LRRK2_free, function(archivo) {
  datos <- readxl::read_excel(archivo, sheet = 6)
  return(datos)
})
behaviour_data_LRRK2_free <- bind_rows(behaviour_data_LRRK2_free)

LRRK2_free <- full_join(combine_data_LRRK2_free,
                          behaviour_data_LRRK2_free,by=c("time (s)","larvaID"))

total_running_LRRK2_free <- sum(LRRK2_free$isRunning,na.rm = T)
total_turning_LRRK2_free <- sum(LRRK2_free$isTurning,na.rm = T)
total_casting_LRRK2_free <- sum(LRRK2_free$isCasting,na.rm = T)
total_stop_LRRK2_free <- sum(LRRK2_free$isStopped,na.rm = T)

LRRK2_free_heading_odor <- subset(LRRK2_free,orientacion=="heading odor")
LRRK2_free_opposite_odor <- subset(LRRK2_free,orientacion=="opposite to odor")

prop_run_Ho_lf <- sum(LRRK2_free_heading_odor$isRunning,na.rm = T) / total_running_LRRK2_free
prop_run_Oo_lf <- sum(LRRK2_free_opposite_odor$isRunning,na.rm = T) / total_running_LRRK2_free

prop_turn_Ho_lf <- sum(LRRK2_free_heading_odor$isTurning,na.rm = T) / total_turning_LRRK2_free
prop_turn_Oo_lf <- sum(LRRK2_free_opposite_odor$isTurning,na.rm = T) / total_turning_LRRK2_free

prop_cast_Ho_lf <- sum(LRRK2_free_heading_odor$isCasting,na.rm = T) / total_casting_LRRK2_free
prop_cast_Oo_lf <- sum(LRRK2_free_opposite_odor$isCasting,na.rm = T) / total_casting_LRRK2_free

prop_stop_Ho_lf <- sum(LRRK2_free_heading_odor$isStopped,na.rm = T) / total_stop_LRRK2_free
prop_stop_Oo_lf <- sum(LRRK2_free_opposite_odor$isStopped,na.rm = T) / total_stop_LRRK2_free


# Statistic: control odour vs mutations odour

run_c_a <- as.table(rbind(c(102604,71355),c(79800,48764)))
chisq.test(run_c_a)
run_c_l <- as.table(rbind(c(102604,71355),c(95993,66243)))
chisq.test(run_c_l)

cast_c_a <- as.table(rbind(c(20664,15251),c(23119,18774)))
chisq.test(cast_c_a)
cast_c_l <- as.table(rbind(c(20664,15251),c(18129,14941)))
chisq.test(cast_c_l)

turn_c_a <- as.table(rbind(c(917,665),c(1635,1108)))
chisq.test(turn_c_a)
turn_c_l <- as.table(rbind(c(917,665),c(1709,1268)))
chisq.test(turn_c_l)

stop_c_a <- as.table(rbind(c(406,318),c(334,222)))
chisq.test(stop_c_a)
stop_c_l <- as.table(rbind(c(406,318),c(711,404)))
chisq.test(stop_c_l)

# statistic orientations
orientation_proportions <- read.csv("orientation_proportions")

shapiro.test(orientation_proportions$proportion)
ks.test(orientation_proportions$proportion,"pnorm")
leveneTest(proportion ~ population,orientation_proportions)
# No cumplen ni normalidad ni igualdad de varianzas

#Diferencias en poblaciones para cada orientación
resultados_kruskal <- list()
for (orientation in unique(orientation_proportions$orientation)) {
  resultado <- kruskal.test(proportion ~ population, data = orientation_proportions[orientation_proportions$orientation == orientation, ])
  resultados_kruskal[[orientation]] <- resultado
}

# Dunn-test
mc_results <- list()
for (orientation in unique(orientation_proportions$orientation)) {
  data_actual_orientation <- orientation_proportions[orientation_proportions$orientation == orientation, ]
  comparisons_dunn <- dunn.test::dunn.test(data_actual_orientation$proportion, 
                                           g = data_actual_orientation$population, 
                                           method = "bonferroni")
  mc_results[[as.character(orientation)]] <- comparisons_dunn
}

desviaciones <- orientation_proportions %>%
  group_by(population,orientation) %>%
  summarise(Mean=mean(proportion),SD=sd(proportion))
