# For each population:

# Packages

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# File names

files_control_odor <- c("20230517_152119.xlsx", "20230519_150636.xlsx", 
                        "20230519_151704.xlsx", "20230519_152911.xlsx", 
                        "20230519_153939.xlsx", "20230524_115654.xlsx", 
                        "20230524_182947.xlsx", "20230524_184045.xlsx", 
                        "20230526_130217.xlsx", "20230526_162454.xlsx")

# Read sheet 5 with larvae angle data

larvae_angle_list_control_odor <- lapply(files_control_odor, function(archivo) {
  datos <- readxl::read_excel(archivo, sheet = 5)
  return(datos)
})

combine_data_control_odor <- bind_rows(larvae_angle_list_control_odor)

# Function to asign orientation
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

# Calculate proportions
proporciones <- combine_data_control_odor %>%
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

# line plot, visualization
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

# Read sheet 6 with behaviour data
behaviour_data_control_odor <- lapply(files_control_odor, function(archivo) {
  datos <- readxl::read_excel(archivo, sheet = 6)
  return(datos)
})
behaviour_data_control_odor <- bind_rows(behaviour_data_control_odor)

turn_control_odor <- full_join(combine_data_control_odor,
                               behaviour_data_control_odor,by=c("time (s)","larvaID"))

heading_odor <- subset(turn_control_odor,orientacion=="heading odor")

turn_probability_Ho_tiempo <- heading_odor %>%
  group_by(`time (s)`) %>%
  summarize(turn_probability = mean(isTurning, na.rm = TRUE))

write.table(turn_probability_Ho_tiempo,file = "turn_probability_Ho_tiempo")

turn_probability_Ho_larva <- heading_odor %>%
  group_by(larvaID) %>%
  summarize(turn_probability = mean(isTurning,na.rm = T))

write.table(turn_probability_Ho_larva,file = "turn_probability_Ho_larva")

# casting and turning when heading odor
dir_change_Ho <- heading_odor %>%
  group_by(`time (s)`) %>%
  summarize(dir_change_prob = mean(isTurning+isCasting,na.rm = T))
write.table(dir_change_Ho,file = "dir_change_Ho")

turning_rate_heading_odor <- mean(heading_odor$isTurning,na.rm = T)

opposite_odor <- subset(turn_control_odor,orientacion=="opposite to odor")

turn_probability_Oo_tiempo <- opposite_odor %>%
  group_by(`time (s)`) %>%
  summarize(turn_probability = mean(isTurning, na.rm = TRUE))

write.table(turn_probability_Oo_tiempo,file = "turn_probability_Oo_tiempo")

turn_probability_Oo_larva <- opposite_odor %>%
  group_by(larvaID) %>%
  summarize(turn_probability = mean(isTurning,na.rm = T))

write.table(turn_probability_Oo_larva,file = "turn_probability_Oo_larva")

turning_rate_opposite_odor <- mean(opposite_odor$isTurning,na.rm = T)
# casting and turning when opposite to odor
dir_change_Oo <- opposite_odor %>%
  group_by(`time (s)`) %>%
  summarize(dir_change_prob = mean(isTurning+isCasting,na.rm = T))
write.table(dir_change_Oo,file = "dir_change_Oo")

# events proportions in different orientations

pause_heading_odor <- mean(heading_odor$isStopped,na.rm = T)
pause_opposite_odor <- mean(opposite_odor$isStopped,na.rm = T)

casting_heading_odor <- mean(heading_odor$isCasting,na.rm = T)
casting_opposite_odor <- mean(opposite_odor$isCasting,na.rm = T)

run_heading_odor <- mean(heading_odor$isRunning,na.rm = T)
run_opposite_odor <- mean(opposite_odor$isRunning,na.rm = T)