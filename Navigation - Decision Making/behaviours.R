# BEHAVIOURS PERFORMED BY EACH POPULATION

# Packages

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# File names
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

# Turning rates and proportions

all_turning_rates_control_odor <- list()
all_turning_rates_control_free <- list()
all_turning_rates_asyn_odor <- list()
all_turning_rates_asyn_free <- list()
all_turning_rates_LRRK2_odor <- list()
all_turning_rates_LRRK2_free <- list()

all_prop_running_co <- list()
all_prop_casting_co <- list()
all_prop_turning_co <- list()
all_prop_stopped_co <- list()
all_prop_running_cf <- list()
all_prop_casting_cf <- list()
all_prop_turning_cf <- list()
all_prop_stopped_cf <- list()
all_prop_running_ao <- list()
all_prop_casting_ao <- list()
all_prop_turning_ao <- list()
all_prop_stopped_ao <- list()
all_prop_running_af <- list()
all_prop_casting_af <- list()
all_prop_turning_af <- list()
all_prop_stopped_af <- list()
all_prop_running_lo <- list()
all_prop_casting_lo <- list()
all_prop_turning_lo <- list()
all_prop_stopped_lo <- list()
all_prop_running_lf <- list()
all_prop_casting_lf <- list()
all_prop_turning_lf <- list()
all_prop_stopped_lf <- list()

# CONTROL ODOR

# read and process files
setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@Uempty/n_1ul1000EA_600s@n")

for (file_name in file_names_control_odor) {
  data <- read_excel(file_name,sheet = "Behaviour Class")
  # min time per larvae
  min_time <- data %>%
    group_by(`larvaID`) %>%
    summarise(min_time = min(`time (s)`))
  # Aling data
  data <- data %>%
    left_join(min_time, by = "larvaID") %>%
    mutate(time_aligned = `time (s)` - min_time)
  # calculate turning rate
  turning_rates <- data %>%
    group_by(`larvaID`) %>%
    summarise(turning_rate = mean(`isTurning`))
  all_turning_rates_control_odor[[file_name]] <- turning_rates$turning_rate
  #proportion of larvae running
  running_co <- data %>%
    select(`time_aligned`, isRunning) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isRunning = mean(isRunning)) 
  all_prop_running_co[[file_name]] <- running_co$prop_isRunning
  #proportion of larvae casting
  casting_co <- data %>%
    select(`time_aligned`, isCasting) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isCasting = mean(isCasting)) 
  all_prop_casting_co[[file_name]] <- casting_co$prop_isCasting
  #proportion of larvae turning
  turning_co <- data %>%
    select(`time_aligned`, isTurning) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isTurning = mean(isTurning)) 
  all_prop_turning_co[[file_name]] <- turning_co$prop_isTurning
  #proportion of larvae stopped
  stopped_co <- data %>%
    select(`time_aligned`, isStopped) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isStopped = mean(isStopped)) 
  all_prop_stopped_co[[file_name]] <- stopped_co$prop_isStopped
  
}

# CONTROL FREE

setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@Uempty/n_freeNavigation_600s@n")
for (file_name in file_names_control_free) {
  data <- read_excel(file_name,sheet = "Behaviour Class")
  min_time <- data %>%
    group_by(`larvaID`) %>%
    summarise(min_time = min(`time (s)`))
  data <- data %>%
    left_join(min_time, by = "larvaID") %>%
    mutate(time_aligned = `time (s)` - min_time)
  turning_rates <- data %>%
    group_by(`larvaID`) %>%
    summarise(turning_rate = mean(`isTurning`))
  all_turning_rates_control_free[[file_name]] <- turning_rates$turning_rate
  #proportion of larvae running
  running_cf <- data %>%
    select(`time_aligned`, isRunning) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isRunning = mean(isRunning)) 
  all_prop_running_cf[[file_name]] <- running_cf$prop_isRunning
  #proportion of larvae casting
  casting_cf <- data %>%
    select(`time_aligned`, isCasting) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isCasting = mean(isCasting)) 
  all_prop_casting_cf[[file_name]] <- casting_cf$prop_isCasting
  #proportion of larvae turning
  turning_cf <- data %>%
    select(`time_aligned`, isTurning) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isTurning = mean(isTurning)) 
  all_prop_turning_cf[[file_name]] <- turning_cf$prop_isTurning
  #proportion of larvae stopped
  stopped_cf <- data %>%
    select(`time_aligned`, isStopped) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isStopped = mean(isStopped)) 
  all_prop_stopped_cf[[file_name]] <- stopped_cf$prop_isStopped
}

# ASYN ODOR

setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UaSynA53T/n_1ul1000EA_600s@n")
for (file_name in file_names_asyn_odor) {
  data <- read_excel(file_name,sheet = "Behaviour Class")
  min_time <- data %>%
    group_by(`larvaID`) %>%
    summarise(min_time = min(`time (s)`))
  data <- data %>%
    left_join(min_time, by = "larvaID") %>%
    mutate(time_aligned = `time (s)` - min_time)
  turning_rates <- data %>%
    group_by(`larvaID`) %>%
    summarise(turning_rate = mean(`isTurning`))
  all_turning_rates_asyn_odor[[file_name]] <- turning_rates$turning_rate
  #proportion of larvae running
  running_ao <- data %>%
    select(`time_aligned`, isRunning) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isRunning = mean(isRunning)) 
  all_prop_running_ao[[file_name]] <- running_ao$prop_isRunning
  #proportion of larvae casting
  casting_ao <- data %>%
    select(`time_aligned`, isCasting) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isCasting = mean(isCasting)) 
  all_prop_casting_ao[[file_name]] <- casting_ao$prop_isCasting
  #proportion of larvae turning
  turning_ao <- data %>%
    select(`time_aligned`, isTurning) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isTurning = mean(isTurning)) 
  all_prop_turning_ao[[file_name]] <- turning_ao$prop_isTurning
  #proportion of larvae stopped
  stopped_ao <- data %>%
    select(`time_aligned`, isStopped) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isStopped = mean(isStopped)) 
  all_prop_stopped_ao[[file_name]] <- stopped_ao$prop_isStopped
}

# ASYN FREE

setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UaSynA53T/n_freeNavigation_600s@n")
for (file_name in file_names_asyn_free) {
  data <- read_excel(file_name,sheet = "Behaviour Class")
  min_time <- data %>%
    group_by(`larvaID`) %>%
    summarise(min_time = min(`time (s)`))
  data <- data %>%
    left_join(min_time, by = "larvaID") %>%
    mutate(time_aligned = `time (s)` - min_time)
  turning_rates <- data %>%
    group_by(`larvaID`) %>%
    summarise(turning_rate = mean(`isTurning`))
  all_turning_rates_asyn_free[[file_name]] <- turning_rates$turning_rate
  #proportion of larvae running
  running_af <- data %>%
    select(`time_aligned`, isRunning) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isRunning = mean(isRunning)) 
  all_prop_running_af[[file_name]] <- running_af$prop_isRunning
  #proportion of larvae casting
  casting_af <- data %>%
    select(`time_aligned`, isCasting) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isCasting = mean(isCasting)) 
  all_prop_casting_af[[file_name]] <- casting_af$prop_isCasting
  #proportion of larvae turning
  turning_af <- data %>%
    select(`time_aligned`, isTurning) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isTurning = mean(isTurning)) 
  all_prop_turning_af[[file_name]] <- turning_af$prop_isTurning
  #proportion of larvae stopped
  stopped_af <- data %>%
    select(`time_aligned`, isStopped) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isStopped = mean(isStopped)) 
  all_prop_stopped_af[[file_name]] <- stopped_af$prop_isStopped
}

# LRRK2 ODOR

setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UG2019S/n_1ul1000EA_600s@n")
for (file_name in file_names_LRRK2_odor) {
  data <- read_excel(file_name,sheet = "Behaviour Class")
  min_time <- data %>%
    group_by(`larvaID`) %>%
    summarise(min_time = min(`time (s)`))
  data <- data %>%
    left_join(min_time, by = "larvaID") %>%
    mutate(time_aligned = `time (s)` - min_time)
  turning_rates <- data %>%
    group_by(`larvaID`) %>%
    summarise(turning_rate = mean(`isTurning`))
  all_turning_rates_LRRK2_odor[[file_name]] <- turning_rates$turning_rate
  #proportion of larvae running
  running_lo <- data %>%
    select(`time_aligned`, isRunning) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isRunning = mean(isRunning)) 
  all_prop_running_lo[[file_name]] <- running_lo$prop_isRunning
  #proportion of larvae casting
  casting_lo <- data %>%
    select(`time_aligned`, isCasting) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isCasting = mean(isCasting)) 
  all_prop_casting_lo[[file_name]] <- casting_lo$prop_isCasting
  #proportion of larvae turning
  turning_lo <- data %>%
    select(`time_aligned`, isTurning) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isTurning = mean(isTurning)) 
  all_prop_turning_lo[[file_name]] <- turning_lo$prop_isTurning
  #proportion of larvae stopped
  stopped_lo <- data %>%
    select(`time_aligned`, isStopped) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isStopped = mean(isStopped)) 
  all_prop_stopped_lo[[file_name]] <- stopped_lo$prop_isStopped
}

# LRRK2 FREE

setwd("C:/Users/viky/Desktop/TFM/decision making/Navigation - Decision Making/thG@UG2019S/n_freeNavigation_600s@n")
for (file_name in file_names_LRRK2_free) {
  data <- read_excel(file_name,sheet = "Behaviour Class")
  min_time <- data %>%
    group_by(`larvaID`) %>%
    summarise(min_time = min(`time (s)`))
  data <- data %>%
    left_join(min_time, by = "larvaID") %>%
    mutate(time_aligned = `time (s)` - min_time)
  turning_rates <- data %>%
    group_by(`larvaID`) %>%
    summarise(turning_rate = mean(`isTurning`))
  all_turning_rates_LRRK2_free[[file_name]] <- turning_rates$turning_rate
  #proportion of larvae running
  running_lf <- data %>%
    select(`time_aligned`, isRunning) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isRunning = mean(isRunning)) 
  all_prop_running_lf[[file_name]] <- running_lf$prop_isRunning
  #proportion of larvae casting
  casting_lf <- data %>%
    select(`time_aligned`, isCasting) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isCasting = mean(isCasting)) 
  all_prop_casting_lf[[file_name]] <- casting_lf$prop_isCasting
  #proportion of larvae turning
  turning_lf <- data %>%
    select(`time_aligned`, isTurning) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isTurning = mean(isTurning)) 
  all_prop_turning_lf[[file_name]] <- turning_lf$prop_isTurning
  #proportion of larvae stopped
  stopped_lf <- data %>%
    select(`time_aligned`, isStopped) %>%
    group_by(`time_aligned`) %>%
    summarise(prop_isStopped = mean(isStopped)) 
  all_prop_stopped_lf[[file_name]] <- stopped_lf$prop_isStopped
}

# plot

combined_proportions_behaviour <- data.frame(population=c("control odor","control free",
                                                          "aSyn-A53T odor","aSyn-A53T free",
                                                          "LRRK2-G2019S odor","LRRK2-G2019S free"),
                                             Casting=c(mean(unlist(all_prop_casting_co)),
                                                       mean(unlist(all_prop_casting_cf)),
                                                       mean(unlist(all_prop_casting_ao)),
                                                       mean(unlist(all_prop_casting_af)),
                                                       mean(unlist(all_prop_casting_lo)),
                                                       mean(unlist(all_prop_casting_lf))),
                                             Turning=c(mean(unlist(all_prop_turning_co)),
                                                       mean(unlist(all_prop_turning_cf)),
                                                       mean(unlist(all_prop_turning_ao)),
                                                       mean(unlist(all_prop_turning_af)),
                                                       mean(unlist(all_prop_turning_lo)),
                                                       mean(unlist(all_prop_turning_lf))),
                                             Running=c(mean(unlist(all_prop_running_co)),
                                                       mean(unlist(all_prop_running_cf)),
                                                       mean(unlist(all_prop_running_ao)),
                                                       mean(unlist(all_prop_running_af)),
                                                       mean(unlist(all_prop_running_lo)),
                                                       mean(unlist(all_prop_running_lf))),
                                             Stopped=c(mean(unlist(all_prop_stopped_co)),
                                                       mean(unlist(all_prop_stopped_cf)),
                                                       mean(unlist(all_prop_stopped_ao)),
                                                       mean(unlist(all_prop_stopped_af)),
                                                       mean(unlist(all_prop_stopped_lo)),
                                                       mean(unlist(all_prop_stopped_lf))),
                                             Casting_SD=c(sd(unlist(all_prop_casting_co)),
                                                          sd(unlist(all_prop_casting_cf)),
                                                          sd(unlist(all_prop_casting_ao)),
                                                          sd(unlist(all_prop_casting_af)),
                                                          sd(unlist(all_prop_casting_lo)),
                                                          sd(unlist(all_prop_casting_lf))),
                                             Turning_SD=c(sd(unlist(all_prop_turning_co)),
                                                       sd(unlist(all_prop_turning_cf)),
                                                       sd(unlist(all_prop_turning_ao)),
                                                       sd(unlist(all_prop_turning_af)),
                                                       sd(unlist(all_prop_turning_lo)),
                                                       sd(unlist(all_prop_turning_lf))),
                                             Running_SD=c(sd(unlist(all_prop_running_co)),
                                                       sd(unlist(all_prop_running_cf)),
                                                       sd(unlist(all_prop_running_ao)),
                                                       sd(unlist(all_prop_running_af)),
                                                       sd(unlist(all_prop_running_lo)),
                                                       sd(unlist(all_prop_running_lf))),
                                             Stopped_SD=c(sd(unlist(all_prop_stopped_co)),
                                                       sd(unlist(all_prop_stopped_cf)),
                                                       sd(unlist(all_prop_stopped_ao)),
                                                       sd(unlist(all_prop_stopped_af)),
                                                       sd(unlist(all_prop_stopped_lo)),
                                                       sd(unlist(all_prop_stopped_lf))))

combined_proportions_behaviour <- combined_proportions_behaviour %>%
  mutate(Total = rowSums(select(., -population))) %>%
  mutate_at(vars(Casting:Stopped), funs(. *100 / Total))

combined_proportions_behaviour_long <- combined_proportions_behaviour %>%
  pivot_longer(cols = 2:5, names_to = "behaviour") 

ggplot(combined_proportions_behaviour_long, aes(x = population, y = value, fill = behaviour)) +
  geom_bar(stat = "identity") +
  labs(title = "Larvae proportions performing each behaviour", y = "Larvae proportions") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# view through time to explain high SD

df <- bind_rows(lapply(all_prop_casting_ao, as.data.frame), .id = "Source")
longitud_maxima <- max(sapply(all_prop_casting_ao, length))
tiempo <- 1:longitud_maxima
lista_valores <- lapply(all_prop_casting_ao, function(x) c(x, rep(NA, longitud_maxima - length(x))))
df <- as.data.frame(do.call(cbind, lista_valores))
df <- cbind(tiempo, df)
df$Media <- rowMeans(df[, -1], na.rm = TRUE)

# line plot
ggplot(df, aes(x = tiempo, y = Media)) +
  geom_line() +
  labs(x = "Tiempo", y = "Media de Valores") +
  ggtitle("Gráfico de Línea de la Media de Valores")

# statistic
all_prop_running <- data.frame(population=c(rep("control odor",length(unlist(all_prop_running_co))),
                                            rep("control free",length(unlist(all_prop_running_cf))),
                                            rep("aSyn-A53T odor",length(unlist(all_prop_running_ao))),
                                            rep("aSyn-A53T free",length(unlist(all_prop_running_af))),
                                            rep("LRRK2-G2019S odor",length(unlist(all_prop_running_lo))),
                                            rep("LRRK2-G2019S free",length(unlist(all_prop_running_lf)))),
                               running_prop=c(unlist(all_prop_running_co),unlist(all_prop_running_cf),
                                             unlist(all_prop_running_ao),unlist(all_prop_running_af),
                                             unlist(all_prop_running_lo),unlist(all_prop_running_lf)))

kruskal.test(running_prop ~ population, all_prop_running)
dunn.test::dunn.test(all_prop_running$running_prop,all_prop_running$population,"Bonferroni")

all_prop_casting <- data.frame(population=c(rep("control odor",length(unlist(all_prop_running_co))),
                                            rep("control free",length(unlist(all_prop_running_cf))),
                                            rep("aSyn-A53T odor",length(unlist(all_prop_running_ao))),
                                            rep("aSyn-A53T free",length(unlist(all_prop_running_af))),
                                            rep("LRRK2-G2019S odor",length(unlist(all_prop_running_lo))),
                                            rep("LRRK2-G2019S free",length(unlist(all_prop_running_lf)))),
                               casting_prop=c(unlist(all_prop_casting_co),unlist(all_prop_casting_cf),
                                              unlist(all_prop_casting_ao),unlist(all_prop_casting_af),
                                              unlist(all_prop_casting_lo),unlist(all_prop_casting_lf)))

kruskal.test(casting_prop ~ population, all_prop_casting)
dunn.test::dunn.test(all_prop_casting$casting_prop,all_prop_casting$population,"Bonferroni")

all_prop_stopped <- data.frame(population=c(rep("control odor",length(unlist(all_prop_stopped_co))),
                                            rep("control free",length(unlist(all_prop_stopped_cf))),
                                            rep("aSyn-A53T odor",length(unlist(all_prop_stopped_ao))),
                                            rep("aSyn-A53T free",length(unlist(all_prop_stopped_af))),
                                            rep("LRRK2-G2019S odor",length(unlist(all_prop_stopped_lo))),
                                            rep("LRRK2-G2019S free",length(unlist(all_prop_stopped_lf)))),
                               stopped_prop=c(unlist(all_prop_stopped_co),unlist(all_prop_stopped_cf),
                                              unlist(all_prop_stopped_ao),unlist(all_prop_stopped_af),
                                              unlist(all_prop_stopped_lo),unlist(all_prop_stopped_lf)))

kruskal.test(stopped_prop ~ population, all_prop_stopped)
dunn.test::dunn.test(all_prop_stopped$stopped_prop,all_prop_stopped$population,"Bonferroni")

all_prop_turning <- data.frame(population=c(rep("control odor",length(unlist(all_prop_turning_co))),
                                            rep("control free",length(unlist(all_prop_turning_cf))),
                                            rep("aSyn-A53T odor",length(unlist(all_prop_turning_ao))),
                                            rep("aSyn-A53T free",length(unlist(all_prop_turning_af))),
                                            rep("LRRK2-G2019S odor",length(unlist(all_prop_turning_lo))),
                                            rep("LRRK2-G2019S free",length(unlist(all_prop_turning_lf)))),
                               turning_prop=c(unlist(all_prop_turning_co),unlist(all_prop_turning_cf),
                                              unlist(all_prop_turning_ao),unlist(all_prop_turning_af),
                                              unlist(all_prop_turning_lo),unlist(all_prop_turning_lf)))

kruskal.test(turning_prop ~ population, all_prop_turning)
dunn.test::dunn.test(all_prop_turning$turning_prop,all_prop_turning$population,"Bonferroni")
