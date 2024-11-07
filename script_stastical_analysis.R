##Packages####
library(tidyverse)
library(MASS)
library(gridExtra)
library(lmodel2)
library(grid)
library(readxl)


##Clearance####

#load data
Clearance <- read_excel("data_compilation_merged.xlsx", sheet = "Clearance")


# Clearance in function of carbon mass

lm_robust = rlm(log10(`clearance_15C_corrected_(L_d-1)`) ~ log10(`CW_(en_gC)`), data=Clearance)
cor_test_1 <- cor.test(log10(Clearance$`CW_(en_gC)`), log10(Clearance$`clearance_15C_corrected_(L_d-1)`), method = "pearson", formula = lm_robust)

intercept <- coef(lm_robust)[1]
slope <- coef(lm_robust)[2]
r_squared_1 <- cor_test_1$estimate^2


eq_1 = paste("cor =", round(cor_test_1$estimate, 2),
             "; R^2 =", round(r_squared_1, 2))

p1 <- Clearance %>%
  ggplot(aes(x=log10(`CW_(en_gC)`), y=log10(`clearance_15C_corrected_(L_d-1)`))) +
  geom_point(mapping=aes(color=`carbon_percentage_(McConville)_(en_%)` , shape = group),size = 2)+
  scale_shape_manual(values = c(0,1,2,3,12,4,10,7,9,5))+
  scale_color_gradient(low = "#4575b4", high = "#d73027")+
  labs(x="log( carbon masse (g) )", y="log( clearance (L.d-1) )",color ="carbon percentage \n (McConville et al., 2016)", shape = "Functional groups" ) +
  geom_smooth(color="black", method = "rlm",level=0.95,se=T,linewidth = 0.5) +
  annotate("text", x = min(log10(Clearance$`CW_(en_gC)`)), y = max(log10(Clearance$`clearance_15C_corrected_(L_d-1)`)),
           label = eq_1, hjust = 0, vjust = 1, size = 3.5, color = "black") +
  #ggtitle(eq, "p.value<0,001 ; cor=0,88")+
  theme_linedraw()
p1

# Clearance in function of wet mass

lm_robust = rlm(log10(`clearance_15C_corrected_(L_d-1)`) ~ log10(`WW_(en gC)`), data=Clearance)
cor_test_2 <- cor.test(log10(Clearance$`WW_(en gC)`), log10(Clearance$`clearance_15C_corrected_(L_d-1)`), method = "pearson", formula = lm_robust)

intercept <- coef(lm_robust)[1]
slope <- coef(lm_robust)[2]
r_squared_2 <- cor_test_2$estimate^2
eq_2 = paste("cor =", round(cor_test_2$estimate, 2),
             "; R^2 =", round(r_squared_2, 2))


p2 <- Clearance %>%
  ggplot(aes(x=log10(`WW_(en gC)`), y=log10(`clearance_15C_corrected_(L_d-1)`))) +
  geom_point(mapping=aes(color=`carbon_percentage_(McConville)_(en_%)` , shape = group),size = 2)+
  scale_shape_manual(values = c(0,1,2,3,12,4,10,7,9,5))+
  scale_color_gradient(low = "#4575b4", high = "#d73027") +
  labs(x="log( wet mass (g) )", y="log( clearance (L.d-1) )",color ="carbon percentage \n (McConville et al., 2016)", shape = "functional groupe" ) +
  #geom_line(aes(x = LogWW, y = y_q3), color = "black",size=0.8)+
  geom_smooth(color="black", method = "rlm",level=0.95,se=T,linewidth = 0.5) +
  annotate("text", x = min(log10(Clearance$`WW_(en gC)`)), y = max(log10(Clearance$`clearance_15C_corrected_(L_d-1)`)),
           label = eq_2, hjust = 0, vjust = 1, size = 3.5, color = "black") +
  theme_linedraw()
p2


# Carbon mass specific clearance rate in function of carbon percentage

carbon_mass_specific_clearance = Clearance$`clearance_15C_corrected_(L_d-1)` / (Clearance$`CW_(en_gC)`^0.84)

lm_model2 <- lmodel2(log10(carbon_mass_specific_clearance) ~ `carbon_percentage_(McConville)_(en_%)`, data=Clearance)
cor_test_3 <- cor.test(Clearance$`carbon_percentage_(McConville)_(en_%)`, log10(carbon_mass_specific_clearance), method = "pearson", formula = lm_model2)

r_squared_3 <- cor_test_3$estimate^2

eq_3 <- paste("cor =", round(cor_test_3$estimate, 2),
              "; R^2 =", round(r_squared_3, 2))

p3 <- Clearance %>%
  ggplot(aes(x=`carbon_percentage_(McConville)_(en_%)`, y=log10(carbon_mass_specific_clearance))) +
  geom_point(mapping=aes( shape = group),size = 2)+
  scale_shape_manual(values = c(0,1,2,3,12,4,10,7,9,5))+
  labs(x="carbon percentage (McConville et al., 2016)", y="log( carbon mass specific clearance rate (en L.d-1/Wb))",color ="log(Carbon mass (g))", shape = "Functional groups" ) +
  geom_smooth(color="black", method = "rlm",level=0.95,se=T,linewidth = 0.5) +
  annotate("text", x = 10, y = -1.5, label = eq_3, hjust = 0, vjust = 0, size = 3.5, color = "black") +
  theme_linedraw()

p3


# Wet mass specific clearance rate in function of carbon percentage

wet_mass_specific_clearance = Clearance$`clearance_15C_corrected_(L_d-1)` / (Clearance$`WW_(en gC)`^0.72)

lm_model2 <- lmodel2(log10(wet_mass_specific_clearance) ~ `carbon_percentage_(McConville)_(en_%)`, data=Clearance)
cor_test_4 <- cor.test(Clearance$`carbon_percentage_(McConville)_(en_%)`, log10(wet_mass_specific_clearance), method = "pearson", formula = lm_model2)


r_squared_4 <- cor_test_4$estimate^2

eq_4 <- paste("cor =", round(cor_test_4$estimate, 2),
              "; R^2 =", round(r_squared_4, 2))



p4 <- Clearance %>%
  ggplot(aes(x=`carbon_percentage_(McConville)_(en_%)`, y=log10(wet_mass_specific_clearance))) +
  geom_point(mapping=aes( shape = group),size = 2)+
  scale_shape_manual(values = c(0,1,2,3,12,4,10,6,7,9,5))+
  labs(x="carbon percentage (McConville et al., 2016)", y="log(wet mass specific clearance rate (L.d-1/Wb))",color ="log(Wet mass (g))", shape = "Functional groups" )+
  annotate("text", x = 0, y = 0,
           label = eq_4, hjust = 0, vjust = 1, size = 3.5, color = "black") +
  theme_linedraw()

p4


svg("clearance_2.svg", width = 8, height = 6)

clearance_2 <- ggarrange(p1, p2,p3, p4,
                         labels = c("A", "B","C","D"),
                         ncol = 2, nrow = 2,
                         common.legend = TRUE, legend = "bottom")
clearance_2

dev.off()



##Growth####

#load data
Growth <- read_excel("data_compilation_merged.xlsx", sheet = "Growth")


#Growth rate in function of carbon mass

lm_robust = lm(log10(`growth_15C_corrected_(d-1)`) ~ log10(`CW_(en_gC)`), data=Growth)
cor_test_1 <- cor.test(log10(Growth$`CW_(en_gC)`), log10(Growth$`growth_15C_corrected_(d-1)`), method = "pearson", formula = lm_robust)

intercept <- coef(lm_robust)[1]
slope <- coef(lm_robust)[2]

r_squared_1 <- cor_test_1$estimate^2

eq_1 = paste("cor =", round(cor_test_1$estimate, 2),
             "; R^2 =", round(r_squared_1, 2))

p1 <- Growth %>%
  ggplot(aes(x=log10(`CW_(en_gC)`), y=log10(`growth_15C_corrected_(d-1)`))) +
  geom_point(mapping=aes(color=`carbon_percentage_(McConville)_(en_%)` , shape = group),size = 2)+
  scale_shape_manual(values = c(0,1,2,3,4,10,6,7,8,9,5))+
  scale_color_gradient(low = "#4575b4", high = "#d73027")+
  labs(x="log( carbon mass (g) )", y="log( growth (d-1) )",color ="carbon percentage \n (McConville et al., 2016)", shape = "Functional groups" ) +
  geom_smooth(color="black", method = "lm",level=0.95,se=T,linewidth = 0.5) +
  annotate("text", x = -8, y = -3,
           label = eq_1, hjust = 0, vjust = 1, size = 3.5, color = "black") +
  theme_linedraw()

p1

#Growth rate in function of wet mass

lm_robust = rlm(log10(`growth_15C_corrected_(d-1)`) ~ log10(`WW_(en gC)`), data=Growth)
cor_test_2 <- cor.test(log10(Growth$`WW_(en gC)`), log10(Growth$`growth_15C_corrected_(d-1)`), method = "pearson", formula = lm_robust)

intercept <- coef(lm_robust)[1]
slope <- coef(lm_robust)[2]

r_squared_2 <- cor_test_2$estimate^2

eq_2 = paste("cor =", round(cor_test_2$estimate, 2),
             "; R^2 =", round(r_squared_2, 2))


p2 <- Growth %>%
  ggplot(aes(x=log10(`WW_(en gC)`), y=log10(`growth_15C_corrected_(d-1)`))) +
  geom_point(mapping=aes(color=`carbon_percentage_(McConville)_(en_%)` , shape = group),size = 2)+
  scale_shape_manual(values = c(0,1,2,3,4,10,6,7,8,9,5))+
  scale_color_gradient(low = "#4575b4", high = "#d73027") +
  labs(x="log( wet mass ( g) )", y="log( Growth (d-1) )",color ="carbon percentage \n (McConville et al., 2016)", shape = "Functional group" ) +
  geom_smooth(color="black", method = "lm",level=0.95,se=T,linewidth = 0.5) +
  annotate("text", x = -7, y = -2,
           label = eq_2, hjust = 0, vjust = 1, size = 3.5, color = "black") +
  theme_linedraw()
p2

#Carbon mass specific growth rate in function of carbon percentage

carbon_mass_specific_growth = Growth$`growth_15C_corrected_(d-1)` / (Growth$`CW_(en_gC)`^0.39)

lm_model2 <- lmodel2(log10(carbon_mass_specific_growth) ~ `carbon_percentage_(McConville)_(en_%)`, data=Growth)
cor_test_3 <- cor.test(Growth$`carbon_percentage_(McConville)_(en_%)`, log10(carbon_mass_specific_growth), method = "pearson", formula = lm_model2)

r_squared_3 <- cor_test_3$estimate^2

eq_3 <- paste("cor =", round(cor_test_3$estimate, 2),
              "; R^2 =", round(r_squared_3, 2))

p3 <- Growth %>%
  ggplot(aes(x=`carbon_percentage_(McConville)_(en_%)`, y=log10(carbon_mass_specific_growth))) +
  geom_point(mapping=aes(shape = group),size = 2)+
  scale_shape_manual(values = c(0,1,2,3,4,10,6,7,8,9,5))+
  #scale_color_gradient(low = "#4575b4", high = "#d73027")+
  labs(x="Pourcentage en carbone (McConville et al., 2016)", y="log( croissance spécifique à la masse de carbone \n (en d-1/g^b) )",color ="log( masse en carbone (en g) )", shape = "Groupes fonctionnels" )+
  annotate("text", x = 10, y = -1.5,
           label = eq_3, hjust = 0, vjust = 0, size = 3.5, color = "black") +
  theme_linedraw()

p3

#Wet mass specific growth rate in function of carbon percentage

wet_mass_specific_growth = Growth$`growth_15C_corrected_(d-1)` / (Growth$`WW_(en gC)`^0.33)

lm_model2 <- lmodel2(log10(wet_mass_specific_growth) ~ `carbon_percentage_(McConville)_(en_%)`, data=Growth)
cor_test_4 <- cor.test(Growth$`carbon_percentage_(McConville)_(en_%)`, log10(wet_mass_specific_growth), method = "pearson", formula = lm_model2)

r_squared_4 <- cor_test_4$estimate^2

eq_4 <- paste("cor =", round(cor_test_4$estimate, 2),
              "; R^2 =", round(r_squared_4, 2))

p4 <- Growth %>%
  ggplot(aes(x=`carbon_percentage_(McConville)_(en_%)`, y=log10(wet_mass_specific_growth))) +
  geom_point(mapping=aes( shape = group),size = 2)+
  scale_shape_manual(values = c(0,1,2,3,4,10,6,7,8,9,5))+
  #scale_color_gradient(low = "#4575b4", high = "#d73027")+
  labs(x="Pourcentage en carbone (McConville et al., 2016)", y="log( croissance spécifique à la masse humide \n (en d-1/g^b) )",color ="log(Poids mouillé (en g)", shape = "Groupes fonctionnels" ) +
  geom_smooth(color="black", method = "lm",level=0.95,se=T,linewidth = 0.5) +
  annotate("text", x = 0, y = 0,
           label = eq_4, hjust = 0, vjust = 1, size = 3.5, color = "black") +
  theme_linedraw()

p4



croissance <- ggarrange(p1, p2,p3,p4,
                        labels = c("A", "B","C","D"),
                        ncol = 2, nrow = 2,
                        common.legend = TRUE, legend = "bottom")
croissance

grid.export("figur_croissance.svg")




##Respiration####

#load data
Respiration <- read_excel("data_compilation_merged.xlsx", sheet = "Respiration")

#Respiration rate in function of carbon mass

lm_robust = rlm(log10(`respiration_15C_corrected_(mmol_O2_d-1)`) ~ log10(`CW_(en_gC)`), data=Respiration)
cor_test_1 <- cor.test(log10(Respiration$`CW_(en_gC)`), log10(Respiration$`respiration_15C_corrected_(mmol_O2_d-1)`), method = "pearson", formula = lm_robust)

intercept <- coef(lm_robust)[1]
slope <- coef(lm_robust)[2]

r_squared_1 <- cor_test_1$estimate^2

eq_1 = paste("cor =", round(cor_test_1$estimate, 2),
             "; R^2 =", round(r_squared_1, 2))


p1 <- Respiration %>%
  ggplot(aes(x=log10(`CW_(en_gC)`), y=log10(`respiration_15C_corrected_(mmol_O2_d-1)`))) +
  geom_point(mapping=aes(color=`carbon_percentage_(McConville)_(en_%)` , shape = group),size = 2)+
  scale_shape_manual(values = c(0,1,2,3,12,4,10,6,7,8,9,5))+
  scale_color_gradient(low = "#4575b4", high = "#d73027")+
  labs(x="Carbon mass (g)", y="Respiration (mmol02.d-1)",color ="Carbon percentage \n (McConville et al., 2016)", shape = "Functional groups" ) +
  geom_smooth(color="black", method = "rlm",level=0.95,se=T,linewidth = 0.5) +
  annotate("text", x = min(log10(Respiration$`CW_(en_gC)`)), y = max(log10(Respiration$`respiration_15C_corrected_(mmol_O2_d-1)`)),
           label = eq_1, hjust = 0, vjust = 1, size = 3.5, color = "black") +
  theme_linedraw()

p1

#Respiration rate in function of wet mass

lm_robust = rlm(log10(`respiration_15C_corrected_(mmol_O2_d-1)`) ~ log10(`WW_(en gC)`), data=Respiration)
cor_test_2 <- cor.test(log10(Respiration$`WW_(en gC)`), log10(Respiration$`respiration_15C_corrected_(mmol_O2_d-1)`), method = "pearson", formula = lm_robust)

r_squared_2 <- cor_test_2$estimate^2

intercept <- coef(lm_robust)[1]
slope <- coef(lm_robust)[2]

eq_2 = paste("cor =", round(cor_test_2$estimate, 2),
             "; R^2 =", round(r_squared_2, 2))

p2 <-Respiration %>%
  ggplot(aes(x=log10(`WW_(en gC)`), y=log10(`respiration_15C_corrected_(mmol_O2_d-1)`))) +
  geom_point(mapping=aes(color=`carbon_percentage_(McConville)_(en_%)` , shape = group),size = 2)+
  scale_shape_manual(values = c(0,1,2,3,12,4,10,6,7,8,9,5))+
  scale_color_gradient(low = "#4575b4", high = "#d73027")+
  labs(x="Wet mass (g)", y="Respiration (mmolO2.d-1)",color ="Carbon percentage \n (McConville et al., 2016)", shape = "Functional groups" ) +
  geom_smooth(color="black", method = "rlm",level=0.95,se=T,linewidth = 0.5) +
  annotate("text", x = min(log10(Respiration$`WW_(en gC)`)), y = max(log10(Respiration$`respiration_15C_corrected_(mmol_O2_d-1)`)),
           label = eq_2, hjust = 0, vjust = 1, size = 3.5, color = "black") +
  theme_linedraw()


p2

#Carbon mass specific respiration rate in function of carbon percentage

carbon_mass_specific_respiration = Respiration$`respiration_15C_corrected_(mmol_O2_d-1)` / (Respiration$`CW_(en_gC)`^0.9)

lm_model2 <- lmodel2(log10(carbon_mass_specific_respiration) ~ `carbon_percentage_(McConville)_(en_%)`, data=Respiration)
cor_test_3 <- cor.test(Respiration$`carbon_percentage_(McConville)_(en_%)`, log10(carbon_mass_specific_respiration), method = "pearson", formula = lm_model2)

r_squared_3 <- cor_test_3$estimate^2

eq_3 <- paste("cor =", round(cor_test_3$estimate, 2),
              "; R^2 =", round(r_squared_3, 2))

p3 <-Respiration %>%
  ggplot(aes(x=`carbon_percentage_(McConville)_(en_%)`, y=log10(carbon_mass_specific_respiration))) +
  geom_point(mapping=aes( shape = group),size = 2)+
  scale_shape_manual(values = c(0,1,2,3,12,4,10,6,7,8,9,5))+
  labs(x="Carbon percentage (CM%WM)", y="Respiration (mmolO2.d-1/Wb)",color ="log( carbon mass (en g))", shape = "Functional group" ) +
  geom_smooth(color="black", method = "rlm",level=0.95,se=T,linewidth = 0.5) +
  # ggtitle("p.value<0,001 ; cor=-0,1 ; R2 =0,01  ; R2-adjusted = ")+
  annotate("text", x = 10, y = -1.5,
           label = eq_3, hjust = 0, vjust = 0, size = 3.5, color = "black") +
  theme_linedraw()

p3

#Wet mass specific respiration rate in function of carbon percentage

wet_mass_specific_respiration = Respiration$`respiration_15C_corrected_(mmol_O2_d-1)` / (Respiration$`WW_(en gC)`^0.78)


lm_model2 <- lmodel2(log10(wet_mass_specific_respiration) ~ `carbon_percentage_(McConville)_(en_%)`, data=Respiration)
cor_test_4 <- cor.test(Respiration$`carbon_percentage_(McConville)_(en_%)`, log10(wet_mass_specific_respiration), method = "pearson", formula = lm_model2)

r_squared_4 <- cor_test_4$estimate^2

eq_4 <- paste("cor =", round(cor_test_4$estimate, 2),
              "; R^2 =", round(r_squared_4, 2))

p4 <- Respiration %>%
  ggplot(aes(x=`carbon_percentage_(McConville)_(en_%)`, y=log10(wet_mass_specific_respiration))) +
  geom_point(mapping=aes( shape = group),size = 2)+
  scale_shape_manual(values = c(0,1,2,3,12,4,10,6,7,8,9,5))+
  # scale_color_gradient(low = "#4575b4", high = "#d73027")+
  labs(x="Carbon percentage (CM%WM)", y="Respiration (mmolO2.d-1/Wb)",color ="log(Poids humide (en g))", shape = "Groupes fonctionnels" ) +
  geom_smooth(color="black", method = "rlm",level=0.95,se=T,linewidth = 0.5) +
  #ggtitle("p.value<0,001 ; cor=0,51 ; R2 = 0.26  ; R2-adjusted = ")+
  annotate("text", x = 0, y = 0,
           label = eq_4, hjust = 0, vjust = 1, size = 3.5, color = "black") +
  theme_linedraw()

p4


svg("figure.svg", width = 8, height = 6)

figure <- ggarrange(p1, p2,p3, p4,
                    labels = c("A", "B","C","D"),
                    ncol = 2, nrow = 2,
                    common.legend = TRUE, legend = "bottom")
figure

dev.off()







##Assimilation####
# Assimilation in function of carbon percentage

Assimilation <- read_excel("data_compilation_merged.xlsx", sheet = "Assimilation")

p1 <- Assimilation %>%
  ggplot(aes(x=carbon_percent_mc_conville, y=`assimilation (%C)`)) +
  geom_point(mapping=aes( shape = Group),size = 4)+
  scale_shape_manual(values = c(1,0,1,2,3,12,4,10,6,7,9,5))+
  labs(x="Pourcentage en carbone (McConville et al., 2016)", y="assimilation (en %)",color ="log(Poids mouillé (en g)", shape = "Groupes fonctionnels" ) +
  theme_linedraw()


svg("assimilation_2.svg", width = 8, height = 6)

assimilation_2 <- ggarrange(p1,
                            ncol = 1, nrow = 1,
                            common.legend = TRUE, legend = "bottom")
assimilation_2

dev.off()


##Predator-prey mass ratio
##Predator_prey size function####
Predator_prey_size_ratio <- read_excel("data_compilation_merged.xlsx", sheet = "Predator-prey size ratio")

#Non-specific predators####

ratio_split <- split(Predator_prey_size_ratio$ratio_pred_prey, Predator_prey_size_ratio$mode_feeding_2)
percent_split <- split(Predator_prey_size_ratio$carbon_percent_Mconville__, Predator_prey_size_ratio$mode_feeding_2)
group_split <- split(Predator_prey_size_ratio$group, Predator_prey_size_ratio$mode_feeding_2)
optimal_split <- split(Predator_prey_size_ratio$`Efficency_%`, Predator_prey_size_ratio$mode_feeding_2)
mean_split <- split(Predator_prey_size_ratio$mean, Predator_prey_size_ratio$mode_feeding_2)
std_split <- split(Predator_prey_size_ratio$range,Predator_prey_size_ratio$mode_feeding_2)

##feeding current feeders
carbon_ff =percent_split[["Filter feeders"]]
ratio_ff =ratio_split[["Filter feeders"]]
group_ff  = group_split[["Filter feeders"]]
optimal_ff = optimal_split[["Filter feeders"]]
mean_ff = mean_split[["Filter feeders"]]
std_ff = std_split[["Filter feeders"]]

data_split_ff = data.frame(carbon_ff, ratio_ff,group_ff,optimal_ff,mean_ff,std_ff)

data_split_ff$optimal_ff <- as.numeric(data_split_ff$optimal_ff)

data_split_ff$optimal_ff[is.na(data_split_ff$optimal_ff)] <- 0

model_ff <- lm(log10(data_split_ff$ratio_ff) ~ data_split_ff$carbon_ff, weights = data_split_ff$optimal_ff)
cor.test(data_stat_ff$carbon_ff,log10(data_split_ff$ratio_ff),method = "pearson" )
predicted_ff <- predict(model_ff)


intercept_ff <- coef(model_ff)[1]
slope_ff <- coef(model_ff)[2]

y_predit_optimal_ff = intercept_ff + slope_ff * data_split_ff$carbon_ff

optimal_ff <- y_predit_optimal_ff
moyenne_ff<- data_split_ff$mean_ff
sigma_ff <- data_split_ff$std_ff

sigma_ff <- as.numeric(sigma_ff)

minimum_ff <- optimal_ff - 3 * sigma_ff
maximum_ff <- optimal_ff + 3 * sigma_ff

moyenne_ff <- as.numeric(moyenne_ff)

q1_ff <- qnorm(0.25, mean = moyenne_ff, sd = sigma_ff)
q3_ff <- qnorm(0.99, mean = moyenne_ff, sd = sigma_ff)

data_stat_ff = tibble(carbon_ff = data_split_ff$carbon_ff ,optimal_ff = y_predit_optimal_ff , minimum_ff,maximum_ff,q1_ff,q3_ff)

model_minimum_ff <- lm(data_stat_ff$minimum_ff ~ data_stat_ff$carbon_ff)
cor.test(data_stat_ff$carbon_ff,data_stat_ff$minimum_ff,method = "pearson" )

model_maximum_ff <- lm(data_stat_ff$maximum_ff ~ data_stat_ff$carbon_ff)
cor.test(data_stat_ff$carbon_ff,data_stat_ff$maximum_ff,method = "pearson" )


model_q1_ff <- lm(data_stat_ff$q1_ff ~ data_stat_ff$carbon_ff)
cor.test(data_stat_ff$carbon_ff,data_stat_ff$q1_ff,method = "pearson" )

model_q3_ff <- lm(data_stat_ff$q3_ff ~ data_stat_ff$carbon_ff)
cor.test(data_stat_ff$carbon_ff,data_stat_ff$q1_ff,method = "pearson" )


interceptmin_ff <- coef(model_minimum_ff)[1]
slopemin_ff <- coef(model_minimum_ff)[2]

interceptmax_ff <- coef(model_maximum_ff)[1]
slopemax_ff <- coef(model_maximum_ff)[2]

interceptq1_ff <- coef(model_q1_ff)[1]
slopeq1_ff <- coef(model_q1_ff)[2]

interceptq3_ff <- coef(model_q3_ff)[1]
slopeq3_ff <- coef(model_q3_ff)[2]

y_min_ff = interceptmin_ff + slopemin_ff * data_split_ff$carbon_ff
y_max_ff = interceptmax_ff + slopemax_ff * data_split_ff$carbon_ff
y_q1_ff = interceptq1_ff + slopeq1_ff * data_split_ff$carbon_ff
y_q3_ff = interceptq3_ff + slopeq3_ff * data_split_ff$carbon_ff
y_q1_ff = interceptq1_ff - 0.135 * data_split_ff$carbon_ff
y_q3_ff = interceptq3_ff - 0.25 * data_split_ff$carbon_ff

##passive ambush feeders

carbon_pa =percent_split[["Passive ambush feeders"]]
ratio_pa =ratio_split[["Passive ambush feeders"]]
group_pa  = group_split[["Passive ambush feeders"]]
optimal_pa = optimal_split[["Passive ambush feeders"]]
mean_pa = mean_split[["Passive ambush feeders"]]
std_pa = std_split[["Passive ambush feeders"]]

data_split_pa = data.frame(carbon_pa, ratio_pa,group_pa,optimal_pa,mean_pa,std_pa)
data_split_pa$optimal_pa <- as.numeric(data_split_pa$optimal_pa)

model_pa <- lm(log10(data_split_pa$ratio_pa) ~ data_split_pa$carbon_pa)
cor.test(data_split_pa$carbon_pa,log10(data_split_pa$ratio_pa),method = "pearson" )

predicted_pa <- predict(model_pa)

intercept_pa <- coef(model_pa)[1]
slope_pa <- coef(model_pa)[2]

y_predit_optimal_pa = intercept_pa + slope_pa * data_split_pa$carbon_pa

optimal_pa <- y_predit_optimal_pa
moyenne_pa<- data_split_pa$mean_pa
sigma_pa <- data_split_pa$std_pa

sigma_pa <- as.numeric(sigma_pa)

minimum_pa <- optimal_pa - 3 * sigma_pa
maximum_pa<- optimal_pa+ 3 * sigma_pa

moyenne_pa <- as.numeric(moyenne_pa)

q1_pa <- qnorm(0.25, mean = moyenne_pa, sd = sigma_pa)
q3_pa<- qnorm(0.99, mean = moyenne_pa, sd = sigma_pa)

data_stat_pa = tibble(carbon_pa = data_split_pa$carbon_pa ,optimal_pa = y_predit_optimal_pa , minimum_pa,maximum_pa,q1_pa,q3_pa)

model_minimum_pa <- lm(data_stat_pa$minimum_pa ~ data_stat_pa$carbon_pa)
cor.test(data_stat_pa$carbon_pa,data_stat_pa$minimum_pa,method = "pearson" )

model_maximum_pa <- lm(data_stat_pa$maximum_pa ~ data_stat_pa$carbon_pa)
cor.test(data_stat_pa$carbon_pa,data_stat_pa$maximum_pa,method = "pearson" )


## plot

p1 <- ggplot() +
  geom_line(aes(x = data_split_ff$carbon_ff, y = y_min_ff),color = "#a6bddb",linewidth=0.3)+
  geom_line(aes(x = data_split_ff$carbon_ff, y = y_max_ff), color = "#a6bddb",linewidth=0.3)+
  geom_line(aes(x = data_split_ff$carbon_ff, y = y_q1_ff), color = "#2b8cbe",linewidth=0.3)+
  geom_line(aes(x = data_split_ff$carbon_ff, y = y_q3_ff), color = "#2b8cbe",linewidth=0.3)+
  geom_ribbon(aes(x = carbon_ff, ymin = y_q1_ff, ymax = y_q3_ff), data = data_split_ff, fill = "#a6bddb", alpha = 0.2)+
  geom_ribbon(aes(x = carbon_ff, ymin = y_min_ff, ymax = y_max_ff), data = data_split_ff, fill = "#a6bddb", alpha = 0.2)+
  geom_line(aes(x = data_split_ff$carbon_ff, y = predicted_ff), color = "blue", linetype = "dashed")+
  geom_line(aes(x = data_split_pa$carbon_pa, y = predicted_pa), color = "orange", linetype = "dashed")+
  geom_point(mapping=aes(x=carbon_ff, y=log10(ratio_ff),shape=group_ff),data = data_split_ff, color ="#619CFF", size=4)+
  geom_point(mapping=aes(x=carbon_pa, y=log10(ratio_pa),shape=group_pa),data = data_split_pa, color ="#fc9272", size=4)+
  scale_shape_manual(values = c(0,2,3,4,10,6,9,5,0,2,3,5))+
  theme_linedraw()+
  ylim(-1, 6)+
  xlim(min(Predator_prey_size_ratio$carbon_percent_Mconville__), max(Predator_prey_size_ratio$carbon_percent_Mconville__))+
  labs(x="Carbon percentage (CM%WM)", y="Predator prey size ratio (µm.µm-1)", shape = "Groupes fonctionnels")+
  ggtitle("Non-specific predators")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14), plot.title = element_text(size = 20, face = "bold"),legend.title = element_text(size = 14), legend.text = element_text(size = 12))
p1

#Specific predators####

#cruise feeders

carbon_cf =percent_split[["Cruise feeders"]]
ratio_cf =ratio_split[["Cruise feeders"]]
group_cf  = group_split[["Cruise feeders"]]
optimal_cf = optimal_split[["Cruise feeders"]]
mean_cf = mean_split[["Cruise feeders"]]
std_cf = std_split[["Cruise feeders"]]

#active ambush feeders
carbon_aa =percent_split[["Active ambush feeders"]]
ratio_aa =ratio_split[["Active ambush feeders"]]
group_aa  = group_split[["Active ambush feeders"]]
optimal_aa = optimal_split[["Active ambush feeders"]]
mean_aa = mean_split[["Active ambush feeders"]]
std_aa = std_split[["Active ambush feeders"]]

data_split_cf = data.frame(carbon_cf, ratio_cf,group_cf,optimal_cf,mean_cf,std_cf)

data_split_aa = data.frame(carbon_aa, ratio_aa,group_aa,optimal_aa,mean_aa,std_aa)


#plot

p2 <- ggplot() +
  geom_point(mapping=aes(x=carbon_cf, y=log10(ratio_cf),shape=group_cf),data = data_split_cf, color ="#1b9e77", size=4)+
  geom_point(mapping=aes(x=carbon_aa, y=log10(ratio_aa),shape=group_aa),data = data_split_aa, color ="#756bb1", size=4)+
  scale_shape_manual(values = c(1,2,4,6,7,1,2))+
  theme_linedraw()+
  ylim(-1, 6)+
  xlim(min(Predator_prey_size_ratio$carbon_percent_Mconville__), max(Predator_prey_size_ratio$carbon_percent_Mconville__))+
  labs(x="Carbon percentage (CM%WM)", y="Predator prey size ratio (µm.µm-1)", shape = "Groupes fonctionnels")+
  ggtitle("Specific predators")+
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(size = 20, face = "bold"),legend.title = element_text(size = 14),legend.text = element_text(size = 12) )



p2

svg("figure_1.svg", width = 7, height = 9)

figure_1 <- ggarrange(p2,
                      labels = c("a)"),
                      ncol = 1, nrow = 1,
                      common.legend = TRUE, legend = "bottom")
figure_1

dev.off()

