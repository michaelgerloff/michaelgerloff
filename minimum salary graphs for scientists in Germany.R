# clear workspace
rm(list = ls())

library(ggplot2)
library(dplyr)
library(ggrepel)

E13_1 <- 4187.45
E13_2 <- 4526.02

working_hours <- c(0.5*40, 0.65*40, 0.75*40, 1*40, 45, 50, 55, 60, 65)
E13_1_50 <- c(0.5*E13_1, 0.5*E13_1, 0.5*E13_1, 0.5*E13_1, 0.5*E13_1, 0.5*E13_1, 0.5*E13_1, 0.5*E13_1, 0.5*E13_1)
E13_1_65 <- c(0.65*E13_1, 0.65*E13_1, 0.65*E13_1, 0.65*E13_1, 0.65*E13_1, 0.65*E13_1, 0.65*E13_1, 0.65*E13_1, 0.65*E13_1)
E13_1_75 <- c(0.75*E13_1, 0.75*E13_1, 0.75*E13_1, 0.75*E13_1, 0.75*E13_1, 0.75*E13_1, 0.75*E13_1, 0.75*E13_1, 0.75*E13_1)
E13_1_100 <- c(E13_1, E13_1, E13_1, E13_1, E13_1, E13_1, E13_1, E13_1, E13_1)

E13_2_50 <- c(0.5*E13_2, 0.5*E13_2, 0.5*E13_2, 0.5*E13_2, 0.5*E13_2, 0.5*E13_2, 0.5*E13_2, 0.5*E13_2, 0.5*E13_2)
E13_2_65 <- c(0.65*E13_2, 0.65*E13_2, 0.65*E13_2, 0.65*E13_2, 0.65*E13_2, 0.65*E13_2, 0.65*E13_2, 0.65*E13_2, 0.65*E13_2)
E13_2_75 <- c(0.75*E13_2, 0.75*E13_2, 0.75*E13_2, 0.75*E13_2, 0.75*E13_2, 0.75*E13_2, 0.75*E13_2, 0.75*E13_2, 0.75*E13_2)
E13_2_100 <- c(E13_2, E13_2, E13_2, E13_2, E13_2, E13_2, E13_2, E13_2, E13_2)


E13_1_50_df <- data.frame(col1=c("50% TVöD E13 I", "50% TVöD E13 I", "50% TVöD E13 I", "50% TVöD E13 I", "50% TVöD E13 I", "50% TVöD E13 I", "50% TVöD E13 I", "50% TVöD E13 I", "50% TVöD E13 I"), col2=working_hours, col3=E13_1_50)
E13_1_50_df$label <- NA
E13_1_50_df$label[which(E13_1_50_df$col2 == min(E13_1_50_df$col2))] <- E13_1_50_df$col1[which(E13_1_50_df$col2 == min(E13_1_50_df$col2))]
E13_1_65_df <- data.frame(col1=c("65% TVöD E13 I", "65% TVöD E13 I", "65% TVöD E13 I", "65% TVöD E13 I", "65% TVöD E13 I", "65% TVöD E13 I", "65% TVöD E13 I", "65% TVöD E13 I", "65% TVöD E13 I"), col2=working_hours, col3=E13_1_65) %>% subset(!(col2 < 0.65*40))
E13_1_65_df$label <- NA
E13_1_65_df$label[which(E13_1_65_df$col2 == min(E13_1_65_df$col2))] <- E13_1_65_df$col1[which(E13_1_65_df$col2 == min(E13_1_65_df$col2))]
E13_1_75_df <- data.frame(col1=c("75% TVöD E13 I", "75% TVöD E13 I", "75% TVöD E13 I", "75% TVöD E13 I", "75% TVöD E13 I", "75% TVöD E13 I", "75% TVöD E13 I", "75% TVöD E13 I", "75% TVöD E13 I"), col2=working_hours, col3=E13_1_75) %>% subset(!(col2 < 0.75*40))
E13_1_75_df$label <- NA
E13_1_75_df$label[which(E13_1_75_df$col2 == min(E13_1_75_df$col2))] <- E13_1_75_df$col1[which(E13_1_75_df$col2 == min(E13_1_75_df$col2))]
E13_1_100_df <- data.frame(col1=c("100% TVöD E13 I", "100% TVöD E13 I", "100% TVöD E13 I", "100% TVöD E13 I", "100% TVöD E13 I", "100% TVöD E13 I", "100% TVöD E13 I", "100% TVöD E13 I", "100% TVöD E13 I"), col2=working_hours, col3=E13_1_100) %>% subset(!(col2 < 1*40))
E13_1_100_df$label <- NA
E13_1_100_df$label[which(E13_1_100_df$col2 == min(E13_1_100_df$col2))] <- E13_1_100_df$col1[which(E13_1_100_df$col2 == min(E13_1_100_df$col2))]

E13_2_50_df <- data.frame(col1=c("50% TVöD E13 II", "50% TVöD E13 II", "50% TVöD E13 II", "50% TVöD E13 II", "50% TVöD E13 II", "50% TVöD E13 II", "50% TVöD E13 II", "50% TVöD E13 II", "50% TVöD E13 II"), col2=working_hours, col3=E13_2_50)
E13_2_50_df$label <- NA
E13_2_50_df$label[which(E13_2_50_df$col2 == min(E13_2_50_df$col2))] <- E13_2_50_df$col1[which(E13_2_50_df$col2 == min(E13_2_50_df$col2))]
E13_2_65_df <- data.frame(col1=c("65% TVöD E13 II", "65% TVöD E13 II", "65% TVöD E13 II", "65% TVöD E13 II", "65% TVöD E13 II", "65% TVöD E13 II", "65% TVöD E13 II", "65% TVöD E13 II", "65% TVöD E13 II"), col2=working_hours, col3=E13_2_65) %>% subset(!(col2 < 0.65*40))
E13_2_65_df$label <- NA
E13_2_65_df$label[which(E13_2_65_df$col2 == min(E13_2_65_df$col2))] <- E13_2_65_df$col1[which(E13_2_65_df$col2 == min(E13_2_65_df$col2))]
E13_2_75_df <- data.frame(col1=c("75% TVöD E13 II", "75% TVöD E13 II", "75% TVöD E13 II", "75% TVöD E13 II", "75% TVöD E13 II", "75% TVöD E13 II", "75% TVöD E13 II", "75% TVöD E13 II", "75% TVöD E13 II"), col2=working_hours, col3=E13_2_75) %>% subset(!(col2 < 0.75*40))
E13_2_75_df$label <- NA
E13_2_75_df$label[which(E13_2_75_df$col2 == min(E13_2_75_df$col2))] <- E13_2_75_df$col1[which(E13_2_75_df$col2 == min(E13_2_75_df$col2))]
E13_2_100_df <- data.frame(col1=c("100% TVöD E13 II", "100% TVöD E13 II", "100% TVöD E13 II", "100% TVöD E13 II", "100% TVöD E13 II", "100% TVöD E13 II", "100% TVöD E13 II", "100% TVöD E13 II", "100% TVöD E13 II"), col2=working_hours, col3=E13_2_100) %>% subset(!(col2 < 1*40))
E13_2_100_df$label <- NA
E13_2_100_df$label[which(E13_2_100_df$col2 == min(E13_2_100_df$col2))] <- E13_2_100_df$col1[which(E13_2_100_df$col2 == min(E13_2_100_df$col2))]


x <- 20:65
y <- 12*4.3*x
Mindestlohn_df <- data.frame(x,y)
Mindestlohn_df$contract <- NA
names(Mindestlohn_df) <- c("hours", "salary", "contract")

comb_df <- rbind(E13_1_50_df, E13_1_65_df, E13_1_75_df, E13_1_100_df, E13_2_50_df, E13_2_65_df, E13_2_75_df, E13_2_100_df)
colnames(comb_df) <- c("contract", "hours", "salary", "label")

ggplot(data=comb_df, aes(x=hours, y=salary, group=contract, color=contract)) +
  geom_line(data = comb_df)+
  geom_point(data = comb_df)+
  geom_label_repel(aes(label = label),
                   nudge_x = 0,
                   na.rm = TRUE) +
  geom_ribbon(data=Mindestlohn_df, ymin=-Inf, aes(ymax=salary), fill="red", alpha=0.2)+
  scale_x_continuous(breaks = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65))+
  labs(x="wöchentliche Arbeitsstunden", y="Monatslohn [€]")+
  annotate(geom="text", x=40, y=1000, label="Monatslohn unterhalb des gesetlichen Mindestlohns", color="black")+
  theme_classic()+
  theme(legend.position = "none")


comb_hourly_df <- comb_df
comb_hourly_df$salary <- comb_df$salary/(comb_df$hours*4.3)
colnames(comb_hourly_df) <- c("contract", "hours", "salary/hour", "label")

z <- rep(12, length(x))
a <- rep("Mindestlohn", length(x))
x_Z_df <- data.frame(x, z, a)
colnames(x_Z_df) <- c("hours", "salary/hour", "contract")

ggplot(data=comb_hourly_df, aes(x=hours, y=`salary/hour`, group=contract, color=contract)) +  
  geom_line() +
  geom_point()+
  geom_hline(yintercept = max(subset(comb_hourly_df, contract == "50% TVöD E13 I", select = `salary/hour`)), color="grey", size = 1)+
  geom_hline(yintercept = max(subset(comb_hourly_df, contract == "50% TVöD E13 II", select = `salary/hour`)), color="grey", size = 1)+
  geom_label_repel(aes(label = label),
                   nudge_x = 2,
                   nudge_y = 0,
                   na.rm = TRUE) +
  geom_ribbon(data = x_Z_df, ymin=-Inf, aes(ymax=12), fill='red', alpha=0.2) +
  scale_x_continuous(breaks = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65))+
  scale_y_continuous(breaks = c(seq(0, by = 5, len = 6), 12, round(max(subset(comb_hourly_df, contract == "50% TVöD E13 I", select = `salary/hour`)), digits = 2), round(max(subset(comb_hourly_df, contract == "50% TVöD E13 II", select = `salary/hour`)), digits = 2)))+
  labs(x="wöchentliche Arbeitsstunden", y="Stundenlohn [€/h]")+
  annotate(geom="text", x=37, y=8, label="Stundenlohn unterhalb des gesetlichen Mindestlohns", color="black")+
  theme_classic()+
  theme(legend.position = "none")
