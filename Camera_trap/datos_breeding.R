


library(tidyverse)
library(ggplot2)
library(ggforce)



setwd("D:/biato/Grallaria/data_analysis/breeding")

df1<- read.csv("cameratrapping_nestlings_1.csv", header=T, sep=",", dec=".")

head(df1)

df1$newdate <- strptime(as.character(df1$Fecha), "%Y/%m/%d")
df1$newdate <- format(df1$newdate, "%Y-%m-%d")
head(df1)

df1$newour <- strptime(as.character(df1$Hora), "%H:%M:%S")
df1$newour <- format(df1$newour, "%H:%M:%S")
head(df1)

df1$dates2<-as.POSIXct(paste(df1$newdate, df1$newour), format="%Y-%m-%d %H:%M:%S")
head(df1)


df1<- df1 %>% dplyr::select(-c(Hora, Fecha, newdate, newour))
head(df1)



tes<-df1 %>% dplyr::select(dates2) %>%
  mutate(dates2 = lubridate::floor_date(dates2,"hours")) %>%
  group_by(dates2) %>%
  summarise() %>%
  full_join(df1) %>%
  arrange(dates2)

feed <- tes %>% fill(Dato, .direction = "up") %>%
  filter(Accion == "f") %>% 
  mutate(on.time.diff.hour = ifelse(Dato == "off",
                                    difftime(dates2, lag(dates2),
                                             "secs"), NA)) 

brooding <- tes %>% fill(Dato, .direction = "up") %>%
  filter(Accion == "br") %>% 
  mutate(on.time.diff.hour = ifelse(Dato == "off",
                                    difftime(dates2, lag(dates2),
                                             "secs"), NA))

######################

f2<-feed  %>%
  filter(lubridate::hour(dates2) >= 4.30 & lubridate::hour(dates2 ) < 18) %>% 
  na.omit() %>% dplyr::select(Accion, seg= on.time.diff.hour, dates2) %>% 
  mutate(min= seg/60) %>% 
  mutate(day = format(as.Date(dates2), "%d-%m")) %>% dplyr::select(-c(dates2))



brooding2<-brooding  %>%
  filter(lubridate::hour(dates2) >= 4.30 & lubridate::hour(dates2 ) < 18) %>% 
  na.omit() %>% dplyr::select(Accion, seg= on.time.diff.hour, dates2) %>% 
  mutate(min= seg/60) %>%
  mutate(day = format(as.Date(dates2), "%d-%m")) %>% dplyr::select(-c(dates2))



nestling <- rbind(f2, brooding2) 
nestling<-complete(nestling, Accion, day)
nestling[is.na(nestling)] <- 0



#df.summary2 <- nestling %>%
#  group_by(day, Accion) %>%
#  summarise( sd = sd(min), len = mean(min))
#df.summary2
#df.summary2[is.na(df.summary2)] <- 0


feeding<-nestling %>% dplyr::filter(Accion=="f") %>%  ggplot(aes(x= day
                                                                 , y= as.numeric(seg))) +
  geom_boxplot(varwidth = TRUE, alpha=0.5, 
               outlier.colour="grey") + xlab("Days after hatching") +
  ylab("Feeding time (seg)") +
geom_jitter(color="black", shape=16, size=2, alpha=0.1,
              position=position_jitter(0.2)) + scale_fill_grey() +
  theme_classic() +
theme(
    legend.position="none",
    plot.title = element_text(size=9),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    axis.title.x = element_text(size = 11)
  ) + scale_y_continuous(limits = c(0, 150)) +
  scale_x_discrete(limits = c("30-11", "01-12", "02-12", "03-12", "04-12",
                              "05-12", "06-12", "07-12"),
                   breaks=c("30-11", "01-12", "02-12", "03-12", "04-12",
                            "05-12", "06-12", "07-12"),
                   labels=c("5", "6", "7", "8", "9",
                            "10", "11", "12")) 


Totals <- nestling %>% dplyr::filter(Accion=="f") %>% 
 group_by(day) %>% summarize(N = n())

f<-feeding  + geom_text(aes(y = 100 +N, label = N), data = Totals,
                     color = "blue") 

f

Totals_1 <- nestling %>% dplyr::filter(Accion=="f") %>% 
  group_by(day) %>% summarize(N = n()) %>% mutate(rate= N/13.5)
write.csv(Totals_1, file = "feeding_rate.csv")




brooding<-nestling %>% dplyr::filter(Accion=="br") %>% ggplot(aes(x=day, y= as.numeric(min))) +
  geom_boxplot(varwidth = TRUE, alpha=0.5, 
               outlier.colour="grey") + xlab("Days after hatching") +
  ylab("Brooding time (min)") +
  geom_jitter(color="black", shape=16, size=2, alpha=0.1,
              position=position_jitter(0.2)) + scale_fill_grey() +
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=9),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    axis.title.x = element_text(size = 11)
  ) +
scale_y_continuous(limits = c(0, 70)) +
  scale_x_discrete(limits = c("30-11", "01-12", "02-12", "03-12", "04-12",
                              "05-12", "06-12", "07-12"),
                   breaks=c("30-11", "01-12", "02-12", "03-12", "04-12",
                            "05-12", "06-12", "07-12"),
                   labels=c("5", "6", "7", "8", "9",
                            "10", "11", "12")) 

Totals2 <- nestling %>% dplyr::filter(Accion=="br") %>% 
  group_by(day) %>% summarize(N = n())


b<-brooding + geom_text(aes(y = 40+N , label = ifelse(N ==1, 0, N)),
                     data = Totals2, color = "blue")


b

Totals_2 <- nestling %>% dplyr::filter(Accion=="br") %>% 
  group_by(day) %>% summarize(N = n()) %>% mutate(rate= N/13.5)
write.csv(Totals_2, file = "brooding_rate.csv")




library(gridExtra)



######
graf_bre<- grid.arrange(f, b,  ncol=1)

#### grabar table
#ggsave(filename = "Breeding_grallaria.png",
 #      plot = graf_bre, width = 20, height = 6, dpi = 350, units = "cm")


##########################################
###################################################################################
#################################### Ultimos dias
#############################
#########

df2<- read.csv("cameratrapping_nestlings_2.csv", header=T, sep=",", dec=".")

head(df2)

df2$newdate <- strptime(as.character(df2$Fecha), "%Y/%m/%d")
df2$newdate <- format(df2$newdate, "%Y-%m-%d")
head(df2)

df2$newour <- strptime(as.character(df2$Hora), "%H:%M:%S")
df2$newour <- format(df2$newour, "%H:%M:%S")
head(df2)

df2$dates2<-as.POSIXct(paste(df2$newdate, df2$newour), format="%Y-%m-%d %H:%M:%S")
head(df2)


df2<- df2 %>% dplyr::select(-c(Hora, Fecha, newdate, newour))
head(df2)



tes2<-df2 %>% dplyr::select(dates2) %>%
  mutate(dates2 = lubridate::floor_date(dates2,"hours")) %>%
  group_by(dates2) %>%
  summarise() %>%
  full_join(df2) %>%
  arrange(dates2)

feed2 <- tes2 %>% fill(Dato, .direction = "up") %>%
  filter(Accion == "f") %>% 
  mutate(on.time.diff.hour = ifelse(Dato == "off",
                                    difftime(dates2, lag(dates2),
                                             "secs"), NA)) 

brooding2 <- tes2 %>% fill(Dato, .direction = "up") %>%
  filter(Accion == "br") %>% 
  mutate(on.time.diff.hour = ifelse(Dato == "off",
                                    difftime(dates2, lag(dates2),
                                             "secs"), NA))

######################

f4<-feed2  %>%
  filter(lubridate::hour(dates2) >= 5 & lubridate::hour(dates2 ) < 18) %>% 
  na.omit() %>% dplyr::select(Accion, seg= on.time.diff.hour, dates2) %>% 
  mutate(min= seg/60) %>% 
  mutate(day = format(as.Date(dates2), "%d-%m")) %>% dplyr::select(-c(dates2))



brooding4<-brooding2  %>%
  filter(lubridate::hour(dates2) >= 5 & lubridate::hour(dates2 ) < 18) %>% 
  na.omit() %>% dplyr::select(Accion, seg= on.time.diff.hour, dates2) %>% 
  mutate(min= seg/60) %>%
  mutate(day = format(as.Date(dates2), "%d-%m")) %>% dplyr::select(-c(dates2))

view(brooding4)

nestling2 <- rbind(f4, brooding4) 
nestling2<-complete(nestling2, Accion, day)
nestling2[is.na(nestling2)] <- 0


finale<- rbind(nestling, nestling2)
view(finale)

#################################################################################
library(ggbreak) 
library(patchwork)
levels(as.factor(finale$day))





feeding<-finale %>% dplyr::filter(Accion=="f") %>%  ggplot(aes(x= day
                                                                 , y= as.numeric(seg))) +
  geom_boxplot(varwidth = TRUE, alpha=0.5, color="grey40",
               outlier.colour="grey") + xlab("Days after hatching") +
  ylab("Feeding time (seg)") +
  geom_jitter(color="black", shape=16, size=2, alpha=0.1,
              position=position_jitter(0.2)) + scale_fill_grey() +
  theme_classic() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=9),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    axis.title.x = element_text(size = 11)
  ) + 
  scale_y_continuous(limits = c(0, 180)) +
  scale_x_discrete(limits = c("30-11", "01-12", "02-12", "03-12", "04-12",
                              "05-12", "06-12", "07-12", "10-12", "11-12",
                              "12-12", "13-12"),
                   breaks=c("30-11", "01-12", "02-12", "03-12", "04-12",
                            "05-12", "06-12", "07-12", "10-12", "11-12",
                            "12-12", "13-12"),
                   labels=c("5", "6", "7", "8", "9",
                            "10", "11", "12", "15", "16",
                            "17", "18")) 


Totals3 <- finale %>% dplyr::filter(Accion=="f") %>% 
  group_by(day) %>% dplyr::summarize(N = n()) 

f<-feeding  + geom_text(aes(y =ifelse(N <8, 128+N, 130+N), 
                            label = round(N/13.5, 2) ), data = Totals3,
                        color = "blue",
                        size = 3) 
f



brooding<-finale %>% dplyr::filter(Accion=="br") %>% ggplot(aes(x=day, y= as.numeric(min))) +
  geom_boxplot(varwidth = TRUE, alpha=0.5,  color="grey10",
               outlier.colour="grey") + xlab("") +
  ylab("Brooding time (min)") +
  geom_jitter(color="black", shape=16, size=2, alpha=0.1,
              position=position_jitter(0.2)) + scale_fill_grey() +
  theme_classic() +
  theme(
    legend.position="",
    plot.title = element_text(size=9),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    axis.title.x = element_text(size = 11)
  ) +
  scale_y_continuous(limits = c(0, 75)) +
  scale_x_discrete(limits = c("30-11", "01-12", "02-12", "03-12", "04-12",
                              "05-12", "06-12", "07-12", "10-12", "11-12",
                              "12-12", "13-12"),
                   breaks=c("30-11", "01-12", "02-12", "03-12", "04-12",
                            "05-12", "06-12", "07-12", "10-12", "11-12",
                            "12-12", "13-12"),
                   labels=c("5", "6", "7", "8", "9",
                            "10", "11", "12", "15", "16",
                            "17", "18")) 

Totals2 <- finale %>% dplyr::filter(Accion=="br") %>% 
  group_by(day) %>% dplyr::summarize(N = n())


b<-brooding + geom_text(aes(y =ifelse(N <8, 50+N, 51+N)  ,
                            label = ifelse(N ==1, 0, round(N/13.5, 2))),
                        data = Totals2, color = "blue",
                        size = 3)


b


################################3 Number of trips/hour total
Trips <- finale %>% 
  group_by(day) %>% dplyr::summarize(N = n()) %>% 
  mutate(trips =  round(N/13.5, 1))

write.csv(Trips, file = "trips_a_hour.csv", sep = ",", dec = ".")

#library(gridExtra)



######




dg<-  tapply(finale$min,finale$day, FUN=sum)
dg2<-  tapply(finale$min, list(finale$day, finale$Accion), FUN=sum)
dg2<-as.data.frame(dg2)

dg2<-setNames(cbind(rownames(dg2), dg2, row.names = NULL), 
         c("date", "BR", "fee"))


dg2<-dg2 %>% mutate(totaly= BR+fee, porcent=totaly*100/810,
               BRpor=BR*100/810, Feepor=fee*100/810) %>% as.data.frame()



graf3<-dg2 %>% dplyr::select(date, BRpor, Feepor) %>%
  arrange(match(date, c("30-11", "01-12", "02-12", "03-12", "04-12",
                        "05-12", "06-12", "07-12", "10-12", "11-12",
                        "12-12", "13-12")), desc(BRpor), desc(Feepor)) %>% 
  pivot_longer( cols=c(BRpor, Feepor),
                                                      names_to = "duty",
                                                      values_to = "porcent") %>% 
ggplot(aes(x=date,y=porcent, fill=duty)) + 
  theme_light()  +
  geom_col(alpha=.8) +
  xlab("") +
  ylab("Time Invested (%)") +
scale_fill_grey() +
  theme( legend.text = element_text(size = 9),
    legend.position="top",
    axis.text = element_text(size = 9),
    axis.title.y = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    legend.title = element_text(size=10)
  ) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_x_discrete(limits = c("30-11", "01-12", "02-12", "03-12", "04-12",
                              "05-12", "06-12", "07-12", "10-12", "11-12",
                              "12-12", "13-12"),
                   breaks=c("30-11", "01-12", "02-12", "03-12", "04-12",
                            "05-12", "06-12", "07-12", "10-12", "11-12",
                            "12-12", "13-12"),
                   labels=c("5", "6", "7", "8", "9",
                            "10", "11", "12", "15", "16",
                            "17", "18")) +
  scale_fill_manual("Activity", values = c("BRpor" = 'grey10', "Feepor" = 'grey40'),
                    labels = c("brooding", "feeding"))







graf3

ggsave(filename = "Breeding_spent.png",
       plot = graf3, width = 14, height = 12, dpi = 350, units = "cm")


graf_bre<- grid.arrange( graf3,b,f,  ncol=1)


#ggsave(filename = "Breeding_grallaria.png",
 #      plot = graf_bre, width = 18, height = 16, dpi = 350, units = "cm")


#############################################################################

library(lubridate)
library(bbmle)
library("multcomp")
library("car")
library(emmeans)
library(MASS)



BR<-finale %>% dplyr::filter(Accion=="br") %>% transform(as.numeric(min)) %>% 
  arrange(match(day, c("30-11", "01-12", "02-12", "03-12", "04-12",
                        "05-12", "06-12", "07-12", "10-12", "11-12",
                        "12-12", "13-12")), desc(seg), desc(min))
###
par(mfrow = c(1, 1))
plot(BR)

write.csv(BR, file = "BR_data_total.csv", sep = ",", dec = ".")

BR %>% 
ggplot(aes(day,seg)) + 
  geom_point() +
  scale_x_discrete(limits = c("30-11", "01-12", "02-12", "03-12", "04-12",
                              "05-12", "06-12", "07-12", "10-12", "11-12",
                              "12-12", "13-12"),
                   breaks=c("30-11", "01-12", "02-12", "03-12", "04-12",
                            "05-12", "06-12", "07-12", "10-12", "11-12",
                            "12-12", "13-12"),
                   labels=c("5", "6", "7", "8", "9",
                            "10", "11", "12", "15", "16",
                            "17", "18")) 




nbGLM <- glm.nb(as.numeric(seg)~ day, data=BR)
nullbGLM <- glm.nb(as.numeric(seg)~ 1, data=BR)

broom::glance(nullbGLM)
broom::glance(nbGLM)



summary(nbGLM)

result1<-Anova(nbGLM,
               type="II",
               test="LR")
result1
day_point = emmeans(nbGLM, ~ day)
posthoc<-pairs(day_point, letters = TRUE, letter_case = "lower")
posthoc<-data.frame(posthoc)


library(rcompanion)
library(multcompView)



post<-cld(day_point,
          Letters=letters,      ### Use lower-case letters for .group
          adjust="bonferroni",
          alpha = 1 - sqrt(0.95))    ### Tukey adjustment for multiple comparisons


post<-post %>% group_by(day) %>% 
  merge(Totals2) %>% mutate(gr= .group) %>% dplyr::select(-.group) %>% 
  arrange(match(day, c("30-11", "01-12", "02-12", "03-12", "04-12",
                       "05-12", "06-12", "07-12", "10-12", "11-12",
                       "12-12", "13-12")), desc(emmean), desc(SE))

b2<-b + geom_text(aes(y = ifelse(N <8, 40+N, 42+N) ,
                  label = gr),
              data = post, color = "black",
              size = 3)  


b2
###################################################


FEE<-finale %>% dplyr::filter(Accion=="f") %>% transform(as.numeric(min)) %>% 
  arrange(match(day, c("30-11", "01-12", "02-12", "03-12", "04-12",
                       "05-12", "06-12", "07-12", "10-12", "11-12",
                       "12-12", "13-12")), desc(seg), desc(min))

write.csv(FEE, file = "feeding_trips_finale.csv")

nbGLM2 <- glm.nb(as.numeric(seg) ~ day, data=FEE)
nullbGLM2 <- glm.nb(as.numeric(seg)~ 1, data=FEE)

broom::glance(nullbGLM2)
broom::glance(nbGLM2)

summary(nbGLM2)

result2<-Anova(nbGLM2,
               type="II",
               test="LR")
result2
day_point = emmeans(nbGLM2, ~ day)
posthoc2<-pairs(day_point)
posthoc<-data.frame(posthoc2)
write.csv(posthoc, "posthoc_nestlings_f.csv", sep = ",", dec = ".")

post2<-cld(day_point,
          Letters=letters,      ### Use lower-case letters for .group
          adjust="bonferroni",
          alpha = 1 - sqrt(0.95))    ### Tukey adjustment for multiple comparisons

post2<-post2 %>% group_by(day) %>% 
  merge(Totals2) %>% mutate(gr= .group) %>% dplyr::select(-.group) %>% 
  arrange(match(day, c("30-11", "01-12", "02-12", "03-12", "04-12",
                       "05-12", "06-12", "07-12", "10-12", "11-12",
                       "12-12", "13-12")), desc(emmean), desc(SE))

write.csv(post2, file = "D:/biato/Grallaria/data_analysis/breeding/feeding_average_2.csv", dec = ".")

post2<-post2 %>% group_by(day) %>% 
  merge(Totals3) %>% mutate(gr= .group) %>% dplyr::select(-.group)

f2<-f + geom_text(aes(y =ifelse(N <8, 115+N, 118+N) ,
                  label = gr),
              data = post2, color = "black",
              size = 3)





graf_bre2<- grid.arrange( graf3,b2,f,  ncol=1)


ggsave(filename = "Breeding_grallaria_2_B.png",
       plot = graf_bre2, width = 18, height = 16, dpi = 350, units = "cm")

