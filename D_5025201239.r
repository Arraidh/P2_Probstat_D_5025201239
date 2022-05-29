
#Soal 1

subject = c(seq(1:9))

#A
kadarSebelum = c(78,75,67,77,70,72,78,74,77)
kadarSesudah = c(100,95,70,90,90,90,89,90,100)
data_frame = data.frame(subject, kadarSebelum, kadarSesudah)

sd(data_frame$kadarSebelum-data_frame$kadarSesudah)

#B
t.test(kadarSebelum, kadarSesudah, alternative = "greater", var.equal = FALSE)

#C
# Terdapat pengaruh signifikan secara statistika terkaut kadar saturasi oksigen sebelum dan sesudah aktivitas A
# Hal ini dikarenakan nilai t lebih besar dari nilai kritis t dengan tingkat signifikansi 5%

#Soal 2
install.packages("BSDA")
library(BSDA)

#B
tsum.test(23500, 3900, 100)
#(Rata-rata, standar deviasi, pemilik mobil)

#Soal 3
#B
tsum.test(mean.x=3.64,
          s.x = 1.67,
          n.x = 19,
          mean.y =2.79,
          s.y = 1.32,
          n.y = 27,
          alternative = "greater", var.equal = TRUE)

#C
t3 = (2.79-3.64)/(1.32/sqrt(27))
t3

#D
# alpha = 0.05
t.alpha3 = qt(1-0.05, df=2)
t.alpha3




#Soal 4

#A
data <- read.delim(file.choose())

data$Group <- as.factor(data$Group)
data$Group = factor(data$Group, labels = c("grup1", "grup1", "grup3"))


grup1 <- subset(data, Group == "grup1")
grup2 <- subset(data, Group == "grup1")
grup3 <- subset(data, Group == "grup3")

qqnorm(grup1$Length)

qqnorm(grup2$Length)

qqnorm(grup3$Length)

#B
bartlett.test(Length ~ Group, data = data)

#C
model1 = lm(Length ~ Group, data = data)
anova(model1)

#D

#E
TukeyHSD(aov(model1))

#Soal 5
install.packages("multcompView")
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

dataGTL <- read.csv(file.choose())

#A
qplot(x = Temp, y = Light, geom = "point", data = dataGTL) +
  facet_grid(.~Glass, labeller = label_both)


#B
dataGTL$Glass <- as.factor(dataGTL$Glass)
dataGTL$Temp_Factor <- as.factor(dataGTL$Temp)
str(dataGTL)

anova <- aov(Light ~ Glass*Temp_Factor, data = dataGTL)
summary(anova)

# Soal 5c
data_summary <- group_by(dataGTL, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)

# Soal 5d
ujiTukey <- TukeyHSD(anova)
print(ujiTukey)

# e)
ujiTukey.cld <- multcompLetters4(anova, ujiTukey)
print(ujiTukey.cld)