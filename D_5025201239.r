
library(BSDA)
library("ggpubr")

#Soal 1

#A
kadarSebelum = c(78,75,67,77,70,72,78,74,77)
kadarSesudah = c(100,95,70,90,90,90,89,90,100)

sd(mean(kadarSesudah-kadarSebelum))

#B
t.test(kadarSebelum, kadarSesudah, paired = TRUE)

#C
# Tidak terdapat pengaruh signifikan secara statistika terkaut kadar saturasi oksigen sebelum dan sesudah aktivitas A
# Hal ini dikarenakan nilai t lebih kecil dari nilai kritis t dengan tingkat signifikansi 5%

#Soal 2

zsum.test(
  mean.x=23500, sigma.x = 3900, n.x = 100,  
  alternative = "two.sided", mu = 20000,
  conf.level = 0.95
)

#A
# Setuju, Mobil dikemudikan dapat memiliki rata-rata lebih dari
# 20000 kilometer per tahun dikarenakan kesimpulan dari uji z menolak H0


#B


#C
#karena p-value kurang dari 0.05 yang menyebabkan h0 ditolak
#maka, kesimpulannyaadalah bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun


#Soal 3
#A
#H0 : mu = mu0 
#mu != mu0

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
