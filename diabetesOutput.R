
setwd("~/Desktop")

d1 <- read.csv("inputs.diabetes.2arms.3.Rdtalpha.mn,alpha.prc,30___mos_table.csv")
d2 <- read.csv("inputs.diabetes.2arms.3.Rdtalpha.mn,alpha.prc,15___mos_table.csv")
d3 <- read.csv("inputs.diabetes.2arms.3.Rdtalpha.mn,alpha.prc,3___mos_table.csv")
d4 <- read.csv("inputs.diabetes.2arms.3.Rdnormalpha.mn,alpha.prc___mos_table.csv")

d5 <- read.csv("inputs.diabetes.2arms.2.Rdtalpha.mn,alpha.prc,30___mos_table.csv")
d6 <- read.csv("inputs.diabetes.2arms.2.Rdtalpha.mn,alpha.prc,15___mos_table.csv")
d7 <- read.csv("inputs.diabetes.2arms.2.Rdtalpha.mn,alpha.prc,3___mos_table.csv")
d8 <- read.csv("inputs.diabetes.2arms.2.Rdnormalpha.mn,alpha.prc___mos_table.csv")



mos_table <- rbind(d1,d2,d3,d4,d5,d6,d7,d8)

mos_table$Value <- as.factor(mos_table$Value)

colnames(mos_table)[colnames(mos_table)=="X."] <- "Frequency"


mos_table <- read.csv( "~/Desktop/diabetesMosTable2.csv")

filtered_mos_table <- mos_table[!(mos_table["Value"] == c("(0.95,1]")),]

plot <- ggplot(filtered_mos_table, aes(analysis_name, Value)) + geom_tile(aes(fill = Frequency),colour = "white") +
  scale_fill_gradient2(low="white",  high = "black")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot







# NOT 2 arms


d1 <- read.csv("inputs.diabetes.3.Rdtalpha.mn,alpha.prc,30___mos_table.csv")
d2 <- read.csv("inputs.diabetes.3.Rdtalpha.mn,alpha.prc,15___mos_table.csv")
d3 <- read.csv("inputs.diabetes.3.Rdtalpha.mn,alpha.prc,3___mos_table.csv")
d4 <- read.csv("inputs.diabetes.3.Rdnormalpha.mn,alpha.prc___mos_table.csv")

d5 <- read.csv("inputs.diabetes.2.Rdtalpha.mn,alpha.prc,30___mos_table.csv")
d6 <- read.csv("inputs.diabetes.2.Rdtalpha.mn,alpha.prc,15___mos_table.csv")
d7 <- read.csv("inputs.diabetes.2.Rdtalpha.mn,alpha.prc,3___mos_table.csv")
d8 <- read.csv("inputs.diabetes.2.Rdnormalpha.mn,alpha.prc___mos_table.csv")



mos_table <- rbind(d2,d3,d4,d7,d8)

mos_table$Value <- as.factor(mos_table$Value)

colnames(mos_table)[colnames(mos_table)=="X."] <- "Frequency"


mos_table <- read.csv( "~/Desktop/diabetesMosTable2.csv")

filtered_mos_table <- mos_table[!(mos_table["Value"] == c("(0.95,1]")),]

plot <- ggplot(filtered_mos_table, aes(analysis_name, Value)) + geom_tile(aes(fill = Frequency),colour = "white") +
  scale_fill_gradient2(low="white",  high = "black")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot




