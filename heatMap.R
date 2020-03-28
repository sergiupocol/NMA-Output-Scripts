library(ggplot2)
setwd("~/Desktop")
comb_expgrid_dep <- expand.grid("depression-mos-tables-allarms/", "inputs.depression.", 
            c("2","4", "6"), 
            c(".Rdtalpha.mn,alpha.prc,?", ".Rdnormalpha.mn,alpha.prc", ".Rdtalpha.mn,alpha.prc,1", ".Rdtalpha.mn,alpha.prc,5", ".Rdtalpha.mn,alpha.prc,15"),
            "___mos_table.csv")

comb_expgrid_atr <- expand.grid("atrial-mos-tables3/", "inputs.atrial.", 
                                c("2","5", "8"), 
                                c(".Rdtalpha.mn,alpha.prc,?", ".Rdnormalpha.mn,alpha.prc", ".Rdtalpha.mn,alpha.prc,1", ".Rdtalpha.mn,alpha.prc,5", ".Rdtalpha.mn,alpha.prc,15"),
                                "___mos_table.csv")


comb_expgrid_thro <- expand.grid("thrombolytic-mos-tables/", "inputs.thrombolytic.", 
                                 c("2","4"), 
                                 c(".Rdtalpha.mn,alpha.prc,?", ".Rdnormalpha.mn,alpha.prc", ".Rdtalpha.mn,alpha.prc,1", ".Rdtalpha.mn,alpha.prc,5", ".Rdtalpha.mn,alpha.prc,15"),
                                 "___mos_table.csv")

comb_expgrid_diab <- expand.grid("diab-folup-mos-tables/", "inputs.diabetes.2arms.", 
                            c("2","3"), 
                            c(".Rdtalpha.mn,alpha.prc,?", ".Rdnormalpha.mn,alpha.prc", ".Rdtalpha.mn,alpha.prc,1", ".Rdtalpha.mn,alpha.prc,5", ".Rdtalpha.mn,alpha.prc,15"),
                            "___mos_table.csv")


#comb_expgrid_diab <- expand.grid("diabetes-mos-tables/", "inputs.diabetes.", 
 #                           c("2","3"), 
  #                          c(".Rdnormalpha.mn,alpha.prc", ".Rdtalpha.mn,alpha.prc,1", ".Rdtalpha.mn,alpha.prc,5", ".Rdtalpha.mn,alpha.prc,15", ".Rdtalpha.mn,alpha.prc,?"),
   #                         "___mos_table.csv")

names_d <- apply(comb_expgrid_atr, 1, paste0, collapse = "")

mos_table <- read.csv(names_d[1])
for(i in 2:length(names_d)) mos_table <- rbind(mos_table, read.csv(names_d[i]))

colnames(mos_table)[colnames(mos_table)=="X."] <- "Percent (%)"
filtered_mos_table <- mos_table[!(mos_table["Value"] == c("(0.95,1]")),]
filtered_mos_table <- filtered_mos_table[!(filtered_mos_table["Value"] == c("(0.9,0.95]")),]
filtered_mos_table <- filtered_mos_table[!(filtered_mos_table["Value"] == c("(0.85,0.9]")),]
filtered_mos_table <- filtered_mos_table[!(filtered_mos_table["Value"] == c("(0.8,0.85]")),]

filtered_mos_table$facet <- rep(c(rep(2, 16), rep(5, 16), rep(8, 16)), 5)#rep(c(rep(2, 16), rep(4, 16)), 5)#rep(c(rep(2, 16), rep(4, 16), rep(6, 16)), 5)#rep(c(rep(2, 16), rep(3, 16)), 5)## #
n <- 3
filtered_mos_table$df <- c(rep("?", n*16), rep("∞", n*16), rep("1", n*16),rep("5", n*16),rep("15", n*16))


names(filtered_mos_table)[names(filtered_mos_table) == "Value"] <- "Discrepancy Measure"
names(filtered_mos_table)[names(filtered_mos_table) == "df"] <- "Degrees of Freedom"
filtered_mos_table$facet <- sapply(filtered_mos_table$facet, function(x) {
  paste0(as.character(x), " disconnected nodes")
})
plot <- ggplot(filtered_mos_table, aes_string("`Degrees of Freedom`", "`Discrepancy Measure`")) + geom_tile(aes(fill = `Percent (%)`), colour = "white") +
  scale_fill_gradient2(low="white" ,limits=c(0,2),high = "black",guide = guide_colourbar())+ theme(text = element_text(size=20), axis.text.x = element_text(angle = 0, hjust = 1))+
  #facet_wrap(vars(facet), nrow=1)
  facet_grid(. ~ facet, scales="free_x")+
  scale_y_discrete(limits=as.character(unique(filtered_mos_table$`Discrepancy Measure`)))+
  scale_x_discrete(limits=c("?","1","5","15","∞")) 

plot





### ecdf STUFF
test <- read.csv("/Users/sergiupocol/Desktop/NMA Raw Data/diabetes-mos-tables/inputs.diabetes.2.Rdnormalpha.mn,alpha.prc___mos_table.csv")
second <- read.csv("/Users/sergiupocol/Desktop/NMA Raw Data/diabetes-mos-tables/inputs.diabetes.2.Rdtalpha.mn,alpha.prc,1___mos_table.csv")

ecdf(test$X.)
## Remember the value of count is a FREQUENCY so it is directly what needs to be considered
stuff <- c()
test$Value <- factor(test$Value, levels(test$Value)[c(20, 1:19)])
for (row in 1:nrow(test)) {
  stuff <- c(stuff, rep(test[row, "Value"], test[row, "Count"]))
}
tea <- data.frame(stuff)



ecdf(second$X.)
## Remember the value of count is a FREQUENCY so it is directly what needs to be considered
stuff2 <- c()
second$Value <- factor(second$Value, levels(second$Value)[c(20, 1:19)])
for (row in 1:nrow(second)) {
  stuff2 <- c(stuff2, rep(second[row, "Value"], second[row, "Count"]))
}
tea2 <- data.frame(stuff2)


plot <- ggplot(tea, aes(stuff)) + stat_ecdf(geom = "step")+ 
  labs(title="Empirical Cumulative for Diabetes (2) All Arms dnorm",
       y = "log(F(Bin Number))", log = 'x', x="Bin Number")+
  theme_classic() + scale_y_log10()

plot









d.f <- data.frame(
  df = as.factor( rep( c("∞","1"), each=135 ) ) ,
  val = c( stuff, stuff2 )
)
d.f <- arrange(d.f,df,val)
d.f.ecdf <- ddply(d.f, .(df), transform, ecdf=ecdf(val)(val) )

p <- ggplot( d.f.ecdf, aes(val, ecdf, colour = df) )
p + geom_step() + labs(title="Empirical Cumulative for Diabetes (2) All Arms",
                     y = "log(F(Bin Number))", log = 'x', x="Bin Number")+
  theme_classic() + scale_y_log10()

































