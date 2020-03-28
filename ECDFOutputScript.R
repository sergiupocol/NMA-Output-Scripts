



comb_expgrid_diab <- expand.grid("diab-folup-mos-tables/", "inputs.diabetes.2arms.", 
                                 c("2","3"), 
                                 c(".Rdtalpha.mn,alpha.prc,?", ".Rdnormalpha.mn,alpha.prc", ".Rdtalpha.mn,alpha.prc,1", ".Rdtalpha.mn,alpha.prc,5", ".Rdtalpha.mn,alpha.prc,15"),
                                 "___mos_table.csv")


names_d <- apply(comb_expgrid_atr, 1, paste0, collapse = "")

mos_table <- read.csv(names_d[1])
for(i in 2:length(names_d)) mos_table <- rbind(mos_table, read.csv(names_d[i]))









### ecdf STUFF
test <- read.csv("/Users/sergiupocol/Desktop/NMA Raw Data/diabetes-mos-tables/inputs.diabetes.2.Rdnormalpha.mn,alpha.prc___mos_table.csv")
second <- read.csv("/Users/sergiupocol/Desktop/NMA Raw Data/diabetes-mos-tables/inputs.diabetes.2.Rdtalpha.mn,alpha.prc,1___mos_table.csv")

## Remember the value of count is a FREQUENCY so it is directly what needs to be considered
stuff <- c()
test$Value <- factor(test$Value, levels(test$Value)[c(20, 1:19)])
for (row in 1:nrow(test)) {
  stuff <- c(stuff, rep(test[row, "Value"], test[row, "Count"]))
}
tea <- data.frame(stuff)




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

