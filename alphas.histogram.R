
setwd("~/Desktop/NMA_Output_Scripts")

results_data <- read.csv("thromb_2arms_2discnodes_dnorm.csv")

# grab a dataframe containing only alpha data
alphas <- results_data[which(startsWith(x = as.character(results_data$var), prefix = "alpha")),]

hist(stack(alphas)$values)

# FOR A ALPHA HIST PLOT ACROSS ALL NETWORKS
# Should follow a normal of students t distn
for(network.name in colnames(alphas)) {
  if (network.name != "var") {
    png(file = paste0(network.name, "_plot.png"))
    hist(alphas[,network.name])
    dev.off()
  }
}

# MAIN PLOT OF INTEREST
hist(alphas[,"Full-network-model-fixed"])

