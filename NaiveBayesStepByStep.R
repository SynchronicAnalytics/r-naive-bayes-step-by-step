# NaiveBayesStepByStep.R
# Illustrates the calculations used by the naive Bayes algorithm to predict a query target level given a training data set
# c. 2017 David Schwab

# Preliminaries

rm(list=ls()) 
options(digits=2)    # This makes the numbers display better; feel free to change it.

# Construct the data set

pct.cash <- factor(c(1,1,1,2,2,1,2,2,1,3,3,3,3,2,3,3,1,1))
wire.transfers <- factor(c(1,1,2,1,2,1,2,2,1,3,3,3,3,2,3,3,1,1))
home.office <- factor(c(1,1,1,1,2,2,1,2,2,2,2,1,1,1,2,1,2,1))
tax.owed <- factor(c(1,1,1,2,2,2,1,1,2,3,3,3,3,1,3,3,2,1))
audit.risk <- factor(c(1,1,1,1,2,2,2,2,2,3,3,3,3,3,1,2,3,2))

# Add the factor levels and make the data frame

levels(pct.cash) <- c("Less than 25%", "Between 25% & 50%", "More than 50%")
levels(wire.transfers) <- c("Zero", "Less than 5", "5 or More")
levels(home.office) <- c("Yes", "No")
levels(tax.owed) <- c("Less than $1,000", "Between $1,000 and $5,000", "More than $5,000")
levels(audit.risk) <- c("Low", "Moderate", "High")

audit.risk.data <- data.frame(pct.cash,wire.transfers,home.office,tax.owed,audit.risk)

# Next, count the instances of each target level

audit.risk.targets <- as.numeric(table(audit.risk.data$audit.risk))

# Now, calculate the conditional probabilities needed to predict a target with the naive Bayes model

audit.risk.cond.prob <- apply(audit.risk.data, 2, function(x){
  t(table(x,audit.risk.data$audit.risk)) / audit.risk.targets
})

audit.risk.cond.prob$audit.risk <- NULL       # Remove extraneous data

# Calculate the target priors and format them for display

audit.risk.priors <- audit.risk.targets / nrow(audit.risk.data)
audit.risk.priors.display <- data.frame(target=c("Low", "Moderate", "High"),priors=audit.risk.priors)

# Examine the prior and conditional probabalities
# We can calculate the target level for a query using these probabilities
# We display each element of the list separately to order the columns correctly, using the levels defined earlier

audit.risk.priors.display

audit.risk.cond.prob$pct.cash[,levels(pct.cash)]
audit.risk.cond.prob$wire.transfers[,levels(wire.transfers)]
audit.risk.cond.prob$home.office[,levels(home.office)]
audit.risk.cond.prob$tax.owed[,levels(tax.owed)]
