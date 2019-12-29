#install.packages("lme4")
#install.packages("emmeans")
library(lme4)
library(emmeans)

# Each .csv file in the current working directory will be analyzed.
files <- list.files(path='.',pattern='\\.csv$')
files

for (f in files) {
  file = read.csv(f,as.is=T,header = T,check.names=F,na.strings="",blank.lines.skip=T)
  data = file
  Group = NULL
  Drive = NULL
  m = NULL
  d = NULL
  # Iterate through the current file and composite data into the "Group" and "Drive" lists.
  for (i in 1:nrow(data)) {
    dr = data[i, 1]
    res = data[i, 2]
    dr_vial = rep(i - 1, dr)
    res_vial = rep(i - 1, res)
    dr_inds = rep(1, dr)
    res_inds = rep(0, res)
    Group = c(Group, dr_vial, res_vial)
    Drive =  c(Drive, dr_inds, res_inds)
  }
  d <- data.frame(Group, Drive)
  # Fit a generalized linear model to the data, with the "Group" as a random-effects part of the model.
  m <- glmer(Drive ~ 1 + (1 | Group), data = d, family = binomial, nAGQ = 25)
  print(summary(m))
  outfile <- file(paste(f, "_analysis.txt", sep=""), "w")
  sink(outfile)
  print(summary(m))
  print(confint(m))
  print(coef(m))
  # Least squared means analysis of the model.
  print(emmeans(m, ~1, type="response"))
  sink()
  close(outfile)
}
print("Finished!")