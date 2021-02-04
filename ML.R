#Load libraries
library(bnlearn)
library(gRain)
library(readr)

#Load data
data = read.csv("C:/Users/Usuario/Desktop/ML 4/cardiovascular_disease_clean.csv",sep = ",", header = TRUE)

data$Age <- as.factor(data$Age)
data$BMI <- as.factor(data$BMI)
data$Gender <- as.factor(data$Gender)
data$Cholesterol <- as.factor(data$Cholesterol) 
data$Glucose <- as.factor(data$Glucose)
data$Smoke <- as.factor(data$Smoke)
data$Alcohol <- as.factor(data$Alcohol)
data$PhysicalActivity <- as.factor(data$PhysicalActivity)
data$CardioDisease <- as.factor(data$CardioDisease)

##### Learning Structure #####
#Blacklist
bl = matrix(c("CardioDisease", "BMI", "CardioDisease","Gender","CardioDisease","Cholesterol", "CardioDisease","Smoke","CardioDisease","Glucose","CardioDisease","Alcohol","CardioDisease","PhysicalActivity", "CardioDisease","Age"), ncol = 2, byrow = TRUE)
#Whitelist
wl = c("PhysicalActivity","CardioDisease")
#Hybrid
hpc = list(test = 'mi-sh', alpha = 0.01)
hc = list(score = 'bic', restart=10, perturb=1)
h2pc <- h2pc(data, restrict.args = hpc,
             maximize.args = hc)
plot(h2pc)

#HPC por partes
hpc <- hpc(data, test = 'mi', alpha =0.01)
plot(hpc)

#### Learning Parameter ####
fit = bn.fit(h2pc, data, method='mle')
fit
bn.fit.barchart(fit$CardioDisease)
bn.fit.dotplot(fit$BMI)
graphviz.chart(fit, type = "dot" )

#Query 1 

cpquery(fit, (CardioDisease == "Yes"), (PhysicalActivity == "Yes"), debug=TRUE, n=10000)

gr.fit <- as.grain(fit)


gr.fit <- setEvidence(object=gr.fit, nodes="Smoke",
                        states="Yes")
gr.fit <- setEvidence(object=gr.fit, nodes="BMI",
                      states="Obese")
querygrain(gr.fit, nodes =c("CardioDisease", 'Age'), type="marginal")

