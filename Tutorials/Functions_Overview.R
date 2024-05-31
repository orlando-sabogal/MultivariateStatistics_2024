

# PCA
eigen_results <- eigen(cov_matrix)
eigen_results$values
eigen_results$vectors


MyFirst_PCA <- prcomp(MyData, scale = TRUE)
MyFirst_PCA$rotation
MyFirst_PCA$sdev^2
MyFirst_PCA$sdev^2/sum(MyFirst_PCA$sdev^2)

PCA_Results <- princomp(MyData, cor = TRUE, scores = TRUE)


# Reliability
alpha(Data)
omega(Data)
KMO(Data)
cortest.bartlett(Data)


# EFA
Eigenvalues_Correlation <- eigen(cor(Data_EFA))
Eigenvalues_Correlation$values
library(psych)
scree(cor(Data_EFA))
fa.parallel(Data_EFA, fa = "both", n.iter = 500)

EFA_Three_Factors <- fa(Data_EFA, nfactors = 3, 
                        rotate = "oblimin", fm = "ml",
                        scores = "Bartlett")
EFA_Three_Factors
EFA_Scores <- EFA_Three_Factors$scores
EFA_Scores <- as.data.frame(EFA_Scores)

# CFA
library(lavaan)
Model <- cfa(model = YourModel, data = YourData)
Model <- sem(model = YourModel, data = YourData)
summary(Model, standardized = TRUE, rsquare = TRUE,  fit.measures = TRUE)
GoF = c("srmr", "rmsea", "tli", "cfi") 
fitmeasures(Model, fit.measures = GoF)
