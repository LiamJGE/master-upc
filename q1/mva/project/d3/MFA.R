library("FactoMineR")
library("factoextra")
library(readxl)
df <- read_excel("../datasets/mfa3.xlsx")

sapply(df, class)

#n=categorical
#s=numerical

res.mfa <- MFA(df, 
               group = c(3, 3, 1, 11, 3, 1, 3, 1), 
               type = c("n", "s", "n", "n", "s", "n", "s", "s"),
               name.group = c("Host information categorical", "Host information numerical", 
                              "Location of AirBnB (categorical)","Physical form (categorical)",
                              "Physical form (numerical)", "Booking Process (categorical)", 
                              "Booking Process (numerical)",  "Price(numeric)" ),
               num.group.sup = c(8),
               graph = FALSE)

print(res.mfa)



#'*EIGENVALUES & SCREEPLOT*

eig.val <- get_eigenvalue(res.mfa)
head(eig.val)
eig.val

fviz_screeplot(res.mfa,ncp=12)



#'*GRAPH VARIABLES*

group <- get_mfa_var(res.mfa, "group")
group

# Coordinates of groups
head(group$coord)
# Cos2: quality of representation on the factore map
head(group$cos2)
# Contributions to the  dimensions
head(group$contrib)

fviz_mfa_var(res.mfa, "group", axes = c(1, 2))
fviz_mfa_var(res.mfa, "group", axes = c(1, 3))
fviz_mfa_var(res.mfa, "group", axes = c(1, 4))


# Contribution to the first dimension
fviz_contrib(res.mfa, "group", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.mfa, "group", axes = 2)




quanti.var <- get_mfa_var(res.mfa, "quanti.var")
quanti.var 



# Coordinates
head(quanti.var$coord)
# Cos2: quality on the factore map
head(quanti.var$cos2)
# Contributions to the dimensions
head(quanti.var$contrib)


summary(res.mfa)
fviz_mfa_var(res.mfa, "quanti.var", axes = c(1, 2), palette = "jco", 
             col.var.sup = "violet", repel = TRUE)
fviz_mfa_var(res.mfa, "quanti.var", axes = c(1, 3), palette = "jco", 
             col.var.sup = "violet", repel = TRUE)
fviz_mfa_var(res.mfa, "quanti.var", axes = c(1, 4), palette = "jco", 
             col.var.sup = "violet", repel = TRUE)




# Contributions to dimension 1
fviz_contrib(res.mfa, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco")



fviz_contrib(res.mfa, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")


fviz_mfa_var(res.mfa, "quanti.var", col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"))

--------------------------

ind <- get_mfa_ind(res.mfa)
ind

fviz_mfa_ind(res.mfa, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


fviz_mfa_quali_biplot(res.mfa, repel = F, col.var = "#E7B800",
                      habillage = as.factor(df[,"host_acceptance_rate"] <= 0.2), geom = "point")

fviz_mfa_quali_biplot(res.mfa, repel = F, col.var = "#E7B800",
                      habillage = as.factor(df[,"host_response_rate"] <= 0.2), geom = "point")
