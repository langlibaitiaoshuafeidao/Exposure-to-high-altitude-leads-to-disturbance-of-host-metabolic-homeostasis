
#remotes::install_github("cardiomoon/multipleROC")
#remotes::install_git("https://gitee.com/swcyo/multipleROC/")
library(multipleROC)
df <-  read.table("18 pm dem.txt",row.names = 1,header=T, sep="\t")
head(df)
p <- multipleROC(Type~L_arginine,data=df)
plot_ROC(p,
         show.points = T, 
         show.eta = T, 
         show.sens = T, 
         show.AUC = T, 
         facet = F )
p$auc
p$cutpoint
p$cutoff
p1 <- multipleROC(Type~L_arginine,data=df)
p2 <- multipleROC(Type~alpha_D_Glucose,data=df)
p3 <- multipleROC(Type~Creatine,data=df)
p4 <- multipleROC(Type~Cortisol,data=df)
p5 <- multipleROC(Type~Desoxycortone,data=df)
p6 <- multipleROC(Type~dione,data=df)
p7 <- multipleROC(Type~MANNOSE,data=df)
p8 <- multipleROC(Type~Galactitol,data=df)
p9 <- multipleROC(Type~Argininosuccinic_acid,data=df)
p10 <- multipleROC(Type~ Methoxyestrone ,data=df)
p11 <- multipleROC(Type~Polysulfide,data=df)
p12 <- multipleROC(Type~Acetyl_phosphate,data=df)
p13<- multipleROC(Type~ PC,data=df)
p14 <- multipleROC(Type~ PE,data=df)
p15 <- multipleROC(Type~Methadone,data=df)
p16 <- multipleROC(Type~ Hydroxyfelbamate,data=df)
p17 <- multipleROC(Type~LysoPC,data=df)
p18 <- multipleROC(Type~Cer,data=df)
plot_ROC(list(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18),
         show.points = T, 
         show.eta = F, 
         show.sens = F, 
         show.AUC = T, 
         facet = F )
