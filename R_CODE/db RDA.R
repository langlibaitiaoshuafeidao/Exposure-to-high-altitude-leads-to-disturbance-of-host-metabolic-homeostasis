rm(list = ls())
library(vegan)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(rdacca.hp)

otu = read.table('OTU.txt', head = T, row.names=1)           
env = read.table('env.txt', header = T, row.names=1)             
group = read.table('group.txt', header = T, row.names = 1)   

otu = t(otu) 

mod = capscale(otu ~., env, distance = 'bray')

score = scores(mod)
score$sites
mod$CCA$biplot
score$species

CAP1 = score$sites[,1]
CAP2 = score$sites[,2]

seg = as.data.frame(mod$CCA$biplot)


plotdata = data.frame(rownames(score$sites), CAP1, CAP2, group$Treatment)
colnames(plotdata) = c('sample','CAP1','CAP2','Treatment')                       
write.csv(plotdata,'dbRDA.csv')                                                  


CAP1_exp = round(mod$CCA$eig[1]/sum(mod$CCA$eig)*100,2)
CAP2_exp = round(mod$CCA$eig[2]/sum(mod$CCA$eig)*100,2)

p1 = ggplot(plotdata, aes(CAP1, CAP2)) +
  geom_point(aes(fill = Treatment, color = Treatment),size = 5) + 
  scale_fill_manual(values = c('#00C0A3','#B0A8B9','#4B4453','#00896F', '#845EC2'))+
  scale_color_manual(values = c('#00C0A3','#B0A8B9','#4B4453','#00896F', '#845EC2'))+
  xlab(paste('CAP1 ( ',CAP1_exp,'%',' )', sep = '')) + 
  ylab(paste('CAP2 ( ',CAP2_exp,'%',' )', sep = '')) +
  geom_segment(data = seg, aes(x = 0, y = 0, xend = seg[,1], yend = seg[,2]),
               colour = 'red', size = 0.8,
               arrow = arrow(angle = 30, length = unit(0.4, 'cm'))) +
  geom_text_repel(data = seg, segment.colour = 'black',
                  aes(x = seg[,1], y = seg[,2], 
                      label = rownames(seg)),size = 5) +
  geom_vline(aes(xintercept = 0), linetype = 'dotted') +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  theme_bw()+
  theme(text = element_text(family = 'sans', size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')+ 
  theme(axis.text = element_text(colour = 'black',size = 20))
p1
bray = vegdist(otu, method = 'bray')
cap.hp = rdacca.hp(bray, env, method = 'dbRDA', type = 'R2', scale = FALSE)
cap.hp$Total_explained_variation
cap.hp$Hier.part
write.csv(cap.hp$Hier.part, 'env_effect.csv')
