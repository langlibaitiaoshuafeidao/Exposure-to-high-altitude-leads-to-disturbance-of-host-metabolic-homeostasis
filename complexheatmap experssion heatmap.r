
library(ComplexHeatmap)

mat <- read.delim('gene_express2.txt', row.names = 1, check.names = FALSE)
for (i in 1:nrow(mat)) mat[i, ] <- scale(log(unlist(mat[i, ]) + 1, 2))
mat <- as.matrix(mat)

sample_group <- as.matrix(read.delim('sample_group2.txt', row.names = 1))
gene_anno <- as.matrix(read.delim('gene_anno2V2.txt', row.names = 1))

Heatmap(
  mat, name = 'gene_express2', 
  col = colorRampPalette(c('#240EFF', '#B491F9', '#ECEAEF', '#FFA48C', '#FF1A09'))(100), 
  cluster_rows = FALSE, cluster_columns = TRUE,  
  row_names_gp = gpar(fontsize = 8), column_names_gp = gpar(fontsize = 10),  
  
  right_annotation = HeatmapAnnotation(
    Class = gene_anno, which = 'row', show_annotation_name = FALSE, 
    col = list(Class= c('Amino acids' = '#FFAD30', 'Carbohydrates' = '#634FB8', 'Amino acids_peptides_and analogues' = '#68AD30', 'Steroids and derivatives' = '#76daff', 'Organic acids
' = '#936C00', 'Glycerophospholipids[GP]' = '#D65DB1', 'Benzene and derivatives' = '#ECEAEF','Glycerolipids[GL]' = '#007cc0','Sphingolipids[SP]' = '#2dde98','Organic acids' = '#ff6c5f','NA' = '#4B4453'))
  ),
  
 
  top_annotation = HeatmapAnnotation(
    Group = sample_group, which = 'column', show_annotation_name = FALSE, 
    col = list(Group = c('LS' = '#00C0A3', 'CD' = '#845EC2' ))
  )
)

Heatmap(
  mat, name = 'expression', 
  col = colorRampPalette(c('#240EFF', '#B491F9', '#ECEAEF', '#FFA48C', '#FF1A09'))(100),  
  cluster_rows = TRUE, cluster_columns = TRUE,  
  row_names_gp = gpar(fontsize = 8), column_names_gp = gpar(fontsize = 10),  
  row_split = gene_anno, column_split = sample_group,  
  row_title_gp = gpar(fontsize = 10), column_title_gp = gpar(fontsize = 10), 
  
 
  right_annotation = HeatmapAnnotation(
    Class = gene_anno, which = 'row', show_annotation_name = FALSE, 
    col = list(Class= c('Amino acids' = '#FFAD30', 'Carbohydrates' = '#634FB8', 'Amino acids_peptides_and analogues' = '#68AD30', 'Steroids and derivatives' = '#76daff', 'Organic acids
' = '#936C00', 'Glycerophospholipids[GP]' = '#D65DB1', 'Benzene and derivatives' = '#ECEAEF','Glycerolipids[GL]' = '#007cc0','Sphingolipids[SP]' = '#2dde98','Organic acids' = '#ff6c5f','NA' = '#4B4453'))
  ),
  
 
  top_annotation = HeatmapAnnotation(
    Group = sample_group, which = 'column', show_annotation_name = FALSE, 
    col = list(Group = c('LS' = '#00C0A3', 'CD' = '#845EC2' ))
  )
)

