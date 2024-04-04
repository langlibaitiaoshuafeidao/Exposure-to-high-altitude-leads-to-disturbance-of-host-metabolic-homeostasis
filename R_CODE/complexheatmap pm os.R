library(ComplexHeatmap)
mat <- read.delim('pm os.txt', row.names = 1, check.names = FALSE)
for (i in 1:nrow(mat)) mat[i, ] <- scale(log(unlist(mat[i, ]) + 1, 2))
mat <- as.matrix(mat)
afsplit=data.frame(ID=rownames(mat),
                   Type=c('Glutathione metabolism',
                          'Glutathione metabolism',
                          'Apoptosis',
                          'Citrate cycle (TCA cycle)',
                          'Citrate cycle (TCA cycle)',
                          'Glycolysis / Gluconeogenesis',
                          'Glycolysis / Gluconeogenesis',
                          'pyruvate metabolism',
                          'pyruvate metabolism',
                          'pyruvate metabolism',
                          'Oxidative phosphorylation',
                          'Oxidative phosphorylation'
                   ),
                   ann=as.vector(mat[,1])
                   )
group=data.frame( ID=rownames(mat),
                 Type=c('PM',
                        'PM',
                        'PM',
                        'PM',
                        'PM',
                        'PM',
                        'PM',
                        'PM',
                        'PM',
                        'PM',
                        'PM',
                        'PM'
                 ))
Heatmap(mat, 
        name = "log(mean expression+1)",  
        cluster_rows = F, 
        cluster_columns = F, 
        column_title = "Amino acid metabolism", 
        #row_title = "I am a row title",  
        row_labels = rownames(mat),   
        show_row_names = T, show_column_names = T,  
        column_names_gp = gpar(fontsize = 10,    
                               fontface = "bold", 
                               fill = c('#00C0A3','#B0A8B9','#4B4453','#00896F','#240EFF', '#B491F9', '#ECEAEF', '#FFA48C', '#FF1A09'), 
                               col = "black",
                               border = "grey"
                               ),
        column_title_side = "bottom",row_title_side="left", 
        column_title_rot = 0,row_title_rot = 90,
        column_title_gp = gpar(fontsize = 10, 
                               fontface = "bold", 
                               fill = "green3", 
                               col = "white", 
                               border = "black"
                               ), 
        row_title_gp = gpar(fontsize = 8, 
                               fontface = "bold" , 
                               fill = c("yellow3","#D65DB1","#7b3294","#008837",'#240EFF', '#B491F9', '#ECEAEF', '#FFA48C', '#FF1A09'),  
                               col = "white", 
                               border = "black"
                             )  ,
        col = circlize::colorRamp2(c(-2, 0, 2), c("#008837", "white", "#7b3294")), 
        na_col = "black",
        border_gp = gpar(col = "grey", lwd = 2), 
        rect_gp = gpar(col = "white", lwd = 3), 
        row_split = group$Type,       
        column_split = c("0",	"1",	"2",	"3",	"4"),
        row_gap = unit(3, "mm"), column_gap = unit(3, "mm"), 
        right_annotation = rowAnnotation(typer1 = afsplit$Type,                        
                                         typer2 = anno_points(afsplit$ann),  
                                         typer3 = anno_barplot(afsplit$ann)
                                         ), 
        top_annotation = HeatmapAnnotation(     
                                           typet1 = anno_boxplot(mat, height = unit(1, "cm"), gp = gpar(fill = 1:4))), 
        bottom_annotation = HeatmapAnnotation(typeb1 = anno_block(gp = gpar(fill = 1:5),
                                                                  labels = c("LS",	"HADA_1w",	"HADA_1m",	"HADA_4m",	"CD"), 
                                                                  labels_gp = gpar(col = list(column_split = c('LS' = '#00C0A3', 'HADA_1w' = '#B0A8B9', 'HADA_1m' = '#4B4453', 'HADA_4m' = '#00896F', 'CD' = '#845EC2'), fontsize = 5))
        )
        
)) 
