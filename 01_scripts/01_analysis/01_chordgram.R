require(circlize)
require(ggpubr)
require(scales)

load("00_data/ceps_full_mapped_all_versions.Rdata")


graph <- as.matrix(ceps_full_mapped_subset_1977[colnames(ceps_full_mapped_subset_1977)[grepl("[0-9]", colnames(ceps_full_mapped_subset_1977))]])
graph <- t(graph) %*% graph
diag(graph) <- NA
graph <- graph/rowSums(graph,na.rm = T)

adjacency_list <- melt(graph) 
adjacency_list<- na.omit(adjacency_list)
colnames(adjacency_list) <-c("from","to","value")


adjacency_list$from <-str_to_title(tolower(adjacency_list$from)) 
adjacency_list$to <-str_to_title(tolower(adjacency_list$to)) 

colors <- c(
  "#011627",
  "#2EC4B6",
  "#FF3366",
  "#ED9B40",
  "#613DC1",
  "#20A4F3",
  "#254441",
  "#F2F7F2",
  "#DB504A"
  
)

fun <- (colorRampPalette(colors))

pdf("02_figures/figure4.pdf",height=15, width=15)
circos.clear()
set.seed(6)
circos.par(gap.after = 10)
circlize::chordDiagram((adjacency_list),
                       directional = 1,
                       transparency = .3,
                       grid.col = fun(16),
                       direction.type = c("diffHeight", "arrows"),
                       link.arr.type = "big.arrow")
dev.off()


data.frame(graph) %>% 
  round(2) %>% 
  rownames_to_column() %>% 
  flextable::flextable()


df <- data.frame(graph)
