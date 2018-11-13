
exploratory_plot <- function(inputdata){
  
  require(ggplot2)
  
  plotname <- unique(inputdata$variable)
  
  plotname <- gsub("/", ".",plotname)
  
  pdf(file = paste0("output/", plotname, ".pdf"), height = 60, width = 30)
  
  p <- ggplot(data = inputdata, aes(x = as.numeric(timeID), y = as.numeric(value), color = as.numeric(dose_uM) )) +
    geom_point(aes(shape = replID)) +
    facet_grid(treatment ~ cell_line)
  
  print(p)
    
  dev.off()
  
}


