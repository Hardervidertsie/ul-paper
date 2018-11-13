source("00_lockAndload.R")
source("02_run_format_functions.R")
source("03_exploratory_plot.R")

raw_data <- load_data(rootdir = "J:/Workgroups/FWN/LACDR/TOX/data steven wink/Unilever2/imaging_data_files",
                      debug = FALSE)

save(raw_data, file = "tmp/raw_data.Rdata")
load("tmp/raw_data.Rdata")

combined_data <- run_formats(raw_data)
unique(combined_data$sheet_name)

write.table(combined_data, file = "output/combined_data.txt", sep ="\t", col.names = NA)


# plot one variable or use a list with 1 variable per entry together with lapply
combined_data_list <- split(combined_data, f = combined_data$variable)

lapply(combined_data_list, function(x) print(unique(x$variable)))

exploratory_plot(combined_data_list[[1]])

lapply( combined_data_list, function(x) { exploratory_plot(inputdata = x) })

