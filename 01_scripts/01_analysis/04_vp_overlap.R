# load necessary data
load("00_data/ceps_full_mapped_all_versions.Rdata")


ceps_full_mapped_subset_1977_long_project <-ceps_full_mapped_subset_1977_long


# create project team groupings
ceps_full_mapped_subset_1977_long_project$Foreign_Affairs_and_Security_Policy <-
  ceps_full_mapped_subset_1977_long_project$variable %in%
  c("08 International Relations",
    "20 Trade",
    "52 Environment",
    "66 Energy",
    "16 Economics")

ceps_full_mapped_subset_1977_long_project$Digital_Single_Market <-
  ceps_full_mapped_subset_1977_long_project$variable %in%
  c(
    "16 Economics",
    "20 Trade",
    "24 Finance",
    "28 Social Questions",
    "32 Education and Communications",
    "36 Science",
    "40 Business and Competition",
    "48 Transport",
    "52 Environment",
    "56 Agriculture, Forestry and Fisheries",
    "64 Production, Technology and Research",
    "66 Energy",
    "68 Industry"
  )

ceps_full_mapped_subset_1977_long_project$Energy_Union <-
  ceps_full_mapped_subset_1977_long_project$variable %in% c(
    "16 Economics",
    "52 Environment",
    "56 Agriculture,Forestry And Fisheries",
    "60 Agri-Foodstuffs",
    "64 Production, Technology and Research",
    "66 Energy",
    "68 Industry"
  )

ceps_full_mapped_subset_1977_long_project$Euro_and_Social_Dialogue <-
  ceps_full_mapped_subset_1977_long_project$variable %in% c("16 Economics",
                                                       "24 Finance",
                                                       "28 Social Questions",
                                                       "40 Business and Competition")

ceps_full_mapped_subset_1977_long_project$Growth_Investment_and_Competetiveness <-
  ceps_full_mapped_subset_1977_long_project$variable %in% c(
    "16 Economics",
    "20 Trade",
    "24 Finance",
    "28 Social Questions",
    "32 Education And Communications",
    "36 Science",
    "40 Business And Competition",
    "48 Transport",
    "52 Environment",
    "56 Agriculture, Forestry and Fisheries",
    "64 Production, Technology and Research",
    "66 Energy",
    "68 Industry"
  )



# Convert to long?
ceps_full_mapped_subset_1977_long_project_2 <-
  ceps_full_mapped_subset_1977_long_project %>%
  select(
    Date_document,
    integration,
    id,
    term,
    Foreign_Affairs_and_Security_Policy,
    Digital_Single_Market,
    Energy_Union,
    Euro_and_Social_Dialogue,
    Growth_Investment_and_Competetiveness
  ) %>%
  pivot_longer(
    cols = c(
      Foreign_Affairs_and_Security_Policy,
      Digital_Single_Market,
      Energy_Union,
      Euro_and_Social_Dialogue,
      Growth_Investment_and_Competetiveness
    )
  ) %>%
  filter(value == T)

ceps_full_mapped_subset_1977_long_project_2$variable <-
  ceps_full_mapped_subset_1977_long_project_2$name

ceps_full_mapped_subset_1977_long_project_2$var_id <-
  paste(
    ceps_full_mapped_subset_1977_long_project_2$id,
    ceps_full_mapped_subset_1977_long_project_2$variable
  )

ceps_full_mapped_subset_1977_long_project_2 <-
  ceps_full_mapped_subset_1977_long_project_2[!duplicated(ceps_full_mapped_subset_1977_long_project_2$var_id), ]

ceps_full_mapped_subset_1977_long_project_2$variable <-
  gsub("_", " ", ceps_full_mapped_subset_1977_long_project_2$variable)

#####
source("01_scripts/01_analysis/00_new_Plot.R")

# Filter only post Barrosso 1
ceps_full_mapped_subset_1977_long_project_2 <-
  ceps_full_mapped_subset_1977_long_project_2 %>%
  filter(Date_document > as.Date("11-22-2004", "%m-%d-%Y"))

fields_cat <- unique(ceps_full_mapped_subset_1977_long_project_2$variable)
list_plot_cat <- list()

for (i in 1:length(fields_cat)) {
  p <-
    plot_field(
      ceps_full_mapped_subset_1977_long_project_2,
      fields_cat[[i]],
      ymin = 1.5,
      ymax = 3.5
    )
  
  list_plot_cat[[i]] <- p
}
split_cat <- arrangeGrob(grobs = list_plot_cat, ncol = 3)
split_cat


ggsave("02_figures/figure4.pdf",
       split_cat,
       width = 25,
       height = 15)
