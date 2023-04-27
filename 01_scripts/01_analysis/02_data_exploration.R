require(tidyverse)
require(igraph)
require(ggthemes)
require(stringr)
require(gridExtra)

load("00_data/ceps_full_mapped.Rdata")

# Check for availability of identifiers (NA = No eurovoc assigned)
ceps_full_mapped$Available <- !is.na(ceps_full_mapped$`04 POLITICS`)
ceps_full_mapped$Available <- ifelse(ceps_full_mapped$Available == T, "Yes", "No")

# Unify type
ceps_full_mapped$Act_type[grepl("Decision",ceps_full_mapped$Act_type)] <- "Decision"
ceps_full_mapped$Act_type[grepl("Directive",ceps_full_mapped$Act_type)] <- "Directive"
ceps_full_mapped$Act_type[grepl("Regulation",ceps_full_mapped$Act_type)] <- "Regulation"

# Add year
ceps_full_mapped$year <- lubridate::year(as.Date(ceps_full_mapped$Date_document, "%Y-%m-%d"))

# Remove domains not of interest
ceps_full_mapped$`10 EUROPEAN UNION` <- NULL
ceps_full_mapped$`72 GEOGRAPHY` <- NULL
ceps_full_mapped$`76 INTERNATIONAL ORGANISATIONS` <- NULL
ceps_full_mapped$`04 POLITICS` <- NULL
ceps_full_mapped$`12 LAW` <-  NULL

# Format date
ceps_full_mapped$Date_document <- as.Date(ceps_full_mapped$Date_document, "%Y-%m-%d")

# Create vector of commission terms and dates
commissions <-
  c(
    "Jenkins",
    "Thorn",
    "Delors 1",
    "Delors 2",
    "Santer",
    "Marin",
    "Prodi",
    "Barroso 1",
    "Barroso 2",
    "Junker",
    "von der Leyen"
  )
commission_dates <-
  c(
    as.Date("01-06-1977", "%m-%d-%Y"),
    as.Date("01-06-1981", "%m-%d-%Y"),
    as.Date("01-06-1985", "%m-%d-%Y"),
    as.Date("01-06-1990", "%m-%d-%Y"),
    as.Date("01-23-1995", "%m-%d-%Y"),
    as.Date("03-15-1999", "%m-%d-%Y"),
    as.Date("09-16-1999", "%m-%d-%Y"),
    as.Date("11-22-2004", "%m-%d-%Y"),
    as.Date("02-10-2010", "%m-%d-%Y"),
    as.Date("11-01-2014", "%m-%d-%Y"),
    as.Date("12-01-2019", "%m-%d-%Y")
  )

# Use loop to set term and term_order variables for each commission term
for (i in seq_along(commissions)) {
  ceps_full_mapped$term[ceps_full_mapped$Date_document >= commission_dates[i]] <- commissions[i]
  ceps_full_mapped$term_order[ceps_full_mapped$Date_document >= commission_dates[i]] <- i
}

# create variables for each treaty date
treaties <- c(
  "Paris",
  "Rom",
  "SEA",
  "Amsterdam",
  "Maastricht",
  "Nice",
  "Lisbon"
)

treaties_dates <- c(
  as.Date("1951-04-18"),
  as.Date("1957-03-25"),
  as.Date("1986-02-17"),
  as.Date("1992-02-07"),
  as.Date("1997-10-02"),
  as.Date("2001-02-26"),
  as.Date("2007-12-13")
)

for (i in seq_along(treaties)) {
  ceps_full_mapped$treaty[ceps_full_mapped$Date_document >= treaties_dates[i]] <- treaties[i]
  ceps_full_mapped$treaty_order[ceps_full_mapped$Date_document >= treaties_dates[i]] <- i
}

# Create plot for data availabilty
figure1_data <- ceps_full_mapped %>%
  filter(!is.na(Act_type)) %>% 
  group_by(term, term_order,Available, Act_type) %>%
  count() %>% 
  arrange(term_order) %>% 
  na.omit()

figure1_plot <- ggplot(figure1_data,aes(x = term, y = n, fill = Available)) +
  geom_col(alpha=1, color="black") +
  facet_wrap( ~ Act_type, scales = "free_y")+
  scale_fill_manual(values = c(
    "white",
    "grey30"))+
  theme_light()+
  scale_x_discrete(limits=figure1_data$term)+
  xlab("Commission Term") +
  ylab("Count")+
  theme_base()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.background = element_rect(color ="white"))

figure1_plot

ggsave(figure1_plot,
       file="02_figures/figure1.pdf",
       height = 5,
       width = 15)

#### Integration over time ####

# Filter for available documents after 1977
ceps_full_mapped_subset_1977 <- ceps_full_mapped %>%
  select(Act_type,
         CELEX,
         Date_document, 
         term,
         treaty,
         colnames(ceps_full_mapped)[grepl("[0-9]", colnames(ceps_full_mapped))]) %>%
  filter(Date_document > as.Date("01-01-1977", "%d-%m-%Y")) %>% 
  na.omit()

# Ensure numeric
eurovoc_domains <- names(ceps_full_mapped_subset_1977)[grepl("[0-9]", colnames(ceps_full_mapped_subset_1977))]
ceps_full_mapped_subset_1977[eurovoc_domains] <-lapply(ceps_full_mapped_subset_1977[eurovoc_domains], as.numeric)

# rowsums as integration score
ceps_full_mapped_subset_1977$integration <- rowSums(ceps_full_mapped_subset_1977[eurovoc_domains])

# add id
ceps_full_mapped_subset_1977<- rowid_to_column(ceps_full_mapped_subset_1977, var = "id")


### Convert to long ####
ceps_full_mapped_subset_1977_long <- 
  ceps_full_mapped_subset_1977 %>%
  select(Date_document, Act_type, integration, term, id, all_of(eurovoc_domains)) %>% 
  pivot_longer(cols=all_of(eurovoc_domains)) %>% 
  filter(value == 1)


ceps_full_mapped_subset_1977_long$variable <- str_to_title(ceps_full_mapped_subset_1977_long$name)

ceps_full_mapped_subset_1977_long %>% 
mutate(Domain = variable) %>%
  group_by(Domain) %>%
  summarize(`Number of documents` = n()) %>%
  arrange(desc(`Number of documents`)) %>%
  flextable::flextable() %>% 
  flextable::save_as_docx(path="03_tables/table_1.docx")


# filter 0 scores from full data (documents with only irrelevant domain binaries)
ceps_full_mapped_subset_1977_no0 <- ceps_full_mapped_subset_1977 %>% 
  filter(integration > 0)


### Summary statistics ####
# Summary statistics integration
summary_statistics_integration <- ceps_full_mapped_subset_1977_long %>%
  group_by(variable) %>%
  summarize(
     N = n(),
    `Min.` = min(integration),
    `0.25 Quantile` = quantile(integration,.25),
    `Median` = median(integration),
    `Mean` = round(mean(integration),2),
    `0.75 Quantile`= quantile(integration,.75),
    `Max.` = max(integration)
  ) 

require(flextable)

bind_rows(summary_statistics_integration) %>% 
  flextable() %>% 
  width(j=1, width=2) %>% 
  width(j=2:8, width=.6) %>% 
  align_nottext_col(align = "center") %>% 
  fontsize(size=8) %>% 
  save_as_docx(path="03_tables/summary_integration.docx")


### Create integration figures ####
source("01_scripts/01_analysis/00_new_Plot.R")

# Create figure 2, see new_plot for the plot_field function
plot_term <- plot_field(ceps_full_mapped_subset_1977_no0,
                          label = "term",
                          ymin=1,
                          ymax=3)

ggsave("02_figures/figure2.pdf",plot_term, width=12,
       height=7)


# Replicate figure 2 with treaty annotation
plot_treaty <- plot_field(ceps_full_mapped_subset_1977_no0,
                          label = "treaty",
                          ymin=1,
                          ymax=3)
ggsave("02_figures/figure2_treaty.pdf",plot_treaty, width=12,
       height=7)


# Create figure 3 by domain integration
fields <- sort(unique(ceps_full_mapped_subset_1977_long$variable))
list_plot <- list()
for(i in 1:length(fields)) {
  
 p <-  plot_field(ceps_full_mapped_subset_1977_long, 
                  fields[[i]],
                  ymin=1.5,
                  ymax=4,
                  label = "term")

 list_plot[[i]] <- p
}

figure_3 <- arrangeGrob(grobs=list_plot, ncol=3)

ggsave("02_figures/figure3.pdf",figure_3, width=25,
       height=30)



save(ceps_full_mapped,
     ceps_full_mapped_subset_1977,
     ceps_full_mapped_subset_1977_long, 
     file="00_data/ceps_full_mapped_all_versions.Rdata")
rm(list = ls())
