require(geomtextpath)
require(ggthemes)

#### Commission annotation ####
Jenkins <- as.Date("01-06-1977", "%m-%d-%Y")
Thorn <- as.Date("01-06-1981", "%m-%d-%Y")
Delors_1 <- as.Date("01-06-1985", "%m-%d-%Y")
Delors_2 <- as.Date("01-06-1990", "%m-%d-%Y")
Santer <- as.Date("01-23-1995", "%m-%d-%Y")
Marin <- as.Date("03-15-1999", "%m-%d-%Y")
Prodi <- as.Date("09-16-1999", "%m-%d-%Y")
Barroso_1 <- as.Date("11-22-2004", "%m-%d-%Y")
Barroso_2 <- as.Date("02-10-2010", "%m-%d-%Y")
Junker <- as.Date("11-01-2014", "%m-%d-%Y")
von_der_Leyen <- as.Date("12-01-2019", "%m-%d-%Y")

annotations_commission <-
  list(
    geom_vline(
      xintercept = Delors_1,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = Delors_2,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = Thorn,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = Jenkins,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = Junker,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = Marin,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = Barroso_1,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = Barroso_2,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = Prodi,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = von_der_Leyen,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = Santer,
      linetype = "dotted",
      color = "grey60"
    )
  )

#### Treaty annotation ####



# create variables for each treaty date
treaty_of_paris <- as.Date("1951-04-18")
treaty_of_rome <- as.Date("1957-03-25")
single_european_act <- as.Date("1986-02-17")
maastricht_treaty <- as.Date("1992-02-07")
amsterdam_treaty <- as.Date("1997-10-02")
nice_treaty <- as.Date("2001-02-26")
lisbon_treaty <- as.Date("2007-12-13")


annotations_treaty <-
  list(
    geom_vline(
      xintercept = treaty_of_paris,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = treaty_of_rome,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = single_european_act,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = maastricht_treaty,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = amsterdam_treaty,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = nice_treaty,
      linetype = "dotted",
      color = "grey60"
    ),
    geom_vline(
      xintercept = lisbon_treaty,
      linetype = "dotted",
      color = "grey60"
    )
  )


#### Plot function using GAM ####

plot_field <-
  function(data,
           var_choice,
           ymin = 1,
           ymax = 3,
           label = "term") {
    
  if (!missing("var_choice"))
    {
data <- data %>%  
  filter(variable == var_choice)
title <- var_choice
  }
  else {
    title <- ""
  }

data$num_date <- as.numeric(data$Date_document)  
gam_fit <-
  mgcv::gam(
    formula = integration ~ s(num_date, bs = "cs"),
    method = "REML",
    data = data)

pred <- predict(gam_fit, data, se=T)

data$pred <- pred$fit
data$se1 <- pred$fit-1.96*pred$se.fit
data$se2 <- pred$fit+1.96*pred$se.fit


data$term[data$term == "Marin"] <- " "


if (label == "term") {
  plot_fun <- data %>% 
    ggplot(aes(x = Date_document, y = pred)) +
    annotations_commission+
    geom_ribbon(aes(ymin = se1, ymax=se2), fill = "grey90") +
    geom_textline(aes(label = get(label), 
                      group = get(label)),
                  linewidth=.4,
                  size = 4,
                  fontface=1,
                  vjust=-0.5) +
    ylab("Degree of legal integration")+
    xlab("Date")+
    theme_par()+
    ggtitle(label= title)+
    scale_color_grey()+
    theme(legend.position = "none")+
    theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
    coord_cartesian(ylim = c(ymin,ymax))
  
}

if (label == "treaty") {
  plot_fun <- data %>% 
    ggplot(aes(x = Date_document, y = pred)) +
    annotations_treaty+
    geom_ribbon(aes(ymin = se1, ymax=se2), fill = "grey90") +
    geom_textline(aes(label = get(label), 
                      group = get(label)),
                  linewidth=.4,
                  size = 4,
                  fontface=1,
                  vjust=-0.5) +
    ylab("Degree of legal integration")+
    xlab("Date")+
    theme_par()+
    ggtitle(label= title)+
    scale_color_grey()+
    theme(legend.position = "none")+
    theme(plot.margin=unit(c(1,1,-0.5,1), "cm"))+
    coord_cartesian(ylim = c(ymin,ymax))
  
}

return(plot_fun)

}
