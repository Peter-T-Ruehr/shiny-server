library(dplyr)
library(tidyr)
library(ggplot2)
library(gsheet)
library(shiny)
library(viridisLite)
library(ggrepel)
library(googlesheets4)
gs4_deauth()
library(plotly)
library(rvest)

sheet_id <- "https://docs.google.com/spreadsheets/d/1tL-9rg_K9rf5hpzj63MewlQLms1qV91Nt3RwMsFamaU/"
PS.data <- read_sheet(sheet_id, sheet = 1)
# remove unnamed columns
PS.data <- PS.data %>% 
  select(-contains('...'))
# get PsyChild data
# PS.data <- gsheet2tbl(url = 'https://docs.google.com/spreadsheets/d/1tL-9rg_K9rf5hpzj63MewlQLms1qV91Nt3RwMsFamaU/edit?usp=sharing')

# get iso codes
iso_codes <- read_sheet(sheet_id, sheet = "iso_codes")

# remove NULL rows
nulls <- NULL
for(i in 1:nrow(PS.data)){
  curr_row <- unlist(PS.data$Date[i])
  if(is.null(curr_row)){
    nulls <- c(nulls, i)
  }
}
PS.data <- PS.data[-nulls, ]

# change Date from list to readable character
PS.data$Date <- unlist(PS.data$Date)

# change current to 2023
PS.data$Date[PS.data$Date == "Current"] <- 2023

# change Date to numeric
PS.data$Date <- as.numeric(PS.data$Date)

# rename some columns
# PS.data <- rename(PS.data, Class = 'Substance class')
# PS.data <- rename(PS.data, Compound = 'Compound(s)') # was before: 'Psychedelic Compound(s) in children/adolescents')
# PS.data <- rename(PS.data, Indication = 'Indication (for children/adolescents)/Field of Application')
# PS.data <- rename(PS.data, Indication_ICD11 = 'Indication (Current terminology according to ICD-11)')
# PS.data <- rename(PS.data, Psychotherapy = 'Adjacent psychotherapy?')
# PS.data <- rename(PS.data, Psychiatric_indication = 'Psychiatric indication?')

# remove Date == NA columns
PS.data <- PS.data %>% 
  filter(!is.na(Date))

# create old new names for Compounds list
compound_translation <- tibble(old = c("2-AG \\(2-Arachidonoylglycerol\\)",
                                       "AA Arachidonic acid",
                                       "AEA \\(Anandamide\\)",
                                       "LAE-32 \\(D-Lysergic acid diethylamide\\)",
                                       "LSD \\(Lysergic acid diethylamide\\)",
                                       "OEA \\(Oleoylethanolamide\\)",
                                       "PCP \\(Phencyclidine\\)",
                                       "mCPP \\(meta-Chlorophenylpiperazine\\)",
                                       "PEA \\(Palmitoylethanolamide\\)",
                                       "THC \\(Delta-8-THC\\)",
                                       "THC \\(Delta-9 THC\\)",
                                       "THC \\(THC-homologs, Numbers 122 and 125A\\)",
                                       "αET \\(alpha-Ethyltryptamine\\)"),
                               new = c("AG",
                                       "AA",
                                       "AEA",
                                       "LAE",
                                       "LSD",
                                       "OEA",
                                       "PCP",
                                       "mCPP",
                                       "PEA",
                                       "THC",
                                       "THC",
                                       "THC",
                                       "αET"))

# arrange PS_data
PS.data <- PS.data %>% 
  arrange(Date, Author, `Substance class`)

# get unique classes
classes <- sort(unique(unlist(strsplit(PS.data$`Substance class`, split = "; "))))

# Class and compound: arrange by date and add cumulative columns
PS.data <- PS.data %>%
  arrange(Date) %>%
  mutate(one = 1) %>% # ,
  # cumul_years_all = cumsum(one)) %>%
  group_by(`Substance class`) %>%
  mutate(cumul_years_class = cumsum(one)) %>%
  ungroup()

# save for later processing
PS.data.compounds <- PS.data

# change input compounds and data compounds to workable strings
# unique(PS.data.compounds$`Compound(s)`)
PS.data.compounds$Compound_new_name <- PS.data.compounds$`Compound(s)`
for(i in 1:nrow(compound_translation)){
  PS.data.compounds$Compound_new_name <- gsub(compound_translation$old[i], compound_translation$new[i], PS.data.compounds$Compound_new_name)
}
# unique(PS.data.compounds$Compound_new_name)

# get unique compounds
compounds <- sort(unique(unlist(strsplit(PS.data.compounds$Compound_new_name, split = "; "))))

# get countries
# PS.data$Location
# i=1
PS.data$Country <- NA
for(i in 1:nrow(PS.data)){
  curr_Location <- unlist(strsplit(PS.data$Location[i], split = "; "))
  PS.data$Country[i] <- curr_Location[length(curr_Location)]
}
PS.data$Country <- gsub("USA", "United States of America", PS.data$Country)
PS.data$Country <- gsub("^United States$", "United States of America", PS.data$Country)
PS.data$Country <- gsub("Czechoslovakia", "Czech Republic", PS.data$Country)
PS.data$Country <- gsub("England", "United Kingdom", PS.data$Country)
PS.data$Country <- gsub("Scotland", "United Kingdom", PS.data$Country)
PS.data$Country <- gsub("Iran", "Iran, Islamic Republic of", PS.data$Country)
PS.data$Country <- gsub("Russia", "Russian Federation", PS.data$Country)

# save for printing
PS.data.print_Class <- PS.data
PS.data.print_Compound <- PS.data

# separate multiple compounds from each other
# i=1
# i=25
for(i in 1:nrow(PS.data.compounds)){
  # if compound cell contains ";"
  if(grepl(";", PS.data.compounds$Compound_new_name[i])){
    curr_compounds <- unlist(strsplit(PS.data.compounds$Compound_new_name[i], "; "))
    for(k in curr_compounds){
      PS.data.compounds <- PS.data.compounds %>% 
        add_row(PS.data.compounds[i, ])
      # replace original compounds with new compounds
      PS.data.compounds$Compound_new_name[nrow(PS.data.compounds)] <- k
    }
    # remove original row
    PS.data.compounds <- PS.data.compounds[-i, ]
  }
}

# cumulative years of compounds
PS.data.compounds <- PS.data.compounds %>%
  arrange(Date) %>%
  group_by(Compound_new_name) %>%
  mutate(cumul_years_compound = cumsum(one)) %>%
  ungroup() %>%
  select((-one))


# class -------------------------------------------------------------------
# add all missing years
for(i in classes){
  # i <- classes.selected[1]
  missing_years <- setdiff(min(PS.data$Date):max(PS.data$Date), PS.data$Date[PS.data$`Substance class` == i])
  PS.data <- PS.data %>%
    add_row(`Substance class` = i,
            Date = missing_years) %>% 
    arrange(`Substance class`, Date)
}

# add line at first year of current selection table
first_year <- min(PS.data$Date) # input$range[1]
for(i in classes){
  # i <- classes.selected[1]
  curr_author <- PS.data %>%
    filter(Date == first_year, `Substance class` == i) %>%
    pull(Author)
  if(is.na(curr_author)){
    PS.data$cumul_years_class[PS.data$`Substance class` == i & PS.data$Date == first_year] <- 0
  }
}

# fill empty years with previous cumul_years_class value
PS.data <- PS.data %>% 
  group_by(`Substance class`) %>% 
  fill(cumul_years_class)

# define constant viridis colours and add to PS.data
cols_class <- tibble(`Substance class` = unique(PS.data$`Substance class`), col_class = viridis(n=length(unique(PS.data$`Substance class`))))
PS.data <- PS.data %>% 
  left_join(cols_class, by = "Substance class")
# plot(1:nrow(cols), col = unique(PS.data$col), pch = 16, cex = 5)
# plot(1:nrow(cols), col = unique(PS.data.plot.Class$col), pch = 16, cex = 5)

# compound -------------------------------------------------------------------
# add all missing years
for(i in compounds){
  # i <- classes.selected[1]
  missing_years <- setdiff(min(PS.data.compounds$Date):max(PS.data.compounds$Date), PS.data.compounds$Date[PS.data.compounds$Compound_new_name == i])
  PS.data.compounds <- PS.data.compounds %>%
    add_row(Compound_new_name = i,
            Date = missing_years)
  # PS.data <- PS.data %>%
  #   arrange(`Substance class`, Date)
}

# add line at first year of current selection table
first_year <- min(PS.data.compounds$Date) # input$range[1]
# i = "2-AG"
for(i in compounds){
  # i <- classes.selected[1]
  curr_author <- PS.data.compounds %>%
    filter(Date == first_year, Compound_new_name == i) %>%
    pull(Author)
  if(is.na(curr_author)){
    PS.data.compounds$cumul_years_compound[PS.data.compounds$Compound_new_name == i & PS.data.compounds$Date == first_year] <- 0
  }
}

# arrange before filling
PS.data.compounds <- PS.data.compounds %>% 
  arrange(Compound_new_name, Date) 


# fill empty years with previous cumul_years_compound value
PS.data.compounds <- PS.data.compounds %>% 
  group_by(Compound_new_name) %>% 
  fill(cumul_years_compound)

# define constant viridis colours and add to PS.data
cols_compound <- tibble(Compound_new_name = unique(PS.data.compounds$Compound_new_name), 
                        col_compound = viridis(n=length(unique(PS.data.compounds$Compound_new_name))))
PS.data.compounds <- PS.data.compounds %>% 
  left_join(cols_compound, by = "Compound_new_name")
# plot(1:nrow(cols_compound), col = unique(PS.data.compounds$col_compound), pch = 16, cex = 5)


# Map ---------------------------------------------------------------------
# url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
# iso_codes <- url %>%
#   read_html() %>%
#   html_table() %>% 
#   bind_rows() %>% 
#   subset(nchar(as.character(X2)) > 1) %>% 
#   select(-X1)
# names(iso_codes) <- c("Country", "ISO2", "ISO3", "UN")
# head(iso_codes)


PS.data.map <- PS.data
PS.data.map['ISO3'] <- iso_codes$ISO3[match(PS.data.map$Country, iso_codes$Country)]
PS.data.map <- PS.data.map %>% 
  filter(Country != "Unknown")

PS.data.map.reduced <- PS.data.map %>%
  select(Author, Location, Country, ISO3) %>% 
  group_by(Country) %>% 
  summarise(n = n(),
            Publications = paste(Author, collapse = "\n")) %>% 
  left_join(iso_codes %>% 
              select(Country, ISO3)) %>% 
  mutate(log_n = log(n))

PS.data.map.reduced$Publications[PS.data.map.reduced$Country == "United States of America"] <- 
  paste(PS.data.map$Author[PS.data.map$Country == "United States of America"], collapse = ";")

# User interface ----
ui <- navbarPage("PsyChild - Tracking clinical psychedelics in children and adolescents.",
                 # Classes -----------------------------------------------------------------
                 tabPanel("Substance classes",
                          # sidebarLayout(
                          #   sidebarPanel(
                          
                          # mainPanel(
                          # helpText(h3("Tracking clinical psychedelics in children and adolescents.")), # "Visualize psychedelic drug use in children through time and space"
                          verbatimTextOutput("class_selected"),
                          plotlyOutput("studies_over_year_plot_Class"), # plotOutput
                          # plotOutput("test_plot"),
                          sliderInput("range",
                                      label = "Years of interest:",
                                      min = min(PS.data$Date),
                                      max = max(PS.data$Date),
                                      value = c(1950, # c(min(PS.data$Date), 1950,
                                                max(PS.data$Date)),
                                      step = 1,
                                      sep = ''),
                          checkboxGroupInput("Class",
                                             # h3("Class"),
                                             label = "Choose one or more substance class(es) to display",
                                             choices = list("all",
                                                            "Deliriants",
                                                            "Dissociatives",
                                                            "Entactogens",
                                                            "MAOIs",
                                                            "Phytocannabinoids",
                                                            "Psychedelics",
                                                            "Synthetic cannabinoids"),
                                             selected = "all"),
                          div(dataTableOutput("table_print_Class"), style = "font-size:80%")
                 ),
                 
                 # Compounds --------------------------------------------------------------
                 tabPanel("Compounds",
                          # sidebarLayout(
                          #   sidebarPanel(
                          #     helpText(h3("Tracking clinical psychedelics in children and adolescents.")),
                          
                          # mainPanel(
                          verbatimTextOutput("compound_selected"),
                          plotlyOutput("studies_over_year_plot_Compound"), # plotOutput
                          
                          sliderInput("range_compounds",
                                      label = "Years of interest:",
                                      min = min(PS.data$Date),
                                      max = max(PS.data$Date),
                                      value = c(1950, # c(min(PS.data$Date), 1950,
                                                max(PS.data$Date)),
                                      step = 1,
                                      sep = ''),
                          
                          checkboxGroupInput("Compound",
                                             # h3("Compound"),
                                             label = "Choose one or more compound(s) to display",
                                             choices = c("all",
                                                         compounds),
                                             # "2-AG (2-Arachidonylglycerol)",
                                             # "AA",
                                             # "AEA (Anandamid)",
                                             # "Delta-8-tetrahydrocannabinol (delta-8-THC)",
                                             # "Dexanabinol",
                                             # "Dronabinol",
                                             # "Esketamine",
                                             # "Harmaline",
                                             # "Harmine",
                                             # "Iofetamine",
                                             # "Ketamine",
                                             # "Ketodex",
                                             # "Ketofol",
                                             # "LAE-32 (D-Lysergic acid ethylamide)",
                                             # "Lenabasum",
                                             # "Levonantradol",
                                             # "LSD (Lysergic acid diethylamide)",
                                             # "Marinol",
                                             # "mCPP (meta-Chlorphenylpiperazin)",
                                             # "Mescaline",
                                             # "Methysergide",
                                             # "Nabilone",
                                             # "Nabiximols",
                                             # "Naboline",
                                             # "OEA (Oleoylethanolamide)",
                                             # "PCP (Phencyclidin)",
                                             # "PEA (Palmitoylethanolamid)",
                                             # "Physostigmine",
                                             # "Phytocannabinoids",
                                             # "Psilocybin",
                                             # "Scopolamine",
                                             # "THC",
                                             # "αET (alpha-Ethyltryptamine)"
                                             selected = "all"),
                          
                          div(dataTableOutput("table_print_Compound"), style = "font-size:80%")
                 ),
                 # )
                 # ))
                 tabPanel("Map",
                          plotlyOutput("map_plot"))
)


# Server logic ------------------------------------------------------------
server <- function(input, output) {
  # Class -------------------------------------------------------------------  
  
  # output$test_plot <- renderPlot({
  #   # plot(1:10)
  #   ggplot(data = data.frame(x=1:10, y=1:10), aes(x=x, y=y)) + 
  #     geom_point()
  # })
  
  output$class_selected <-renderText({
    paste0("Classes selected: ", paste(input$Class, collapse = ", "), ".")
  })
  
  output$studies_over_year_plot_Class <- renderPlotly({ # renderPlot
    
    # testing
    # input=list(range = c(1839, 2023), # 1839 1950 2023 1980
    #            range_compounds = c(1839, 2023), # 1839 1950 2023 1980
    #            Class = c("Dissociatives, Entactogens"), # "Dissociatives" "all"
    #            Compound = c("αET (alpha-Ethyltryptamine)")) # LSD Phytocannabinoids αET (alpha-Ethyltryptamine) all OEA (Oleoylethanolamide)
    
    # filter by input range
    PS.data.plot.Class <- PS.data %>%
      filter(Date >= input$range[1],
             Date <= input$range[2])
    
    # select input classes
    if("all" %in% input$Class == FALSE){
      PS.data.plot.Class <- PS.data.plot.Class %>%
        filter(`Substance class` %in% unlist(strsplit(input$Class, split = ", ")))
    } else {
      PS.data.plot.Class <- PS.data.plot.Class
    }
    
    globalcolors <- PS.data.plot.Class$col_class
    opacity <- 0.75
    fig_classes <- plot_ly(data = PS.data.plot.Class, x=~Date, y=~cumul_years_class,
                           type="scatter",
                           color=~`Substance class`, 
                           mode="lines", 
                           colors = globalcolors, 
                           opacity=opacity, 
                           line = list(width=4)) # ,
    # height=800)
    fig_classes %>% layout(legend = list(orientation = 'h', y=-0.25),
                           xaxis = list(
                             dtick = 10,
                             # tick0 = 10, 
                             tickmode = "linear"),
                           yaxis = list(
                             range = list(0, max(PS.data$cumul_years_class))))
  })
  
  output$table_print_Class <- renderDataTable({df <- reactiveVal(PS.data.print_Class)
  
  PS.data.print_Class <- PS.data.print_Class %>% 
    arrange(Date) %>% 
    filter(Date >= input$range[1],
           Date <= input$range[2])
  
  # select input classes
  if("all" %in% input$Class == FALSE){
    PS.data.print_Class <- PS.data.print_Class %>%
      filter(`Substance class` %in% unlist(strsplit(input$Class, split = ", ")))
  } else {
    PS.data.print_Class <- PS.data.print_Class
  }
  
  PS.data.print_Class <- PS.data.print_Class %>% 
    arrange(Date, `Substance class`) %>% 
    # rename(`Compound(s)` = Compound) %>% 
    select(c(# `Date`,
      `Author`,
      `Location`,
      # `Location photo`,
      `Title`,
      `Type`,
      `Compound(s)`,
      `Substance class`,
      `ICD-11 Indication or field of application`,
      `ICD-11 Indication as Groups or field of application`,
      # `Psychiatric indication?`,
      `Adjunct psychotherapy`,
      # `Adjacent psychotherapy?`,
      `Subjects`,
      # `Only children and adolescents?`,
      `Main psychiatric outcomes`,
      # `Reported side effects/adverse events`,
      `Side effects (MedDRA)`,
      `Consent`,
      `in/out patient`,
      # `Route of administration`,
      `Regimen (route of administration, dose, frequency)`,
      `Concomitant Medications`,
      `Comment`#,
      # `comment 1`,
      # `comment 2`,
      # `Compound_new_name,
      # Country
    ))
  
  PS.data.print_Class},
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE))
  
  # Compound -------------------------------------------------------------------  
  output$compound_selected <-renderText({
    paste0("Compounds selected: ", paste(input$Compound, collapse = ", "), ".")
  })
  
  output$studies_over_year_plot_Compound <- renderPlotly({ # renderPlot
    
    # # testing
    # input=list(range = c(1839, 2023), # 1839 1950 2023 1980
    #            range_compounds = c(1839, 2023), # 1839 1950 2023 1980
    #            Class = c("Dissociatives, Entactogens"), # "Dissociatives" "all"
    #            Compound = c("αET (alpha-Ethyltryptamine)", "LSD (Lysergic acid diethylamide)")) # LSD Phytocannabinoids αET (alpha-Ethyltryptamine) all OEA (Oleoylethanolamide)
    
    # filter by input range
    PS.data.compounds.plot <- PS.data.compounds  %>% 
      ungroup() %>%
      filter(Date >= input$range_compounds[1],
             Date <= input$range_compounds[2])
    
    # select input compounds
    if("all" %in% input$Compound == FALSE){
      # i=2
      tmp <- NULL
      for(i in 1:length(unlist(strsplit(input$Compound, split = ", ")))){
        tmp <- rbind(tmp, PS.data.compounds.plot %>%
                       ungroup() %>%
                       # filter(`Compound(s)` %in% unlist(strsplit(input$Compound, split = ", ")))
                       filter(grepl(unlist(strsplit(input$Compound[i], split = ", ")), Compound_new_name))) # "Oleoylethanolamide"
      }
      PS.data.compounds.plot <- tmp
      rm(tmp)
    } else {
      PS.data.compounds.plot <- PS.data.compounds.plot
    }
    
    globalcolors <- PS.data.compounds.plot$col_compound
    opacity <- 0.75
    fig_compounds <- plot_ly(data = PS.data.compounds.plot, x=~Date, y=~cumul_years_compound,
                             type="scatter",
                             color=~Compound_new_name, 
                             mode="lines", 
                             colors = globalcolors, 
                             opacity=opacity, 
                             line = list(width=4)) # ,
    # height=800)
    fig_compounds %>% layout(legend = list(orientation = 'h', y=-0.25),
                             xaxis = list(
                               dtick = 10,
                               # tick0 = 10, 
                               tickmode = "linear"),
                             yaxis = list(
                               range = list(0, max(PS.data.compounds$cumul_years_compound))))
  })
  
  output$table_print_Compound <- renderDataTable({df <- reactiveVal(PS.data.print_Compound)
  
  PS.data.print_Compound <- PS.data.print_Compound %>% 
    arrange(Date) %>% 
    filter(Date >= input$range_compounds[1],
           Date <= input$range_compounds[2])
  
  if("all" %in% input$Compound == FALSE){
    # i=2
    tmp <- NULL
    for(i in 1:length(unlist(strsplit(input$Compound, split = ", ")))){
      tmp <- rbind(tmp, PS.data.print_Compound %>%
                     # filter(Compound %in% unlist(strsplit(input$Compound, split = ", ")))
                     filter(grepl(unlist(strsplit(input$Compound[i], split = ", ")), `Compound(s)`)))
    }
    PS.data.print_Compound <- tmp
    rm(tmp)
  } else {
    PS.data.print_Compound <- PS.data.print_Compound
  }
  
  PS.data.print_Compound <- PS.data.print_Compound %>% 
    arrange(Date, `Compound(s)`) %>% 
    select(c(# `Date`,
      `Author`,
      `Location`,
      # `Location photo`,
      `Title`,
      `Type`,
      `Compound(s)`,
      `Substance class`,
      `ICD-11 Indication or field of application`,
      `ICD-11 Indication as Groups or field of application`,
      # `Psychiatric indication?`,
      `Adjunct psychotherapy`,
      # `Adjacent psychotherapy?`,
      `Subjects`,
      # `Only children and adolescents?`,
      `Main psychiatric outcomes`,
      # `Reported side effects/adverse events`,
      `Side effects (MedDRA)`,
      `Consent`,
      `in/out patient`,
      # `Route of administration`,
      `Regimen (route of administration, dose, frequency)`,
      `Concomitant Medications`,
      `Comment`#,
      # `comment 1`,
      # `comment 2`,
      # `Compound_new_name,
      # Country
    ))
  PS.data.print_Compound},
  options = list(pageLength = 1000,
                 searching = FALSE,
                 lengthChange = FALSE))
  
  # MAP ---------------------------------------------------------------------
  output$map_plot <- renderPlotly({ # renderPlot
    fig_map <- plot_ly(PS.data.map.reduced, type='choropleth', 
                       locations=PS.data.map.reduced$ISO3, 
                       z=PS.data.map.reduced$log_n, 
                       # text=paste0(PS.data.map.reduced$Country, ":\n", PS.data.map.reduced$Publications), 
                       colorscale="Viridis",
                       hovertemplate = paste0(PS.data.map.reduced$Country, ":", PS.data.map.reduced$n, " Publications.\n", PS.data.map.reduced$Publications))

    fig_map %>% 
      hide_colorbar() %>%
      layout(title = 'Map of Publications per country')
  })
}

# Run app ----
shinyApp(ui, server)
