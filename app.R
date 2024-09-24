# Project for data analysis, visualization and biostatistics using R
# 
# Author: Katarzyna Marta Zoltowska
# E-mail: k.zoltowska@oxfordalumni.org


# This shiny app allows visualization and analysis of publicly available dataset
# collected during clinical trial in patients suffering from primary biliary cholingitis

# get all the packages needed
packages <- c(
  "shiny", "visdat", "ggplot2", "RColorBrewer", "ggpubr", "gridExtra",
  "PerformanceAnalytics", "pheatmap", "viridis", "shinyWidgets",
  "ggsurvfit", "MASS"
)
for (i in packages) {
  if (!require(i, character.only = TRUE)) {
    install.packages(i, dependencies = TRUE)
    library(i, character.only = TRUE)
  }
}

# load dataset
pbc <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/survival/pbc.csv")
pbc <- pbc[, -1]

# change data type and names
pbc[, c("status", "trt", "sex", "ascites", "hepato", "spiders", "edema", "stage")] <-
  lapply(pbc[, c("status", "trt", "sex", "ascites", "hepato", "spiders", "edema", "stage")], factor)
pbc$id <- as.character(pbc$id)
pbc$trt <- as.character(pbc$trt)
pbc$trt[is.na(pbc$trt)] <- "Not_randomised"
pbc$trt <- as.factor(pbc$trt)
colnames(pbc) <- c(
  "ID", "Time", "Endpoint", "Treatment", "Age", "Gender",
  "Ascites", "Hepatomegaly", "Spider_angioma", "Edema",
  "Bilirubin", "Cholesterol", "Albumin", "Copper", "ALP",
  "ASPAT", "Triglycerides", "PLT", "PT", "Stage"
)
levels(pbc$Endpoint) <- c("Censored", "Transplant", "Dead")
levels(pbc$Treatment) <- c("Dpenicillamine", "Placebo", "Not_randomised")
levels(pbc$Gender) <- c("Female", "Male")
levels(pbc$Ascites) <- c("Absent", "Present")
levels(pbc$Hepatomegaly) <- c("Absent", "Present")
levels(pbc$Edema) <- c("Absent", "Treated", "Untreatable")
levels(pbc$Spider_angioma) <- c("Absent", "Present")

# age in the original table is not an integer, thus round it
pbc$Age <- round(pbc$Age)

# Define UI for application that presents the data or it's summary
ui <- fluidPage(

  # Application title
  titlePanel("Primary biliary cholangitis"),

  # create tabset
  tabsetPanel(

    # create first tab
    tabPanel(
      title = "Data overview", fluid = TRUE,
      # Sidebar with a radioButtons and checkboxes input
      sidebarLayout(
        sidebarPanel(
          width = 2,
          # create radiobuttons to select how to view the data
          radioButtons(
            inputId = "description",
            label = "How would you like to view the data?",
            choices = c("Entire dataset", "Tabular summary", "Graphical summary")
          ),
          br(),
          # create checkboxes to allow displaying extra text information about the data
          checkboxInput(inputId = "Data_bg", label = "Show information about the study"),
          checkboxInput(inputId = "Data_cat", label = "Show information about categorical variables"),
          checkboxInput(inputId = "Data_num", label = "Show information about numerical variables")
        ),
        # Show entire dataset
        mainPanel(
          width = 10,
          br(),
          # show text description of the dataset
          conditionalPanel(
            condition = "input.Data_bg == 1",
            textOutput("data_bg")
          ),
          br(),
          # show text info on categorical variables
          conditionalPanel(
            condition = "input.Data_cat == 1",
            textOutput("data_cat")
          ),
          br(),
          # show text info on numerical variables
          conditionalPanel(
            condition = "input.Data_num == 1",
            textOutput("data_num")
          ),
          br(),
          # show entire data
          conditionalPanel(
            condition = "input.description == 'Entire dataset'",
            dataTableOutput("entire_data")
          ),
          # Show a summary table
          conditionalPanel(
            condition = "input.description == 'Tabular summary'",
            tableOutput("summary_table")
          ),
          # Show a graphical summary
          conditionalPanel(
            condition = "input.description == 'Graphical summary'",
            plotOutput("summary_plot", height = "300px")
          )
        )
      )
    ),

    # create second tab
    tabPanel(
      title = "Visualisation of categorical variables", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          width = 2,
          # create radio buttons to select the type of plot to show categorical
          # variables
          radioButtons(
            inputId = "plot_type",
            label = "Select graph type",
            choices = c("Barplot", "Pie chart", "Doughnut plot")
          ),
          # create checkbox to allow displaying conclusions
          checkboxInput(inputId = "cat_concl", label = "Show conclusion"),
        ),
        mainPanel(
          width = 10,
          br(),
          # show conclusion
          conditionalPanel(
            condition = "input.cat_concl==1",
            textOutput("cat_out_concl")
          ),
          br(),
          # show barplots
          conditionalPanel(
            condition = "input.plot_type=='Barplot'",
            plotOutput("barplot", height = "550px")
          ),
          # show pie charts
          conditionalPanel(
            condition = "input.plot_type=='Pie chart'",
            plotOutput("pie", height = "550px")
          ),
          # show doughnut plot
          conditionalPanel(
            condition = "input.plot_type=='Doughnut plot'",
            plotOutput("doughnut", height = "550px")
          )
        )
      )
    ),

    # create a third tab
    tabPanel(
      title = "Visualisation of individual numerical variables", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          # create dropdown menu
          selectInput(
            inputId = "num_var", label = "Select variable",
            choices = colnames(pbc[, sapply(pbc, is.numeric)])
          ),
          br(),
          # create slider
          sliderInput(
            inputId = "bins", label = "Select bin number", min = 5, max = 30, step = 5,
            value = 10
          ),
          br(),
          # create checkbox for density line
          checkboxInput(inputId = "line", label = "Plot density line"),
          br(),
          # create checkbox to show conclusions
          checkboxInput(inputId = "hist_concl_in", labe = "Show conclusion")
        ),
        mainPanel(
          width = 9,
          br(),
          # show conclusion/purpose of the analysis
          conditionalPanel(
            condition = "input.hist_concl_in==1",
            textOutput("hist_concl")
          ),
          br(),
          # show histogram
          plotOutput("histogram", height = "400px")
        )
      )
    ),

    # create a fourth tab
    tabPanel(
      title = "Numerical variables in the context of treatment", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          # create picker input for numerical variables
          pickerInput(
            inputId = "numerical", choices = colnames(pbc[, sapply(pbc, is.numeric)]),
            label = "Select numerical variable"
          ),
          br(),
          # create picker input for categorical variables
          pickerInput(
            inputId = "facet", choices = c("Do not split", colnames(pbc[, sapply(pbc, is.factor)])[!(colnames(pbc[, sapply(pbc, is.factor)]))
            == "Treatment"]),
            label = "Select categorical variable to split by"
          ),
          br(),
          # create checkbox to show conclusions
          checkboxInput(inputId = "vio_concl_in", label = "Show conclusion")
        ),
        mainPanel(
          width = 9,
          br(),
          # show conclusion/purpose of the violin plots
          conditionalPanel(
            condition = "input.vio_concl_in==1",
            textOutput("vio_concl")
          ),
          br(),
          # show violin plots without facet_wrap splitting
          conditionalPanel(
            condition = "input.facet=='Do not split'",
            plotOutput("violinall", height = "400px", width = "800px")
          ),
          # show violin plots with facet_wrap
          conditionalPanel(
            condition = "input.facet!='Do not split'",
            plotOutput("violin", height = "400px")
          )
        )
      )
    ),

    # create a fifth tab
    tabPanel(
      title = "Relationships between numerical variables", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          width = 2,
          # create radio buttons
          radioButtons(
            inputId = "graphtype", choices = c(
              "Heatmap", "Parallel coordinates plot",
              "Pairwise correlations"
            ),
            label = "Select graph type"
          ),
          # create additional dropdown menu for the heatmap
          conditionalPanel(
            condition = "input.graphtype=='Heatmap'",
            pickerInput(inputId = "cases", choices = c(312, 418), label = "Choose what to
                        show on heatmap", choicesOpt = list(subtext = c("Largely complete cases", "All cases")))
          ),
          # create checkbox for the conclusions
          checkboxInput(inputId = "pair_concl_in", label = "Show conclusion derived
                        from all three plots")
        ),
        mainPanel(
          width = 10,
          br(),
          # show text with conclusion
          conditionalPanel(
            "input.pair_concl_in==1",
            textOutput("pair_concl")
          ),
          br(),
          # show heatmap
          conditionalPanel(
            condition = "input.graphtype=='Heatmap'",
            plotOutput("heatmap", height = "550px")
          ),
          # show parallel coordinates plot
          conditionalPanel(
            condition = "input.graphtype=='Parallel coordinates plot'",
            plotOutput("parcoord", height = "550px")
          ),
          # show correlation plots
          conditionalPanel(
            condition = "input.graphtype=='Pairwise correlations'",
            plotOutput("corplots", height = "550px")
          )
        )
      )
    ),

    # create a sixth tab
    tabPanel(
      title = "Multiple linear regression",
      # create checkboxes to allow selection of what to show
      sidebarLayout(
        sidebarPanel(
          width = 2,
          checkboxInput(
            inputId = "lmdescription",
            label = "Description of the analysis"
          ),
          checkboxInput(inputId = "lmfinal", label = "Final model"),
          checkboxInput(inputId = "lmfull", label = "Full analysis results")
        ),
        mainPanel(
          width = 8,
          br(),
          # show analysis description
          conditionalPanel(
            condition = "input.lmdescription == 1",
            textOutput("description")
          ),
          br(),
          # Show full analysis result
          conditionalPanel(
            condition = "input.lmfull == 1",
            verbatimTextOutput("analysis_table")
          ),
          br(),
          # Show summary of the final model
          conditionalPanel(
            condition = "input.lmfinal == 1",
            verbatimTextOutput("final")
          )
        )
      )
    ),

    # create seventh tab
    tabPanel(
      title = "Survival analysis",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          # create dropdown menu
          pickerInput(
            inputId = "catsurv",
            label = "Choose categorical variable to look at its impact on survival",
            choices = colnames(pbc[, sapply(pbc, is.factor)])
            [!(colnames(pbc[, sapply(pbc, is.factor)])) == "Endpoint"]
          ),
          # create dropdown menu to define whether to show entire dataset or only the 312 complete cases
          conditionalPanel(
            condition = "input.catsurv != 'Hepatomegaly' & input.catsurv !='Spider_angioma' &
            input.catsurv != 'Ascites'",
            pickerInput(
              inputId = "cases1", choices = c(312, 418), label = "Choose cases to view",
              choicesOpt = list(subtext = c("Largely complete cases", "All cases"))
            )
          ),
          # create checkbox to show conclusions
          checkboxInput(inputId = "surv_concl_in", label = "Show conclusion"),
          conditionalPanel(
            condition = "input.surv_concl_in==1",
            textOutput("surv_concl")
          )
        ),
        # show the graph with the survival curves
        mainPanel(
          width = 10,
          br(),
          plotOutput("survplot", height = "550px")
        )
      )
    )
  )
)

# Define server logic required to display the table or plot
server <- function(input, output) {
  # create text about data
  output$data_bg <- renderText({
    "Primary biliary cholangitis (PBC) is an autoimmune disease leading to
destruction of the small bile ducts in the liver.
The dataset comes from the randomized placebo controlled clinical trial
of D-penicillamine conducted in the Mayo Clinic. The first 312 cases
participated in the randomized trial and contain largely complete data.
The additional 106 cases did not participate in the clinical trial,
but consented to have basic measurements recorded. The data for the latter
    106 cases is largely incomplete."
  })

  # create text about categorical variables
  output$data_cat <- renderText({
    "Edema:	no edema/untreated/successfully treated;
Hepatomegaly:	presence/absence of hepatomegaly;
Gender:	male/female;
Spider_angioma:	presence/absence of blood vessel malformations in the skin;
Stage:	histologic stage of the disease (1/2/3/4);
Endpoint:	status at endpoint: censored/transplant/dead;
Treatment: D-penicillamine/placebo/not_randomised"
  })

  # create text about numerical variables
  output$data_num <- renderText({
    "Age:	in years;
Albumin:	serum albumin (g/dl);
ALP:	alkaline phosphotase (U/liter);
ASPAT:	aspartate aminotransferase (U/ml);
Bilirubin:	serum bilirubin (mg/dl);
Cholesterol:	serum cholesterol (mg/dl);
Copper:	urine copper (ug/day);
PLT:	platelet count;
PT:	standardised blood clotting time;
Time:	number of days between registration and the earlier of death,
transplantion, or study analysis in July, 1986;
Triglycerides:	triglycerides (mg/dl)"
  })

  # create entire data table
  output$entire_data <- renderDataTable({
    pbc
  })

  # create summary table
  output$summary_table <- renderTable({
    df <- as.data.frame(summary(pbc))
    df <- df[, c(2, 3)]
    colnames(df) <- c("Variable", "info")
    df$Variable <- as.factor(df$Variable)
    ls <- list()
    levels <- levels(df$Variable)
    for (i in levels) {
      ls[[i]] <- subset(df, df[, 1] == i, select = c("info"))
    }
    df2 <- as.data.frame(ls)
    colnames(df2) <- levels
    df2[is.na(df2) == TRUE] <- ""
    df2
  })

  # create summary plot
  output$summary_plot <- renderPlot({
    vis_dat(pbc) +
      theme(
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 15),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16, face = "bold.italic")
      ) +
      ggtitle("Graphical summary of the data")
  })

  # create barplots for all categorical variables in a loop using base R
  output$barplot <- renderPlot({
    par(mfrow = c(4, 2), mar = c(5, 6, 6, 4))
    for (i in colnames(pbc[, sapply(pbc, is.factor)])) {
      barplot(sort(table(pbc[, i])),
        main = paste("Number of cases per", tolower(i)),
        cex.main = 1.8,
        font.main = 4,
        xlab = "Number of cases",
        cex.lab = 1.4,
        xlim = c(0, (1.4 * max(table(pbc[, i])))),
        names = c(rep("", length(levels(pbc[, i])))),
        col = brewer.pal(n = length(levels(pbc[, i])), name = "Dark2"),
        horiz = TRUE
      )
      text(
        y = 1:length(levels(pbc[, i])),
        x = -7,
        labels = names(sort(table(pbc[, i]))),
        xpd = NA,
        srt = 45,
        adj = 1,
        cex = 1.1
      )
    }
  })

  # create pie chart for all categorical variables in a loop using base R
  output$pie <- renderPlot({
    par(mfrow = c(2, 4), mar = c(2, 0, 8, 2))
    for (i in colnames(pbc[, sapply(pbc, is.factor)])) {
      pie(sort(table(pbc[, i])),
        main = i,
        cex.main = 2,
        font.main = 4,
        cex = 1.3,
        col = brewer.pal(n = length(levels(pbc[, i])), name = "Dark2")
      )
    }
  })

  # create doughnut plot using ggplot2
  output$doughnut <- renderPlot({
    p <- list()
    for (i in colnames(pbc[, sapply(pbc, is.factor)])) {
      data <- as.data.frame(sort(table(pbc[, i])))
      data$ymax <- cumsum(data$Freq)
      data$ymin <- c(0, head(data$ymax, n = -1))
      p[[i]] <- ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Var1)) +
        geom_rect() +
        coord_polar(theta = "y") +
        xlim(c(2, 4)) +
        ggtitle(i) +
        theme_pubr() +
        theme(
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold.italic")
        ) +
        scale_fill_brewer(palette = "Dark2", name = i)
    }
    ncol <- list(ncol = 4)
    p <- append(p, ncol)
    do.call(grid.arrange, p)
  })

  # create conclusion text for categorical variable analysis
  output$cat_out_concl <- renderText({
    "Similar number of patients were assigned to treatment (DPenicillamine) and
    placebo group, but markedly more women participated in the study, compared to
    men. The majority of the participants were already at the advanced stage of the
    disease (stage 3 or 4)."
  })

  # create histogram using base R to visualize numerical variables
  output$histogram <- renderPlot({
    hist(pbc[, input$num_var],
      breaks = seq(min(pbc[, input$num_var], na.rm = TRUE), max(pbc[, input$num_var], na.rm = TRUE),
        length.out = input$bins + 1
      ),
      prob = TRUE,
      main = paste(
        "Distribution of",
        if (input$num_var %in% c("ALP", "ASPAT", "ALAT", "PLT", "PT")) {
          input$num_var
        } else {
          tolower(input$num_var)
        }, "measurements"
      ),
      col = rev(colorRampPalette(brewer.pal(11, "Spectral"))(30)),
      ylab = "Density",
      cex.lab = 1.5,
      cex.main = 2,
      xlab = NULL,
      ylim = c(0, 1.5 * max(unlist(density(pbc[, input$num_var], na.rm = TRUE)["y"]))),
      xlim = c(0.5 * min(pbc[, input$num_var], na.rm = TRUE), 1.2 * max(pbc[, input$num_var], na.rm = TRUE))
    )
    if (input$line == 1) {
      lines(density(pbc[, input$num_var], na.rm = TRUE), lwd = 3, lty = 5, col = "darkred")
    }
  })

  # create conclusion text for histograms
  output$hist_concl <- renderText({
    "The histograms (with the optional density line) present the distribution of the numerical variables.
    We can conclude that the majority of the numerical variables in the dataset present
    unimodal distribution with long right tail (positively skewed). Exceptions are
    Age and Albumin, which are rather normally distributed, although there is some
    negative skewness for the Albumin."
  })

  # create violin plots with ggplot2 using faceting technique to split for gender
  output$violinall <- renderPlot({
    ggplot(
      pbc,
      aes(
        y = pbc[, input$numerical], x = Treatment,
        fill = Treatment
      )
    ) +
      geom_violin() +
      geom_boxplot(width = 0.2) +
      scale_fill_brewer(palette = "Dark2") +
      theme_pubr() +
      ylab(input$numerical) +
      theme(
        legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        strip.text.x = element_text(
          size = 18, face = "bold"
        )
      )
  })
  output$violin <- renderPlot({
    ggplot(
      pbc,
      aes(
        y = pbc[, input$numerical], x = Treatment,
        fill = Treatment
      )
    ) +
      geom_violin() +
      geom_boxplot(width = 0.2) +
      scale_fill_brewer(palette = "Dark2") +
      theme_pubr() +
      ylab(input$numerical) +
      facet_wrap(~ pbc[, input$facet]) +
      theme(
        legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12),
        strip.text.x = element_text(
          size = 18, face = "bold"
        )
      )
  })

  # create conclusion text for violin plots
  output$vio_concl <- renderText({
    "The violin plots show impact of the treatment on the numerical variables in
    the dataset. The option to split the data by categorical variable allows to
    check whether any categorical variable may impact the response to the treatment.
    There is no marked impact of the treatment on any of the measurements.
    In addition, the violin plot presenting age allows us to verify the study design,
    i.e. whether the age distribution of study participants is similar in
    different groups (defined by categorical variables)."
  })

  # create heatmap with annotations using pheatmap package
  graphics.off() # this ensures that the heatmap is always loading in shiny app
  output$heatmap <- renderPlot({
    pbc_matrix <- log(pbc[, sapply(pbc, is.numeric)])
    pbc_matrix <- pbc_matrix[1:input$cases, ]
    rownames(pbc_matrix) <- pbc$ID[1:input$cases]
    pbc_matrix <- t(pbc_matrix)
    annotations <- data.frame(
      Stage = as.character(pbc$Stage[1:input$cases]),
      Edema = as.character(pbc$Edema[1:input$cases]),
      Treatment = as.character(pbc$Treatment[1:input$cases])
    )
    row.names(annotations) <- pbc$ID[1:input$cases]
    colors <- lapply(annotations, as.factor)
    levels(colors$Treatment) <- c("Dpenicillamine", "Placebo", "Not_randomised")
    ann_colors <- list(
      Stage = c("1" = "#66C2A5", "2" = "#FC8D62", "3" = "#8DA0CB", "4" = "#E78AC3"),
      Edema = c(Untreatable = "#E41A1C", Treated = "#377EB8", Absent = "#4DAF4A"),
      Treatment = c(Dpenicillamine = "purple", Placebo = "darkgreen", Not_randomised = "darkorange")
    )
    colors <- as.data.frame(lapply(colors, as.character))
    row.names(colors) <- pbc$ID[1:input$cases]
    plot <- pheatmap(pbc_matrix,
      show_colnames = FALSE, annotation_col = annotations,
      scale = "row", color = viridis(10), annotation_colors = ann_colors,
      fontsize = 10
    )
    print(plot)
  })

  # create parralel coordinates plot for numerical variables
  output$parcoord <- renderPlot({
    colors <- pbc$Treatment
    levels(colors) <- list(
      "#1B9E77" = "Dpenicillamine",
      "#D95F02" = "Placebo", "#7570B3" = "Not_randomised"
    )
    par(mar = c(2, 0, 6, 0), xpd = TRUE)
    parcoord(pbc[, sapply(pbc, is.numeric)],
      col = as.character(colors),
      var.label = TRUE, lwd = 2, cex = 5
    )
    legend(
      x = 1.2, y = 1.2, cex = 1.3, title = "Treatment group", legend = levels(pbc$Treatment),
      pch = 16, col = unique(as.character(colors)), horiz = TRUE
    )
  })

  # create correlation plots with statistics with PerformanceAnalytics
  output$corplots <- renderPlot({
    chart.Correlation(pbc[, sapply(pbc, is.numeric)],
      method = "pearson",
      histogram = TRUE, pch = 16, cex = 10
    )
  })

  # create conclusion for the entire tab
  output$pair_concl <- renderText({
    "Collectively, the plots in this tab allow us to visualise the
    relationships between different variables in the dataset. From the heatmap
    it is apparent that patients at stage 4 and untreatable edema present shorter
    time to the endpoint, lower albumin levels and higher bilirubin and PT levels.
    The parallel coordinates plot does not reveal any clear patterns of the numerical
    variables or differences between the treatment groups. The pairwise correlation
    plot demonstrates significant correlations between the numerical variables in
    the dataset. Of particular interest might be the finding that all tested numerical
    variables show some correlation with Time. This aspect is further explored in
    the multiple linear regression tab."
  })

  # statistical analysis: building multiple linear regression to determine which
  # numerical variables correlate with time of survival defined as
  # number of days between registration and the earlier of death,
  # transplantion, or study analysis time in July, 1986
  # na.omit function was use to remove rows with missing values
  model.null <- lm(Time ~ 1, data = na.omit(pbc))
  model.full <- lm(Time ~ Age + Bilirubin + Cholesterol + Albumin + Copper +
    ALP + ASPAT + Triglycerides + PLT + PT, data = na.omit(pbc))

  # description of the analysis and conclusions
  output$description <- renderText({
    "Multiple linear regression analysis was performed to define which
    numerical variables correlate with the time of survival defined as
    the number of days between registration and the earlier of death,
    transplantion, or study analysis time in July, 1986.
    Akaike's Information Criterion was used to define the model that best fits the
    data without overfitting.
    The analysis demonstrates that levels of bilirubin and copper are negatively
    correlated with the survival time, while levels of albumin and ALP are positively
    correlated with the survival time. Addition of other numerical variables does
    not improve the multiple linear regression model."
  })

  # display only the final model derived from the analysis
  output$final <- renderPrint({
    options(width = 100)
    summary(lm(
      formula = Time ~ Bilirubin + Albumin + ALP + Copper,
      data = na.omit(pbc)
    ))
  })

  # present entire analysis as printed in the console
  output$analysis_table <- renderPrint({
    options(width = 100)
    step(model.null,
      scope = list(upper = model.full), direction = "both", test = "F",
      data = na.omit(pbc)
    )
  })

  # create plots showing survival curves in the context of categorical
  # variables
  output$survplot <- renderPlot({
    survfit2(Surv(Time, Endpoint == "Dead") ~ pbc[1:input$cases1, input$catsurv], pbc[1:input$cases1, ]) %>%
      ggsurvfit() +
      add_confidence_interval() +
      add_risktable(size = 4) +
      add_pvalue("annotation", size = 7) +
      scale_color_manual(values = brewer.pal(length(levels(pbc[, input$catsurv])), "Dark2")) +
      scale_fill_manual(values = brewer.pal(length(levels(pbc[, input$catsurv])), "Dark2")) +
      scale_x_continuous(limits = c(0, 5500), breaks = seq(0, 5200, 500)) +
      theme_pubr() +
      theme(
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 14),
        text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold.italic"),
        plot.margin = margin(t = 1, r = 2, unit = "cm")
      ) +
      ggtitle(paste("Impact of", tolower(input$catsurv), "on survival"))
  })

  # create conclusion text, which will be displayed based on the input in the
  # dropdown menu
  # created as a list with key:value pairs
  output$surv_concl <- renderText({
    surv_conc_list <- list(
      "Treatment" = "Treatment has no impact on survival.",
      "Gender" = "Females present higher survival rate based on the
                         analysis of 312 almost complete cases. However, gender
                         seem to have no impact when all 418 cases are taken into
                         the analysis.",
      "Stage" = "The more advanced the stage of the disease the
                         lower the survival rate.",
      "Ascites" = "Patients with ascites present lower
                         survival rate.",
      "Hepatomegaly" = "Patients with hepatomegaly present
                         lower survival rate.",
      "Spider_angioma" = "Patients with spider_angioma present
                         lower survival rate.",
      "Edema" = "Edema impacts survival rate, with the patients
                         with untreatable edema presenting the lowest survival
                         rate."
    )
    surv_conc_list[[input$catsurv]]
  })
}

# Run the application
shinyApp(ui = ui, server = server)
