library(shiny)
library(SuppDists)
library(shinyWidgets)
library(ggplot2)
library(DT)

# p_X(x) = \frac{\delta}{\sqrt{2\pi((x-xi)^2 + lambda^2)}}exp[-0.5(\gamma + \delta ln(\frac{x-xi + \sqrt{(x-xi)^2 + lambda^2}}{lambda}))^2].

ui <- fluidPage(
  titlePanel(h3("Distribution de Johnson")),
  withMathJax(),
  tags$div(HTML("<script type='text/x-mathjax-config' >
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script >
                ")),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          6,
          switchInput(
            inputId = "input_type",
            label = "Entrée",
            onLabel = "Curseur",
            offLabel = "Numérique",
            value = TRUE,
            size = "mini"
          )
        ),
        column(3,
          offset = 3,
          actionBttn("reset_button",
            "Réinit",
            icon = icon("refresh"),
            class = "btn-danger",
            style = "bordered",
            size = "xs",
            color = "default"
          )
        )
      ),
      conditionalPanel(
        condition = "input.input_type == true",
        sliderInput("gamma",
          "$\\gamma$:",
          min = -10,
          max = 10,
          step = 0.1,
          value = 0
        ),
        sliderInput("delta",
          "$\\delta$:",
          min = 0.1,
          max = 20,
          step = 0.1,
          value = 1
        ),
        sliderInput("xi",
          "$\\xi$:",
          min = -3,
          max = 3,
          step = 0.1,
          value = 0
        ),
        sliderInput("lambda",
          "$\\lambda$",
          min = 0.1,
          max = 10,
          step = 0.1,
          value = 1
        )
      ),
      conditionalPanel(
        condition = "input.input_type == false",
        numericInput("gamma_num",
          "$\\gamma$:",
          min = -10,
          max = 10,
          step = 0.1,
          value = 0,
          width = 150
        ),
        numericInput("delta_num",
          "$\\delta$:",
          min = 0.1,
          max = 20,
          step = 0.1,
          value = 1,
          width = 150
        ),
        numericInput("xi_num",
          "$\\xi$:",
          min = -3,
          max = 3,
          step = 0.1,
          value = 0,
          width = 150
        ),
        numericInput("lambda_num",
          "$\\lambda$:",
          min = 0.1,
          max = 10,
          step = 0.1,
          value = 1,
          width = 150
        )
      ),
      radioButtons("type",
        "Type:",
        choices = c("SN", "SL", "SU", "SB"),
        selected = "SN",
        inline = TRUE
      ),
      switchInput(
        inputId = "input_mode",
        label = "Données",
        onLabel = "Générées",
        offLabel = "Chargées",
        value = TRUE,
        size = "mini"
      ),
      conditionalPanel(
        condition = "input.input_mode == true",
        numericInput("num_observations",
          "# Données à générer:",
          value = 100,
          min = 1,
          max = 10000,
          width = 150
        ),
        fluidRow(
          column(
            6,
            actionBttn(
              inputId = "generate_button",
              label = "Générer",
              style = "unite",
              color = "primary",
              icon = icon("sliders"),
              size = "sm"
            )
          ),
          column(
            6,
            downloadBttn(
              outputId = "download_data",
              label = "Télécharger",
              style = "unite",
              color = "default",
              icon = icon("download"),
              size = "sm"
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.input_mode == false",
        fileInput("file1", "Fichier:",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        DTOutput("fittedParams")
      ),
      p(),
      wellPanel(
        style = "background: lightblue",
        fluidRow(
          column(
            4,
            a(h4("Par Daniel Coulombe, Ph.D.")),
            p("2024")
          ),
          column(
            4,
            tags$a(
              href = "https://isteah.org",
              tags$img(
                src = "ISTEAH_LOGO.png",
                title = "ISTEAH",
                width = "160",
                height = "140"
              )
            )
          )
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          strong(h4("Introduction")),
          helpText(
            "La distribution de Johnson est définie par 4 paramètres qui permettent de transformer une distribution normale standardisée en une distribution non-normale. ", br(),
            "Essentiellement, elle permet de manipuler tant la symétrie que la voussure. Elle se présente sous 4 formes, définies à partir de l'équation suivante: ", p(),
            "$$z=\\gamma+\\delta log f(u), \\; u=\\frac{x-\\xi}{\\lambda} $$",
            h4("Selon la définition de $f(u)$, on obtient les formes suivantes: "), p(),
            "SL [log normale]: $f(u)=u$", br(),
            "SU [distribution non bordée]: $f(u)=u+\\sqrt{1+u^2}$", br(),
            "SB [distribution non bordée]: $f(u)=u/(1-u)$", br(),
            "SN [normale standardisée]: $f(u)=e^u$", p(),
            "On pourra sélectionner le type de distribution dans le tableau de bord.", p(),
            "Les paramètres de la distribution de Johnson sont les suivants:", p(),
            "Gamma $\\gamma$: contrôle le degré d'asymétrie de la distribution.  $-\\infty < \\gamma < \\infty$$", p(),
            "Delta $\\delta$: contrôle le degré de voussure. $\\delta > 0$", p(),
            "Xi $\\xi$: contrôle la location ou la position de la distribution sur l'axe horizontal. $-\\infty < \\xi < \\infty$", p(),
            "Lambda $\\lambda$: contrôle l'échelle, ou la dispersion de la distribution. $\\lambda > 0$", p(),
            p(),
            "Type = SN produit des distributions symétriques et le paramètre $\\delta$ permettra de manipular la voussure.", p(),
            "Type = SL produit des distributions asymétriques positives, contrôlées par le paramètre $\\delta$.", p(),
            "Type = SU produit des distributions asymétriques négatives si $\\gamma$ est positif, et des distributions asymétriques positives dans le cas contraire. Le paramètre $\\delta$ contrôle l'importance de l'asymétrie. ", p(),
            "Type = SB produit des distributions asymétriques positives si $\\gamma$ est positif, et des distributions asymétriques négatives dans le cas contraire. Le paramètre $\\delta$ contrôle l'importance de l'asymétrie. ", p(),
            "L'application permet de varier indépendemment les quatres paramètres à l'aide de curseurs. Les graphiques affichés à la portion supérieure du panneau principal illustrent le résultat de ces manipulations. Le diagramme de probabilité normale permet d'examiner les écarts par rapport à une distribution normale.", p(),
            "Une fois les paramètres traduisant une distribution désirée, on peut générer un échantillon de $n$ observations, que l'on peut sauvegarder. Un histogramme sur lequel on juxtappose une courbe de densité normale et une courbe de densité observée s'affiche, accompagné d'un diagramme de probabilités normales. ", p(),
            "Finalement, les valeurs des paramètres de la distribution et les statistiques calculées sur les données générées à partir de cette dernière sont présentés sous forme d'un tableau."
          ),


            p("La distribution de Johnson est une famille de distributions de probabilité continue qui est utilisée pour modéliser des données qui ne suivent pas les distributions classiques comme la normale, la lognormale, etc. Elle permet de modéliser une grande variété de formes de distribution en fonction de ses paramètres."),
            h5("Fonction de densité de probabilité (PDF) :"),
            p("La fonction de densité de probabilité (PDF) de la distribution de Johnson est définie par :"),
            tags$p(HTML("$$f(x) = \\frac{\\gamma \\delta}{\\sqrt{2\\pi}} \\frac{1}{\\sigma(x)} \\phi(\\gamma + \\delta \\lambda(x))$$")),
            p("où"),
          tags$ul(
            tags$li(HTML("<b>μ</b> est le paramètre de localisation qui déplace la distribution horizontalement.")),
            tags$li(HTML("<b>σ</b> est le paramètre d'échelle qui règle la taille de la distribution.")),
            tags$li(HTML("<b>γ</b> et <b>δ</b> sont les paramètres de forme qui déterminent la forme de la distribution.")),
            tags$li(HTML("<b>λ</b> est une transformation qui peut être linéaire ou non linéaire, utilisée pour ajuster la distribution.")),
            tags$li(HTML("<b>φ</b> est la fonction de densité de probabilité de la distribution normale standard.")),
            tags$li(HTML("La notation <b>σ(x)</b> représente la fonction de dispersion, définie par <b>σ(x) = σ / √(1 + x²)</b>."))
            ),
            h5("Références pour En Savoir Plus :"),
            tags$ul(
              tags$li("Wikipedia: ", tags$a("Distribution de Johnson", href = "https://en.wikipedia.org/wiki/Johnson%27s_SU_distribution")),
              tags$li("Johnson, N. L. (1949). Systems of frequency curves generated by methods of translation. Biometrika, 36(1/2), 149-176."),
              tags$li("Johnson, N. L., Kotz, S., & Balakrishnan, N. (1995). Continuous univariate distributions (Vol. 2). Wiley.")
            ),
            p("Pour plus d'informations, veuillez consulter les références ci-dessus.")
          
        ),
        tabPanel(
          strong(h4("Simulation")),
          fluidRow(
            column(
              width = 6,
              plotOutput("distPlot")
            ),
            column(
              width = 6,
              plotOutput("qqPlot")
            )
          ),
          fluidRow(
            column(
              width = 6,
              conditionalPanel(
                condition = "input.input_mode == true",
                plotOutput("histPlot")
              ),
              conditionalPanel(
                condition = "input.input_mode == false",
                plotOutput("histPlot_sample")
              )
            ),
            column(
              width = 6,
              conditionalPanel(
                condition = "input.input_mode == true",
                plotOutput("qqPlot_generated")
              ),
              conditionalPanel(
                condition = "input.input_mode == false",
                plotOutput("qqPlot_sample")
              )
            )
          ),
          DT::DTOutput("statsOutput")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  observeEvent(input$reset_button, {
    updateSliderInput(session, "gamma", value = 0)
    updateSliderInput(session, "delta", value = 1)
    updateSliderInput(session, "xi", value = 0)
    updateSliderInput(session, "lambda", value = 1)

    updateNumericInput(session, "gamma_num", value = 0)
    updateNumericInput(session, "delta_num", value = 1)
    updateNumericInput(session, "xi_num", value = 0)
    updateNumericInput(session, "lambda_num", value = 1)

    updateSwitchInput(session, "input_type", value = TRUE)
    updateSwitchInput(session, "input_mode", value = TRUE)
    updateRadioButtons(session, "type", selected = "SN")
    updateNumericInput(session, "num_observations", value = 100)

    output$histPlot_sample <- renderPlot(NULL)
    output$qqPlot_sample <- renderPlot(NULL)
    output$qqPlot_generated <- renderPlot(NULL)
    output$histPlot <- renderPlot(NULL)
    output$statsOutput <- DT::renderDT(NULL)
    output$fittedParams <- renderDT(NULL)
  })

  observe({
    if (input$input_mode) {
      output$data <- reactive({
        generate_observations()
      })
    } else {
      output$data <- reactive({
        req(input$file1)
        data <- read.csv(input$file1$datapath)
        if (!"x" %in% names(data) || !is.numeric(data$x)) {
          showNotification("Le fichier doit contenir une variable numérique nommée 'x'.", type = "error")
          return(NULL)
        }
        return(data$x)
      })
    }
  })

  parms <- reactive({
    if (input$input_type) {
      list(
        gamma = input$gamma,
        delta = input$delta,
        xi = input$xi,
        lambda = input$lambda,
        type = input$type
      )
    } else {
      list(
        gamma = input$gamma_num,
        delta = input$delta_num,
        xi = input$xi_num,
        lambda = input$lambda_num,
        type = input$type
      )
    }
  })

  generate_observations <- eventReactive(input$generate_button, {
    params <- parms()
    observations <- rJohnson(input$num_observations, parms = params)
    return(observations)
  })

  observeEvent(input$file1, {
    req(input$file1)
    data <- read.csv(input$file1$datapath)
    if (!"x" %in% names(data) || !is.numeric(data$x)) {
      showNotification("Le fichier doit contenir une variable numérique nommée 'x'.", type = "error")
      return(NULL)
    }
    fit <- JohnsonFit(data$x, moment = "quant")

    updateSliderInput(session, "gamma", value = fit$gamma)
    updateSliderInput(session, "delta", value = fit$delta)
    updateSliderInput(session, "xi", value = fit$xi)
    updateSliderInput(session, "lambda", value = fit$lambda)
    updateRadioButtons(session, "type", selected = fit$type)

    output$fittedParams <- DT::renderDT({
      formatted_params <- sprintf("%.4f", fit[c("gamma", "delta", "xi", "lambda")])
      formatted_params <- c(formatted_params, fit$type)
      datatable(
        data.frame(
          Paramètre = c("γ", "δ", "ξ", "λ", "Type"),
          Valeur = formatted_params
        ),
        options = list(
          searching = FALSE,
          info = FALSE,
          columnDefs = list(
            list(
              targets = c(1),
              className = "dt-center"
            )
          )
        )
      )
    })

    output$histPlot_sample <- renderPlot({
      observations <- data$x

      df <- data.frame(x = observations)
      bins <- length(hist(observations, breaks = "FD")$breaks)

      p <- ggplot(df, aes(x)) +
        geom_histogram(aes(y = after_stat(density)),
          bins = bins,
          fill = "lightblue",
          color = "black"
        ) +
        labs(
          title = "Distribution de X",
          x = "X",
          y = "Densité"
        ) +
        theme_minimal() +
        theme(
          axis.line = element_line(color = "black", linewidth = 0.5),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold"),
          legend.position.inside = c(1, 1),
          legend.justification = c(1, 1), # Justification for the position
          legend.text = element_text(size = 12)
        )

      p <- p + stat_function(
        fun = dnorm,
        args = list(mean = mean(df$x), sd = sd(df$x)),
        aes(color = "Normale"), show.legend = TRUE
      ) +
        geom_density(aes(color = "Observée"), alpha = 0.3, show.legend = TRUE) +
        scale_color_manual(
          values = c("Normale" = "red", "Observée" = "darkblue"),
          name = "Courbe...",
          labels = c("Normale", "Observée")
        ) +
        guides(color = guide_legend(override.aes = list(
          linetype = c("solid", "solid"),
          shape = c(NA, NA)
        )))

      print(p)
    })


    output$qqPlot_sample <- renderPlot({
      observations <- data$x
      df <- data.frame(Observations = observations)

      ggplot(df, aes(sample = Observations)) +
        geom_qq() +
        geom_qq_line(color = "red", linetype = "dashed") +
        labs(
          title = "Distribution observée vs Distribution Normale",
          x = "Quantiles théoriques (Normale)",
          y = "Quantiles observés"
        ) +
        theme_minimal() +
        theme(
          axis.line = element_line(color = "black", linewidth = 0.5),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold")
        )
    })
  })

  output$distPlot <- renderPlot({
    params <- parms()
    x_range <- qJohnson(c(0.001, 0.999), parms = params)
    x <- seq(x_range[1], x_range[2], length.out = 1000)
    y <- dJohnson(x, parms = params)

    df <- data.frame(x = x, y = y)

    ggplot(df, aes(x = x, y = y)) +
      geom_line() +
      labs(
        title = paste("Distribution de Johnson - Type:", input$type),
        x = "X",
        y = "Densité"
      ) +
      theme_minimal() +
      theme(
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")
      )
  })

  output$qqPlot <- renderPlot({
    params <- parms()
    samples <- rJohnson(1000, parms = params)

    theoretical_quantiles <- qJohnson(ppoints(1000), parms = params)
    normal_quantiles <- qnorm(ppoints(1000), mean = mean(samples), sd = sd(samples))
    df <- data.frame(
      Theoretical_Normal_Quantiles = normal_quantiles,
      Theoretical_Johnson_Quantiles = theoretical_quantiles
    )

    ggplot(df, aes(x = Theoretical_Normal_Quantiles, y = Theoretical_Johnson_Quantiles)) +
      geom_point(color = "blue") +
      geom_abline(intercept = 0, slope = 1, color = "red") +
      labs(
        title = "Distribution de Johnson vs Distribution Normale",
        x = "Quantiles théoriques (Normale)",
        y = "Quantiles théoriques (Johnson)"
      ) +
      theme_minimal() +
      theme(
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")
      )
  })

  output$statsOutput <- DT::renderDT({
    params <- parms()
    stats <- sJohnson(parms = params)
    observations <- generate_observations()

    stats_generated <- c(
      mean = round(mean(observations), 4),
      sd = round(sd(observations), 4),
      skewness = round(moments::skewness(observations), 4),
      kurtosis = round(moments::kurtosis(observations), 4)
    )

    df <- data.frame(
      Statistiques = c("Moyenne", "Ecart-Type", "Symétrie", "Voussure"),
      Théoriques = c(round(stats$Mean, 4), round(stats$SD, 4), round(stats$Skewness...sqrtB1, 4), round(stats$Kurtosis...B2.minus.3, 4)),
      Observées = c(stats_generated["mean"], stats_generated["sd"], stats_generated["skewness"], stats_generated["kurtosis"])
    )
    rownames(df) <- NULL
    options("digits" = 4)

    datatable(df,
      caption = htmltools::tags$caption(
        style = "caption-side: bottom; text-align: center;",
        htmltools::em(h4("Paramètres de la distribution théorique et statistiques des données générées"))
      ),
      class = "cell-border stripe",
      options = list(
        info = TRUE,
        paging = TRUE,
        searching = FALSE,
        autoWidth = TRUE,
        columnDefs = list(list(width = "100px", targets = "_all"))
      )
    )
  })

  output$histPlot <- renderPlot({
    observations <- generate_observations()

    df <- data.frame(x = observations)
    bins <- length(hist(observations, breaks = "FD")$breaks)

    p <- ggplot(df, aes(x)) +
      geom_histogram(aes(y = after_stat(density)),
        bins = bins,
        fill = "lightblue",
        color = "black"
      ) +
      labs(
        title = "Distribution de X",
        x = "X",
        y = "Densité"
      ) +
      theme_minimal() +
      theme(
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = c(.55, .7),
        legend.justification = c(-1, -1), # Justification for the position
        legend.text = element_text(size = 12)
      )

    p <- p + stat_function(
      fun = dnorm,
      args = list(mean = mean(df$x), sd = sd(df$x)),
      aes(color = "Normale"), show.legend = TRUE
    ) +
      geom_density(aes(color = "Observée"), alpha = 0.3, show.legend = TRUE) +
      scale_color_manual(
        values = c("Normale" = "red", "Observée" = "darkblue"),
        name = "Courbe...",
        labels = c("Normale", "Observée")
      ) +
      guides(color = guide_legend(override.aes = list(
        linetype = c("solid", "solid"),
        shape = c(NA, NA)
      )))
    print(p)
  })


  output$qqPlot_generated <- renderPlot({
    observations <- generate_observations()
    df <- data.frame(Observations = observations)

    ggplot(df, aes(sample = Observations)) +
      geom_qq() +
      geom_qq_line(color = "red", linetype = "dashed") +
      labs(
        title = "Distribution observée vs Distribution Normale",
        x = "Quantiles théoriques (Normale)",
        y = "Quantiles observés"
      ) +
      theme_minimal() +
      theme(
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")
      )
  })

  # DOWNLOAD BUTTON
  output$download_data <- downloadHandler(
    filename = function() {
      paste("datase-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(generate_observations(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
