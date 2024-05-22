
# Function to check if a package is installed, install it if not, and load it
install_and_load <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

# List of required packages
required_packages <- c(
  "shiny", "bslib", "shinyjs", "tm", "pdftools", 
  "topicmodels", "LDAvis", "wordcloud", "ggplot2", 
  "tidyverse", "tidytext", "gridExtra", "jsonlite","scales"
)

# Install and load required packages
sapply(required_packages, install_and_load)


# Setting up working directory and paths
working_directory <- getwd()
folder_path <- paste0(working_directory, '/Collection_of_Manifestos')
files_in_directory <- list.files(path = folder_path)

# Define a list of lists for political parties
political_parties <- {list(
  ANC = list(
    name = "African National Congress - ANC",
    manifesto_path = paste0(folder_path, "/", files_in_directory[4]),
    colors = c("black", "darkgreen", "gold")
  ),
  DA = list(
    name = "Democratic Alliance - DA",
    manifesto_path = paste0(folder_path, "/", files_in_directory[7]),
    colors = c("royalblue")
  ),
  EFF = list(
    name = "Economic Freedom Fighters - EFF",
    manifesto_path = paste0(folder_path, "/", files_in_directory[8]),
    colors = c("darkred")
  ),
  FF_Plus = list(
    name = "Freedom Front Plus - FF Plus",
    manifesto_path = paste0(folder_path, "/", files_in_directory[9]),
    colors = c("darkorange", "darkgreen")
  ),
  UDM = list(
    name = "United Democratic Movement - UDM",
    manifesto_path = paste0(folder_path, "/", files_in_directory[15]),
    colors = c("darkgoldenrod1", "darkgreen", "red2")
  ),
  Al_Jama_Ah = list(
    name = "Al Jama-Ah",
    manifesto_path = paste0(folder_path, "/", files_in_directory[3]),
    colors = c("darkgreen", "black", "red3")
  ),
  BOSA = list(
    name = "Build One South Africa - BOSA",
    manifesto_path = paste0(folder_path, "/", files_in_directory[6]),
    colors = c("goldenrod1", "red1", "royalblue3", "seagreen")
  ),
  RISE = list(
    name = "RISE Mzansi - RISE",
    manifesto_path = paste0(folder_path, "/", files_in_directory[14]),
    colors = c("black", "slateblue4", "goldenrod1", "darkgreen", "red2")
  ),
  ATM = list(
    name = "African Transformation Movement - ATM",
    manifesto_path = paste0(folder_path, "/", files_in_directory[5]),
    colors = c("darkgreen", "gold", "lightblue", "black")
  ),
  ActionSA = list(
    name = "ActionSA",
    manifesto_path = paste0(folder_path, "/", files_in_directory[2]),
    colors = c("limegreen")
  ),
  IFP = list(
    name = "Inkatha Freedom Party - IFP",
    manifesto_path = paste0(folder_path, "/", files_in_directory[11]),
    colors = c("red", "gold", "green4", "black")
  ),
  GOOD = list(
    name = "GOOD Party - GOOD",
    manifesto_path = paste0(folder_path, "/", files_in_directory[10]),
    colors = c("orange")
  ),
  ACDP = list(
    name = "African Christian Democratic Party - ACDP",
    manifesto_path = paste0(folder_path, "/", files_in_directory[1]),
    colors = c("turquoise", "red3")
  ),
  PA = list(
    name = "Patriotic Alliance - PA",
    manifesto_path = paste0(folder_path, "/", files_in_directory[12]),
    colors = c("black", "lightblue", "brown", "darkgreen", "red", "gold")
  ),
  PRM = list(
    name = "People's Revolutionary Movement - PRM",
    manifesto_path = paste0(folder_path, "/", files_in_directory[13]),
    colors = c("springgreen3", "black")
  )
)
}

# Generate a list of political party names for selection in a Shiny app
political_party_choices<-NULL

for (i in 1:length(political_parties) ){
  political_party_name <- political_parties[[i]]$name
  political_party_choices<-append(political_party_choices,political_party_name)
}



# Function to create and preprocess a corpus from a PDF or JSON file
createCorpus <- function(pdfFilePath) {
  text <- if (grepl(".json", pdfFilePath)) {
    json_data <- fromJSON(pdfFilePath)
    json_data$analyzeResult$content
  } else {
    pdf_text(pdfFilePath)
  }
  
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_filter(corpus, function(x) length(unlist(strsplit(as.character(x), " "))) > 0)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[\\*•\\*●*\\*▶*\\*–*\\*▪*]", replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "^\\d+\\.\\s*", replacement = "")
  return(corpus)
}

# Function to generate and display a word cloud
createWordCloud <- function(corpus, numWords, number_of_colors, color_vector) {
  group_size <- numWords / number_of_colors
  tdm <- TermDocumentMatrix(corpus)
  m <- as.matrix(tdm)
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  df_word_freqs <- data.frame(word = names(word_freqs), freq = word_freqs)
  
  df_word_freqs <- df_word_freqs %>%
    arrange(desc(freq)) %>%
    mutate(rank = row_number(), color_group = ((rank - 1) %/% group_size) + 1)
  
  sa_colors <- color_vector
  df_word_freqs$color <- sa_colors[((df_word_freqs$color_group - 1) %% length(sa_colors)) + 1]
  
  wordcloud(words = df_word_freqs$word, 
            freq = df_word_freqs$freq,
            scale = c(4, 0.8), 
            min.freq = 5,
            max.words = numWords,
            random.order = FALSE, 
            rot.per = 0.35, 
            colors = df_word_freqs$color, 
            ordered.colors = TRUE, vfont = c("serif", "plain"), font = 3)
}


# Function to check if the result is an empty integer vector with a names attribute
is_named_integer_0 <- function(x) {
  is.integer(x) && length(x) == 0 && !is.null(names(x))
}

# Function to create an LDA model
createLDAmodel <- function(corpus, numTopics = 5) {
  dtm <- DocumentTermMatrix(corpus)
  sparse_dtm <- removeSparseTerms(dtm, 0.999)
  empty_docs <- which(rowSums(as.matrix(sparse_dtm)) == 0)
  filtered_dtm <- if (is_named_integer_0(empty_docs)) sparse_dtm else sparse_dtm[-empty_docs, ]
  lda_model <- LDA(filtered_dtm, k = numTopics)
  return(lda_model)
}


# Function to run LDA and visualize the model
createLDAVisualization <- function(corpus, numTopics = 5, termsPerTopic = 10) {
  dtm <- DocumentTermMatrix(corpus)
  sparse_dtm <- removeSparseTerms(dtm, 0.999)
  empty_docs <- which(rowSums(as.matrix(sparse_dtm)) == 0)
  filtered_dtm <- if (is_named_integer_0(empty_docs)) sparse_dtm else sparse_dtm[-empty_docs, ]
  lda_model <- LDA(filtered_dtm, k = numTopics)
  topics <- terms(lda_model, termsPerTopic)
  posterior_results <- posterior(lda_model)
  theta <- posterior_results$topics
  phi <- posterior_results$terms
  doc_lengths <- rowSums(as.matrix(filtered_dtm))
  vocab <- colnames(as.matrix(filtered_dtm))
  term_frequency <- colSums(as.matrix(filtered_dtm))
  
  if (length(doc_lengths) == nrow(theta)) {
    LDAvis::createJSON(
      phi = phi,
      theta = theta,
      doc.length = doc_lengths,
      vocab = vocab,
      term.frequency = term_frequency,
      R = termsPerTopic
    )
  } else {
    print("Mismatch in the number of documents")
    print(paste("Doc lengths:", length(doc_lengths)))
    print(paste("Theta rows:", nrow(theta)))
  }
}

# Extract top terms from each topic
extract_top_terms <- function(phi, vocab, n = 10) {
  top_terms <- apply(phi, 2, function(topic) {
    top_indices <- order(topic, decreasing = TRUE)[1:n]
    data.frame(term = vocab[top_indices], probability = topic[top_indices])
  })
  
  top_terms <- do.call(rbind, lapply(1:length(top_terms), function(i) {
    top_terms[[i]]$topic <- i
    top_terms[[i]]
  }))
  
  return(top_terms)
}



# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "journal"),
  useShinyjs(),
  tags$div(
    style = "position: fixed; width: 100%; height: 100%; z-index: -1; opacity: 0.05; background-image: url('https://upload.wikimedia.org/wikipedia/commons/thumb/a/af/Flag_of_South_Africa.svg/1920px-Flag_of_South_Africa.svg.png'); background-size: cover;"
  ),
  tags$style(
    HTML("
      navbar {
        background-color: #FF69B4;
        color: white;
      }
      .well {
        background-color: #FDEEF4;
      }
    ")
  ),
  br(),
  titlePanel(
    title = div(
      img(src = "https://upload.wikimedia.org/wikipedia/commons/a/af/Flag_of_South_Africa.svg", height = "50px"),
      "Political Manifesto Analysis"
    )
  ),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput("political_party_id", "Select a Political Party", choices = political_party_choices, selected = "African National Congress - ANC"),
      br(),
      selectInput("word_num", "Number of Words in Word Cloud:", choices = c(25, 50, 75), selected = 50),
      br(),
      sliderInput("nTerms", "Number of terms to display", min = 5, max = 10, step = 1, value = 10),
      br(),
      sliderInput("nTopics", "Number of topics to display", min = 3, max = 9, step = 1, value = 5),
      width = 2,
      height = 700
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Analysis",
          fluidRow(
            column(5,div(h3("Word Cloud"), style = "text-align: center;"), plotOutput("wordcloud", height = 700)),
            column(7,div(h3("Top Terms per Topic"), style = "text-align: center;"),  plotOutput("top_terms_plot", height = 700))
          )
        ),
        tabPanel("LDA Visualization", visOutput("lda_vis"))
      ),
      width = 10
    )
  )
)

# Server
server <- function(input, output) {
  
  selected_party_reactive <- reactive({
    selected_party <- political_parties[[which(sapply(political_parties, function(party) party$name == input$political_party_id))]]
    return(selected_party)
  })
  
  manifesto_pdf_path <- reactive({
    selected_party_reactive()$manifesto_path
  })
  
  manifesto_number_of_cols <- reactive({
    length(selected_party_reactive()$colors)
  })
  
  manifesto_color_vector <- reactive({
    selected_party_reactive()$colors
  })
  
  manifesto_corpus <- reactive({
    createCorpus(manifesto_pdf_path())
  })
  
  # Render the word cloud plot
  output$wordcloud <- renderPlot({
    createWordCloud(manifesto_corpus(), numWords = as.numeric(input$word_num), number_of_colors = manifesto_number_of_cols(), color_vector = manifesto_color_vector())
  })
  
  output$lda_vis <- renderVis({
    if (!is.null(input$nTerms)) {
      createLDAVisualization(manifesto_corpus(), numTopics = input$nTopics, termsPerTopic = input$nTerms)
    }
  })
  
  output$top_terms_plot <- renderPlot({
    if (!is.null(input$nTerms)) {
      corpus <- manifesto_corpus()
      dtm <- DocumentTermMatrix(corpus)
      sparse_dtm <- removeSparseTerms(dtm, 0.999)
      empty_docs <- which(rowSums(as.matrix(sparse_dtm)) == 0)
      filtered_dtm <- if (is_named_integer_0(empty_docs)) sparse_dtm else sparse_dtm[-empty_docs, ]
      lda_model <- LDA(filtered_dtm, k = input$nTopics)
      topics <- tidy(lda_model)
      plotinput <- topics %>%
        mutate(topic = as.factor(paste0('Topic ', topic))) %>%
        group_by(topic) %>%
        top_n(input$nTerms, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
      names <- levels(unique(plotinput$topic))
      colors <- rep(manifesto_color_vector(), length.out = length(names))
      plist <- list()
      for (i in 1:length(names)) {
        d <- subset(plotinput, topic == names[i])[1:input$nTerms, ]
        d$term <- factor(d$term, levels = d[order(d$beta), ]$term)
        p1 <- ggplot(d, aes(x = term, y = beta, width = 0.75)) +
          labs(y = NULL, x = NULL, fill = NULL) +
          geom_bar(stat = "identity", fill = colors[i]) +
          facet_wrap(~topic) +
          coord_flip() +
          guides(fill = FALSE) +
          theme_bw() +
          theme(
            strip.background = element_blank(),
            panel.grid.major = element_line(colour = "grey80"),
            panel.border = element_blank(),
            axis.ticks = element_line(size = 0),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.text = element_text(size = 11),
            strip.text = element_text(size = 16)
          ) +
          scale_y_continuous(labels = percent_format(accuracy = 0.1))+
          theme(legend.position = "bottom")
        plist[[names[i]]] <- p1
      }
      do.call("grid.arrange", c(plist, ncol = 3))
    }
  })
}

shinyApp(ui, server)



