# Political Manifesto Analysis Shiny App

## Description

This Shiny app allows users to analyze and visualize the political manifestos of various South African political parties. The app generates word clouds and Latent Dirichlet Allocation (LDA) topic models to help users understand the key themes and terms in each manifesto.

## Features

- **Word Cloud Visualization**: Displays the most frequent words in the selected political manifesto.
- **LDA Topic Modeling**: Visualizes topics extracted from the manifesto using LDA.
- **Customizable Parameters**: Users can select the number of words for the word cloud, the number of topics, and the number of terms per topic.

## Prerequisites

The app requires the following R packages:

- `shiny`
- `bslib`
- `shinyjs`
- `tm`
- `pdftools`
- `topicmodels`
- `LDAvis`
- `wordcloud`
- `ggplot2`
- `tidyverse`
- `tidytext`
- `gridExtra`
- `jsonlite`
- `scales`

## Installation

To install and load the required packages, the following function is used:

```r
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
  "tidyverse", "tidytext", "gridExtra", "jsonlite", "scales"
)

# Install and load required packages
sapply(required_packages, install_and_load)
```

## Setup

1. **Working Directory and Paths**: The app assumes that the manifestos are stored in a folder named `Collection_of_Manifestos` in the working directory.

```r
working_directory <- getwd()
folder_path <- paste0(working_directory, '/Collection_of_Manifestos')
files_in_directory <- list.files(path = folder_path)
```

2. **Define Political Parties**: A list of political parties and their corresponding manifesto files and colors is defined.

```r
political_parties <- list(
  # Define each party's name, manifesto path, and colors here...
)
```

3. **Generate Political Party Choices**: The app generates a list of political party names for selection.

```r
political_party_choices <- NULL
for (i in 1:length(political_parties)) {
  political_party_name <- political_parties[[i]]$name
  political_party_choices <- append(political_party_choices, political_party_name)
}
```

## Functions

### createCorpus

This function creates and preprocesses a text corpus from a PDF or JSON file.

```r
createCorpus <- function(pdfFilePath) {
  # Function implementation...
}
```

### createWordCloud

This function generates and displays a word cloud.

```r
createWordCloud <- function(corpus, numWords, number_of_colors, color_vector) {
  # Function implementation...
}
```

### createLDAmodel

This function creates an LDA model.

```r
createLDAmodel <- function(corpus, numTopics = 5) {
  # Function implementation...
}
```

### createLDAVisualization

This function runs LDA and visualizes the model.

```r
createLDAVisualization <- function(corpus, numTopics = 5, termsPerTopic = 10) {
  # Function implementation...
}
```

### extract_top_terms

This function extracts the top terms from each topic.

```r
extract_top_terms <- function(phi, vocab, n = 10) {
  # Function implementation...
}
```

## Shiny App Structure

### UI

The UI consists of a sidebar for user input and a main panel for displaying the results.

```r
ui <- fluidPage(
  # UI implementation...
)
```

### Server

The server contains the logic to process user inputs, generate the word cloud, and create the LDA visualization.

```r
server <- function(input, output) {
  # Server implementation...
}
```

### Running the App

To run the app, use the `shinyApp` function:

```r
shinyApp(ui, server)
```

## Additional Files

Include the following in your GitHub repository:

- **app.R**: Contains the complete Shiny app code.
- **README.md**: This readme file.
- **Collection_of_Manifestos/**: A folder containing the manifesto files.
- **Dependencies**: A script or documentation listing all dependencies and installation instructions.

## How to Contribute

If you wish to contribute to this project, please fork the repository and submit a pull request with your changes. Ensure your code follows the project's style guidelines and is well-documented.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

---

This `README.md` file provides a detailed explanation of the project, setup instructions, and the structure of the code. Make sure to adjust paths, filenames, and other details according to your specific implementation.