library(shiny)
library(stringr)

# Define the UI
ui <- fluidPage(
  titlePanel("FASTA Header Reformatter"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fastaFile", "Upload FASTA File",
                accept = c(".fasta", ".fa", ".txt")),
      hr(),
      checkboxInput("sampleData", "Use sample data", FALSE),
      hr(),
      downloadButton("downloadData", "Download Reformatted File"),
      hr(),
      helpText("This app takes FASTA headers with format:"),
      tags$code(">ID description [Organism] Sequence"),
      helpText("And reformats them to:"),
      tags$code(">Organism_with_underscores ID description Sequence")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Input Preview", 
                 h3("Original FASTA"),
                 verbatimTextOutput("originalFasta")),
        tabPanel("Output Preview", 
                 h3("Reformatted FASTA"),
                 verbatimTextOutput("reformattedFasta")),
        tabPanel("About", 
                 h3("How it works"),
                 p("This app reformats FASTA headers by:"),
                 tags$ol(
                   tags$li("Extracting the text within square brackets [organism name]"),
                   tags$li("Replacing spaces with underscores in the organism name"),
                   tags$li("Moving the organism name to the beginning of the header"),
                   tags$li("Keeping the rest of the header and sequence intact")
                 ),
                 hr(),
                 h4("Example:"),
                 tags$b("Input:"),
                 tags$pre(">QXV75782.1:1-167 i-spanin [Escherichia phage AlfredRasser] MKTSLIKLTVCLLGGVALGATLFLLGSQHGEKKVQALWDED..."),
                 tags$b("Output:"),
                 tags$pre(">Escherichia_phage_AlfredRasser QXV75782.1:1-167 i-spanin MKTSLIKLTVCLLGGVALGATLFLLGSQHGEKKVQALWDED...")
        )
      )
    )
  )
)

# Define the server
server <- function(input, output, session) {
  
  # Create reactive value for the fasta data
  fastaData <- reactive({
    if(input$sampleData) {
      return(">QXV75782.1:1-167 i-spanin [Escherichia phage AlfredRasser] MKTSLIKLTVCLLGGVALGATLFLLGSQHGEKKVQALWDEDKKEYSKELDRVKLAYDQKNREHSYEVGQLTLRLDKAKES\nYEVVIDSITNQYNSRLLQSEKRAESYKRQASTGTTQCLNLASHAARLDSSLEEGRRLVEELKATVRLRDNQLIELGNQIR\nSDRKLFE\n>QXV75783.1:1-100 o-spanin [Escherichia phage ExamplePhage] MTKTSLAVTFCVMGLASFALATLFMVVGLKGQKRVEHLWEWDKKEYSRELTREKADYDLKNRKATYEIGQLTLRLDKAKMS\nYEVIIDSVTRQYNGLLLP")
    }
    
    req(input$fastaFile)
    readLines(input$fastaFile$datapath, warn = FALSE) |> 
      paste(collapse = "\n")
  })
  
  # Process and reformat the fasta data
  processedFasta <- reactive({
    data <- fastaData()
    
    # Split the data into lines
    lines <- strsplit(data, "\n")[[1]]
    
    # Process the header lines (starting with ">")
    for (i in seq_along(lines)) {
      if (grepl("^>", lines[i])) {
        # Extract organism name from square brackets
        organism_match <- str_match(lines[i], "\\[(.*?)\\]")
        if (!is.na(organism_match[1, 2])) {
          organism_name <- organism_match[1, 2]
          organism_name <- gsub(" ", "_", organism_name)
          
          # Remove the organism part from the original line
          header_without_organism <- gsub("\\s*\\[.*?\\]\\s*", " ", lines[i])
          
          # Remove the initial ">" 
          header_without_organism <- substr(header_without_organism, 2, nchar(header_without_organism))
          
          # Create the new header
          lines[i] <- paste0(">", organism_name, " ", header_without_organism)
        }
      }
    }
    
    # Join back into a single string
    paste(lines, collapse = "\n")
  })
  
  # Display the original fasta
  output$originalFasta <- renderText({
    fastaData()
  })
  
  # Display the reformatted fasta
  output$reformattedFasta <- renderText({
    processedFasta()
  })
  
  # Handle the download
  output$downloadData <- downloadHandler(
    filename = function() {
      if (!is.null(input$fastaFile)) {
        paste0("reformatted_", input$fastaFile$name)
      } else {
        "reformatted_sequences.fasta"
      }
    },
    content = function(file) {
      writeLines(processedFasta(), file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)