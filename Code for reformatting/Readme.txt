FASTA Header Reformatter

This R Shiny application offers a straightforward tool to reformat FASTA file headers. It's designed specifically for bioinformatics tasks where a consistent header format is required for downstream analysis, particularly by moving organism names to the beginning of the header.
Features

    FASTA File Upload: Easily upload your .fasta, .fa, or .txt files.

    Sample Data: Quickly test the application's functionality using a built-in sample FASTA sequence.

    Header Reformatting: Transforms FASTA headers from a general format like >ID description [Organism] Sequence to a more structured format: >Organism_with_underscores ID description Sequence.

    Live Previews: View both the original input FASTA and the reformatted FASTA directly within the application's interface.

    Download Reformatted File: Download the entire FASTA file with the updated headers.

How it Works

The application processes each header line (starting with >) in your FASTA file.

    It identifies and extracts the organism name, which is expected to be enclosed in square brackets (e.g., [Escherichia phage AlfredRasser]).

    Spaces within the extracted organism name are replaced with underscores (e.g., Escherichia_phage_AlfredRasser).

    The original ID and description parts of the header are retained.

    A new header is constructed by placing the reformatted organism name at the beginning, followed by the original ID and description.

    The sequence associated with each header remains untouched.

Example
Input FASTA Header Format:

>QXV75782.1:1-167 i-spanin [Escherichia phage AlfredRasser] MKTSLIKLTVCLLGGVALGATLFLLGSQHGEKKVQALWDED...

Reformatted Output Header Format:

>Escherichia_phage_AlfredRasser QXV75782.1:1-167 i-spanin MKTSLIKLTVCLLGGVALGATLFLLGSQHGEKKVQALWDED...

Installation and Usage (Local R Environment)

To run this application on your local machine, you'll need R and RStudio (recommended for ease of use).

    Install Required Packages: If you don't already have them, install the necessary R packages from CRAN:

    install.packages(c("shiny", "stringr"))

    Save the Code: Copy the entire R code provided into a file named app.R.

    Run the App: Open app.R in RStudio and click the "Run App" button, or execute the following command in your R console:

    library(shiny)
    runApp("app.R")

The application will then launch in your default web browser.
Contributing

Feel free to suggest improvements or report any issues you encounter!