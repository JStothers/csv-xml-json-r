# This script converts a CSV patient data file with headers to XML or JSON, depending on
# the command line argument supplied.
#
# This script will be executed by calling:
#
#    ```
#    Rscript src/lab1.R xml/json <filename>
#    ```
#
# Output of XML and JSON should be printed to stdout.

require(jsonlite)
require(XML)

library(jsonlite)
library(XML)

parse_csv <- function(filename) {
  # Parse a CSV file by separating it into headers and additional data.
  #
  # Args
  # filename: A path to a CSV file.
  #     
  # Returns
  # An R structure containing the headers from the csv file and the data encoded as characters.
  return(read.csv(file = filename, header = TRUE, sep = ",", colClasses = c('character')))
}

#https://stackoverflow.com/questions/35234863/how-convert-a-data-frame-into-a-xml-file-with-r
#http://r.789695.n4.nabble.com/Convert-data-frame-to-XML-Tree-td1478281.html

to_xml <- function(df) {
  #
  # Convert a data frame to XML format.
  #
  # Args
  # df: A dataframe.
  #     
  # Returns
  # An stdout of the dataframe in XML format.
  #
  x <- xmlTree("records") #start an XML tree with "records" as the root node
  for (i in 1:nrow(df)) { #iterate over dataframe
    x$addTag("patient", close=FALSE) #parent tag of dataframe row is "patient"
    for (j in names(df)) {
      x$addTag(j, df[i, j]) #create a tag from column name for each cell in row
    }
    x$closeTag()
  }
  x$closeTag()
  cat(saveXML(x)) #write XML tree to string
}

to_json <- function(df){
  #
  # Convert a data frame to JSON format.
  #
  # Args
  # df: A dataframe.
  #     
  # Returns
  # An stdout of the dataframe in JSON format.
  #
  df$patient = df # create column named patient within dataframe that contains entire dataframe
  df <- df["patient"] # only use subset of dataframe with patient in column name
  cat(toJSON(list("records" = df), pretty = TRUE)) # name whole dataframe records, then perform toJSON
}


#https://swcarpentry.github.io/r-novice-inflammation/05-cmdline/
main <- function() {
  #
  # Converts a CSV file with headers to XML or JSON, depending on the command line argument supplied.
  #
  # Executed by calling:
  #
  #    ```
  #    Rscript src/lab1.py xml/json <filename>
  #    ```
  #
  # Output of XML and JSON are printed to stdout.
  #
  args <- commandArgs(trailingOnly = TRUE)  
  # trailingOnly=TRUE means that only your arguments are returned
  filename <- args[2]
  data <- parse_csv(filename)
  if (args[1] == "xml"){
    to_xml(data)
  } else if (args[1] == "json"){
    to_json(data)
  }
}

main()
