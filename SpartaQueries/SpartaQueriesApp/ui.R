ui <- bootstrapPage(
  wellPanel(h2("Arxiu mare"),
    helpText("Exemple: original.xlsx"),
    fileInput("file1", ""),
    uiOutput("tablenames1list")
  ),
  wellPanel(h2("Arxiu nou"),
    helpText("Exemple: DataProblem.xlsx"),
    fileInput("file2", ""),
    uiOutput("tablenames2list")
  ), 
  wellPanel(h2("Arxiu resultat"),
    downloadButton('down', 'Descargar')
  )
)