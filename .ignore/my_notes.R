# start app
#install.packages('devtools')
devtools::load_all(path = "../metamet")
shiny::runApp(system.file("shinyApp/metqc_app", package = "metamet"))

