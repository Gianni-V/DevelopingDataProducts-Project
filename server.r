library(shiny) 

# lambdas contains the Poisson's law lambda for 10-minute interval from 00:00 to 23:50.
# 1st col TIME10 contains "00:00", "00:10"...
# 2nd col LAMBDA contains the lambdas ie mean number of e-mails for each interval.
lambdas <- read.csv(file = "mail_final_2.csv", header = T, colClasses = c("factor", "numeric"))

shinyServer( function(input, output) {
    # it plots the lambdas and draws a red line for the lambda of the interval the user chooses.
    output$lambdas <- renderPlot({
        plot(lambdas, xlab = "Time", ylab = "Poisson law's lambda")
        abline(v = which(lambdas$TIME10 == input$time10), col = "red")
    })
    # it plots the Poisson distribution with the correct lambda and draws a red line for 
    # the number of e-mails the user chooses.
    output$poisson <- renderPlot({
        b <- barplot(dpois(0:30, lambdas[which(lambdas$TIME10 == input$time10),]$LAMBDA), xlab = "Number of e-mails received", ylab = "Probability")
        axis(1, at=b, labels = 0:30)
        delta <- b[2]-b[1] # little positioning quirk
        abline(v = input$nbemails * delta + delta/2, col = "red", lwd = 2)
    })
    # it displays the computed probability
    output$result <- renderPrint({dpois(input$nbemails, lambdas[which(lambdas$TIME10 == input$time10),]$LAMBDA)})
} )
