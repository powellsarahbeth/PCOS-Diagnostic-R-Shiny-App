#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


shinyServer(function(input, output) {
  
  output$value <- renderPrint({
    input$weight_gain
  })
  
  output$value <- renderPrint({
    input$hair_growth
  })
  
  output$value <- renderPrint({
    input$skin_darkening
  })
  
  output$value <- renderPrint({
    input$Cycle_Length 
  })
  
  output$value <- renderPrint({
    input$LH 
  })
  
  output$value <- renderPrint({
    input$Follicle_L 
  })
  
  output$value <- renderPrint({
    input$Follicle_R
  })
  
  
  newdata = function(){
    data_frame(`Weight gain(Y/N)` = input$weight_gain, `hair growth(Y/N)` = input$hair_growth, 
               `Skin darkening (Y/N)` = input$skin_darkening,`Cycle length(days)` = input$Cycle_Length,
               `LH(mIU/mL)` = input$LH, `Follicle No. (L)` = input$Follicle_L, `Follicle No. (R)` = input$Follicle_R)
  }
    
 output$PCOS_prob = renderText({
   paste("The patient's probability of a PCOS diagnosis is", round(predict(sig_feat_model2, newdata(), type = "response"), 4) * 100, "%")
 })
  

 output$PCOS_sigmoid = plotly::renderPlotly({
   ggplot(data = predicted_data, aes(x = rank, y = probability_of_PCOS, color = PCOS)) +
     geom_point(alpha = 0.6, shape = 4, stroke = 2, position = "identity") +
     scale_colour_manual(values=c("#69b3a2", "#404080"), labels = c('No PCOS', 'PCOS')) +
     xlab("Ranked Order of Predicted Values") +
     ylab("Predicted probability of having PCOS") + 
     ggtitle("Probability Function to Predict PCOS") + 
     labs(fill = "")
 })
 
 output$PCOS_Cycle_Hist = plotly::renderPlotly({
   ggplot(data = PCOS_df, aes(x = `Cycle length(days)`, fill = `PCOS (Y/N)`)) + 
   geom_histogram(color="#e9ecef", alpha=0.6, binwidth = 1, position = "identity") +
   geom_vline(aes(xintercept = input$Cycle_Length), color = "#f3969a", linetype = "dashed", linewidth = 1) +
   scale_fill_manual(values=c("#69b3a2", "#404080"), labels = c('No PCOS', 'PCOS')) + 
   xlab("Cycle Length") + 
   ylab("Frequency of Cycle Length") + 
   ggtitle("Histogram of Cycle Lengths") + 
   labs(fill = "")
})
 
 output$LH_Hist = plotly::renderPlotly({
   ggplot(data = PCOS_df, aes(x = `LH(mIU/mL)`, fill = `PCOS (Y/N)`)) + 
   geom_histogram(color="#e9ecef", alpha=0.6, binwidth = 1, position = "identity") +
   geom_vline(aes(xintercept = input$LH), color = "#f3969a", linetype = "dashed", size = 1) +
   scale_fill_manual(values=c("#69b3a2", "#404080"), labels = c('No PCOS', 'PCOS')) + 
   xlab("LH IU/mL") + 
   ylab("Frequency of LH Levels") + 
   ggtitle("Histogram of Luteinizing Hormone Levels") + 
   labs(fill = "")
})

 output$Follicle_Left = plotly::renderPlotly({
   ggplot(data = PCOS_df, aes(x = `Follicle No. (L)`, fill = `PCOS (Y/N)`)) + 
   geom_histogram(color="#e9ecef", alpha=0.6, binwidth = 1, position = "identity") +
   geom_vline(aes(xintercept = input$Follicle_L), color = "#f3969a", linetype = "dashed", size = 1) +
   scale_fill_manual(values=c("#69b3a2", "#404080"), labels = c('No PCOS', 'PCOS')) + 
   xlab("Follicle No. (L)") + 
   ylab("Frequency of Follicles Found in Left Ovary") + 
   ggtitle("Histogram of Follicles Found in Left Ovary") + 
   labs(fill = "")
 })
 
 output$Follicle_Right = plotly::renderPlotly({
   ggplot(data = PCOS_df, aes(x = `Follicle No. (R)`, fill = `PCOS (Y/N)`)) + 
   geom_histogram(color="#e9ecef", alpha=0.6, binwidth = 1, position = "identity") +
   geom_vline(aes(xintercept = input$Follicle_R), color = "#f3969a", linetype = "dashed", size = 1) +
   scale_fill_manual(values=c("#69b3a2", "#404080"), labels = c('No PCOS', 'PCOS')) + 
   xlab("Follicle No. (R)") + 
   ylab("Frequency of Follicles Found in Right Ovary") + 
   ggtitle("Histogram of Follicles Found in Right Ovary") + 
   labs(fill = "")
 })
 
 output$Weight_Gain = plotly::renderPlotly({
   ggplot(data = PCOS_df, aes(y = `Weight gain(Y/N)`, fill = `PCOS (Y/N)`)) + 
   geom_bar(width = 0.40, color="#e9ecef", alpha=0.6, position = position_dodge(0.40)) +
   geom_hline(aes(yintercept = as.numeric(input$weight_gain) + 1), color = "#f3969a", linetype = "dashed", size = 1) +
   scale_fill_manual(values=c("#69b3a2", "#404080"), labels = c('No PCOS', 'PCOS')) + 
   xlab("# of Patients") + 
   ylab("Weight Gain") + 
   ggtitle("Number of Patients Experiencing Weight Gain") + 
   labs(fill = "")
})

output$Hair_Growth = plotly::renderPlotly({  
   ggplot(data = PCOS_df, aes(y = `hair growth(Y/N)`, fill = `PCOS (Y/N)`)) + 
   geom_bar(width = 0.40, color="#e9ecef", alpha=0.6, position = position_dodge(0.40)) +
   geom_hline(aes(yintercept = as.numeric(input$hair_growth) + 1), color = "#f3969a", linetype = "dashed", size = 1) +
   scale_fill_manual(values=c("#69b3a2", "#404080"), labels = c('No PCOS', 'PCOS')) +
   xlab("# of Patients") + 
   ylab("Hair Growth") + 
   ggtitle("Number of Patients Experiencing Hair Growth") + 
   labs(fill = "")
})

 
output$Skin_Darkening = plotly::renderPlotly({
  ggplot(data = PCOS_df, aes(y = `Skin darkening (Y/N)`, fill = `PCOS (Y/N)`)) + 
  geom_bar(width = 0.40, color="#e9ecef", alpha=0.6, position = position_dodge(0.40)) +
  geom_hline(aes(yintercept = as.numeric(input$skin_darkening) + 1), color = "#f3969a", linetype = "dashed", size = 1) +
  scale_fill_manual(values=c("#69b3a2", "#404080"), labels = c('No PCOS', 'PCOS')) + 
  xlab("# of Patients") + 
  ylab("Skin Darkening") + 
  ggtitle("Number of Patients Experiencing Skin Darkening") + 
  labs(fill = "")
})

observeEvent(input$show_about,{
  showModal(modalDialog("The probability of a patient having Polycystic Ovarian Syndrome (PCOS) is calculated using a Logistic Regression model with 91.36% Accuracy, 92.66% Sensitivity and 88.69% Specificity.
                        The data used for modelling was collected from 10 different hospital across Kerala, India. This tool was built for exploratory and educational purposes only.", title = "About"))
})


})
