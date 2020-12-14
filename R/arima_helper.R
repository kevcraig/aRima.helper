#' Create an ARIMA UI to test terms
#'
#' Simplifies ARIMA modeling by providing a user interface and immediate results. User can specify non-seasonal and or/seasonal AR, MA, and difference terms as well as dummy & Fourier terms. Stationary test, model convergence, fit statistics, ACF & PACF plots, residual plots, and white noise plots all provided. Function takes a time-series object with or without associated frequency. Create time series object with ts()
#' @param time_series_object time-series
#' @return Interactive ARIMA testing shiny app
#' @export

aRima_helper <- function(time_series_object){
  # data validation
  if(class(time_series_object) != 'ts'){
    stop(paste0('Enter time-series object. Object entered is class', class(time_series_object)))
  }
  # non seasonal model
  if(stats::frequency(time_series_object) == 1){
    # Non Seasonal Server ------------------------------------------------------------------
    server_non_seasonal <- function(input, output){

      # get train
      train <- shiny::reactive({
        return(time_series_object)
      })

      # get period
      period <- shiny::reactive({
        stats::frequency(train())
      })

      # stationary test for non-seasonal data
      output$stationary <- shiny::renderText({
        diffs <- forecast::ndiffs(train(), test = 'adf')
        if(diffs == 0){
          shiny::HTML('Time Series Stationary')
        }else{
          shiny::HTML(paste0('Not stationary - differences of ', diffs, ' suggested'))
        }
      })

      # model setup
      model <- shiny::reactive({
        # ar ma diff terms
        ar <- input$choose_AR
        diff <- input$choose_DIFF
        ma <- input$choose_MA
        order_ts <- c(ar, diff, ma)

        # build model
        forecast::Arima(train(),
                        order = order_ts,
                        method = 'ML')
      })

      # model output summary / metrics
      output$summary <- shiny::renderTable({
        AIC <- base::as.data.frame(model()$aic)
        BIC <- base::as.data.frame(model()$bic) # same as log liklihood?
        fit <- base::as.data.frame(summary(model()))
        summ_final <- base::cbind(AIC, BIC, fit)
        base::colnames(summ_final) <- c('AIC', 'BIC', 'ME', 'RMSE','MAE', 'MPE', 'MAPE', 'MASE', 'ACF1')
        base::subset(summ_final, select = c('AIC', 'BIC', 'RMSE', 'MAE', 'MAPE'))
      })

      # acf plot
      output$acf <- shiny::renderPlot({
        stats::acf(model()$residuals, main = "Residual Autocorrelation")
      })

      # pacf plot
      output$pacf <- shiny::renderPlot({
        stats::pacf(model()$residuals, main = "Residual Partial Autocorrelation")
      })

      # white noise
      output$wn <- shiny::renderPlot({

        # ar ma terms
        ar <- input$choose_AR
        ma <- input$choose_MA

        # LB test df calc
        lb_df <- base::sum(ar,ma, na.rm = TRUE)

        # Produce LB test for up up to 50 lags
        White.LB_resid <- base::rep(NA, (50 - lb_df))
        for(i in 1:base::length(White.LB_resid)){
          White.LB_resid[i] <- stats::Box.test(model()$residuals, lag = i, type = "Lj", fitdf = lb_df)$p.value
        }
        # cutoff at p = .2 for graphing
        White.LB_resid <- base::pmin(White.LB_resid, 0.2)

        # graph LB test
        graphics::barplot(White.LB_resid,
                          main = paste0("Ljung-Box Test for White Noise \n", "DF = ", lb_df),
                          ylab = "Probabilities",
                          xlab = "Lags",
                          ylim = c(0, 0.2),
                          names.arg = base::seq(lb_df, 49))
        graphics::abline(h = 0.01, lty = "dashed", col = "black")
        graphics::abline(h = 0.05, lty = "dashed", col = "black")
      })

      # residuals and actual
      output$resid_plot <- shiny::renderPlot({
        graphics::plot(model()$x,type="l",col="grey", xlab = 'Time', ylab = 'Value')
        graphics::lines(model()$fitted,col="red")
        graphics::legend('topright', legend=c("Actual", "Fitted"),
                         col=c("grey", "red"), cex=0.8, lty=1)
      })

      # convergence
      output$converge <- shiny::renderText({
        if(model()$code == 0){
          shiny::HTML("<b> Model Converged </b>")
        } else {
          shiny::HTML("<b> Model NOT Converged </b>")
        }
      })

      # model form
      output$form <- shiny::renderText({
        form <- paste0('ARIMA Form: (', input$choose_AR, ',', input$choose_DIFF, ',', input$choose_MA, ')')
        shiny::HTML("<b>",paste(form),"</b>")
      })
    }
    # Non-Seasonal UI ----------------------------------------------------------------------

    ui_non_seasonal <- shiny::fluidPage(

      ### inputs
      shiny::fluidRow(
        shiny::h3('Interactive ARIMA Testing', align = 'center'),
        shiny::column(3,offset = 0, shiny::numericInput("choose_AR",label = 'AR Terms', value = 0, min = 0, step = 1)),
        shiny::column(4,offset = 0, shiny::numericInput("choose_DIFF",label = 'Diff Terms', value = 0, min = 0, step = 1)),
        shiny::column(5,offset = 0, shiny::numericInput("choose_MA",label = 'MA Terms', value = 0, min = 0, step = 1))),

      shiny::tags$hr(style="border-color: black;"),

      ### outputs
      shiny::fluidRow(shiny::column(3, offset = 0,
                             shiny::span(shiny::htmlOutput("converge")),
                             shiny::span(shiny::htmlOutput("stationary"), style="color:red")),
                      shiny::column(4, offset = 1, align = 'center', shiny::HTML('<h3> Diagnostics </h3>')),
                      shiny::column(4, offset = 0,  shiny::htmlOutput('form'))),
      shiny::fluidRow(shiny::column(
        12,
        offset = 0,
        align = 'center',
        shiny::tableOutput("summary")
      )),
      shiny::plotOutput("resid_plot"),
      shiny::fluidRow(shiny::column(6, offset = 0,
                                    shiny::plotOutput("acf")),
                      shiny::column(6, offset = 0,
                                    shiny::plotOutput('pacf'))),
      shiny::plotOutput('wn')
    )

    # Non-Seasonal Shiny App ---------------------------------------------------------------
    shiny::shinyApp(ui_non_seasonal,server_non_seasonal)

  }
  # seasonal model
  else if(stats::frequency(time_series_object) > 1){
    # Seasonal Server ------------------------------------------------------------------
    server_seasonal <- function(input, output){

      # get train
      train <- shiny::reactive({
        return(time_series_object)
      })

      # get period
      period <- shiny::reactive({
        stats::frequency(train())
      })

      # stationary test
      output$stationary <- shiny::renderText({
        diffs <- forecast::nsdiffs(train())
        if(diffs == 0){
          shiny::HTML('Time Series Stationary')
        }else{
          shiny::HTML(paste0('Not stationary - seasonal differences of ', diffs, ' suggested'))
        }
      })

      # model setup
      model <- shiny::reactive({
        # ar ma sar sma diff sdiff terms
        ar <- input$choose_AR
        diff <- input$choose_DIFF
        ma <- input$choose_MA
        sar <- input$choose_SAR
        sdiff <- input$choose_SDIFF
        sma <- input$choose_MA
        order_ts <- c(ar, diff, ma)
        order_seasonal_ts <- c(sar, sdiff, sma)

        # dummy terms
        dummy_terms <- forecast::seasonaldummy(train())

        # fourier functions - include period / 2
        fourier_terms <- forecast::fourier(train(), K = floor(period() / 2))

        ### Build models

        # if  fourier terms are selected
        if (input$radio_input == 3) {
          forecast::Arima(
            train(),
            order = order_ts,
            seasonal = order_seasonal_ts,
            xreg = fourier_terms,
            method = 'ML'
          )
        }
        # if dummy terms are selected
        else if (input$radio_input == 2) {
          forecast::Arima(
            train(),
            order = order_ts,
            seasonal = order_seasonal_ts,
            xreg = dummy_terms,
            method = 'ML'
          )
        }
        # if no special terms is selected
        else if(input$radio_input == 1) {
          forecast::Arima(train(),
                          order = order_ts,
                          seasonal = order_seasonal_ts,
                          method = 'ML')
        }
      })

      # model output summary / metrics
      output$summary <- shiny::renderTable({
        AIC <- base::as.data.frame(model()$aic)
        BIC <- base::as.data.frame(model()$bic) # same as log liklihood?
        fit <- base::as.data.frame(summary(model()))
        summ_final <- base::cbind(AIC, BIC, fit)
        base::colnames(summ_final) <- c('AIC', 'BIC', 'ME', 'RMSE','MAE', 'MPE', 'MAPE', 'MASE', 'ACF1')
        base::subset(summ_final, select = c('AIC', 'BIC', 'RMSE', 'MAE', 'MAPE'))
      })

      # acf plot
      output$acf <- shiny::renderPlot({
        stats::acf(model()$residuals, main = "Residual Autocorrelation")
      })

      # pacf plot
      output$pacf <- shiny::renderPlot({
        stats::pacf(model()$residuals, main = "Residual Partial Autocorrelation")
      })

      # white noise
      output$wn <- shiny::renderPlot({

        # ar ma terms
        ar <- input$choose_AR
        ma <- input$choose_MA
        sar <- input$choose_SAR
        sma <- input$choose_SMA

        # LB test df calc
        lb_df <- base::sum(ar,ma,sar,sma, na.rm = TRUE)

        # produce LB test for lags = 2 * time series frequency
        White.LB_resid <- base::rep(NA, (2* period()) - lb_df)
        for(i in 1:base::length(White.LB_resid)){
          White.LB_resid[i] <- stats::Box.test(model()$residuals, lag = i, type = "Lj", fitdf = lb_df)$p.value
        }
        # cutoff at p = .2 for graphing
        White.LB_resid <- base::pmin(White.LB_resid, 0.2)

        # graph LB test
        graphics::barplot(White.LB_resid,
                          main = paste0("Ljung-Box Test for White Noise \n", "DF = ", lb_df),
                          ylab = "Probabilities",
                          xlab = "Lags",
                          ylim = c(0, 0.2),
                          names.arg = base::seq(lb_df, (2*period()-1)))
        graphics::abline(h = 0.01, lty = "dashed", col = "black")
        graphics::abline(h = 0.05, lty = "dashed", col = "black")
      })

      # residuals and actual
      output$resid_plot <- shiny::renderPlot({
        graphics::plot(model()$x,type="l",col="grey", xlab = 'Time', ylab = 'Value')
        graphics::lines(model()$fitted,col="red")
        graphics::legend('topright', legend=c("Actual", "Fitted"),
                         col=c("grey", "red"), cex=0.8, lty=1)
      })

      # convergence
      output$converge <- shiny::renderText({
        if(model()$code == 0){
          shiny::HTML("<b> Model Converged </b>")
        } else {
          shiny::HTML("<b> Model NOT Converged </b>")
        }
      })

      # model form
      output$form <- shiny::renderText({
        form <- base::paste0('ARIMA Form: (', input$choose_AR, ',', input$choose_DIFF, ',', input$choose_MA, '), (', input$choose_SAR, ',', input$choose_SDIFF, ',', input$choose_SMA, ')')
        shiny::HTML("<b>",paste(form),"</b>")
      })
    }


    # Seasonal UI ----------------------------------------------------------------------

    ui_seasonal <- shiny::fluidPage(

      ### inputs
      shiny::fluidRow(
        shiny::h3('Interactive ARIMA Testing', align = 'center'),
        shiny::column(3,offset = 0,
                      shiny::numericInput("choose_AR",label = 'AR Terms', value = 0, min = 0, step = 1),
                      shiny::numericInput("choose_DIFF",label = 'Diff Terms', value = 0, min = 0, step = 1),
                      shiny::numericInput("choose_MA",label = 'MA Terms', value = 0, min = 0, step = 1)),
        shiny::column(4,offset = 1,
                      shiny::numericInput("choose_SAR",label = 'Seasonal AR Terms', value = 0, min = 0, step = 1),
                      shiny::numericInput("choose_SDIFF",label = 'Seasonal Diff Terms', value = 0, min = 0, step = 1),
                      shiny::numericInput("choose_SMA",label = 'Seasonal MA Terms', value = 0, min = 0, step = 1)),
        shiny::column(3, offset = 0,
                      shiny::radioButtons("radio_input", label = shiny::h5("Special Terms"),
                                          choices = list("None" = 1, "Dummies" = 2, "Fourier" = 3),
                                          selected = 1))),
      shiny::tags$hr(style="border-color: black;"),

      ### outputs
      shiny::fluidRow(shiny::column(3, offset = 0,
                                    shiny::span(shiny::htmlOutput("converge")),
                                    shiny::span(shiny::htmlOutput("stationary"), style="color:red")),
                      shiny::column(4, offset = 1, align = 'center', shiny::HTML('<h3> Diagnostics </h3>')),
                      shiny::column(4, offset = 0,  shiny::htmlOutput('form'))),
      shiny::fluidRow(shiny::column(
        12,
        offset = 0,
        align = 'center',
        shiny::tableOutput("summary")
      )),
      shiny::plotOutput("resid_plot"),
      shiny::fluidRow(shiny::column(6, offset = 0,
                                    shiny::plotOutput("acf")),
                      shiny::column(6, offset = 0,
                                    shiny::plotOutput('pacf'))),
      shiny::plotOutput('wn')
    )
    # Seasonal Shiny App ---------------------------------------------------------------
    shiny::shinyApp(ui_seasonal,server_seasonal)

  }
}
