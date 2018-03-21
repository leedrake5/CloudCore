library(shiny)
library(pbapply)
library(reshape2)
library(TTR)
library(dplyr)
library(data.table)
library(DT)
library(ggtern)
library(ggplot2)
library(shiny)
library(random)
library(rhandsontable)
library(Bchron)
library(scales)
library(zoo)
library(Cairo)



options(shiny.maxRequestSize=180000*1024^2)




shinyServer(function(input, output, session) {
    
    rawSpectra <- reactive({
        
        withProgress(message = 'Processing Data', value = 0, {
            
            inFile <- input$file1
            
            inFile <- if(input$iszip==FALSE){
                inFile
            }else if(input$iszip==TRUE){
                unzip(inFile$datapath, list=TRUE)
            }
            
            
            if (is.null(inFile)) return(NULL)
            temp = inFile$name
            temp <- gsub(".csv", "", temp)
            id.seq <- seq(1, 2048,1)
            
            n <- length(temp)*id.seq
            
            myfiles.x = pblapply(inFile$datapath, read_csv_filename_x)
            
            myfiles.y = pblapply(inFile$datapath, read_csv_filename_y)
            
            xrf.x <- data.frame(id.seq, myfiles.x)
            colnames(xrf.x) <- c("ID", temp)
            xrf.y <- data.frame(id.seq, myfiles.y)
            colnames(xrf.y) <- c("ID", temp)
            
            
            xrf.x <- data.table(xrf.x)
            xrf.y <- data.table(xrf.y)
            
            
            energy.m <- xrf.x[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
            cps.m <- xrf.y[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
            
            
            spectra.frame <- data.frame(energy.m$value, cps.m$value, cps.m$variable)
            colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
            
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        spectra.frame

        
    })
    
    
    
    rawSpectraSecond <- reactive({
        
        withProgress(message = 'Processing Data', value = 0, {
            
            
            inFile <- input$file2
            
            inFile <- if(input$iszip==FALSE){
                inFile
            }else if(input$iszip==TRUE){
                unzip(inFile)
            }
            
            if (is.null(inFile)) return(NULL)
            temp = inFile$name
            temp <- gsub(".csv", "", temp)
            id.seq <- seq(1, 2048,1)
            
            n <- length(temp)*id.seq
            
            myfiles.x = pblapply(inFile$datapath, read_csv_filename_x)
            
            myfiles.y = pblapply(inFile$datapath, read_csv_filename_y)
            
            xrf.x <- data.frame(id.seq, myfiles.x)
            colnames(xrf.x) <- c("ID", temp)
            xrf.y <- data.frame(id.seq, myfiles.y)
            colnames(xrf.y) <- c("ID", temp)
            
            
            xrf.x <- data.table(xrf.x)
            xrf.y <- data.table(xrf.y)
            
            
            energy.m <- xrf.x[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
            cps.m <- xrf.y[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
            
            
            spectra.frame <- data.frame(energy.m$value, cps.m$value, cps.m$variable)
            colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
            
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        spectra.frame
        
        
    })
    
    
    fullSpectra <- reactive({
        
        
        
        spectra.frame <- rawSpectra()
        
        spectra.table <- if(is.null(input$file2)==TRUE){
            spectra.line.fn(spectra.frame)
        } else if(is.null(input$file2)==FALSE) {
            spectra.light.fn(spectra.frame)
        }

        

          spectra.table
    })
    
    fullSpectraSecond <- reactive({
        
        
        
        spectra.frame <- rawSpectraSecond()
        
        spectra.table <- spectra.trace.fn(spectra.frame)
        
        
        
        spectra.table
    })
    
    
    netCounts <- reactive({
        
        withProgress(message = 'Processing Data', value = 0, {
            inFile <- input$file1
            inFile <- if(input$iszip==FALSE){
                inFile
            }else if(input$iszip==TRUE){
                unzip(inFile)
            }


        if (is.null(inFile)) return(NULL)
        
        #inName <- inFile$name
        #inPath <- inFile$datapath
        
        #inList <- list(inName, inPath)
        #names(inList) <- c("inName", "inPath")
        
        
        n <- length(inFile$name)
        net.names <- gsub("\\@.*","",inFile$name)
        
        myfiles = pblapply(inFile$datapath,  read_csv_net)
        
    
        myfiles.frame.list <- pblapply(myfiles, data.frame, stringsAsFactors=FALSE)
        nms = unique(unlist(pblapply(myfiles.frame.list, names)))
        myfiles.frame <- as.data.frame(do.call(rbind, lapply(myfiles.frame.list, "[", nms)))
        myfiles.frame <- as.data.frame(sapply(myfiles.frame, as.numeric))


#myfiles.frame$Spectrum <- net.names
        
        united.frame <- data.frame(net.names, myfiles.frame)
        colnames(united.frame) <- c("Spectrum", names(myfiles.frame))
        united.frame$None <- rep(1, length(united.frame$Spectrum))
        
        
        incProgress(1/n)
        Sys.sleep(0.1)
    })
        
        united.frame <- as.data.frame(united.frame)
        united.frame

    })
    
    netCountsSecond <- reactive({
        
        withProgress(message = 'Processing Data', value = 0, {
            inFile <- input$file2

            inFile <- if(input$iszip==FALSE){
                inFile
            }else if(input$iszip==TRUE){
                unzip(inFile)
            }
            
            if (is.null(inFile)) return(NULL)
            
            #inName <- inFile$name
            #inPath <- inFile$datapath
            
            #inList <- list(inName, inPath)
            #names(inList) <- c("inName", "inPath")
            
            
            n <- length(inFile$name)
            net.names <- gsub("\\@.*","",inFile$name)
            
            myfiles = pblapply(inFile$datapath,  read_csv_net)
            
            
            myfiles.frame.list <- pblapply(myfiles, data.frame, stringsAsFactors=FALSE)
            nms = unique(unlist(pblapply(myfiles.frame.list, names)))
            myfiles.frame <- as.data.frame(do.call(rbind, lapply(myfiles.frame.list, "[", nms)))
            myfiles.frame <- as.data.frame(sapply(myfiles.frame, as.numeric))
            
            
            #myfiles.frame$Spectrum <- net.names
            
            united.frame <- data.frame(net.names, myfiles.frame)
            colnames(united.frame) <- c("Spectrum", names(myfiles.frame))
            united.frame$None <- rep(1, length(united.frame$Spectrum))
            
            
            incProgress(1/n)
            Sys.sleep(0.1)
        })
        
        united.frame <- as.data.frame(united.frame)
        united.frame
        
    })
    

    
    myDataFirst <- reactive({
        if(input$filetype=="Spectra"){
            fullSpectra()
        } else if(input$filetype=="Net"){
            netCounts()
        }
        
    })
    

    
    myDataSecond <- reactive({
        if(input$filetype=="Spectra"){
            fullSpectraSecond()
        } else if(input$filetype=="Net"){
            netCountsSecond()
        }
        
    })
    
    observeEvent(input$actionprocess, {

    
    myValDataFirst <- reactive({
        myDataFirst()
    })
    
    calFileContentsFirst <- reactive({
        
        existingCalFile <- input$calfileinput1
        
        if (is.null(existingCalFile)) return(NULL)
        
        
        Calibration <- readRDS(existingCalFile$datapath)
        
        Calibration
        
    })
    
    
    
    
    calValHoldFirst <- reactive({
        
        calFileContentsFirst()[[6]]
        
    })
    
    calVariablesFirst <- reactive({
        
        calFileContentsFirst()$Intensities
        
    })
    
    calValElementsFirst <- reactive({
        calList <- calValHoldFirst()
        valelements <- ls(calList)
        valelements
    })
    
    calVariableElementsFirst <- reactive({
        variables <- calVariablesFirst()
        variableelements <- ls(variables)
        variableelements
    })
    
    
    
    
    
    
    
    
    
    fullInputValCountsFirst <- reactive({
        valelements <- calValElementsFirst()
        variableelements <- calVariableElementsFirst()
        val.data <- myValDataFirst()
        
        
        if(input$filetype=="Spectra"){spectra.line.list <- lapply(variableelements, function(x) elementGrab(element.line=x, data=rawSpectra()))}
        if(input$filetype=="Spectra"){element.count.list <- lapply(spectra.line.list, `[`, 2)}
        
        
        if(input$filetype=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
        
        if(input$filetype=="Spectra"){dim(spectra.line.vector) <- c(length(val.data$Spectrum), length(variableelements))}
        
        if(input$filetype=="Spectra"){spectra.line.frame <- data.frame(val.data$Spectrum, spectra.line.vector)}
        
        if(input$filetype=="Spectra"){colnames(spectra.line.frame) <- c("Spectrum", variableelements)}
        
        if(input$filetype=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
        
        if(input$filetype=="Spectra"){val.line.table <- spectra.line.frame[,c("Spectrum", variableelements)]}
        
        if(input$filetype=="Net"){val.line.table <- val.data}
        
        val.line.table
    })
    
    
    
    
    
    
    
    tableInputValQuantFirst <- reactive({
        
        count.table <- data.frame(fullInputValCountsFirst())
        the.cal <- calValHoldFirst()
        elements <- calValElementsFirst()
        variables <- calVariableElementsFirst()
        valdata <- if(input$filetype=="Spectra"){
            rawSpectra()
        }else if(input$filetype=="Net"){
            myDataFirst()
        }
        
        
        
        
        
        predicted.list <- pblapply(elements, function (x)
        if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
            predict(
            object=the.cal[[x]][[2]],
            newdata=general.prep(
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x)
            )
        } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
            predict(
            object=the.cal[[x]][[2]],
            newdata=simple.tc.prep(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x
            )
            )
        } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
            predict(
            object=the.cal[[x]][[2]],
            newdata=simple.comp.prep(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
            norm.max=the.cal[[x]][[1]][1]$CalTable$Max
            )
            )
        } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
            predict(
            object=the.cal[[x]][[2]],
            newdata=lukas.simp.prep(
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
            )
            )
        } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
            predict(
            object=the.cal[[x]][[2]],
            newdata=lukas.tc.prep(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
            )
            )
        } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
            predict(
            object=the.cal[[x]][[2]],
            newdata=lukas.comp.prep(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
            norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
            norm.max=the.cal[[x]][[1]][1]$CalTable$Max
            )
            )
        }else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
            predict(
            object=the.cal[[x]][[2]],
            newdata=general.prep.net(
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x)
            )
        } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
            predict(
            object=the.cal[[x]][[2]],
            newdata=simple.tc.prep.net(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x
            )
            )
        } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
            predict(
            object=the.cal[[x]][[2]],
            newdata=simple.comp.prep.net(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
            norm.max=the.cal[[x]][[1]][1]$CalTable$Max
            )
            )
        } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
            predict(
            object=the.cal[[x]][[2]],
            newdata=lukas.simp.prep.net(
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
            )
            )
        } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
            predict(
            object=the.cal[[x]][[2]],
            newdata=lukas.tc.prep.net(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
            )
            )
        } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
            predict(
            object=the.cal[[x]][[2]],
            newdata=lukas.comp.prep.net(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
            norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
            norm.max=the.cal[[x]][[1]][1]$CalTable$Max
            )
            )
        }
        
        
        
        )
        
        predicted.vector <- unlist(predicted.list)
        
        dim(predicted.vector) <- c(length(count.table$Spectrum), length(elements))
        
        predicted.frame <- data.frame(count.table$Spectrum, predicted.vector)
        
        colnames(predicted.frame) <- c("Spectrum", elements)
        
        #predicted.data.table <- data.table(predicted.frame)
        #predicted.values <- t(predicted.values)
        #predicted.data.table
        predicted.frame
        
    })
    
    
    
    
    myValDataSecond <- reactive({
        myDataSecond()
    })
    
    calFileContentsSecond <- reactive({
        
        existingCalFile <- input$calfileinput2
        
        if (is.null(existingCalFile)) return(NULL)
        
        
        Calibration <- readRDS(existingCalFile$datapath)
        
        Calibration
        
    })
    
    
    
    
    calValHoldSecond <- reactive({
        
        calFileContentsSecond()[[6]]
        
    })
    
    calVariablesSecond <- reactive({
        
        calFileContentsSecond()$Intensities
        
    })
    
    calValElementsSecond <- reactive({
        calList <- calValHoldSecond()
        valelements <- ls(calList)
        valelements
    })
    
    calVariableElementsSecond <- reactive({
        variables <- calVariablesSecond()
        variableelements <- ls(variables)
        variableelements
    })
    
    
    
    
    
    
    
    
    
    fullInputValCountsSecond <- reactive({
        valelements <- calValElementsSecond()
        variableelements <- calVariableElementsSecond()
        val.data <- myValDataSecond()
        
        
        if(input$filetype=="Spectra"){spectra.line.list <- lapply(variableelements, function(x) elementGrab(element.line=x, data=rawSpectraSecond()))}
        if(input$filetype=="Spectra"){element.count.list <- lapply(spectra.line.list, `[`, 2)}
        
        
        if(input$filetype=="Spectra"){spectra.line.vector <- as.numeric(unlist(element.count.list))}
        
        if(input$filetype=="Spectra"){dim(spectra.line.vector) <- c(length(val.data$Spectrum), length(variableelements))}
        
        if(input$filetype=="Spectra"){spectra.line.frame <- data.frame(val.data$Spectrum, spectra.line.vector)}
        
        if(input$filetype=="Spectra"){colnames(spectra.line.frame) <- c("Spectrum", variableelements)}
        
        if(input$filetype=="Spectra"){spectra.line.frame <- as.data.frame(spectra.line.frame)}
        
        if(input$filetype=="Spectra"){val.line.table <- spectra.line.frame[,c("Spectrum", variableelements)]}
        
        if(input$filetype=="Net"){val.line.table <- val.data}
        
        val.line.table
    })
    
    
    
    
    
    
    
    tableInputValQuantSecond <- reactive({
        
        count.table <- data.frame(fullInputValCountsSecond())
        the.cal <- calValHoldSecond()
        elements <- calValElementsSecond()
        variables <- calVariableElementsSecond()
        valdata <- if(input$filetype=="Spectra"){
            rawSpectraSecond()
        }else if(input$filetype=="Net"){
            myDataSecond()
        }
        
        
        
        
        
        predicted.list <- pblapply(elements, function (x)
        if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
            predict(
            object=the.cal[[x]][[2]],
            newdata=general.prep(
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x)
            )
        } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
            predict(
            object=the.cal[[x]][[2]],
            newdata=simple.tc.prep(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x
            )
            )
        } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
            predict(
            object=the.cal[[x]][[2]],
            newdata=simple.comp.prep(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
            norm.max=the.cal[[x]][[1]][1]$CalTable$Max
            )
            )
        } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
            predict(
            object=the.cal[[x]][[2]],
            newdata=lukas.simp.prep(
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
            )
            )
        } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
            predict(
            object=the.cal[[x]][[2]],
            newdata=lukas.tc.prep(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
            )
            )
        } else if(input$filetype=="Spectra" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
            predict(
            object=the.cal[[x]][[2]],
            newdata=lukas.comp.prep(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
            norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
            norm.max=the.cal[[x]][[1]][1]$CalTable$Max
            )
            )
        }else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
            predict(
            object=the.cal[[x]][[2]],
            newdata=general.prep.net(
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x)
            )
        } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
            predict(
            object=the.cal[[x]][[2]],
            newdata=simple.tc.prep.net(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x
            )
            )
        } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
            predict(
            object=the.cal[[x]][[2]],
            newdata=simple.comp.prep.net(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
            norm.max=the.cal[[x]][[1]][1]$CalTable$Max
            )
            )
        } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
            predict(
            object=the.cal[[x]][[2]],
            newdata=lukas.simp.prep.net(
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
            )
            )
        } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
            predict(
            object=the.cal[[x]][[2]],
            newdata=lukas.tc.prep.net(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
            )
            )
        } else if(input$filetype=="Net" && the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
            predict(
            object=the.cal[[x]][[2]],
            newdata=lukas.comp.prep.net(
            data=valdata,
            spectra.line.table=as.data.frame(
            count.table
            ),
            element.line=x,
            slope.element.lines=the.cal[[x]][[1]][2]$Slope,
            intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
            norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
            norm.max=the.cal[[x]][[1]][1]$CalTable$Max
            )
            )
        }
        
        
        
        )
        
        predicted.vector <- unlist(predicted.list)
        
        dim(predicted.vector) <- c(length(count.table$Spectrum), length(elements))
        
        predicted.frame <- data.frame(count.table$Spectrum, predicted.vector)
        
        colnames(predicted.frame) <- c("Spectrum", elements)
        
        #predicted.data.table <- data.table(predicted.frame)
        #predicted.values <- t(predicted.values)
        #predicted.data.table
        predicted.frame
        
    })
    
    
    
    hotableInputDepthLight <- reactive({
        
        light.table <- data.frame(myDataFirst()$Spectrum)
        light.table$Spectrum <- myDataFirst()$Spectrum
        light.table$LightDepth <- myDataFirst()[,2]*0
        
        light.table <- light.table[2:3]

    })
    
    valuesLight <- reactiveValues()
    
    observe({
        if (!is.null(input$depthtablelight)) {
            DPL = hot_to_r(input$depthtablelight)
        } else {
            if (is.null(valuesLight[["DPL"]]))
            DPL <- hotableInputDepthLight()
            else
            DPL <- valuesLight[["DPL"]]
        }
        valuesLight[["DPL"]] <- DPL
    })
    
    
    ## Handsontable
    
    output$depthtablelight <- renderRHandsontable({
        DPL <- valuesLight[["DPL"]]
        if (!is.null(DPL))
        rhandsontable(DPL, useTypes = FALSE, stretchH = "all")
    })
    
    
    hotableInputDepthTrace <- reactive({
        
        if(is.null(input$file2)==FALSE){
        
        trace.table <- data.frame(myDataSecond()$Spectrum)
        trace.table$Spectrum <- myDataSecond()$Spectrum
        trace.table$TraceDepth <- myDataSecond()[,2]*0
        
        trace.table <- trace.table[2:3]
        
        colnames(trace.table) <- c("TraceSpectrum", "TraceDepth")
        } else if(is.null(input$file2)==TRUE){
            trace.table <- data.frame(0, 0)
            colnames(trace.table) <- c("TraceSpectrum", "TraceDepth")
        }
        
        trace.table
        
        
    })
    
    valuesTrace <- reactiveValues()
    
    observe({
        if (!is.null(input$depthtabletrace)) {
            DPT = hot_to_r(input$depthtabletrace)
        } else {
            if (is.null(valuesTrace[["DPT"]]))
            DPT <- hotableInputDepthTrace()
            else
            DPT <- valuesTrace[["DPT"]]
        }
        valuesTrace[["DPT"]] <- DPT
    })
    
    
    ## Handsontable
    
    output$depthtabletrace <- renderRHandsontable({
        DPT <- valuesTrace[["DPT"]]
        if (!is.null(DPT))
        rhandsontable(DPT, useTypes = FALSE, stretchH = "all")
    })
    
    
    
    lightFrame <- reactive({
        
        light.frame <- data.frame(myDataFirst(), valuesLight[["DPL"]]$LightDepth)
        colnames(light.frame) <- c(names(myDataFirst()), "Depth")
        light.frame[ ,!(colnames(light.frame) == "Spectrum")]
        
    })
    
    traceFrame <- reactive({
        
        trace.frame <- data.frame(myDataSecond(), valuesTrace[["DPT"]]$TraceDepth)
        colnames(trace.frame) <- c(names(myDataSecond()), "Depth")
        trace.frame[ ,!(colnames(trace.frame) == "Spectrum")]
        
    })
    
    lightQuantFrame <- reactive({
        
        light.frame <- data.frame(tableInputValQuantFirst(), valuesLight[["DPL"]]$LightDepth)
        colnames(light.frame) <- c(names(tableInputValQuantFirst()), "Depth")
        light.frame[ ,!(colnames(light.frame) == "Spectrum")]
        
    })
    
    traceQuantFrame <- reactive({
        
        trace.frame <- data.frame(tableInputValQuantSecond(), valuesTrace[["DPT"]]$TraceDepth)
        colnames(trace.frame) <- c(names(tableInputValQuantSecond()), "Depth")
        trace.frame[ ,!(colnames(trace.frame) == "Spectrum")]
        
    })
    
    
    observeEvent(input$actionprocessdepth, {

    
    
    
        myData <- reactive({
                
                data <- if(is.null(input$file2)==TRUE){
                    lightFrame()
                } else if(is.null(input$file2)==FALSE){
                    merged.frame <- merge(lightFrame(), traceFrame(), by = "Depth", all=TRUE)
                    merged.frame[complete.cases(merged.frame), ]
                }
                
                newdata <- data[order(data$Depth),]

                
                newdata

            
        })
        
        tableInputValQuant <- reactive({
            
            data <- if(is.null(input$file2)==TRUE){
                lightQuantFrame()
            } else if(is.null(input$file2)==FALSE){
                #merged.frame <- merge(lightQuantFrame(), traceQuantFrame(), by = "Depth", all=TRUE)
                #merged.frame[complete.cases(merged.frame), ]
                merge(lightQuantFrame(), traceQuantFrame(), by = "Depth", all=TRUE)
            }
            
            newdata <- data[order(data$Depth),]
            
            
            if(input$zeroout==TRUE){
                newdata[newdata<0] <- 0
            } 
            
            newdata

            
        })
        
        
        
    
        
        
        output$contents <- renderTable({
            
            
            
            myData()
            
        })
        
        # Return the requested dataset
        datasetInput <- reactive({
            switch(input$element,
            "H.table" = H.table,
            "He.table" = He.table,
            "Li.table" = Li.table,
            "Be.table" = Be.table,
            "B.table" = B.table,
            "C.table" = C.table,
            "N.table" = N.table,
            "O.table" = O.table,
            "F.table" = F.table,
            "Ne.table" = Ne.table,
            "Na.table" = Na.table,
            "Mg.table" = Mg.table,
            "Al.table" = Al.table,
            "Si.table" = Si.table,
            "P.table" = P.table,
            "S.table" = S.table,
            "Cl.table" = Cl.table,
            "Ar.table" = Ar.table,
            "K.table" = K.table,
            "Ca.table" = Ca.table,
            "Sc.table" = Sc.table,
            "Ti.table" = Ti.table,
            "V.table" = V.table,
            "Cr.table" = Cr.table,
            "Mn.table" = Mn.table,
            "Fe.table" = Fe.table,
            "Co.table" = Co.table,
            "Ni.table" = Ni.table,
            "Cu.table" = Cu.table,
            "Zn.table" = Zn.table,
            "Ga.table" = Ga.table,
            "Ge.table" = Ge.table,
            "As.table" = As.table,
            "Se.table" = Se.table,
            "Br.table" = Br.table,
            "Kr.table" = Kr.table,
            "Rb.table" = Rb.table,
            "Sr.table" = Sr.table,
            "Y.table" = Y.table,
            "Zr.table" = Zr.table,
            "Nb.table" = Nb.table,
            "Mo.table" = Mo.table,
            "Tc.table" = Tc.table,
            "Ru.table" = Ru.table,
            "Rh.table" = Rh.table,
            "Pd.table" = Pd.table,
            "Ag.table" = Ag.table,
            "Cd.table" = Cd.table,
            "In.table" = In.table,
            "Sn.table" = Sn.table,
            "Sb.table" = Sb.table,
            "Te.table" = Te.table,
            "I.table" = I.table,
            "Xe.table" = Xe.table,
            "Cs.table" = Cs.table,
            "Ba.table" = Ba.table,
            "La.table" = La.table,
            "Ce.table" = Ce.table,
            "Pr.table" = Pr.table,
            "Nd.table" = Nd.table,
            "Pm.table" = Pm.table,
            "Sm.table" = Sm.table,
            "Eu.table" = Eu.table,
            "Gd.table" = Gd.table,
            "Tb.table" = Tb.table,
            "Dy.table" = Dy.table,
            "Ho.table" = Ho.table,
            "Er.table" = Er.table,
            "Tm.table" = Tm.table,
            "Yb.table" = Yb.table,
            "Lu.table" = Lu.table,
            "Hf.table" = Hf.table,
            "Ta.table" = Ta.table,
            "W.table" = W.table,
            "Re.table" = Re.table,
            "Os.table" = Os.table,
            "Ir.table" = Ir.table,
            "Pt.table" = Pt.table,
            "Au.table" = Au.table,
            "Hg.table" = Hg.table,
            "Tl.table" = Tl.table,
            "Pb.table" = Pb.table,
            "Bi.table" = Bi.table,
            "Po.table" = Po.table,
            "At.table" = At.table,
            "Rn.table" = Rn.table,
            "Fr.table" = Fr.table,
            "Ra.table" = Ra.table,
            "Ac.table" = Ac.table,
            "Th.table" = Th.table,
            "Pa.table" = Pa.table,
            "U.table" = U.table)
        })
        
        observeEvent(input$actionplot, {
            
            # Expression that generates a histogram. The expression is
            # wrapped in a call to renderPlot to indicate that:
            #
            #  1) It is "reactive" and therefore should re-execute automatically
            #     when inputs change
            #  2) Its output type is a plot
            ranges <- reactiveValues(x = NULL, y = NULL)
            
            
            
            plotInput <- reactive({
                
                
                
                data <- myData()
                id.seq <- seq(1, 2048,1)
                
                n <- length(data$Energy)
                
                element <- datasetInput()
                intensity.norm <- (element$Intensity/max(element$Intensity))*max(data$CPS)
                intensity.base <- (element$Intensity/max(element$Intensity))
                
                
                
                spectral.plot <- qplot(data$Energy, data$CPS, xlab = "Energy (keV)", ylab = "Counts per Second", geom="line", colour=data$Spectrum) +
                theme_light()+
                theme(legend.position="bottom") +
                geom_segment(aes(x=element$Line, xend=element$Line, y = 0, yend=intensity.norm), colour="grey50", linetype=2)  +
                scale_colour_discrete("Spectrum") +
                coord_cartesian(xlim = ranges$x, ylim = ranges$y)
                
                
                ###Background Subtraction
                # data.n <- as.data.frame(dcast(data=data, formula=Energy~Spectrum, fun.aggregate = sum,value.var = "CPS"))
                
                # background.subtracted <- pbapply(data.n, 2, Hodder.v)
                #background.subtracted <- as.data.frame(background.subtracted)
                # background.subtracted$Energy <- data.n$Energy
                
                # background.melt <- melt(background.subtracted, id="Energy")
                # colnames(background.melt) <- c("Energy", "Spectrum", "CPS")
                
                
                #transformed.spectral.plot <-  qplot(background.melt$Energy+1, SMA(background.melt$CPS, 10), xlab = "Energy (keV)", ylab = "CPS", geom="line", colour=background.melt$Spectrum)+
                # theme_light()+
                # theme(legend.position="bottom") +
                # geom_segment(aes(x=element$Line, xend=element$Line, y = 0, yend=intensity.norm), colour="grey50", linetype=2)  +
                # scale_colour_discrete("Spectrum") +
                # coord_cartesian(xlim = ranges$x, ylim = ranges$y)
                
                
                # if (input$backgroundsubtract == FALSE) {
                #   spectral.plot
                
                #} else if (input$backgroundsubtract == TRUE) {
                #   transformed.spectral.plot
                # }
                
                spectral.plot
                
                
            })
            
            
            output$distPlot <- renderPlot({
                
                print(plotInput())
                
                
            })
            
            # When a double-click happens, check if there's a brush on the plot.
            # If so, zoom to the brush bounds; if not, reset the zoom.
            observeEvent(input$plot1_dblclick, {
                brush <- input$plot1_brush
                if (!is.null(brush)) {
                    ranges$x <- c(brush$xmin*mean(data$Energy), brush$xmax*max(data$Energy))
                    ranges$y <- c(brush$ymin*mean(data$CPS), brush$ymax*max(data$CPS))
                    
                } else {
                    ranges$x <- NULL
                    ranges$y <- NULL
                }
                
                
                
            })
            
            output$downloadPlot <- downloadHandler(
            
            plotname <- "SpectraPlot",
            
            
            filename = function() { paste(paste(c(input$projectname, "-", plotname), collapse=''), '.tiff', sep='') },
            content = function(file) {
                ggplot2::ccsave(file,plotInput(), compression="lzw", dpi=300, width=12, height=7)
            }
            )
            
            
            
            
            
            
            
            
        })
        
       
        
        
        
        
       
       lineOptions <- reactive({
           
           spectra.line.table <- myData()[ ,!(colnames(myData()) == "Depth")]
           if(input$usecalfile==TRUE){
               quant.frame <- tableInputValQuant()[ ,!(colnames(tableInputValQuant()) =="Depth")]
           quantified <- colnames(quant.frame)
           }
           
           standard <- if(input$usecalfile==FALSE && input$filetype=="Spectra"){
               spectralLines
           } else if(input$usecalfile==FALSE && input$filetype=="Net"){
               colnames(spectra.line.table)
           } else if(input$usecalfile==TRUE && input$filetype=="Spectra"){
               quantified
           }else if(input$usecalfile==TRUE && input$filetype=="Net"){
              quantified
           }
           
       })

defaultLines <- reactive({
    
    spectra.line.table <- myData()
    if(input$usecalfile==TRUE){quantified <- colnames(tableInputValQuant()[ ,!(colnames(tableInputValQuant()) =="Depth")])
}
    
    standard <- if(input$usecalfile==FALSE && input$filetype=="Spectra"){
        c("Ca.K.alpha", "Ti.K.alpha", "Fe.K.alpha", "Cu.K.alpha", "Zn.K.alpha")
    } else if(input$usecalfile==FALSE && input$filetype=="Net"){
        colnames(spectra.line.table)
    } else if(input$usecalfile==TRUE && input$filetype=="Spectra"){
        quantified
    }else if(input$usecalfile==TRUE && input$filetype=="Net"){
        quantified
    }
    
})

output$defaultlines <- renderUI({

    
    checkboxGroupInput('show_vars', 'Elemental lines to show:',
    choices=lineOptions(), selected = defaultLines())
})





  #####Age Model
  
  hotableInputAge <- reactive({
      
      empty.depth <- rep(0, 50)
      empty.age <- rep(0, 50)
      empty.sigma <- rep(0, 50)
      
      empty.table <- data.frame(empty.depth, empty.age, empty.sigma)
      colnames(empty.table) <- c("Depth", "14C Age", "Sigma")
      
      empty.table
      
  })
  
  values <- reactiveValues()
  
  observe({
      if (!is.null(input$hotage)) {
          DF2 = hot_to_r(input$hotage)
      } else {
          if (is.null(values[["DF2"]]))
          DF2 <- hotableInputAge()
          else
          DF2 <- values[["DF2"]]
      }
      values[["DF2"]] <- DF2
  })
  
  
  ## Handsontable
  
  output$hotage <- renderRHandsontable({
      DF2 <- values[["DF2"]]
      if (!is.null(DF2))
      rhandsontable(DF2, useTypes = TRUE, stretchH = "all")
  })
  
  
  ageResults <- reactive({
      
      input$hotableprocess3
      
      
      
      DF3 <- values[["DF2"]]
      DF4 <- subset(DF3, !DF3$Sigma==0)
      DF5 <- as.data.frame(DF4)
      
      isolate(
       if(input$ageon==TRUE){
          Bchronology(ages=as.numeric(as.vector(DF5[,2])), ageSds=as.numeric(as.vector(DF5[,3])), positions=as.numeric(as.vector(DF5[,1])), positionThickness=rep(0.5, length(DF5$Sigma)), calCurves=rep(input$curvetype, length(DF5[,1])), burn=2000, jitterPositions=TRUE)
      }else if(input$ageon==FALSE){
          NULL
      }
      
      
      
      )
      
  })
  
  
  
  
  
  
  agePredict <- reactive({
      
      age.math <- ageResults()
      
      spectra.line.table <- myData()
      
      age.results <- if(input$ageon==TRUE){
          predict(age.math, newPositions=spectra.line.table$Depth, newPositionThicknesses=rep(0.0, length(spectra.line.table$Depth)))
      }else if(input$ageon==FALSE) {
          NULL
      }
      
      age.medians <- if(input$ageon==TRUE){
          apply(age.results, 2, median)
      }else if(input$ageon==FALSE) {
          spectra.line.table$Depth
      }
      
      
      age.sd <-  if(input$ageon==TRUE){
          apply(age.results, 2, sd)
      }else if(input$ageon==FALSE) {
          rep(0, length(spectra.line.table$Depth))
      }
      
      
      age.min.sd1 <- age.medians - age.sd
      age.min.sd2 <- age.medians - age.sd*2
      
      age.max.sd1 <- age.medians + age.sd
      age.max.sd2 <- age.medians + age.sd*2
      
      spectra.line.table$Age <- age.medians
      spectra.line.table$Sd <- age.sd
      spectra.line.table$MinSd2 <-age.min.sd2
      spectra.line.table$MinSd1 <-age.min.sd1
      spectra.line.table$MaxSd1 <-age.max.sd1
      spectra.line.table$MaxSd2 <-age.max.sd2
      
      spectra.line.table$None <- rep(1, length(spectra.line.table$Depth))
      
      spectra.line.table
      
      
      
  })
  
  ageTable <- reactive({
      
      age.results <- agePredict()
      
      age.frame <- data.frame(age.results$Depth, age.results$Age, age.results$Sd, age.results$MinSd2, age.results$MinSd1, age.results$MaxSd1, age.results$MaxSd2)
      colnames(age.frame) <- c("Depth", "Age", "Sd", "MinSd2", "MinSd1", "MaxSd1", "MaxSd2")
      
      age.frame
      
  })
  
  agePlot <- reactive({
      
      age.math <- ageResults()
      
      age.plot <- plot(age.math, xlab="Age cal year BP", ylab = "Depth (cm)", las=1)
      
      age.plot
  })
  
  output$allagemodel <- renderTable({
      
      ageTable()
      
      
  })
  
  output$agemodcurve <- renderPlot({
      
      print(agePlot())
      
      
  })
  
  
  
  
  
  
  output$ageresults <- downloadHandler(
  filename = function() { paste(input$projectname, "_AgeTable", '.csv', sep=',') },
  content = function(file
  ) {
      write.csv(ageTable(), file)
  }
  )
  
  
  
  
  tableInput <- reactive({
      
      if(input$usecalfile==FALSE){
          myData()[,c("Depth", input$show_vars)]
      }else if(input$usecalfile==TRUE){
          tableInputValQuant()[,c("Depth", input$show_vars)]
      }
      
      
  })
  
  
  output$mytable1 <- renderDataTable({
      
      tableInput()
      
  })
  
  
  
  
  output$downloadData <- downloadHandler(
  filename = function() { paste(paste(c(input$projectname, "_", "CountTable"), collapse=''), '.csv', sep=',') },
  content = function(file
  ) {
      write.csv(tableInput(), file)
  }
  )
  
  
  
  
  
  hotableInput <- reactive({
      
      spectra.line.table <- myData()
      depths <- spectra.line.table$Depth
      
      empty.line.table <-  spectra.line.table
      empty.line.table <- empty.line.table[1:2]
      colnames(empty.line.table) <- c("Qualitative", "Depth")
      empty.line.table$Depth <- empty.line.table$Depth*0
      empty.line.table$Custom <- empty.line.table$Depth*0
      empty.line.table$Quantitative1 <- empty.line.table$Depth*0
      empty.line.table$Quantitative2 <- empty.line.table$Depth*0
      empty.line.table$Quantitative3 <- empty.line.table$Depth*0
      empty.line.table$Spectrum <- spectra.line.table$Spectrum
      na.vector <- rep("NULL", length(empty.line.table$Qualitative))
      na.matrix <- as.matrix(na.vector)
      na.matrix[1,1] <- "a"
      na.matrix[2,1] <- "b"
      na.matrix[3,1] <- "c"
      na.input <- as.vector(na.matrix[,1])
      
      
      line.table <- data.frame(depths, na.input, empty.line.table$Custom, empty.line.table$Quantitative1, empty.line.table$Quantitative2, empty.line.table$Quantitative3)
      colnames(line.table) <- c("Depth", "Qualitative", "Custom", "Quantitative1", "Quantitative2", "Quantitative3")
      
      
      
      line.table
      
  })
  
  values <- reactiveValues()
  
  observe({
      if (!is.null(input$hot)) {
          DF = hot_to_r(input$hot)
      } else {
          if (is.null(values[["DF"]]))
          DF <- hotableInput()
          else
          DF <- values[["DF"]]
      }
      values[["DF"]] <- DF
  })
  
  
  ## Handsontable
  
  output$hot <- renderRHandsontable({
      DF <- values[["DF"]]
      if (!is.null(DF))
      rhandsontable(DF, useTypes = FALSE, stretchH = "all")
  })
  
  

  
  
  
  xrfKReactive <- reactive({
      
      spectra.line.table <- tableInput()
      
      
      
      
      
      xrf.pca.header <- input$show_vars
      xrf.pca.frame <- spectra.line.table[,input$show_vars]
      xrf.pca.n <- length(xrf.pca.frame)
      xrf.smalls <- xrf.pca.frame
      
      xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
      xrf.pca <- prcomp(xrf.smalls, scale.=FALSE)
      
      xrf.scores <- as.data.frame(xrf.pca$x)
      
      cluster.frame <- data.frame(spectra.line.table$Depth, xrf.k$cluster, xrf.scores)
      
      colnames(cluster.frame) <- c("Assay", "Cluster", names(xrf.scores))
      
      cluster.frame
      
      
      
  })
  
  xrfPCAReactive <- reactive({
      
      
      spectra.line.table <- if(input$usecalfile==FALSE){
          myData()
      } else if(input$usecalfile==TRUE){
          tableInput()
      }
      
      
      xrf.clusters <- xrfKReactive()
      
      element.counts <- spectra.line.table[input$show_vars]
      
      
      
      xrf.pca.results <- data.frame(xrf.clusters, element.counts)
      
      xrf.pca.results
  })
  
  
  
  ###Optimal Clusters
  
  optimalK <- reactive({
      
      
      spectra.line.table <- if(input$usecalfile==FALSE){
          myData()
      } else if(input$usecalfile==TRUE){
          tableInput()
      }
      
      xrf.pca.frame <- spectra.line.table[,input$show_vars]
      xrf.pca.frame <- xrf.pca.frame[complete.cases(xrf.pca.frame),]
      
      wss <- (nrow(xrf.pca.frame)-1)*sum(apply(xrf.pca.frame,2,var))
      for (i in 2:30) wss[i] <- sum(kmeans(xrf.pca.frame,
      centers=i)$withinss)
      
      data.frame(
      clustercount=seq(1, 30, 1),
      wss=wss)
      
  })
  
  
  output$wsstable <- downloadHandler(
  filename = function() { paste(paste(c(input$projectname, "_", "WSSTable"), collapse=''), '.csv', sep=',') },
  content = function(file
  ) {
      write.csv(optimalK(), file)
  }
  )
  
  

  
  screeCrunch <- reactive({
      
      wss.frame <- optimalK()
      
      best.choice <- scree_crunch(dataframe=wss.frame, dependent="wss", independent="clustercount")
      
      best.choice
      
  })
  
  output$knumui <- renderUI({
      
      numericInput("knum", label = "K-Means", value=screeCrunch())

  })
  
  
  optimalKplot <- reactive({
      
      wss.frame <- optimalK()
      
      
      ggplot(wss.frame, aes(clustercount, wss)) +
      geom_line() +
      geom_point() +
      geom_point(data=wss.frame[screeCrunch(), ], aes(clustercount, wss), size=3) +
      geom_point(data=wss.frame[screeCrunch(), ], aes(clustercount, wss), pch=1, size=6) +
      scale_x_continuous("Number of Clusters") +
      scale_y_continuous("Within Groups Sum of Squares", labels=comma) +
      theme_light()
      
  })
  
  
  
  
  output$optimalkplot <- renderPlot({
      optimalKplot()
      
  })
  
  
  output$hover_infooptimalk <- renderUI({
      
      point.table <- optimalK()
      
      hover <- input$plot_hoveroptimalk
      point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
      "<b> Cluster Count: </b>", point$clustercount, "<br/>",
      "<b> WSS: </b>", point$wss, "<br/>",
      "<b> Percent: </b>", percent(round(1-point$wss/max(point.table$wss), 2)), "<br/>"
      
      )))
      )
  })

  
  
  dataProcessed <- reactive({
      
      spectra.line.table <- if(input$usecalfile==FALSE){
          myData()
      } else if(input$usecalfile==TRUE){
          tableInputValQuant()
      }

      xrf.k <- xrfKReactive()
      
      quality.table <- values[["DF"]]
      
      colour.table <- data.frame(xrf.k, quality.table)
      colnames(colour.table) <- c(names(xrf.k), names(quality.table))
      
      
      
      
      unique.spec <- seq(1, length(colour.table$Depth), 1)
      null <- rep(1, length(unique.spec))
      
      spectra.line.table$Cluster <- xrf.k$Cluster
      spectra.line.table$Qualitative <- quality.table$Qualitative
      spectra.line.table$Depth <- as.numeric(as.vector(quality.table$Depth))
      spectra.line.table$Custom <- as.numeric(as.vector(quality.table$Custom))
      spectra.line.table$Quantitative1 <- as.numeric(as.vector(quality.table$Quantitative1))
      spectra.line.table$Quantitative2 <- as.numeric(as.vector(quality.table$Quantitative2))
      spectra.line.table$Quantitative3 <- as.numeric(as.vector(quality.table$Quantitative3))
      spectra.line.table$PC1 <- xrf.k$PC1
      spectra.line.table$PC2 <- xrf.k$PC2
      spectra.line.table
  })
  

  
  
  
  
  ageData <- reactive({
      
      c14ages <- values[["DF2"]]
      c14ages <- subset(c14ages, !c14ages$Sigma==0)
      c14ages <- as.data.frame(c14ages)
      
      c14min <- min(c14ages$Depth)
      c14max <- max(c14ages$Depth)
      
      ages <- agePredict()
      


      spectra.line.table.age.unconstrained <- dataProcessed()
      spectra.line.table.age.unconstrained$Age <- ages$Age
      
      
      lateholocene <- subset(spectra.line.table.age.unconstrained$Age, spectra.line.table.age.unconstrained$Age <= 5000 & spectra.line.table.age.unconstrained$Age > -1000)
      
      altithermal <- subset(spectra.line.table.age.unconstrained$Age, spectra.line.table.age.unconstrained$Age <= 9000 & spectra.line.table.age.unconstrained$Age > 5000)
      
      holocenetransition <- subset(spectra.line.table.age.unconstrained$Age, spectra.line.table.age.unconstrained$Age <= 11700 & spectra.line.table.age.unconstrained$Age > 9000)
      
      youngerdryas <- subset(spectra.line.table.age.unconstrained$Age, spectra.line.table.age.unconstrained$Age <= 12900 & spectra.line.table.age.unconstrained$Age > 11700)
      
      bollingalerod <- subset(spectra.line.table.age.unconstrained$Age, spectra.line.table.age.unconstrained$Age <= 14700 & spectra.line.table.age.unconstrained$Age > 12900)
      
      deglaciation <- subset(spectra.line.table.age.unconstrained$Age, spectra.line.table.age.unconstrained$Age <= 19000 & spectra.line.table.age.unconstrained$Age > 14700)
      
      lastglacialmax <- subset(spectra.line.table.age.unconstrained$Age, spectra.line.table.age.unconstrained$Age <= 25000 & spectra.line.table.age.unconstrained$Age > 19000)
      
      glacial <- subset(spectra.line.table.age.unconstrained$Age, spectra.line.table.age.unconstrained$Age <= 120000 & spectra.line.table.age.unconstrained$Age > 25000)
      
      climateperiods <- c(
      rep("1. Late Holocene", length(lateholocene)),
      rep("2. Altithermal", length(altithermal)),
      rep("3. Holocene Transition", length(holocenetransition)),
      rep("4. Younger Dryas", length(youngerdryas)),
      rep("5. Bølling-Allerød", length(bollingalerod)),
      rep("6. Deglaciation", length(deglaciation)),
      rep("7. Last Glacial Maximum", length(lastglacialmax)),
      rep("8. Glacial", length(glacial))
      )
      
      
      spectra.line.table.age.unconstrained$Climate <- climateperiods
      spectra.line.table.age.unconstrained$None <- rep(1, length(spectra.line.table.age.unconstrained$Depth))
      
      spectra.line.table.age.constrained <- subset(spectra.line.table.age.unconstrained, spectra.line.table.age.unconstrained$Depth > c14min)
      spectra.line.table.age.constrained <- subset(spectra.line.table.age.constrained, spectra.line.table.age.constrained$Depth < c14max)
      
      spectra.line.table.age <- if(input$constrainage==TRUE) {
          spectra.line.table.age.constrained
      } else if(input$constrainage==FALSE) {
          spectra.line.table.age.unconstrained
      }
      
      spectra.line.table.age$Age <- spectra.line.table.age$Age * -1
      
      
      spectra.line.table.age
      
      
  })
  
  

  
  
  
  #####PCA Analysis
  
  plotInput2 <- reactive({
      
      
      spectra.line.table <- ageData()
    
      
      
      
      
      unique.spec <- seq(1, length(spectra.line.table$Depth), 1)
      null <- rep(1, length(unique.spec))
      
      
      basic <- ggplot(data= spectra.line.table) +
      geom_point(aes(PC1, PC2), size = input$spotsize) +
      scale_x_continuous("Principle Component 1") +
      scale_y_continuous("Principle Component 2") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      #guides(colour=guide_legend(title="K-Means"), shape=guide_legend(title="K-Means"))
      
      
      regular <- ggplot(data= spectra.line.table) +
      geom_point(aes(PC1, PC2, colour=as.factor(Cluster), shape=as.factor(Cluster)), size = input$spotsize+1) +
      geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
      scale_x_continuous("Principle Component 1") +
      scale_y_continuous("Principle Component 2") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_shape_manual("Cluster", values=1:nlevels(as.factor(spectra.line.table$Cluster))) +
      scale_colour_discrete("Cluster")
      
      
      ellipse <- ggplot(data= spectra.line.table)+
      geom_point(aes(PC1, PC2, colour=as.factor(Cluster), shape=as.factor(Cluster)), size = input$spotsize+1) +
      geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
      scale_x_continuous("Principle Component 1") +
      scale_y_continuous("Principle Component 2") +
      theme_light() +
      stat_ellipse(aes(PC1, PC2, colour=as.factor(Cluster), linetype=as.factor(Cluster))) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      guides(linetype=FALSE) +
      scale_shape_manual("Cluster", values=1:nlevels(as.factor(spectra.line.table$Cluster))) +
      scale_colour_discrete("Cluster")
      
      clim.regular <- ggplot(data= spectra.line.table) +
      geom_point(aes(PC1, PC2, colour=as.factor(Climate), shape=as.factor(Climate)), size = input$spotsize+1) +
      geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
      scale_x_continuous("Principle Component 1") +
      scale_y_continuous("Principle Component 2") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_shape_manual("Climatic Period", values=1:nlevels(as.factor(spectra.line.table$Climate))) +
      scale_colour_discrete("Climatic Period")
      
      
      clim.ellipse <- ggplot(data= spectra.line.table)+
      geom_point(aes(PC1, PC2, colour=as.factor(Climate), shape=as.factor(Climate)), size = input$spotsize+1) +
      geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
      scale_x_continuous("Principle Component 1") +
      scale_y_continuous("Principle Component 2") +
      theme_light() +
      stat_ellipse(aes(PC1, PC2, colour=as.factor(Climate), linetype=as.factor(Climate))) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      guides(linetype=FALSE) +
      scale_shape_manual("Climatic Period", values=1:nlevels(as.factor(spectra.line.table$Climate))) +
      scale_colour_discrete("Climatic Period")
      
      
      qual.regular <- ggplot(data= spectra.line.table) +
      geom_point(aes(PC1, PC2, colour=as.factor(Qualitative), shape=as.factor(Qualitative)), size = input$spotsize+1) +
      geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
      scale_x_continuous("Principle Component 1") +
      scale_y_continuous("Principle Component 2") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_shape_manual("Qualitative", values=1:nlevels(as.factor(spectra.line.table$Qualitative))) +
      scale_colour_discrete("Qualitative")
      
      
      qual.ellipse <- ggplot(data= spectra.line.table)+
      geom_point(aes(PC1, PC2, colour=as.factor(Qualitative), shape=as.factor(Qualitative)), size = input$spotsize+1) +
      geom_point(aes(PC1, PC2), colour="grey30", size=input$spotsize-2) +
      scale_x_continuous("Principle Component 1") +
      scale_y_continuous("Principle Component 2") +
      theme_light() +
      stat_ellipse(aes(PC1, PC2, colour=as.factor(Qualitative), linetype=as.factor(Qualitative))) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      guides(linetype=FALSE) +
      scale_shape_manual("Qualitative", values=1:nlevels(as.factor(spectra.line.table$Qualitative))) +
      scale_colour_discrete("Qualitative")
      
      
      quant.regular <- ggplot(data= spectra.line.table) +
      geom_point(aes(PC1, PC2, colour=Depth), size = input$spotsize) +
      scale_x_continuous("Principle Component 1") +
      scale_y_continuous("Principle Component 2") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_colour_gradientn("Depth", colours=rainbow(length(spectra.line.table$Depth)))
      
      
      if (input$elipseplot1 == FALSE && input$pcacolour == "Black") {
          basic
      } else if (input$elipseplot1 == TRUE && input$pcacolour == "Cluster") {
          ellipse
      } else if (input$elipseplot1 == FALSE && input$pcacolour == "Cluster") {
          regular
      } else if (input$elipseplot1 == TRUE && input$pcacolour == "Climate") {
          clim.ellipse
      } else if (input$elipseplot1 == FALSE && input$pcacolour == "Climate") {
          clim.regular
      } else if (input$elipseplot1 == TRUE && input$pcacolour == "Qualitative") {
          qual.ellipse
      } else if (input$elipseplot1 == FALSE && input$pcacolour == "Qualitative") {
          qual.regular
      } else if (input$elipseplot1 == TRUE && input$pcacolour == "Depth") {
          quant.regular
      } else if (input$elipseplot1 == FALSE && input$pcacolour == "Depth") {
          quant.regular
      }
      
      
      
  })
  
  
  output$xrfpcaplot <- renderPlot({
      plotInput2()
      
  })
  
  
  

  
  
  
  
  output$hover_infopca <- renderUI({
      
      point.table <- ageData()
      
      if(is.null(point.table$Age)==TRUE){
          point.table$Age <- rep(1, length(point.table$Spectrum))
      }
      
      hover <- input$plot_hoverpca
      point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Depth: </b>", point$Depth, "<br/>",
      "<b> Age: </b>", point$Age, "<br/>"
      
      )))
      )
  })
  
  output$downloadPlot2 <- downloadHandler(
  filename = function() { paste(paste(c(input$projectname, "_", "PCAPlot"), collapse=''), '.tiff',  sep='') },
  content = function(file) {
      ggsave(file,plotInput2(), device="tiff", compression="lzw", type="cairo", dpi=300, width=12, height=7)
  }
  )
  
  
  
  pcaTableInputFull <- reactive({
      xrf.pca.results <- xrfPCAReactive()
      
      xrf.pca.results
      
      
  })
  
  
  
  
  output$xrfpcatable <- DT::renderDataTable({
      
      
      
      df <- xrfKReactive()
      
      
      
      DT::datatable(df)
      
  })
  
  
  
  
  output$xrfpcatablefull <- DT::renderDataTable({
      
      df <- pcaTableInputFull()
      DT::datatable(df)
      
  })
  
  
  
  
  output$downloadPcaTable <- downloadHandler(
  filename = function() { paste(paste(c(input$projectname, "_", "PCATable"), collapse=''), '.csv', sep=',') },
  content = function(file
  ) {
      write.csv(pcaTableInput(), file)
  }
  )
  
  
  
  
  
  outApp <- reactive({
      
      xrf.k <- ageData()
      
      quality.table <- values[["DF"]]
      
      colour.table <- data.frame(xrf.k$Cluster, xrf.k$Climate, quality.table)
      colnames(colour.table) <- c("Cluster", "Climate", names(quality.table))
      
      names(colour.table)
      
      
  })
  
  
  
  output$inApp <- renderUI({
      selectInput(inputId = "app", label = h4("Application"), choices =  outApp())
  })
  
  
  
  choiceLines <- reactive({
      
      spectra.line.table <- ageData()
      
      standard <- if(input$filetype=="Spectra"){
          colnames(spectra.line.table)
      } else if(input$filetype=="Net"){
          colnames(spectra.line.table)
      }
      
  })
  
 
 dataDefaultSelect <- reactive({
     
     data.options <-defaultLines()
     data.selected <- data.options[5]
     data.selected
     
 })
 
 secondDefaultSelect <- reactive({
     
     data.options <-defaultLines()
     data.selected <- data.options[6]
     data.selected
     
 })
 
 output$inelementtrend <- renderUI({
     selectInput("elementtrend", "Element:", choices=choiceLines(), selected=dataDefaultSelect())
 })
 
 output$inelementnorm <- renderUI({
     selectInput("elementnorm", "Ratio:", choices=choiceLines(), selected="None")
 })
 

 
 
 xmindata <- reactive({
     
     spectra.line.table <- ageData()
     
     x.data <- spectra.line.table[input$xaxistype]
     xmindata <- min(x.data) - min(x.data)*.05
     
         xmindata
     

 })
 
 xmaxdata <- reactive({
     spectra.line.table <- ageData()
     
     x.data <- spectra.line.table[input$xaxistype]
     xmaxdata <- max(x.data) + max(x.data)*.05
     xmaxdata

 })
 
 


output$inxlimrange <- renderUI({


    sliderInput("xlimrange", "X axis", min=xmindata(), max=xmaxdata(), value=c(xmindata(), xmaxdata()), round=FALSE)
})






 
  
  plotInput3a <- reactive({
      

      
      
      x.axis <- if (input$xaxistype=="Depth") {
          paste("Depth (", input$lengthunit, ")", sep="", collapse="")
      } else if (input$xaxistype=="Age" && input$timetype=="BP"){
          paste("cal year BP")
      } else if (input$xaxistype=="Age" && input$timetype=="BC") {
          paste("cal year BC")
      } else if (input$xaxistype=="Age" && input$timetype=="AD") {
          paste("cal year AD")
      } else if (input$xaxistype=="Age" && input$timetype=="BC/AD") {
       paste("cal year BC/AD")
      } else if (input$xaxistype=="Custom") {
          input$customxaxis
      }
      
      
      spectra.line.table <- ageData()
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistype] <= input$xlimrange[1] | spectra.line.table[input$xaxistype] >= input$xlimrange[2]))
      
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmm)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistype)], (spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)]*input$ymultiply), spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      
      
      trendy <-  if(input$elementnorm=="None" && input$filetype=="Spectra" && input$usecalfile==FALSE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
      } else if(input$elementnorm=="None" && input$filetype=="Spectra" && input$usecalfile==TRUE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " ", calFileContentsFirst()[[2]])), sep=",", collapse="")
      } else if(input$elementnorm=="None" && input$filetype=="Net" && input$usecalfile==FALSE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Net Counts")), sep=",", collapse="")
      } else if(input$elementnorm=="None" && input$filetype=="Net" && input$usecalfile==TRUE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " ", calFileContentsFirst()[[2]])), sep=",", collapse="")
      } else if(input$elementnorm!="None" && input$usecustumyaxis==FALSE){
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
      } else if(input$usecustumyaxis==TRUE) {
          paste(input$customyaxis)
      }
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
        scale_x_continuous(paste(x.axis), label=comma) +
        scale_y_continuous(paste(trendy), label=comma)

      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
        scale_y_continuous(paste(trendy), label=comma)



      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7)) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
           scale_y_continuous(paste(trendy), label=comma)



      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
           scale_y_continuous(paste(trendy), label=comma)



      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsize) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
           scale_y_continuous(paste(trendy), label=comma)

      
      climate.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)


      qualitative.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      qualitative.time.series.point <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsize) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)


      depth.time.series <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      ####Flipped X Axis
      
      black.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      smooth.time.series.reverse <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      ramp.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7)) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      area.time.series.reverse <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      
      cluster.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsize) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      climate.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      qualitative.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      qualitative.time.series.point.reverse <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsize) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      depth.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)



      if (input$timecolour == "Black" && input$flipx==FALSE) {
          black.time.series
      } else if (input$timecolour == "Smooth" && input$flipx==FALSE) {
          smooth.time.series
      } else if (input$timecolour == "Selected" && input$flipx==FALSE) {
          ramp.time.series
      } else if (input$timecolour == "Cluster" && input$flipx==FALSE) {
          cluster.time.series
      } else if (input$timecolour == "Climate" && input$flipx==FALSE) {
          climate.time.series.line
      } else if (input$timecolour == "QualitativePoint" && input$flipx==FALSE) {
          qualitative.time.series.point
      } else if (input$timecolour == "QualitativeLine" && input$flipx==FALSE) {
          qualitative.time.series.line
      } else if (input$timecolour == "Depth" && input$flipx==FALSE) {
          depth.time.series
      } else if (input$timecolour == "Area" && input$flipx==FALSE) {
          area.time.series
      } else if (input$timecolour == "Black" && input$flipx==TRUE) {
          black.time.series.reverse
      } else if (input$timecolour == "Smooth" && input$flipx==TRUE) {
          smooth.time.series.reverse
      } else if (input$timecolour == "Selected" && input$flipx==TRUE) {
          ramp.time.series.reverse
      } else if (input$timecolour == "Cluster" && input$flipx==TRUE) {
          cluster.time.series.reverse
      } else if (input$timecolour == "Climate" && input$flipx==TRUE) {
          climate.time.series.line.reverse
      } else if (input$timecolour == "QualitativePoint" && input$flipx==TRUE) {
          qualitative.time.series.point.reverse
      } else if (input$timecolour == "QualitativeLine" && input$flipx==TRUE) {
          qualitative.time.series.line.reverse
      } else if (input$timecolour == "Depth" && input$flipx==TRUE) {
          depth.time.series.reverse
      } else if (input$timecolour == "Area" && input$flipx==TRUE) {
          area.time.series.reverse
      }
      
      
      
      
  })
  
  
  observeEvent(input$timeseriesact1, {
      
  })
  
  output$timeseriesplot1 <- renderPlot({
      input$timeseriesact1
      isolate(plotInput3a())      
  })
  
  hoverHold3a <- reactive({
      input$timeseriesact1
      
      spectra.line.table <- ageData()
      
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistype] <= input$xlimrange[1] | spectra.line.table[input$xaxistype] >= input$xlimrange[2]))
      
      if(is.null(spectra.line.table$Age)==TRUE){
          spectra.line.table$Age <- rep(1, length(spectra.line.table$Depth))
      }
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmm)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistype)], (spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)]*input$ymultiply), spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      isolate(spectra.timeseries.table)
      
  })
  
  
  
  output$hover_info3a <- renderUI({
      
      point.table <- hoverHold3a()
      
      hover <- input$plot_hover3a
      point <- nearPoints(point.table,  xvar="Interval", yvar="Selected", coordinfo=hover,   threshold = 10, maxpoints = 1)
      
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
      "<b> Depth: </b>", point$Depth, "<br/>",
      "<b> Age: </b>", point$Age, "<br/>"
      
      )))
      )
  })
  
  trendPlot <- reactive({
      trendy <-  if(input$elementnorm=="None" && input$filetype=="Spectra") {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " CPS")), sep=",", collapse="")
      } else
        if(input$elementnorm=="None" && input$filetype=="Net") {
              paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " NetCounts")), sep=",", collapse="")
          } else if(input$elementnorm!="None"){
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
      }
      
      trendy.label <- paste(c(input$projectname, "_", trendy), collapse='')
      trendy.label
  })
  
  
  output$downloadPlot3a <- downloadHandler(
  
  
  filename = function() { paste(trendPlot(), '.tiff', sep='') },
  content = function(file) {
  ggsave(file,plotInput3a(), device="tiff", compression="lzw", type="cairo", dpi=300, width=12, height=7)
  

  }
  )
  
  
  
  plotInput3b <- reactive({
      
      
      x.axis <- if (input$xaxistype=="Depth") {
          paste("Depth (", input$lengthunit, ")", sep="", collapse="")
      } else if (input$xaxistype=="Age" && input$timetype=="BP"){
          paste("cal year BP")
      } else if (input$xaxistype=="Age" && input$timetype=="BC") {
          paste("cal year BC")
      } else if (input$xaxistype=="Age" && input$timetype=="AD") {
          paste("cal year AD")
      } else if (input$xaxistype=="Age" && input$timetype=="BC/AD") {
          paste("cal year BC/AD")
      } else if (input$xaxistype=="Custom") {
          input$customxaxis
      }
      
      spectra.line.table <- ageData()
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistype] <= input$xlimrange[1] | spectra.line.table[input$xaxistype] >= input$xlimrange[2]))
      
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmm)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistype)], (spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)]*input$ymultiply), spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      
      trendy <-  if(input$elementnorm=="None" && input$filetype=="Spectra" && input$usecalfile==FALSE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
      } else if(input$elementnorm=="None" && input$filetype=="Spectra" && input$usecalfile==TRUE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " ", calFileContentsFirst()[[2]])), sep=",", collapse="")
      } else if(input$elementnorm=="None" && input$filetype=="Net" && input$usecalfile==FALSE && input$usecustumyaxis==FALSE) {
              paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Net Counts")), sep=",", collapse="")
      } else if(input$elementnorm=="None" && input$filetype=="Net" && input$usecalfile==TRUE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " ", calFileContentsFirst()[[2]])), sep=",", collapse="")
          } else if(input$elementnorm!="None" && input$usecustumyaxis==FALSE){
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
          } else if(input$usecustumyaxis==TRUE) {
              paste(input$customyaxis)
          }
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7)) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +

      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsize) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      climate.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      qualitative.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      qualitative.time.series.point <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsize) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      depth.time.series <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      ####Flipped X Axis
      
      black.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      smooth.time.series.reverse <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      ramp.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7)) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      area.time.series.reverse <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      
      cluster.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsize) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      climate.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      qualitative.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      qualitative.time.series.point.reverse <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsize) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      depth.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      if (input$timecolour == "Black" && input$flipx==FALSE) {
          black.time.series
      } else if (input$timecolour == "Smooth" && input$flipx==FALSE) {
          smooth.time.series
      } else if (input$timecolour == "Selected" && input$flipx==FALSE) {
          ramp.time.series
      } else if (input$timecolour == "Cluster" && input$flipx==FALSE) {
          cluster.time.series
      } else if (input$timecolour == "Climate" && input$flipx==FALSE) {
          climate.time.series.line
      } else if (input$timecolour == "QualitativePoint" && input$flipx==FALSE) {
          qualitative.time.series.point
      } else if (input$timecolour == "QualitativeLine" && input$flipx==FALSE) {
          qualitative.time.series.line
      } else if (input$timecolour == "Depth" && input$flipx==FALSE) {
          depth.time.series
      } else if (input$timecolour == "Area" && input$flipx==FALSE) {
          area.time.series
      } else if (input$timecolour == "Black" && input$flipx==TRUE) {
          black.time.series.reverse
      } else if (input$timecolour == "Smooth" && input$flipx==TRUE) {
          smooth.time.series.reverse
      } else if (input$timecolour == "Selected" && input$flipx==TRUE) {
          ramp.time.series.reverse
      } else if (input$timecolour == "Cluster" && input$flipx==TRUE) {
          cluster.time.series.reverse
      } else if (input$timecolour == "Climate" && input$flipx==TRUE) {
          climate.time.series.line.reverse
      } else if (input$timecolour == "QualitativePoint" && input$flipx==TRUE) {
          qualitative.time.series.point.reverse
      } else if (input$timecolour == "QualitativeLine" && input$flipx==TRUE) {
          qualitative.time.series.line.reverse
      } else if (input$timecolour == "Depth" && input$flipx==TRUE) {
          depth.time.series.reverse
      } else if (input$timecolour == "Area" && input$flipx==TRUE) {
          area.time.series.reverse
      }
      
      
      
  })
  
  
  observeEvent(input$timeseriesact2, {
      
      
  })
  output$timeseriesplot2 <- renderPlot({
      input$timeseriesact2
      isolate(plotInput3b())      
  })
  
  
  hoverHold3b <- reactive({
      input$timeseriesact2
      
      spectra.line.table <- ageData()
      
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistype] <= input$xlimrange[1] | spectra.line.table[input$xaxistype] >= input$xlimrange[2]))
      
      if(is.null(spectra.line.table$Age)==TRUE){
          spectra.line.table$Age <- rep(1, length(spectra.line.table$Depth))
      }
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmm)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistype)], (spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)]*input$ymultiply), spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      isolate(spectra.timeseries.table)
      
  })
  
  
  
  output$hover_info3b <- renderUI({
      
      point.table <- hoverHold3b()
      
      hover <- input$plot_hover3b
      point <- nearPoints(point.table,  xvar="Interval", yvar="Selected", coordinfo=hover,   threshold = 10, maxpoints = 1)
      
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
      "<b> Depth: </b>", point$Depth, "<br/>",
      "<b> Age: </b>", point$Age, "<br/>"
      
      )))
      )
  })
  
  output$downloadPlot3b <- downloadHandler(
  
  
  
  filename = function() { paste(trendPlot(), '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput3b(), device="tiff", compression="lzw", type="cairo", dpi=300, width=12, height=7)
  }
  )
  
  
  
  plotInput3c <- reactive({
      
      
      x.axis <- if (input$xaxistype=="Depth") {
          paste("Depth (", input$lengthunit, ")", sep="", collapse="")
      } else if (input$xaxistype=="Age" && input$timetype=="BP"){
          paste("cal year BP")
      } else if (input$xaxistype=="Age" && input$timetype=="BC") {
          paste("cal year BC")
      } else if (input$xaxistype=="Age" && input$timetype=="AD") {
          paste("cal year AD")
      } else if (input$xaxistype=="Age" && input$timetype=="BC/AD") {
          paste("cal year BC/AD")
      } else if (input$xaxistype=="Custom") {
          input$customxaxis
      }
      
      
      spectra.line.table <- ageData()
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistype] <= input$xlimrange[1] | spectra.line.table[input$xaxistype] >= input$xlimrange[2]))
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmm)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistype)], (spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)]*input$ymultiply), spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      
      trendy <-  if(input$elementnorm=="None" && input$filetype=="Spectra" && input$usecalfile==FALSE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
      } else if(input$elementnorm=="None" && input$filetype=="Spectra" && input$usecalfile==TRUE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " ", calFileContentsFirst()[[2]])), sep=",", collapse="")
      } else if(input$elementnorm=="None" && input$filetype=="Net" && input$usecalfile==FALSE && input$usecustumyaxis==FALSE) {
              paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Net Counts")), sep=",", collapse="")
      } else if(input$elementnorm=="None" && input$filetype=="Net" && input$usecalfile==TRUE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " ", calFileContentsFirst()[[2]])), sep=",", collapse="")
          } else if(input$elementnorm!="None" && input$usecustumyaxis==FALSE){
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
          } else if(input$usecustumyaxis==TRUE) {
              paste(input$customyaxis)
          }
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7)) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +

      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsize) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      climate.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      qualitative.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      qualitative.time.series.point <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsize) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      depth.time.series <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      ####Flipped X Axis
      
      black.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      smooth.time.series.reverse <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      ramp.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7)) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      area.time.series.reverse <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      
      cluster.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsize) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      climate.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      qualitative.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      qualitative.time.series.point.reverse <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsize) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      depth.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      if (input$timecolour == "Black" && input$flipx==FALSE) {
          black.time.series
      } else if (input$timecolour == "Smooth" && input$flipx==FALSE) {
          smooth.time.series
      } else if (input$timecolour == "Selected" && input$flipx==FALSE) {
          ramp.time.series
      } else if (input$timecolour == "Cluster" && input$flipx==FALSE) {
          cluster.time.series
      } else if (input$timecolour == "Climate" && input$flipx==FALSE) {
          climate.time.series.line
      } else if (input$timecolour == "QualitativePoint" && input$flipx==FALSE) {
          qualitative.time.series.point
      } else if (input$timecolour == "QualitativeLine" && input$flipx==FALSE) {
          qualitative.time.series.line
      } else if (input$timecolour == "Depth" && input$flipx==FALSE) {
          depth.time.series
      } else if (input$timecolour == "Area" && input$flipx==FALSE) {
          area.time.series
      } else if (input$timecolour == "Black" && input$flipx==TRUE) {
          black.time.series.reverse
      } else if (input$timecolour == "Smooth" && input$flipx==TRUE) {
          smooth.time.series.reverse
      } else if (input$timecolour == "Selected" && input$flipx==TRUE) {
          ramp.time.series.reverse
      } else if (input$timecolour == "Cluster" && input$flipx==TRUE) {
          cluster.time.series.reverse
      } else if (input$timecolour == "Climate" && input$flipx==TRUE) {
          climate.time.series.line.reverse
      } else if (input$timecolour == "QualitativePoint" && input$flipx==TRUE) {
          qualitative.time.series.point.reverse
      } else if (input$timecolour == "QualitativeLine" && input$flipx==TRUE) {
          qualitative.time.series.line.reverse
      } else if (input$timecolour == "Depth" && input$flipx==TRUE) {
          depth.time.series.reverse
      } else if (input$timecolour == "Area" && input$flipx==TRUE) {
          area.time.series.reverse
      }
      
      
      
      
  })
  
  observeEvent(input$timeseriesact3, {
  })
  
  output$timeseriesplot3 <- renderPlot({
      input$timeseriesact3
      
      isolate(plotInput3c())      
      
  })
  
  hoverHold3c <- reactive({
      input$timeseriesact3
      
      spectra.line.table <- ageData()
      
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistype] <= input$xlimrange[1] | spectra.line.table[input$xaxistype] >= input$xlimrange[2]))
      
      if(is.null(spectra.line.table$Age)==TRUE){
          spectra.line.table$Age <- rep(1, length(spectra.line.table$Depth))
      }
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmm)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistype)], (spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)]*input$ymultiply), spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      isolate(spectra.timeseries.table)
      
  })
  
  
  
  output$hover_info3c <- renderUI({
      
      point.table <- hoverHold3c()
      
      hover <- input$plot_hover3c
      point <- nearPoints(point.table,  xvar="Interval", yvar="Selected", coordinfo=hover,   threshold = 10, maxpoints = 1)
      
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
      "<b> Depth: </b>", point$Depth, "<br/>",
      "<b> Age: </b>", point$Age, "<br/>"
      
      )))
      )
  })
  
  output$downloadPlot3c <- downloadHandler(
  
  
  filename = function() { paste(trendPlot(), '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput3c(), device="tiff", compression="lzw", type="cairo", dpi=300, width=12, height=7)
  }
  )
  
  
  plotInput3d <- reactive({
      
      
      x.axis <- if (input$xaxistype=="Depth") {
          paste("Depth (", input$lengthunit, ")", sep="", collapse="")
      } else if (input$xaxistype=="Age" && input$timetype=="BP"){
          paste("cal year BP")
      } else if (input$xaxistype=="Age" && input$timetype=="BC") {
          paste("cal year BC")
      } else if (input$xaxistype=="Age" && input$timetype=="AD") {
          paste("cal year AD")
      } else if (input$xaxistype=="Age" && input$timetype=="BC/AD") {
          paste("cal year BC/AD")
      }
      
      
      spectra.line.table <- ageData()
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistype] <= input$xlimrange[1] | spectra.line.table[input$xaxistype] >= input$xlimrange[2]))
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmm)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistype)], (spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)]*input$ymultiply), spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      
      trendy <-  if(input$elementnorm=="None" && input$filetype=="Spectra" && input$usecalfile==FALSE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
      } else if(input$elementnorm=="None" && input$filetype=="Spectra" && input$usecalfile==TRUE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " ", calFileContentsFirst()[[2]])), sep=",", collapse="")
      } else if(input$elementnorm=="None" && input$filetype=="Net" && input$usecalfile==FALSE && input$usecustumyaxis==FALSE) {
              paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Net Counts")), sep=",", collapse="")
      } else if(input$elementnorm=="None" && input$filetype=="Net" && input$usecalfile==TRUE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " ", calFileContentsFirst()[[2]])), sep=",", collapse="")
          } else if(input$elementnorm!="None" && input$usecustumyaxis==FALSE){
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
          } else if(input$usecustumyaxis==TRUE) {
              paste(input$customyaxis)
          }
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7)) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +

      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsize) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      climate.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      qualitative.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      qualitative.time.series.point <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsize) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      depth.time.series <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      
      
      ####Flipped X Axis
      
      black.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      smooth.time.series.reverse <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      ramp.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7)) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      area.time.series.reverse <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      
      cluster.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsize) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      climate.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      qualitative.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      qualitative.time.series.point.reverse <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsize) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      depth.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      if (input$timecolour == "Black" && input$flipx==FALSE) {
          black.time.series
      } else if (input$timecolour == "Smooth" && input$flipx==FALSE) {
          smooth.time.series
      } else if (input$timecolour == "Selected" && input$flipx==FALSE) {
          ramp.time.series
      } else if (input$timecolour == "Cluster" && input$flipx==FALSE) {
          cluster.time.series
      } else if (input$timecolour == "Climate" && input$flipx==FALSE) {
          climate.time.series.line
      } else if (input$timecolour == "QualitativePoint" && input$flipx==FALSE) {
          qualitative.time.series.point
      } else if (input$timecolour == "QualitativeLine" && input$flipx==FALSE) {
          qualitative.time.series.line
      } else if (input$timecolour == "Depth" && input$flipx==FALSE) {
          depth.time.series
      } else if (input$timecolour == "Area" && input$flipx==FALSE) {
          area.time.series
      } else if (input$timecolour == "Black" && input$flipx==TRUE) {
          black.time.series.reverse
      } else if (input$timecolour == "Smooth" && input$flipx==TRUE) {
          smooth.time.series.reverse
      } else if (input$timecolour == "Selected" && input$flipx==TRUE) {
          ramp.time.series.reverse
      } else if (input$timecolour == "Cluster" && input$flipx==TRUE) {
          cluster.time.series.reverse
      } else if (input$timecolour == "Climate" && input$flipx==TRUE) {
          climate.time.series.line.reverse
      } else if (input$timecolour == "QualitativePoint" && input$flipx==TRUE) {
          qualitative.time.series.point.reverse
      } else if (input$timecolour == "QualitativeLine" && input$flipx==TRUE) {
          qualitative.time.series.line.reverse
      } else if (input$timecolour == "Depth" && input$flipx==TRUE) {
          depth.time.series.reverse
      } else if (input$timecolour == "Area" && input$flipx==TRUE) {
          area.time.series.reverse
      }
      
      
      
      
  })
  
  observeEvent(input$timeseriesact4, {
      
  })
  
  
  
  output$timeseriesplot4 <- renderPlot({
      input$timeseriesact4
      isolate(plotInput3d())      
  })
  
  hoverHold3d <- reactive({
      input$timeseriesact4
      
      spectra.line.table <- ageData()
      
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistype] <= input$xlimrange[1] | spectra.line.table[input$xaxistype] >= input$xlimrange[2]))
      
      if(is.null(spectra.line.table$Age)==TRUE){
          spectra.line.table$Age <- rep(1, length(spectra.line.table$Depth))
      }
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmm)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistype)], (spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)]*input$ymultiply), spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      isolate(spectra.timeseries.table)
      
  })
  
  
  
  output$hover_info3d <- renderUI({
      
      point.table <- hoverHold3d()
      
      hover <- input$plot_hover3d
      point <- nearPoints(point.table,  xvar="Interval", yvar="Selected", coordinfo=hover,   threshold = 10, maxpoints = 1)
      
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
      "<b> Depth: </b>", point$Depth, "<br/>",
      "<b> Age: </b>", point$Age, "<br/>"
      
      )))
      )
  })
  
  output$downloadPlot3d <- downloadHandler(
  
  
  
  filename = function() { paste(trendPlot(), '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput3d(), device="tiff", compression="lzw", type="cairo", dpi=300, width=12, height=7)
  }
  )
  
  
  
  
  
  plotInput3e <- reactive({
      
      
      
      x.axis <- if (input$xaxistype=="Depth") {
          paste("Depth (", input$lengthunit, ")", sep="", collapse="")
      } else if (input$xaxistype=="Age" && input$timetype=="BP"){
          paste("cal year BP")
      } else if (input$xaxistype=="Age" && input$timetype=="BC") {
          paste("cal year BC")
      } else if (input$xaxistype=="Age" && input$timetype=="AD") {
          paste("cal year AD")
      } else if (input$xaxistype=="Age" && input$timetype=="BC/AD") {
          paste("cal year BC/AD")
      } else if (input$xaxistype=="Custom") {
          input$customxaxis
      }
      
      
      spectra.line.table <- ageData()
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistype] <= input$xlimrange[1] | spectra.line.table[input$xaxistype] >= input$xlimrange[2]))
      
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmm)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistype)], (spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)]*input$ymultiply), spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      
      trendy <-  if(input$elementnorm=="None" && input$filetype=="Spectra" && input$usecalfile==FALSE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
      } else if(input$elementnorm=="None" && input$filetype=="Spectra" && input$usecalfile==TRUE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " ", calFileContentsFirst()[[2]])), sep=",", collapse="")
      } else if(input$elementnorm=="None" && input$filetype=="Net" && input$usecalfile==FALSE && input$usecustumyaxis==FALSE) {
              paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Net Counts")), sep=",", collapse="")
      } else if(input$elementnorm=="None" && input$filetype=="Net" && input$usecalfile==TRUE && input$usecustumyaxis==FALSE) {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " ", calFileContentsFirst()[[2]])), sep=",", collapse="")
          } else if(input$elementnorm!="None" && input$usecustumyaxis==FALSE){
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
          } else if(input$usecustumyaxis==TRUE) {
              paste(input$customyaxis)
          }
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7)) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +

      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsize) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      climate.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      qualitative.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      qualitative.time.series.point <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsize) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      depth.time.series <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
       scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      
      ####X Axis Flipped
      
      black.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      smooth.time.series.reverse <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      ramp.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesize) +
      theme_light() +
      scale_colour_gradientn(colours=rainbow(7)) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      area.time.series.reverse <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      
      cluster.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsize) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      climate.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      qualitative.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothing), geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      qualitative.time.series.point.reverse <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsize) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      depth.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothing),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(paste(trendy), label=comma)
      
      
      if (input$timecolour == "Black" && input$flipx==FALSE) {
          black.time.series
      } else if (input$timecolour == "Smooth" && input$flipx==FALSE) {
          smooth.time.series
      } else if (input$timecolour == "Selected" && input$flipx==FALSE) {
          ramp.time.series
      } else if (input$timecolour == "Cluster" && input$flipx==FALSE) {
          cluster.time.series
      } else if (input$timecolour == "Climate" && input$flipx==FALSE) {
          climate.time.series.line
      } else if (input$timecolour == "QualitativePoint" && input$flipx==FALSE) {
          qualitative.time.series.point
      } else if (input$timecolour == "QualitativeLine" && input$flipx==FALSE) {
          qualitative.time.series.line
      } else if (input$timecolour == "Depth" && input$flipx==FALSE) {
          depth.time.series
      } else if (input$timecolour == "Area" && input$flipx==FALSE) {
          area.time.series
      } else if (input$timecolour == "Black" && input$flipx==TRUE) {
          black.time.series.reverse
      } else if (input$timecolour == "Smooth" && input$flipx==TRUE) {
          smooth.time.series.reverse
      } else if (input$timecolour == "Selected" && input$flipx==TRUE) {
          ramp.time.series.reverse
      } else if (input$timecolour == "Cluster" && input$flipx==TRUE) {
          cluster.time.series.reverse
      } else if (input$timecolour == "Climate" && input$flipx==TRUE) {
          climate.time.series.line.reverse
      } else if (input$timecolour == "QualitativePoint" && input$flipx==TRUE) {
          qualitative.time.series.point.reverse
      } else if (input$timecolour == "QualitativeLine" && input$flipx==TRUE) {
          qualitative.time.series.line.reverse
      } else if (input$timecolour == "Depth" && input$flipx==TRUE) {
          depth.time.series.reverse
      } else if (input$timecolour == "Area" && input$flipx==TRUE) {
          area.time.series.reverse
      }
      
      
      
      
  })
  
  observeEvent(input$timeseriesact5, {
      
      
  })
  
  output$timeseriesplot5 <- renderPlot({
      input$timeseriesact5
      
      
      
      isolate(plotInput3e())      
  })
  
  hoverHold3e <- reactive({
      input$timeseriesact5
      
      spectra.line.table <- ageData()
      
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistype] <= input$xlimrange[1] | spectra.line.table[input$xaxistype] >= input$xlimrange[2]))
      
      if(is.null(spectra.line.table$Age)==TRUE){
          spectra.line.table$Age <- rep(1, length(spectra.line.table$Depth))
      }
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmm)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistype)], (spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)]*input$ymultiply), spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      isolate(spectra.timeseries.table)
      
  })
  
  
  
  output$hover_info3e <- renderUI({
      
      point.table <- hoverHold3e()
      
      hover <- input$plot_hover3e
      point <- nearPoints(point.table,  xvar="Interval", yvar="Selected", coordinfo=hover,   threshold = 10, maxpoints = 1)
      
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
      "<b> Depth: </b>", point$Depth, "<br/>",
      "<b> Age: </b>", point$Age, "<br/>"
      
      )))
      )
  })
  
  
  output$downloadPlot3e <- downloadHandler(
  
  
  
  filename = function() { paste(trendPlot(), '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput3e(), device="tiff", compression="lzw", type="cairo", dpi=300, width=12, height=7)
  }
  )
  
  
  
  ratioChooseA <- reactive({
      spectra.line.table <- ageData()
      spectra.line.names <- colnames(spectra.line.table)


      standard <- spectra.line.names[2]
      standard
      
  })
  
  
  
  ratioChooseB <- reactive({
      spectra.line.table <- ageData()
      spectra.line.names <- colnames(spectra.line.table)


      standard <- spectra.line.names[3]
      
      
      standard
      
  })
  
  
  ratioChooseC <- reactive({
      spectra.line.table <- ageData()
      spectra.line.names <- colnames(spectra.line.table)


      standard <- spectra.line.names[4]
      
      
      standard
      
  })
  
  
  ratioChooseD <- reactive({
      spectra.line.table <- ageData()
      spectra.line.names <- colnames(spectra.line.table)
      
      standard <- spectra.line.names[5]
      
      
      standard
      
  })
  
  
  output$inelementratioa <- renderUI({
      selectInput("elementratioa", "Element A", choices=choiceLines(), selected=ratioChooseA())
  })
  
  output$inelementratiob <- renderUI({
      selectInput("elementratiob", "Element B", choices=choiceLines(), selected=ratioChooseB())
  })
  
  output$inelementratioc <- renderUI({
      selectInput("elementratioc", "Element C", choices=choiceLines(), selected=ratioChooseC())
  })
  
  output$inelementratiod <- renderUI({
      selectInput("elementratiod", "Element D", choices=choiceLines(), selected=ratioChooseD())
  })
  
  
  
  plotInput4 <- reactive({
      
      
      spectra.line.table <- ageData()
      
      unique.spec <- seq(1, length(spectra.line.table$Depth), 1)
      null <- rep(1, length(unique.spec))
     
      
      first.ratio <-spectra.line.table[input$elementratioa]
      second.ratio <- spectra.line.table[input$elementratiob]
      third.ratio <- spectra.line.table[input$elementratioc]
      fourth.ratio <- spectra.line.table[input$elementratiod]
      
   
      
      ratio.frame <- data.frame(first.ratio, second.ratio, third.ratio, fourth.ratio, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Age, spectra.line.table$Climate)
      colnames(ratio.frame) <- c(gsub("[.]", "", c(substr(input$elementratioa, 1, 2), substr(input$elementratiob, 1, 2), substr(input$elementratioc, 1, 2), substr(input$elementratiod, 1, 2), "Cluster", "Qualitative", "Depth", "Age", "Climate")))
      

      
      
      ratio.names.x <- if(input$elementratiob!="None"){
          paste(names(ratio.frame[1]), "/", names(ratio.frame[2]), sep="", collapse="")
      } else if(input$elementratiob=="None"){
          paste(names(ratio.frame[1]))
      }
      
      
      ratio.names.y <- if(input$elementratiod!="None"){
          paste(names(ratio.frame[3]), "/", names(ratio.frame[4]), sep="", collapse="")
      } else if(input$elementratiod=="None"){
          paste(names(ratio.frame[3]))
      }
      
      ratio.names.x <- paste(ratio.names.x, sep=",", collapse="")
      ratio.names.y <- paste(ratio.names.y, sep=",", collapse="")
      
      ratio.frame$X <- ratio.frame[,1]/ratio.frame[,2]
      ratio.frame$Y <- ratio.frame[,3]/ratio.frame[,4]
      
      
      black.ratio.plot <- qplot(X, Y, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y  ) +
      geom_point(lwd=input$spotsize2) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      cluster.ratio.plot <- qplot(X, Y, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y ) +
      geom_point(aes(colour=as.factor(ratio.frame$Cluster), shape=as.factor(ratio.frame$Cluster)), size=input$spotsize2+1) +
      geom_point(colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Cluster", values=1:nlevels(as.factor(as.factor(ratio.frame$Cluster)))) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      cluster.ratio.ellipse.plot <- qplot(X, Y, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y  ) +
      stat_ellipse(aes(ratio.frame$X, ratio.frame$Y, colour=as.factor(ratio.frame$Cluster))) +
      geom_point(aes(colour=as.factor(ratio.frame$Cluster), shape=as.factor(ratio.frame$Cluster)), size=input$spotsize2+1) +
      geom_point(colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Cluster", values=1:nlevels(as.factor(as.factor(ratio.frame$Cluster)))) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      
      climate.ratio.plot <- qplot(X, Y, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y  ) +
      geom_point(aes(colour=as.factor(ratio.frame$Climate), shape=as.factor(ratio.frame$Climate)), size=input$spotsize2+1) +
      geom_point(colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Climatic Period", values=1:nlevels(ratio.frame$Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      climate.ratio.ellipse.plot <- qplot(X, Y, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y  ) +
      stat_ellipse(aes(ratio.frame$X, ratio.frame$Y, colour=as.factor(ratio.frame$Climate))) +
      geom_point(aes(colour=as.factor(ratio.frame$Climate), shape=as.factor(ratio.frame$Climate)), size=input$spotsize2+1) +
      geom_point(colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Climatic Period", values=1:nlevels(ratio.frame$Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)

      
      qualitative.ratio.plot <- qplot(X, Y, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y  ) +
      geom_point(aes(colour=as.factor(ratio.frame$Qualitative), shape=as.factor(ratio.frame$Qualitative)), size=input$spotsize2+1) +
      geom_point(colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Qualitative", values=1:nlevels(ratio.frame$Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      qualitative.ratio.ellipse.plot <- qplot(X, Y, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y  ) +
      stat_ellipse(aes(ratio.frame$X, ratio.frame$Y, colour=as.factor(ratio.frame$Qualitative))) +
      geom_point(aes(colour=as.factor(ratio.frame$Qualitative), shape=as.factor(ratio.frame$Qualitative)), size=input$spotsize2+1) +
      geom_point(colour="grey30", size=input$spotsize2-2) +
      scale_shape_manual("Qualitative", values=1:nlevels(ratio.frame$Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      depth.ratio.plot <- qplot(X, Y, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y  ) +
      geom_point(aes(colour = ratio.frame$Depth), size=input$spotsize2+1) +
      geom_point(size=input$spotsize2-2) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(ratio.frame$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      age.ratio.plot <- qplot(X, Y, data=ratio.frame, xlab = ratio.names.x, ylab = ratio.names.y  ) +
      geom_point(aes(colour = ratio.frame$Age), size=input$spotsize2+1) +
      geom_point(size=input$spotsize2-2) +
      scale_colour_gradientn("Age", colours=terrain.colors(length(ratio.frame$Age))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      
      
      
      
      
      
      if (input$ratiocolour == "Black" && input$elipseplot2==FALSE) {
          black.ratio.plot
      } else if (input$ratiocolour == "Cluster" && input$elipseplot2==FALSE) {
          cluster.ratio.plot
      } else if (input$ratiocolour == "Cluster" && input$elipseplot2==TRUE) {
          cluster.ratio.ellipse.plot
      } else if (input$ratiocolour == "Climate" && input$elipseplot2==FALSE) {
          climate.ratio.plot
      } else if (input$ratiocolour == "Climate" && input$elipseplot2==TRUE ) {
          climate.ratio.ellipse.plot
      } else if (input$ratiocolour == "Qualitative" && input$elipseplot2==FALSE) {
          qualitative.ratio.plot
      } else if (input$ratiocolour == "Qualitative" && input$elipseplot2==TRUE ) {
          qualitative.ratio.ellipse.plot
      } else if (input$ratiocolour == "Depth") {
          depth.ratio.plot
      } else if (input$ratiocolour == "Age") {
          age.ratio.plot
      }
      
  })
  
  
  output$elementratiotimeseries <- renderPlot({
      plotInput4()
      
      
  })
  
  
  hoverHoldRatio <- reactive({
      
      spectra.line.table <- ageData()
      
      if(is.null(spectra.line.table$Age)==TRUE){
          spectra.line.table$Age <- rep(1, length(spectra.line.table$Depth))
      }
  
      first.ratio <-spectra.line.table[input$elementratioa]
      second.ratio <- spectra.line.table[input$elementratiob]
      third.ratio <- spectra.line.table[input$elementratioc]
      fourth.ratio <- spectra.line.table[input$elementratiod]
      
      first.ratio.norm <- first.ratio/sum(first.ratio)
      second.ratio.norm <- second.ratio/sum(second.ratio)
      third.ratio.norm <- third.ratio/sum(third.ratio)
      fourth.ratio.norm <- fourth.ratio/sum(fourth.ratio)
      
      
      ratio.frame <- data.frame(first.ratio, second.ratio, third.ratio, fourth.ratio, spectra.line.table$Depth, spectra.line.table$Age)
      colnames(ratio.frame) <- gsub("[.]", "", c(substr(input$elementratioa, 1, 2), substr(input$elementratiob, 1, 2), substr(input$elementratioc, 1, 2), substr(input$elementratiod, 1, 2), "Depth", "Age"))
      
      
      ratio.names.x <- c(names(ratio.frame[1]), "/", names(ratio.frame[2]))
      ratio.names.y <- c(names(ratio.frame[3]), "/", names(ratio.frame[4]))
      
      ratio.names.x <- paste(ratio.names.x, sep=",", collapse="")
      ratio.names.y <- paste(ratio.names.y, sep=",", collapse="")
      

      ratio.frame$X <- ratio.frame[,1]/ratio.frame[,2]
      ratio.frame$Y <- ratio.frame[,3]/ratio.frame[,4]
      
      ratio.frame
      
  })
  
  
  
  
  output$hover_inforatio <- renderUI({
      
      point.table <- hoverHoldRatio()
      
      hover <- input$plot_hoverratio
      point <- nearPoints(point.table,  coordinfo=hover,   threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Depth: </b>", point$Depth, "<br/>",
      "<b> Age: </b>", point$Age, "<br/>"
      
      
      )))
      )
  })
  
  ratioTerm <- reactive({
      
      ratio.names.x <- if(input$elementratiob!="None"){
          paste(substr(input$elementratioa, 1, 2), "-", substr(input$elementratiob, 1, 2), sep="", collapse="")
      } else if(input$elementratiob=="None"){
          paste(substr(input$elementratioa, 1, 2))
      }
      
      
      ratio.names.y <- if(input$elementratiod!="None"){
          paste(substr(input$elementratioc, 1, 2), "-", substr(input$elementratiod, 1, 2), sep="", collapse="")
      } else if(input$elementratiod=="None"){
          paste(substr(input$elementratioc, 1, 2))
      }
      
      ratio.names.x <- paste(ratio.names.x, sep=",", collapse="")
      ratio.names.y <- paste(ratio.names.y, sep=",", collapse="")
      
      ratio.names <- paste(ratio.names.x, ratio.names.y, sep= "-", collapse="")
      
      ratio.label <- paste(input$projectname, "_", ratio.names, collapse="")

      ratio.label
  })
  
  output$downloadPlot4 <- downloadHandler(
  
  
  filename = function() { paste(ratioTerm(), '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput4(), device="tiff", compression="lzw", type="cairo", dpi=300, width=12, height=7)
  }
  )
  
  
  
  ternaryChooseA <- reactive({
      spectra.line.table <- ageData()
      spectra.line.names <- colnames(spectra.line.table)


      standard <- if(input$filetype=="Spectra"){
          "Al.K.alpha"
      } else if(input$filetype=="Net"){
          spectra.line.names[2]
      }
      
  })
  
  ternaryChooseB <- reactive({
      spectra.line.table <- ageData()
      spectra.line.names <- colnames(spectra.line.table)


      standard <- if(input$filetype=="Spectra"){
          "Si.K.alpha"
      } else if(input$filetype=="Net"){
          spectra.line.names[3]
      }
      
  })
  
  ternaryChooseC <- reactive({
      spectra.line.table <- ageData()
      spectra.line.names <- colnames(spectra.line.table)


      standard <- if(input$filetype=="Spectra"){
          "Ca.K.alpha"
      } else if(input$filetype=="Net"){
          spectra.line.names[4]
      }
      
  })
  
  output$inaxisa <- renderUI({
      selectInput("axisa", "Axis A", choices=choiceLines(), selected=ternaryChooseA())
  })
  
  output$inaxisb <- renderUI({
      selectInput("axisb", "Axis B", choices=choiceLines(), selected=ternaryChooseB())
  })
  
  output$inaxisc <- renderUI({
      selectInput("axisc", "Axis C", choices=choiceLines(), selected=ternaryChooseC())
  })
  
  
  
  plotInput5 <- reactive({
      
      
      spectra.line.table <- ageData()

      
      
      unique.spec <- seq(1, length(spectra.line.table$Depth), 1)
      null <- rep(1, length(unique.spec))
      
      first.axis <- spectra.line.table[input$axisa]
      second.axis <- spectra.line.table[input$axisb]
      third.axis <- spectra.line.table[input$axisc]
      
      first.axis.norm <- first.axis/sum(first.axis)
      second.axis.norm <- second.axis/sum(second.axis)
      third.axis.norm <- third.axis/sum(third.axis)
      
      axis.frame <- data.frame(first.axis, second.axis, third.axis, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Age, spectra.line.table$Climate)
      colnames(axis.frame) <- gsub("[.]", "", c(substr(input$axisa, 1, 2), substr(input$axisb, 1, 2), substr(input$axisc, 1, 2), "Cluster", "Qualitative", "Depth", "Age", "Climate"))
      
      axis.frame.norm <- data.frame(first.axis.norm, second.axis.norm, third.axis.norm, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Age, spectra.line.table$Climate)
      colnames(axis.frame.norm) <- gsub("[.]", "", c(substr(input$axisa, 1, 2), substr(input$axisb, 1, 2), substr(input$axisc, 1, 2), "Cluster", "Qualitative", "Depth", "Age", "Climate"))
      
      ternaryplot1 <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
      geom_point(size=input$ternpointsize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplot2 <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
      geom_density_tern() +
      geom_point(size=input$ternpointsize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotcluster <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
      geom_point(aes(colour = as.factor(Cluster), shape=as.factor(Cluster)), size=input$ternpointsize+1) +
      geom_point(colour="grey30", size=input$ternpointsize-2) +
      scale_shape_manual("Cluster", values=1:nlevels(as.factor(axis.frame$Cluster))) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotclusterellipse <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
      geom_density_tern() +
      geom_point(aes(colour = as.factor(Cluster), shape=as.factor(Cluster)), size=input$ternpointsize) +
      geom_point(colour="grey30", size=input$ternpointsize-2) +
      scale_shape_manual("Cluster", values=1:nlevels(as.factor(axis.frame$Cluster))) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotclimate <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
      geom_point(aes(colour = as.factor(Climate), shape=as.factor(Climate)), size=input$ternpointsize+1) +
      geom_point(colour="grey30", size=input$ternpointsize-2) +
      scale_shape_manual("Climatic Period", values=1:nlevels(axis.frame$Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotclimateellipse <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
      geom_density_tern() +
      geom_point(aes(colour = as.factor(Climate), shape=as.factor(Climate)), size=input$ternpointsize+1) +
      geom_point(colour="grey30", size=input$ternpointsize-2) +
      scale_shape_manual("Climatic Period", values=1:nlevels(axis.frame$Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      
      ternaryplotdepthellipse <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
      geom_point(aes(colour = Depth), size=input$ternpointsize+1) +
      geom_point(size=input$ternpointsize-2) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(axis.frame$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotdepth <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
      geom_point(aes(colour = Depth), size=input$ternpointsize+1) +
      geom_point(size=input$ternpointsize-2) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(axis.frame$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotage <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
      geom_point(aes(colour = Age), size=input$ternpointsize+1) +
      geom_point(size=input$ternpointsize-2) +
      scale_colour_gradientn("Age", colours=terrain.colors(length(axis.frame$Age))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotageellipse <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
      geom_density_tern() +
      geom_point(aes(colour = Age), size=input$ternpointsize+1) +
      geom_point(size=input$ternpointsize-2) +
      scale_colour_gradientn("Age", colours=terrain.colors(length(axis.frame$Age))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
    
      
      ternaryplotqualitative <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
      geom_point(aes(colour = as.factor(Qualitative), shape=as.factor(Qualitative)), size=input$ternpointsize+1) +
      geom_point(colour="grey30", size=input$ternpointsize-2) +
      scale_shape_manual("Qualitative", values=1:nlevels(axis.frame$Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotqualitativeellipse <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
      geom_density_tern() +
      geom_point(aes(colour = as.factor(Qualitative), shape=as.factor(Qualitative)), size=input$ternpointsize+1) +
      geom_point(colour="grey30", size=input$ternpointsize-2) +
      scale_shape_manual("Qualitative", values=1:nlevels(axis.frame$Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      

      
      
      #####Normalization
      
      ternaryplot1.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
      geom_point(size=input$ternpointsize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplot2.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
      geom_density_tern() +
      geom_point(size=input$ternpointsize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotcluster.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
      geom_point(aes(colour = as.factor(Cluster), shape=as.factor(Cluster)), size=input$ternpointsize+1) +
      geom_point(colour="grey30", size=input$ternpointsize-2) +
      scale_shape_manual("Cluster", values=1:nlevels(as.factor(axis.frame$Cluster))) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotclusterellipse.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
      geom_density_tern() +
      geom_point(aes(colour = as.factor(Cluster), shape=as.factor(Cluster)), size=input$ternpointsize) +
      geom_point(colour="grey30", size=input$ternpointsize-2) +
      scale_shape_manual("Cluster", values=1:nlevels(as.factor(axis.frame$Cluster))) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotclimate.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
      geom_point(aes(colour = as.factor(Climate), shape=as.factor(Climate)), size=input$ternpointsize+1) +
      geom_point(colour="grey30", size=input$ternpointsize-2) +
      scale_shape_manual("Climatic Period", values=1:nlevels(axis.frame$Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotclimateellipse.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
      geom_density_tern() +
      geom_point(aes(colour = as.factor(Climate), shape=as.factor(Climate)), size=input$ternpointsize+1) +
      geom_point(colour="grey30", size=input$ternpointsize-2) +
      scale_shape_manual("Climatic Period", values=1:nlevels(axis.frame$Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotqualitative.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
      geom_point(aes(colour = as.factor(Qualitative), shape=as.factor(Qualitative)), size=input$ternpointsize+1) +
      geom_point(colour="grey30", size=input$ternpointsize-2) +
      scale_shape_manual("Qualitative", values=1:nlevels(axis.frame$Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotqualitativeellipse.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
      geom_density_tern() +
      geom_point(aes(colour = as.factor(Qualitative), shape=as.factor(Qualitative)), size=input$ternpointsize+1) +
      geom_point(colour="grey30", size=input$ternpointsize-2) +
      scale_shape_manual("Qualitative", values=1:nlevels(axis.frame$Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotdepth.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
      geom_point(aes(colour = Depth), size=input$ternpointsize+1) +
      geom_point(size=input$ternpointsize-2) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(axis.frame$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      
      ternaryplotdepthellipse.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
      geom_density_tern() +
      geom_point(aes(colour = Depth), size=input$ternpointsize) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(axis.frame$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotage.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
      geom_point(aes(colour = Age), size=input$ternpointsize+1) +
      geom_point(size=input$ternpointsize-2) +
      scale_colour_gradientn("Age", colours=terrain.colors(length(axis.frame$Age))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotageellipse.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
      geom_density_tern() +
      geom_point(aes(colour = Age), size=input$ternpointsize+1) +
      geom_point(size=input$ternpointsize-2) +
      scale_colour_gradientn("Age", colours=terrain.colors(length(axis.frame$Age))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      
      if (input$ternarycolour == "black" && input$terndensityplot==FALSE && input$ternnormplot==FALSE) {
          ternaryplot1
      } else if (input$ternarycolour == "black" && input$terndensityplot==TRUE && input$ternnormplot==FALSE) {
          ternaryplot2
      } else if (input$ternarycolour == "Cluster" && input$terndensityplot==FALSE && input$ternnormplot==FALSE) {
          ternaryplotcluster
      } else if (input$ternarycolour == "Cluster" && input$terndensityplot==TRUE && input$ternnormplot==FALSE) {
          ternaryplotclusterellipse
      } else if (input$ternarycolour == "Qualitative" && input$terndensityplot==FALSE && input$ternnormplot==FALSE) {
          ternaryplotqualitative
      } else if (input$ternarycolour == "Qualitative" && input$terndensityplot==TRUE && input$ternnormplot==FALSE) {
          ternaryplotqualitativeellipse
      } else if (input$ternarycolour == "Climate" && input$terndensityplot==FALSE && input$ternnormplot==FALSE) {
          ternaryplotclimate
      } else if (input$ternarycolour == "Climate" && input$terndensityplot==TRUE && input$ternnormplot==FALSE) {
          ternaryplotclimateellipse
      } else if (input$ternarycolour == "Depth" && input$terndensityplot==FALSE && input$ternnormplot==FALSE) {
          ternaryplotdepth
      } else if (input$ternarycolour == "Depth" && input$terndensityplot==TRUE && input$ternnormplot==FALSE) {
          ternaryplotdepthellipse
      } else if (input$ternarycolour == "Age" && input$terndensityplot==FALSE && input$ternnormplot==FALSE) {
          ternaryplotage
      } else if (input$ternarycolour == "Age" && input$terndensityplot==TRUE && input$ternnormplot==FALSE) {
          ternaryplotageellipse
      } else if (input$ternarycolour == "black" && input$terndensityplot==FALSE && input$ternnormplot==TRUE) {
          ternaryplot1.norm
      } else if (input$ternarycolour == "black" && input$terndensityplot==TRUE && input$ternnormplot==TRUE) {
          ternaryplot2.norm
      } else if (input$ternarycolour == "Cluster" && input$terndensityplot==FALSE && input$ternnormplot==TRUE) {
          ternaryplotcluster.norm
      } else if (input$ternarycolour == "Cluster" && input$terndensityplot==TRUE && input$ternnormplot==TRUE) {
          ternaryplotclusterellipse.norm
      } else if (input$ternarycolour == "Climate" && input$terndensityplot==FALSE && input$ternnormplot==TRUE) {
          ternaryplotclimate.norm
      } else if (input$ternarycolour == "Climate" && input$terndensityplot==TRUE && input$ternnormplot==TRUE) {
          ternaryplotclimateellipse.norm
      } else if (input$ternarycolour == "Qualitative" && input$terndensityplot==FALSE && input$ternnormplot==TRUE) {
          ternaryplotqualitative.norm
      } else if (input$ternarycolour == "Qualitative" && input$terndensityplot==TRUE && input$ternnormplot==TRUE) {
          ternaryplotqualitativeellipse.norm
      } else if (input$ternarycolour == "Depth" && input$terndensityplot==FALSE && input$ternnormplot==TRUE) {
          ternaryplotdepth.norm
      } else if (input$ternarycolour == "Depth" && input$terndensityplot==TRUE && input$ternnormplot==TRUE) {
          ternaryplotdepthellipse.norm
      } else if (input$ternarycolour == "Age" && input$terndensityplot==FALSE && input$ternnormplot==TRUE) {
          ternaryplotage.norm
      } else if (input$ternarycolour == "Age" && input$terndensityplot==TRUE && input$ternnormplot==TRUE) {
          ternaryplotageellipse.norm
      }
      
      
  })
  
  output$ternaryplot <- renderPlot({
      
      plotInput5()
      
  })
  
  axisTerm <- reactive({
      axis.names.tern <- paste(gsub("[.]", "-", c(substr(input$axisa, 1, 2), substr(input$axisb, 1, 2), substr(input$axisc, 1, 2), "Ternary")), collapse='')
      axis.labels <- paste(c(input$projectname, "_", axis.names.tern), collapse='')
      axis.labels
  })
  
  output$downloadPlot5 <- downloadHandler(
  
  
  
  filename = function() { paste(axisTerm(), '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput5(), device="tiff", compression="lzw", type="cairo", dpi=300, width=12, height=7)
  }
  )
  
  
  dataTransform <- reactive({
      
      spectra.line.table <- ageData()
      
      
      
      spectra.line.table$Selected <- if(input$transform1=="None" && input$elementnum2=="None" && input$elementden1=="None") {
          spectra.line.table[input$elementnum1]
          ########Two Numerators
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2]
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2]
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2]
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2]
          ######Addition Third Numerator
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3]
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3]
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3]
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3]
          ######Subtraction Third Numerator
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3]
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3]
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3]
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3]
          ######Multiplication Third Numerator
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3]
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3]
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3]
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3]
          ######Division Third Numerator
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3]
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3]
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3]
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1=="None"){
          spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3]
          ##########################
          #####One Denominator######
          ##########################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="None" ){
          spectra.line.table[input$elementnum1]/spectra.line.table[input$elementden1]
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2])/spectra.line.table[input$elementden1]
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2])/spectra.line.table[input$elementden1]
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2])/spectra.line.table[input$elementden1]
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2])/spectra.line.table[input$elementden1]
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$elementden2=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/spectra.line.table[input$elementden1]
          ####################################
          #####Addition Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
          ######Subtraction Third Variable
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
          ######Multiplication Third Variable
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2])
          ####################################
          #####Subtraction Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2])
          ####################################
          #####Multiplication Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2])
          ####################################
          #####Division Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$elementden3=="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2])
          #######################################
          #######################################
          ########################################
          ####Addition Third Denominator#########
          #######################################
          #######################################
          ####################################
          #####Addition Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ####################################
          #####Subtraction Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ####################################
          #####Multiplication Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ####################################
          #####Division Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="+" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] + spectra.line.table[input$elementden3])
          #######################################
          #######################################
          ########################################
          ####Subtraction Third Denominator#########
          #######################################
          #######################################
          ####################################
          #####Addition Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ####################################
          #####Subtraction Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ####################################
          #####Multiplication Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ####################################
          #####Division Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="-" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] - spectra.line.table[input$elementden3])
          #######################################
          #######################################
          ########################################
          ####Multiple Third Denominator#########
          #######################################
          #######################################
          ####################################
          #####Addition Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ####################################
          #####Subtraction Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ####################################
          #####Multiplication Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ####################################
          #####Division Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="*" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] * spectra.line.table[input$elementden3])
          #######################################
          #######################################
          ########################################
          ####Divide Third Denominator#########
          #######################################
          #######################################
          ####################################
          #####Addition Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="+" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ####################################
          #####Subtraction Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="-" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ####################################
          #####Multiplication Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="*" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ####################################
          #####Division Two Denominators######
          ####################################
      } else if (input$transform1=="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          spectra.line.table[input$elementnum1]/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Addition Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="+" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] + spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Subtraction Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="-" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] - spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Multiplication Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="*" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] * spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
          ######Division Third Variable
      } else if(input$transform1=="+" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] + spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="-" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] - spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="*" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] * spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      } else if(input$transform1=="/" && input$elementnum2!="None" && input$transform2=="/" && input$elementnum3!="None" && input$elementden1!="None" && input$transform3=="/" && input$elementden2!="None" && input$transform4=="/" && input$elementden3!="None"){
          (spectra.line.table[input$elementnum1] / spectra.line.table[input$elementnum2] / spectra.line.table[input$elementnum3])/(spectra.line.table[input$elementden1] / spectra.line.table[input$elementden2] / spectra.line.table[input$elementden3])
      }
      
      #spectra.line.table$SelectedNorm <- t(t(spectra.line.table$Selected)/rowSums(t(spectra.line.table$Selected)))
      
      
      spectra.line.table
  })  
  
  
  
  xmindataeq <- reactive({
      
      spectra.line.table <- dataTransform()

      
      x.data <- spectra.line.table[input$xaxistypeeq]
      xmindata <- min(x.data) - min(x.data)*.05

      xmindata
      
      
  })
  
  xmaxdataeq <- reactive({
      spectra.line.table <- dataTransform()

      
      x.data <- spectra.line.table[input$xaxistypeeq]
      xmaxdata <- max(xmaxdata) + max(xmaxdata)*.05

      xmaxdata
      
  })
  
  
  output$inxlimrangeeq <- renderUI({
      
      
      sliderInput("xlimrangeeq", "X axis", min=xmindataeq(), max=xmaxdataeq(), value=c(xmindataeq(), xmaxdataeq()), round=FALSE)
  })
  
  #############Mathematical Transformations
  
  
  
  output$inelementnum1 <- renderUI({
      selectInput("elementnum1", "Numerator 1", choices=choiceLines(), selected=dataDefaultSelect())
  })
  
  output$inelementnum2 <- renderUI({
      selectInput("elementnum2", "Numerator 2", choices=choiceLines(), selected="None")
  })
  
  output$inelementnum3 <- renderUI({
      selectInput("elementnum3", "Numerator 3", choices=choiceLines(), selected="None")
  })
  
  output$inelementden1 <- renderUI({
      selectInput("elementden1", "Denominator 1", choices=choiceLines(), selected="None")
  })
  
  output$inelementden2 <- renderUI({
      selectInput("elementden2", "Denominator 2", choices=choiceLines(), selected="None")
  })
  
  output$inelementden3 <- renderUI({
      selectInput("elementden3", "Denominator 3", choices=choiceLines(), selected="None")
  })
  
  
  


  
  
  plotInput6a <- reactive({
      

      
      x.axis <- if (input$xaxistypeeq=="Depth") {
          paste("Depth (", input$lengthuniteq, ")", sep="", collapse="")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="BP"){
          paste("cal year BP")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="BC") {
          paste("cal year BC")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="AD") {
          paste("cal year AD")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="BC/AD") {
          paste("cal year BC/AD")
      } else if (input$xaxistypeeq=="Custom") {
          input$customxaxiseq
      }
      
      
      spectra.line.table <- dataTransform()
      
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistypeeq] <= input$xlimrangeeq[1] | spectra.line.table[input$xaxistypeeq] >= input$xlimrangeeq[2]))
      

      
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmmeq)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistypeeq)], spectra.line.table$Selected, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq), input$xaxistypeeq, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesizeeq) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)

      
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothingeq),  geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesizeeq) +
      theme_light() +
      scale_colour_gradientn(colours=terrain.colors(length(spectra.timeseries.table$Depth))) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_continuous(input$xaxistypeeq) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsizeeq) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      climate.time.series.line <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      qualitative.time.series.line <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      qualitative.time.series.point <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsizeeq) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      depth.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      
      #####X Axis Reverse
      
      
      black.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq), input$xaxistypeeq, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesizeeq) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      smooth.time.series.reverse <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothingeq),  geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      ramp.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesizeeq) +
      theme_light() +
scale_colour_gradientn(colours=rev(terrain.colors(length(spectra.timeseries.table$Depth)))) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      area.time.series.reverse <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_reverse(input$xaxistypeeq) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      
      cluster.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsizeeq) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      climate.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      qualitative.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      qualitative.time.series.point.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsizeeq) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      depth.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      
     
      if (input$timecoloureq == "Black" && input$flipxeq==FALSE) {
          black.time.series
      } else if (input$timecoloureq == "Smooth" && input$flipxeq==FALSE) {
          smooth.time.series
      } else if (input$timecoloureq == "Selected" && input$flipxeq==FALSE) {
          ramp.time.series
      } else if (input$timecoloureq == "Cluster" && input$flipxeq==FALSE) {
          cluster.time.series
      } else if (input$timecoloureq == "Climate" && input$flipxeq==FALSE) {
          climate.time.series.line
      } else if (input$timecoloureq == "QualitativePoint" && input$flipxeq==FALSE) {
          qualitative.time.series.point
      } else if (input$timecoloureq == "QualitativeLine" && input$flipxeq==FALSE) {
          qualitative.time.series.line
      } else if (input$timecoloureq == "Depth" && input$flipxeq==FALSE) {
          depth.time.series
      } else if (input$timecoloureq == "Black" && input$flipxeq==TRUE) {
          black.time.series.reverse
      } else if (input$timecoloureq == "Smooth" && input$flipxeq==TRUE) {
          smooth.time.series.reverse
      } else if (input$timecoloureq == "Selected" && input$flipxeq==TRUE) {
          ramp.time.series.reverse
      } else if (input$timecoloureq == "Cluster" && input$flipxeq==TRUE) {
          cluster.time.series.reverse
      } else if (input$timecoloureq == "Climate" && input$flipxeq==TRUE) {
          climate.time.series.line.reverse
      } else if (input$timecoloureq == "QualitativePoint" && input$flipxeq==TRUE) {
          qualitative.time.series.point.reverse
      } else if (input$timecoloureq == "QualitativeLine" && input$flipxeq==TRUE) {
          qualitative.time.series.line.reverse
      } else if (input$timecoloureq == "Depth" && input$flipxeq==TRUE) {
          depth.time.series.reverse
      } else if (input$timecoloureq == "Area" && input$flipxeq==TRUE) {
          area.time.series.reverse
      }
      
      
      
  })
  
  

  
  output$timeserieseqplot1 <- renderPlot({
      input$timeserieseq1

      isolate(plotInput6a())
  })
  
  hoverHold6a <- reactive({
      input$timeserieseq1
      
      spectra.line.table <- dataTransform()
      
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistypeeq] <= input$xlimrangeeq[1] | spectra.line.table[input$xaxistypeeq] >= input$xlimrangeeq[2]))
      
      if(is.null(spectra.line.table$Age)==TRUE){
          spectra.line.table$Age <- rep(1, length(spectra.line.table$Depth))
      }
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistypeeq)], spectra.line.table$Selected, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate, spectra.line.table$Age)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate", "Age")
      
      isolate(spectra.timeseries.table)
      
  })
  
  
  output$hover_info6a <- renderUI({
      
      point.table <- hoverHold6a()
      
      hover <- input$plot_hover6a
      point <- nearPoints(point.table,  xvar="Interval", yvar="Selected", coordinfo=hover,   threshold = 10, maxpoints = 1)
      
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
      "<b> Depth: </b>", point$Depth, "<br/>",
      "<b> Age: </b>", point$Age, "<br/>"
      
      )))
      )
  })
  
  
  output$downloadPlot6a <- downloadHandler(
  
  
  filename = function() { paste(input$projectname, "_", input$yaxistype, '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput6a(), device="tiff", compression="lzw", type="cairo", dpi=300, width=12, height=7)
  }
  )
  
  
  
  plotInput6b <- reactive({

      
      x.axis <- if (input$xaxistypeeq=="Depth") {
          paste("Depth (", input$lengthuniteq, ")", sep="", collapse="")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="BP"){
          paste("cal year BP")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="BC") {
          paste("cal year BC")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="AD") {
          paste("cal year AD")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="BC/AD") {
          paste("cal year BC/AD")
      } else if (input$xaxistypeeq=="Custom") {
          input$customxaxiseq
      }
      
      
      
      spectra.line.table <- dataTransform()
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistypeeq] <= input$xlimrangeeq[1] | spectra.line.table[input$xaxistypeeq] >= input$xlimrangeeq[2]))


      
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmmeq)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistypeeq)], spectra.line.table$Selected, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq), input$xaxistypeeq, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesizeeq) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothingeq),  geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesizeeq) +
      theme_light() +
      scale_colour_gradientn(colours=rev(terrain.colors(length(spectra.timeseries.table$Depth)))) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_continuous(input$xaxistypeeq) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsizeeq) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      climate.time.series.line <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      qualitative.time.series.line <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      qualitative.time.series.point <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsizeeq) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      depth.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      #####X Axis Reverse
      
      
      black.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq), input$xaxistypeeq, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesizeeq) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      smooth.time.series.reverse <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothingeq),  geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      ramp.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesizeeq) +
      theme_light() +
      scale_colour_gradientn(colours=rev(terrain.colors(length(spectra.timeseries.table$Depth)))) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      area.time.series.reverse <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_reverse(input$xaxistypeeq) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      
      cluster.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsizeeq) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      climate.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      qualitative.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      qualitative.time.series.point.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsizeeq) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      depth.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
     
     if (input$timecoloureq == "Black" && input$flipxeq==FALSE) {
         black.time.series
     } else if (input$timecoloureq == "Smooth" && input$flipxeq==FALSE) {
         smooth.time.series
     } else if (input$timecoloureq == "Selected" && input$flipxeq==FALSE) {
         ramp.time.series
     } else if (input$timecoloureq == "Cluster" && input$flipxeq==FALSE) {
         cluster.time.series
     } else if (input$timecoloureq == "Climate" && input$flipxeq==FALSE) {
         climate.time.series.line
     } else if (input$timecoloureq == "QualitativePoint" && input$flipxeq==FALSE) {
         qualitative.time.series.point
     } else if (input$timecoloureq == "QualitativeLine" && input$flipxeq==FALSE) {
         qualitative.time.series.line
     } else if (input$timecoloureq == "Depth" && input$flipxeq==FALSE) {
         Depth.time.series
     } else if (input$timecoloureq == "Black" && input$flipxeq==TRUE) {
         black.time.series.reverse
     } else if (input$timecoloureq == "Smooth" && input$flipxeq==TRUE) {
         smooth.time.series.reverse
     } else if (input$timecoloureq == "Selected" && input$flipxeq==TRUE) {
         ramp.time.series.reverse
     } else if (input$timecoloureq == "Cluster" && input$flipxeq==TRUE) {
         cluster.time.series.reverse
     } else if (input$timecoloureq == "Climate" && input$flipxeq==TRUE) {
         climate.time.series.line.reverse
     } else if (input$timecoloureq == "QualitativePoint" && input$flipxeq==TRUE) {
         qualitative.time.series.point.reverse
     } else if (input$timecoloureq == "QualitativeLine" && input$flipxeq==TRUE) {
         qualitative.time.series.line.reverse
     } else if (input$timecoloureq == "Depth" && input$flipxeq==TRUE) {
         depth.time.series.reverse
     } else if (input$timecoloureq == "Area" && input$flipxeq==TRUE) {
         area.time.series.reverse
     }
      
      
  })
  
  
  observeEvent(input$timeserieseq2, {
      
      
  })
  output$timeserieseqplot2 <- renderPlot({
      input$timeserieseq2
      isolate(plotInput6b())      
  })
  
  hoverHold6b <- reactive({
      input$timeserieseq2
      
      spectra.line.table <- dataTransform()
      
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistypeeq] <= input$xlimrangeeq[1] | spectra.line.table[input$xaxistypeeq] >= input$xlimrangeeq[2]))
      
      if(is.null(spectra.line.table$Age)==TRUE){
          spectra.line.table$Age <- rep(1, length(spectra.line.table$Depth))
      }
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistypeeq)], spectra.line.table$Selected, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate, spectra.line.table$Age)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate", "Age")
      
      isolate(spectra.timeseries.table)
      
  })
  
  
  
  output$hover_info6b <- renderUI({
      
      point.table <- hoverHold6b()
      
      hover <- input$plot_hover6b
      point <- nearPoints(point.table,  xvar="Interval", yvar="Selected", coordinfo=hover,   threshold = 10, maxpoints = 1)
      
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
      "<b> Depth: </b>", point$Depth, "<br/>",
      "<b> Age: </b>", point$Age, "<br/>"
      
      )))
      )
  })
  
  
  output$downloadPlot6b <- downloadHandler(
  
  
  
  filename = function() { paste(input$projectname, "_", input$yaxistype, '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput6b(), device="tiff", compression="lzw", type="cairo", dpi=300, width=12, height=7)
  }
  )
  
  
  
  plotInput6c <- reactive({

      
      
      x.axis <- if (input$xaxistypeeq=="Depth") {
          paste("Depth (", input$lengthuniteq, ")", sep="", collapse="")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="BP"){
          paste("cal year BP")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="BC") {
          paste("cal year BC")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="AD") {
          paste("cal year AD")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="BC/AD") {
          paste("cal year BC/AD")
      } else if (input$xaxistypeeq=="Custom") {
          input$customxaxiseq
      }
      
      
      
      
      spectra.line.table <- dataTransform()
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistypeeq] <= input$xlimrangeeq[1] | spectra.line.table[input$xaxistypeeq] >= input$xlimrangeeq[2]))


      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmmeq)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistypeeq)], spectra.line.table$Selected, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq), input$xaxistypeeq, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesizeeq) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothingeq),  geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesizeeq) +
      theme_light() +
      scale_colour_gradientn(colours=rev(terrain.colors(length(spectra.timeseries.table$Depth)))) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_continuous(input$xaxistypeeq) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsizeeq) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      climate.time.series.line <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      qualitative.time.series.line <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      qualitative.time.series.point <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsizeeq) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      depth.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      #####X Axis Reverse
      
      
      black.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq), input$xaxistypeeq, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesizeeq) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      smooth.time.series.reverse <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothingeq),  geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      ramp.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesizeeq) +
      theme_light() +
      scale_colour_gradientn(colours=rev(terrain.colors(length(spectra.timeseries.table$Depth)))) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      area.time.series.reverse <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_reverse(input$xaxistypeeq) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      
      cluster.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsizeeq) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      climate.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      qualitative.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      qualitative.time.series.point.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsizeeq) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      depth.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
     
     if (input$timecoloureq == "Black" && input$flipxeq==FALSE) {
         black.time.series
     } else if (input$timecoloureq == "Smooth" && input$flipxeq==FALSE) {
         smooth.time.series
     } else if (input$timecoloureq == "Selected" && input$flipxeq==FALSE) {
         ramp.time.series
     } else if (input$timecoloureq == "Cluster" && input$flipxeq==FALSE) {
         cluster.time.series
     } else if (input$timecoloureq == "Climate" && input$flipxeq==FALSE) {
         climate.time.series.line
     } else if (input$timecoloureq == "QualitativePoint" && input$flipxeq==FALSE) {
         qualitative.time.series.point
     } else if (input$timecoloureq == "QualitativeLine" && input$flipxeq==FALSE) {
         qualitative.time.series.line
     } else if (input$timecoloureq == "Depth" && input$flipxeq==FALSE) {
         Depth.time.series
     } else if (input$timecoloureq == "Black" && input$flipxeq==TRUE) {
         black.time.series.reverse
     } else if (input$timecoloureq == "Smooth" && input$flipxeq==TRUE) {
         smooth.time.series.reverse
     } else if (input$timecoloureq == "Selected" && input$flipxeq==TRUE) {
         ramp.time.series.reverse
     } else if (input$timecoloureq == "Cluster" && input$flipxeq==TRUE) {
         cluster.time.series.reverse
     } else if (input$timecoloureq == "Climate" && input$flipxeq==TRUE) {
         climate.time.series.line.reverse
     } else if (input$timecoloureq == "QualitativePoint" && input$flipxeq==TRUE) {
         qualitative.time.series.point.reverse
     } else if (input$timecoloureq == "QualitativeLine" && input$flipxeq==TRUE) {
         qualitative.time.series.line.reverse
     } else if (input$timecoloureq == "Depth" && input$flipxeq==TRUE) {
         depth.time.series.reverse
     } else if (input$timecoloureq == "Area" && input$flipxeq==TRUE) {
         area.time.series.reverse
     }
      
      
      
  })
  
  observeEvent(input$timeserieseq3, {
  })
  
  output$timeserieseqplot3 <- renderPlot({
      input$timeserieseq3
      
      isolate(plotInput6c())      
      
  })
  
  hoverHold6c <- reactive({
      input$timeserieseq3
      
      spectra.line.table <- dataTransform()
      
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistypeeq] <= input$xlimrangeeq[1] | spectra.line.table[input$xaxistypeeq] >= input$xlimrangeeq[2]))
      
      if(is.null(spectra.line.table$Age)==TRUE){
          spectra.line.table$Age <- rep(1, length(spectra.line.table$Depth))
      }
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistypeeq)], spectra.line.table$Selected, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate, spectra.line.table$Age)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate", "Age")
      
      isolate(spectra.timeseries.table)
      
  })
  
  
  output$hover_info6c <- renderUI({
      
      point.table <- hoverHold6c()
      
      hover <- input$plot_hover6c
      point <- nearPoints(point.table,  xvar="Interval", yvar="Selected", coordinfo=hover,   threshold = 10, maxpoints = 1)
      
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
      "<b> Depth: </b>", point$Depth, "<br/>",
      "<b> Age: </b>", point$Age, "<br/>"
      
      )))
      )
  })
  
  output$downloadPlot6c <- downloadHandler(
  
  
  filename = function() { paste(input$projectname, "_", input$yaxistype, '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput6c(), device="tiff", compression="lzw", type="cairo", dpi=300, width=12, height=7)
  }
  )
  
  
  plotInput6d <- reactive({
      
      
      
      x.axis <- if (input$xaxistypeeq=="Depth") {
          paste("Depth (", input$lengthuniteq, ")", sep="", collapse="")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="BP"){
          paste("cal year BP")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="BC") {
          paste("cal year BC")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="AD") {
          paste("cal year AD")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="BC/AD") {
          paste("cal year BC/AD")
      } else if (input$xaxistypeeq=="Custom") {
          input$customxaxiseq
      }
      
      
      
      spectra.line.table <- dataTransform()
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistypeeq] <= input$xlimrangeeq[1] | spectra.line.table[input$xaxistypeeq] >= input$xlimrangeeq[2]))



      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmmeq)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistypeeq)], spectra.line.table$Selected, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq), input$xaxistypeeq, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesizeeq) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothingeq),  geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesizeeq) +
      theme_light() +
      scale_colour_gradientn(colours=rev(terrain.colors(length(spectra.timeseries.table$Depth)))) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_continuous(input$xaxistypeeq) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsizeeq) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      climate.time.series.line <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      qualitative.time.series.line <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      qualitative.time.series.point <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsizeeq) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      depth.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      #####X Axis Reverse
      
      
      black.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq), input$xaxistypeeq, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesizeeq) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      smooth.time.series.reverse <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothingeq),  geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      ramp.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesizeeq) +
      theme_light() +
      scale_colour_gradientn(colours=rev(terrain.colors(length(spectra.timeseries.table$Depth)))) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      area.time.series.reverse <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_reverse(input$xaxistypeeq) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      
      cluster.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsizeeq) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      climate.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      qualitative.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      qualitative.time.series.point.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsizeeq) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      depth.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
     
     if (input$timecoloureq == "Black" && input$flipxeq==FALSE) {
         black.time.series
     } else if (input$timecoloureq == "Smooth" && input$flipxeq==FALSE) {
         smooth.time.series
     } else if (input$timecoloureq == "Selected" && input$flipxeq==FALSE) {
         ramp.time.series
     } else if (input$timecoloureq == "Cluster" && input$flipxeq==FALSE) {
         cluster.time.series
     } else if (input$timecoloureq == "Climate" && input$flipxeq==FALSE) {
         climate.time.series.line
     } else if (input$timecoloureq == "QualitativePoint" && input$flipxeq==FALSE) {
         qualitative.time.series.point
     } else if (input$timecoloureq == "QualitativeLine" && input$flipxeq==FALSE) {
         qualitative.time.series.line
     } else if (input$timecoloureq == "Depth" && input$flipxeq==FALSE) {
         Depth.time.series
     } else if (input$timecoloureq == "Black" && input$flipxeq==TRUE) {
         black.time.series.reverse
     } else if (input$timecoloureq == "Smooth" && input$flipxeq==TRUE) {
         smooth.time.series.reverse
     } else if (input$timecoloureq == "Selected" && input$flipxeq==TRUE) {
         ramp.time.series.reverse
     } else if (input$timecoloureq == "Cluster" && input$flipxeq==TRUE) {
         cluster.time.series.reverse
     } else if (input$timecoloureq == "Climate" && input$flipxeq==TRUE) {
         climate.time.series.line.reverse
     } else if (input$timecoloureq == "QualitativePoint" && input$flipxeq==TRUE) {
         qualitative.time.series.point.reverse
     } else if (input$timecoloureq == "QualitativeLine" && input$flipxeq==TRUE) {
         qualitative.time.series.line.reverse
     } else if (input$timecoloureq == "Depth" && input$flipxeq==TRUE) {
         depth.time.series.reverse
     } else if (input$timecoloureq == "Area" && input$flipxeq==TRUE) {
         area.time.series.reverse
     }
      
      
  })
  
  observeEvent(input$timeserieseq4, {
      
  })
  
  
  
  output$timeserieseqplot4 <- renderPlot({
      input$timeserieseq4
      isolate(plotInput6d())      
  })
  
  
  hoverHold6d <- reactive({
      input$timeserieseq4
      
      spectra.line.table <- dataTransform()
      
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistypeeq] <= input$xlimrangeeq[1] | spectra.line.table[input$xaxistypeeq] >= input$xlimrangeeq[2]))
      
      if(is.null(spectra.line.table$Age)==TRUE){
          spectra.line.table$Age <- rep(1, length(spectra.line.table$Depth))
      }
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistypeeq)], spectra.line.table$Selected, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate, spectra.line.table$Age)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate", "Age")
      
      isolate(spectra.timeseries.table)
      
  })
  
  
  
  output$hover_info6d <- renderUI({
      
      point.table <- hoverHold6d()
      
      hover <- input$plot_hover6d
      point <- nearPoints(point.table,  xvar="Interval", yvar="Selected", coordinfo=hover,   threshold = 10, maxpoints = 1)
      
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
      "<b> Depth: </b>", point$Depth, "<br/>",
      "<b> Age: </b>", point$Age, "<br/>"
      
      )))
      )
  })
  
  output$downloadPlot6d <- downloadHandler(
  
  
  
  filename = function() { paste(input$projectname, "_", input$yaxistype, '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput6d(), device="tiff", compression="lzw", type="cairo", dpi=300, width=12, height=7)
  }
  )
  
  
  
  
  
  plotInput6e <- reactive({
      

      
      x.axis <- if (input$xaxistypeeq=="Depth") {
          paste("Depth (", input$lengthuniteq, ")", sep="", collapse="")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="BP"){
          paste("cal year BP")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="BC") {
          paste("cal year BC")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="AD") {
          paste("cal year AD")
      } else if (input$xaxistypeeq=="Age" && input$timetypeeq=="BC/AD") {
          paste("cal year BC/AD")
      } else if (input$xaxistypeeq=="Custom") {
          input$customxaxiseq
      }
      
      
      
      
      spectra.line.table <- dataTransform()
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistypeeq] <= input$xlimrangeeq[1] | spectra.line.table[input$xaxistypeeq] >= input$xlimrangeeq[2]))


      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmmeq)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistypeeq)], spectra.line.table$Selected, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq), input$xaxistypeeq, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesizeeq) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothingeq),  geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesizeeq) +
      theme_light() +
      scale_colour_gradientn(colours=rev(terrain.colors(length(spectra.timeseries.table$Depth)))) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_continuous(input$xaxistypeeq) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsizeeq) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      climate.time.series.line <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      qualitative.time.series.line <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      qualitative.time.series.point <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsizeeq) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      depth.time.series <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      #####X Axis Reverse
      
      
      black.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq), input$xaxistypeeq, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesizeeq) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      smooth.time.series.reverse <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothingeq),  geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      ramp.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Selected), lwd=input$linesizeeq) +
      theme_light() +
      scale_colour_gradientn(colours=rev(terrain.colors(length(spectra.timeseries.table$Depth)))) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      area.time.series.reverse <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_reverse(input$xaxistypeeq) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      
      cluster.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Cluster)), size=input$pointsizeeq) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      climate.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      qualitative.time.series.line.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      qualitative.time.series.point.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = as.factor(Qualitative)), lwd=input$pointsizeeq) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
      depth.time.series.reverse <- qplot(Interval, SMA(Selected, input$smoothingeq),  geom="line", data = spectra.timeseries.table) +
      geom_line(aes(colour = Depth), lwd=input$linesize+0.5) +
      geom_line(lwd=input$linesize-0.5) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(spectra.line.table$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_reverse(paste(x.axis), label=comma) +
      scale_y_continuous(input$yaxistype, label=comma)
      
      
     
     if (input$timecoloureq == "Black" && input$flipxeq==FALSE) {
         black.time.series
     } else if (input$timecoloureq == "Smooth" && input$flipxeq==FALSE) {
         smooth.time.series
     } else if (input$timecoloureq == "Selected" && input$flipxeq==FALSE) {
         ramp.time.series
     } else if (input$timecoloureq == "Cluster" && input$flipxeq==FALSE) {
         cluster.time.series
     } else if (input$timecoloureq == "Climate" && input$flipxeq==FALSE) {
         climate.time.series.line
     } else if (input$timecoloureq == "QualitativePoint" && input$flipxeq==FALSE) {
         qualitative.time.series.point
     } else if (input$timecoloureq == "QualitativeLine" && input$flipxeq==FALSE) {
         qualitative.time.series.line
     } else if (input$timecoloureq == "Depth" && input$flipxeq==FALSE) {
         Depth.time.series
     } else if (input$timecoloureq == "Black" && input$flipxeq==TRUE) {
         black.time.series.reverse
     } else if (input$timecoloureq == "Smooth" && input$flipxeq==TRUE) {
         smooth.time.series.reverse
     } else if (input$timecoloureq == "Selected" && input$flipxeq==TRUE) {
         ramp.time.series.reverse
     } else if (input$timecoloureq == "Cluster" && input$flipxeq==TRUE) {
         cluster.time.series.reverse
     } else if (input$timecoloureq == "Climate" && input$flipxeq==TRUE) {
         climate.time.series.line.reverse
     } else if (input$timecoloureq == "QualitativePoint" && input$flipxeq==TRUE) {
         qualitative.time.series.point.reverse
     } else if (input$timecoloureq == "QualitativeLine" && input$flipxeq==TRUE) {
         qualitative.time.series.line.reverse
     } else if (input$timecoloureq == "Depth" && input$flipxeq==TRUE) {
         depth.time.series.reverse
     } else if (input$timecoloureq == "Area" && input$flipxeq==TRUE) {
         area.time.series.reverse
     }
      
      
      
  })
  
  observeEvent(input$timeserieseq5, {
      
      
  })
  
  output$timeserieseqplot5 <- renderPlot({
      input$timeserieseq5
      
      
      
      isolate(plotInput6e())      
  })
  
  
  hoverHold6e <- reactive({
      input$timeserieseq5
      
      spectra.line.table <- dataTransform()
      
      #spectra.line.table <- subset(spectra.line.table, !(spectra.line.table[input$xaxistypeeq] <= input$xlimrangeeq[1] | spectra.line.table[input$xaxistypeeq] >= input$xlimrangeeq[2]))
      
      if(is.null(spectra.line.table$Age)==TRUE){
          spectra.line.table$Age <- rep(1, length(spectra.line.table$Depth))
      }
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistypeeq)], spectra.line.table$Selected, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate, spectra.line.table$Age)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate", "Age")
      
      isolate(spectra.timeseries.table)
      
  })
  
  
  
  output$hover_info6e <- renderUI({
      
      point.table <- hoverHold6e()
      
      hover <- input$plot_hover6e
      point <- nearPoints(point.table,  xvar="Interval", yvar="Selected", coordinfo=hover,   threshold = 10, maxpoints = 1)
      
      if (nrow(point) == 0) return(NULL)
      
      
      
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
      style = style,
      p(HTML(paste0("<b> Spectrum: </b>", point$Spectrum, "<br/>",
      "<b> Depth: </b>", point$Depth, "<br/>",
      "<b> Age: </b>", point$Age, "<br/>"
      
      )))
      )
  })
  
  
  output$downloadPlot6e <- downloadHandler(
  
  
  
  filename = function() { paste(input$projectname, "_", input$yaxistype, '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput6e(), device="tiff", compression="lzw", type="cairo", dpi=300, width=12, height=7)
  }
  )
  
  
  miniDataTransformY <- reactive({
      
      spectra.line.table <- ageData()
      
      
      
      data.transformation.selection <- if(input$ytransform1=="None" && input$yelementden2=="None") {
          spectra.line.table[input$yelementden1]
          ########Two Numerators
      } else if(input$ytransform1=="+" && input$yelementden2!="None"){
          spectra.line.table[input$yelementden1] + spectra.line.table[input$yelementden2]
      } else if(input$ytransform1=="-" && input$yelementden2!="None"){
          spectra.line.table[input$yelementden1] - spectra.line.table[input$yelementden2]
      } else if(input$ytransform1=="*" && input$yelementden2!="None"){
          spectra.line.table[input$yelementden1] * spectra.line.table[input$yelementden2]
      } else if(input$ytransform1=="/" && input$yelementden2!="None"){
          spectra.line.table[input$yelementden1] / spectra.line.table[input$yelementden2]
          ######Addition Third Numerator
      } else if(input$ytransform1=="+" && input$yelementden2!="None" && input$ytransform2=="+" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] + spectra.line.table[input$yelementden2] + spectra.line.table[input$yelementden3]
      } else if(input$ytransform1=="-" && input$yelementden2!="None" && input$ytransform2=="+" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] - spectra.line.table[input$yelementden2] + spectra.line.table[input$yelementden3]
      } else if(input$ytransform1=="*" && input$yelementden2!="None" && input$ytransform2=="+" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] * spectra.line.table[input$yelementden2] + spectra.line.table[input$yelementden3]
      } else if(input$ytransform1=="/" && input$yelementden2!="None" && input$ytransform2=="+" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] / spectra.line.table[input$yelementden2] + spectra.line.table[input$yelementden3]
          ######Subtraction Third Numerator
      } else if(input$ytransform1=="+" && input$yelementden2!="None" && input$ytransform2=="-" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] + spectra.line.table[input$yelementden2] - spectra.line.table[input$yelementden3]
      } else if(input$ytransform1=="-" && input$yelementden2!="None" && input$ytransform2=="-" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] - spectra.line.table[input$yelementden2] - spectra.line.table[input$yelementden3]
      } else if(input$ytransform1=="*" && input$yelementden2!="None" && input$ytransform2=="-" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] * spectra.line.table[input$yelementden2] - spectra.line.table[input$yelementden3]
      } else if(input$ytransform1=="/" && input$yelementden2!="None" && input$ytransform2=="-" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] / spectra.line.table[input$yelementden2] - spectra.line.table[input$yelementden3]
          ######Multiplication Third Numerator
      } else if(input$ytransform1=="+" && input$yelementden2!="None" && input$ytransform2=="*" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] + spectra.line.table[input$yelementden2] * spectra.line.table[input$yelementden3]
      } else if(input$ytransform1=="-" && input$yelementden2!="None" && input$ytransform2=="*" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] - spectra.line.table[input$yelementden2] * spectra.line.table[input$yelementden3]
      } else if(input$ytransform1=="*" && input$yelementden2!="None" && input$ytransform2=="*" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] * spectra.line.table[input$yelementden2] * spectra.line.table[input$yelementden3]
      } else if(input$ytransform1=="/" && input$yelementden2!="None" && input$ytransform2=="*" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] / spectra.line.table[input$yelementden2] * spectra.line.table[input$yelementden3]
          ######Division Third Numerator
      } else if(input$ytransform1=="+" && input$yelementden2!="None" && input$ytransform2=="/" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] + spectra.line.table[input$yelementden2] / spectra.line.table[input$yelementden3]
      } else if(input$ytransform1=="-" && input$yelementden2!="None" && input$ytransform2=="/" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] - spectra.line.table[input$yelementden2] / spectra.line.table[input$yelementden3]
      } else if(input$ytransform1=="*" && input$yelementden2!="None" && input$ytransform2=="/" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] * spectra.line.table[input$yelementden2] / spectra.line.table[input$yelementden3]
      } else if(input$ytransform1=="/" && input$yelementden2!="None" && input$ytransform2=="/" && input$yelementden3!="None"){
          spectra.line.table[input$yelementden1] / spectra.line.table[input$yelementden2] / spectra.line.table[input$yelementden3]
      }
      
      data.transformation.selection
      
  })
  
  miniDataTransformX <- reactive({
      
      spectra.line.table <- ageData()
      
      
      
      spectra.line.table$Selected <- if(input$xtransform1=="None" && input$xelementnum2=="None") {
          spectra.line.table[input$xelementnum1]
          ########Two Numerators
      } else if(input$xtransform1=="+" && input$xelementnum2!="None"){
          spectra.line.table[input$xelementnum1] + spectra.line.table[input$xelementnum2]
      } else if(input$xtransform1=="-" && input$xelementnum2!="None"){
          spectra.line.table[input$xelementnum1] - spectra.line.table[input$xelementnum2]
      } else if(input$xtransform1=="*" && input$xelementnum2!="None"){
          spectra.line.table[input$xelementnum1] * spectra.line.table[input$xelementnum2]
      } else if(input$xtransform1=="/" && input$xelementnum2!="None"){
          spectra.line.table[input$xelementnum1] / spectra.line.table[input$xelementnum2]
          ######Addition Third Numerator
      } else if(input$xtransform1=="+" && input$xelementnum2!="None" && input$xtransform2=="+" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] + spectra.line.table[input$xelementnum2] + spectra.line.table[input$xelementnum3]
      } else if(input$xtransform1=="-" && input$xelementnum2!="None" && input$xtransform2=="+" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] - spectra.line.table[input$xelementnum2] + spectra.line.table[input$xelementnum3]
      } else if(input$xtransform1=="*" && input$xelementnum2!="None" && input$xtransform2=="+" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] * spectra.line.table[input$xelementnum2] + spectra.line.table[input$xelementnum3]
      } else if(input$xtransform1=="/" && input$xelementnum2!="None" && input$xtransform2=="+" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] / spectra.line.table[input$xelementnum2] + spectra.line.table[input$xelementnum3]
          ######Subtraction Third Numerator
      } else if(input$xtransform1=="+" && input$xelementnum2!="None" && input$xtransform2=="-" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] + spectra.line.table[input$xelementnum2] - spectra.line.table[input$xelementnum3]
      } else if(input$xtransform1=="-" && input$xelementnum2!="None" && input$xtransform2=="-" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] - spectra.line.table[input$xelementnum2] - spectra.line.table[input$xelementnum3]
      } else if(input$xtransform1=="*" && input$xelementnum2!="None" && input$xtransform2=="-" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] * spectra.line.table[input$xelementnum2] - spectra.line.table[input$xelementnum3]
      } else if(input$xtransform1=="/" && input$xelementnum2!="None" && input$xtransform2=="-" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] / spectra.line.table[input$xelementnum2] - spectra.line.table[input$xelementnum3]
          ######Multiplication Third Numerator
      } else if(input$xtransform1=="+" && input$xelementnum2!="None" && input$xtransform2=="*" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] + spectra.line.table[input$xelementnum2] * spectra.line.table[input$xelementnum3]
      } else if(input$xtransform1=="-" && input$xelementnum2!="None" && input$xtransform2=="*" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] - spectra.line.table[input$xelementnum2] * spectra.line.table[input$xelementnum3]
      } else if(input$xtransform1=="*" && input$xelementnum2!="None" && input$xtransform2=="*" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] * spectra.line.table[input$xelementnum2] * spectra.line.table[input$xelementnum3]
      } else if(input$xtransform1=="/" && input$xelementnum2!="None" && input$xtransform2=="*" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] / spectra.line.table[input$xelementnum2] * spectra.line.table[input$xelementnum3]
          ######Division Third Numerator
      } else if(input$xtransform1=="+" && input$xelementnum2!="None" && input$xtransform2=="/" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] + spectra.line.table[input$xelementnum2] / spectra.line.table[input$xelementnum3]
      } else if(input$xtransform1=="-" && input$xelementnum2!="None" && input$xtransform2=="/" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] - spectra.line.table[input$xelementnum2] / spectra.line.table[input$xelementnum3]
      } else if(input$xtransform1=="*" && input$xelementnum2!="None" && input$xtransform2=="/" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] * spectra.line.table[input$xelementnum2] / spectra.line.table[input$xelementnum3]
      } else if(input$xtransform1=="/" && input$xelementnum2!="None" && input$xtransform2=="/" && input$xelementnum3!="None"){
          spectra.line.table[input$xelementnum1] / spectra.line.table[input$xelementnum2] / spectra.line.table[input$xelementnum3]
      }
      
      data.transformation.selection
      
  })
  
  
  output$inelementx1 <- renderUI({
      selectInput("xelementnum1", "X Axis 1", choices=choiceLines(), selected=dataDefaultSelect())
  })
  
  output$inelementx2 <- renderUI({
      selectInput("xelementnum2", "X Axis 2", choices=choiceLines(), selected="None")
  })
  
  output$inelementx3 <- renderUI({
      selectInput("xelementnum3", "X Axis 3", choices=choiceLines(), selected="None")
  })
  
  output$inelementy1 <- renderUI({
      selectInput("yelementden1", "Y Axis 1", choices=choiceLines(), selected=secondDefaultSelect())
  })
  
  output$inelementy2 <- renderUI({
      selectInput("yelementden2", "Y Axis 2", choices=choiceLines(), selected="None")
  })
  
  output$inelementy3 <- renderUI({
      selectInput("yelementden3", "Y Axis 3", choices=choiceLines(), selected="None")
  })
  


  
  elementRatioEquation <- reactive({
      
      
      spectra.line.table <- ageData()
      
      unique.spec <- seq(1, length(spectra.line.table$Depth), 1)
      null <- rep(1, length(unique.spec))
      
      x.axis.transform <- miniDataTransformX()
      y.axis.transform <- miniDataTransformY()
      
      
      
      ratio.frame <- data.frame(x.axis.transform, y.axis.transform, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Age, spectra.line.table$Climate)
      colnames(ratio.frame) <- c(input$xaxisdef, input$yaxisdef, "Cluster", "Qualitative", "Depth", "Age", "Climate")
      
      
      ratio.names.x <- input$xaxisdef
      ratio.names.y <- input$yaxisdef
      
      
      black.ratio.plot <- qplot(ratio.frame[input$xaxisdef], ratio.frame[input$yaxisdef] ) +
      geom_point(lwd=input$spotsize3) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      cluster.ratio.plot <- qplot(ratio.frame[input$xaxisdef], ratio.frame[input$yaxisdef] ) +
      geom_point(aes(colour=as.factor(ratio.frame$Cluster), shape=as.factor(ratio.frame$Cluster)), size=input$spotsize3+1) +
      geom_point(colour="grey30", size=input$spotsize3-2) +
      scale_shape_manual("Cluster", values=1:nlevels(as.factor(as.factor(ratio.frame$Cluster)))) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      cluster.ratio.ellipse.plot <- qplot(ratio.frame[input$xaxisdef], ratio.frame[input$yaxisdef] ) +
      stat_ellipse(aes(ratio.frame[input$xaxisdef], ratio.frame[input$yaxisdef], colour=as.factor(ratio.frame$Cluster))) +
      geom_point(aes(colour=as.factor(ratio.frame$Cluster), shape=as.factor(ratio.frame$Cluster)), size=input$spotsize3+1) +
      geom_point(colour="grey30", size=input$spotsize3-2) +
      scale_shape_manual("Cluster", values=1:nlevels(as.factor(as.factor(ratio.frame$Cluster)))) +
      scale_colour_discrete("Cluster") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      
      climate.ratio.plot <- qplot(ratio.frame[input$xaxisdef], ratio.frame[input$yaxisdef] ) +
      geom_point(aes(colour=as.factor(ratio.frame$Climate), shape=as.factor(ratio.frame$Climate)), size=input$spotsize3+1) +
      geom_point(colour="grey30", size=input$spotsize3-2) +
      scale_shape_manual("Climatic Period", values=1:nlevels(ratio.frame$Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      climate.ratio.ellipse.plot <- qplot(ratio.frame[input$xaxisdef], ratio.frame[input$yaxisdef] ) +
      stat_ellipse(aes(ratio.frame[input$xaxisdef], ratio.frame[input$yaxisdef], colour=as.factor(ratio.frame$Climate))) +
      geom_point(aes(colour=as.factor(ratio.frame$Climate), shape=as.factor(ratio.frame$Climate)), size=input$spotsize3+1) +
      geom_point(colour="grey30", size=input$spotsize3-2) +
      scale_shape_manual("Climatic Period", values=1:nlevels(ratio.frame$Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      
      qualitative.ratio.plot <- qplot(ratio.frame[input$xaxisdef], ratio.frame[input$yaxisdef] ) +
      geom_point(aes(colour=as.factor(ratio.frame$Qualitative), shape=as.factor(ratio.frame$Qualitative)), size=input$spotsize3+1) +
      geom_point(colour="grey30", size=input$spotsize3-2) +
      scale_shape_manual("Qualitative", values=1:nlevels(ratio.frame$Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      qualitative.ratio.ellipse.plot <- qplot(ratio.frame[input$xaxisdef], ratio.frame[input$yaxisdef] ) +
      stat_ellipse(aes(ratio.frame[input$xaxisdef], ratio.frame[input$yaxisdef], colour=as.factor(ratio.frame$Qualitative))) +
      geom_point(aes(colour=as.factor(ratio.frame$Qualitative), shape=as.factor(ratio.frame$Qualitative)), size=input$spotsize3+1) +
      geom_point(colour="grey30", size=input$spotsize3-2) +
      scale_shape_manual("Qualitative", values=1:nlevels(ratio.frame$Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      depth.ratio.plot <- qplot(ratio.frame[input$xaxisdef], ratio.frame[input$yaxisdef] ) +
      geom_point(aes(colour = ratio.frame$Depth), size=input$spotsize3+1) +
      geom_point(size=input$spotsize3-2) +
      scale_colour_gradientn("Depth", colours=rev(terrain.colors(length(ratio.frame$Depth)))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))+
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      age.ratio.plot <- qplot(ratio.frame[input$xaxisdef], ratio.frame[input$yaxisdef] ) +
      geom_point(aes(colour = ratio.frame$Age), size=input$spotsize3+1) +
      geom_point(size=input$spotsize3-2) +
      scale_colour_gradientn("Age", colours=terrain.colors(length(ratio.frame$Age))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(ratio.names.x, label=comma) +
      scale_y_continuous(ratio.names.y, label=comma)
      
      
      
      
      
      
      if (input$ratiocolour == "Black" && input$elipseplot3==FALSE) {
          black.ratio.plot
      } else if (input$ratiocolour == "Cluster" && input$elipseplot3==FALSE) {
          cluster.ratio.plot
      } else if (input$ratiocolour == "Cluster" && input$elipseplot3==TRUE) {
          cluster.ratio.ellipse.plot
      } else if (input$ratiocolour == "Climate" && input$elipseplot3==FALSE) {
          climate.ratio.plot
      } else if (input$ratiocolour == "Climate" && input$elipseplot3==TRUE ) {
          climate.ratio.ellipse.plot
      } else if (input$ratiocolour == "Qualitative" && input$elipseplot3==FALSE) {
          qualitative.ratio.plot
      } else if (input$ratiocolour == "Qualitative" && input$elipseplot3==TRUE ) {
          qualitative.ratio.ellipse.plot
      } else if (input$ratiocolour == "Depth") {
          depth.ratio.plot
      } else if (input$ratiocolour == "Age") {
          age.ratio.plot
      }
      
  })
  
  output$elementratiotequation <- renderPlot({
      print(elementRatioEquation())
      
      
  })
  
  
  
    })
    
})

 })






