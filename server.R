library(pbapply)
library(reshape2)
library(TTR)
library(dplyr)
library(shinyIncubator)
library(data.table)
library(ggtern)
library(ggplot2)
library(shiny)
library(shinysky)
library(random)
library(rhandsontable)
library(Bchron)
library(scales)







shinyServer(function(input, output, session) {
    
    
    
    
    
    observeEvent(input$actionprocess, {
        
        myData <- reactive({
            
            withProgress(message = 'Processing Data', value = 0, {
                
                inFile <- input$file1
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
                data <- spectra.frame
                
                
                incProgress(1/n)
                Sys.sleep(0.1)
            })
            
            data
            
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
                ggsave(file,plotInput(), device="tiff", dpi=300, width=12, height=7)
            }
            )
            
            
            
            
            
            
            
            
        })
        
        
        data <- myData()
        
        
        spectra.line.table <- spectra.line.fn(data)
        select.line.table <- datatable(spectra.line.table[, input$show_vars, drop = FALSE])
        select.line.table
        
        
        fordownload <- spectra.line.table[input$show_vars]
        
        tableInput <- reactive({
            spectra.line.table <- spectra.line.fn(data)
            select.line.table <- datatable(spectra.line.table[, input$show_vars, drop = FALSE])
            select.line.table
        })
        
        
        output$mytable1 <- renderDataTable({
            
            tableInput()
            
        })
        
        fullTable <- reactive({
            
            
        })
        
        
        hotableInput <- reactive({
            empty.line.table <-  spectra.line.table[input$show_vars] * 0
            empty.line.table <- empty.line.table[1:2]
            colnames(empty.line.table) <- c("Qualitative", "Depth")
            empty.line.table$Spectrum <- spectra.line.table$Spectrum
            na.vector <- rep("NULL", length(empty.line.table$Qualitative))
            na.matrix <- as.matrix(na.vector)
            na.matrix[1,1] <- "a"
            na.matrix[2,1] <- "b"
            na.matrix[3,1] <- "c"
            na.input <- as.vector(na.matrix[,1])
            
            
            empty.line.table <- data.frame(empty.line.table$Spectrum, na.input, empty.line.table$Depth)
            colnames(empty.line.table) <- c("Spectrum", "Qualitative", "Depth")
            
            
            empty.line.table
            
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
        
        
        
        
        
        
        
        output$downloadData <- downloadHandler(
        filename = function() { paste(paste(c(input$projectname, "_", "CountTable"), collapse=''), '.csv', sep=',') },
        content = function(file
        ) {
            write.csv(tableInput(), file)
        }
        )



xrfKReactive <- reactive({
    
    
    xrf.pca.header <- input$show_vars
    xrf.pca.frame <- spectra.line.table[input$show_vars]
    xrf.pca.n <- length(xrf.pca.frame)
    xrf.smalls <- xrf.pca.frame[2:xrf.pca.n]
    
    xrf.k <- kmeans(xrf.smalls, input$knum, iter.max=1000, nstart=15, algorithm=c("Hartigan-Wong"))
    xrf.pca <- prcomp(xrf.smalls, scale.=FALSE)
    
    xrf.scores <- as.data.frame(xrf.pca$x)
    
    cluster.frame <- data.frame(spectra.line.table$Spectrum, xrf.k$cluster, xrf.scores)
    
    colnames(cluster.frame) <- c("Assay", "Cluster", names(xrf.scores))
    
    cluster.frame
    
    
    
})

xrfPCAReactive <- reactive({
    
    
    
    
    
    xrf.clusters <- xrfKReactive()
    
    element.counts <- spectra.line.table[input$show_vars]
    
    
    
    xrf.pca.results <- data.frame(xrf.clusters, element.counts)
    
    xrf.pca.results
})




  #####Age Model
  
  hotableInputAge <- reactive({
      
      empty.depth <- rep(0, 20)
      empty.age <- rep(0, 20)
      empty.sigma <- rep(0, 20)
      empty.curve <- rep("intcal13", 20)
      
      empty.table <- data.frame(empty.depth, empty.age, empty.sigma, empty.curve)
      colnames(empty.table) <- c("Depth", "14C Age", "Sigma", "CalCurve")
      
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
  
  
  dataProcessed <- reactive({
      
      xrf.k <- xrfKReactive()
      
      quality.table <- values[["DF"]]
      
      colour.table <- data.frame(xrf.k, quality.table)
      colnames(colour.table) <- c(names(xrf.k), names(quality.table))
      
      
      
      
      unique.spec <- seq(1, length(colour.table$Spectrum), 1)
      null <- rep(1, length(unique.spec))
      
      spectra.line.table$Cluster <- xrf.k$Cluster
      spectra.line.table$Qualitative <- quality.table$Qualitative
      spectra.line.table$Depth <- as.numeric(as.vector(quality.table$Depth))
      spectra.line.table$PC1 <- xrf.k$PC1
      spectra.line.table$PC2 <- xrf.k$PC2
      spectra.line.table
  })
  
  
  ageResults <- reactive({
      
    
      

      
      DF3 <- values[["DF2"]]
      DF4 <- subset(DF3, !DF3$Sigma==0)
      DF5 <- as.data.frame(DF4)
      
      age.math <- Bchronology(ages=as.numeric(as.vector(DF5[,2])), ageSds=as.numeric(as.vector(DF5[,3])), positions=as.numeric(as.vector(DF5[,1])), positionThickness=rep(0.5, length(DF5$Sigma)), calCurves=DF5[,4], burn=2000, jitterPositions=TRUE)
      
      age.math

  })
  
  agePredict <- reactive({
      
      age.math <- ageResults()
      
      spectra.line.table <- dataProcessed()

      age.results <- predict(age.math, newPositions=spectra.line.table$Depth, newPositionThicknesses=rep(0.0, length(spectra.line.table$Depth)))
      
      age.medians <- apply(age.results, 2, median)
      
      age.sd <- apply(age.results, 2, sd)
      
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
  
  
  
  
  ageData <- reactive({
      
      c14ages <- values[["DF2"]]
      c14ages <- subset(c14ages, !c14ages$Sigma==0)
      c14ages <- as.data.frame(c14ages)
      
      c14min <- min(c14ages$Depth)
      c14max <- max(c14ages$Depth)
      
      
      spectra.line.table.age.unconstrained <- agePredict()
      
      
      
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
      
      
      
      
      unique.spec <- seq(1, length(spectra.line.table$Spectrum), 1)
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
      
      
      if (input$elipseplot1 == FALSE && input$pcacolour == "black") {
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
      print(plotInput2())
      
  })
  
  
  output$downloadPlot2 <- downloadHandler(
  filename = function() { paste(paste(c(input$projectname, "_", "PCAPlot"), collapse=''), '.tiff',  sep='') },
  content = function(file) {
      ggsave(file,plotInput2(), device="tiff", dpi=300, width=12, height=7)
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
  
  
  
  
  plotInput3a <- reactive({
      
      spectra.line.table <- ageData()
      
      
      x.axis <- if (input$xaxistype=="Depth") {
          paste("Depth (", input$lengthunit, ")", sep="", collapse="")
      } else if (input$xaxistype=="Age" && input$timetype=="BP"){
          paste("cal year BP")
      } else if (input$xaxistype=="Age" && input$timetype=="BC") {
          paste("cal year BC")
      } else if (input$axistype=="Age" && input$timetype=="AD") {
          paste("cal year AD")
      } else if (input$axistype=="Age" && input$timetype=="BC/AD") {
       paste("cal year BC/AD")
      }
      
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmm)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistype)], spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      
      trendy <-  as.vector((if(input$elementnorm=="None") {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
      } else {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
      }))
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)


      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
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
      scale_x_continuous(paste(x.axis), label=comma)


      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_continuous(input$xaxistype) +
      scale_y_continuous(trendy) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)


      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
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
      scale_x_continuous(paste(x.axis), label=comma)
      
      climate.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)


      qualitative.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      qualitative.time.series.point <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
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
      scale_x_continuous(paste(x.axis), label=comma)


      Depth.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = Depth), lwd=input$pointsize) +
      scale_colour_gradientn("Depth", colours=rainbow(length(spectra.line.table$Depth))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)


      
      if (input$timecolour == "Black") {
          black.time.series
      } else if (input$timecolour == "Smooth") {
          smooth.time.series
      } else if (input$timecolour == "Selected") {
          ramp.time.series
      } else if (input$timecolour == "Cluster") {
          cluster.time.series
      } else if (input$timecolour == "Climate") {
          climate.time.series.line
      } else if (input$timecolour == "QualitativePoint") {
          qualitative.time.series.point
      } else if (input$timecolour == "QualitativeLine") {
          qualitative.time.series.line
      } else if (input$timecolour == "Depth") {
          Depth.time.series
      } else if (input$timecolour == "Area") {
          area.time.series
      }
      
      
      
      
  })
  
  
  observeEvent(input$timeseriesact1, {
      
  })
  
  output$timeseriesplot1 <- renderPlot({
      input$timeseriesact1
      isolate(print(plotInput3a()))
      
  })
  
  trendPlot <- reactive({
      trendy <-  as.vector((if(input$elementnorm=="None") {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " CPS")), sep=",", collapse="")
      } else {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
      }))
      
      trendy.label <- paste(c(input$projectname, "_", trendy), collapse='')
      trendy.label
  })
  
  
  output$downloadPlot3a <- downloadHandler(
  
  
  filename = function() { paste(trendPlot(), '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput3a(), device="tiff", dpi=300, width=12, height=7)
  }
  )
  
  
  
  plotInput3b <- reactive({
      spectra.line.table <- ageData()
      
      x.axis <- if (input$xaxistype=="Depth") {
          paste("Depth (", input$lengthunit, ")", sep="", collapse="")
      } else if (input$xaxistype=="Age" && input$timetype=="BP"){
          paste("cal year BP")
      } else if (input$xaxistype=="Age" && input$timetype=="BC") {
          paste("cal year BC")
      } else if (input$axistype=="Age" && input$timetype=="AD") {
          paste("cal year AD")
      } else if (input$axistype=="Age" && input$timetype=="BC/AD") {
          paste("cal year BC/AD")
      }
      
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmm)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistype)], spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      
      trendy <-  as.vector((if(input$elementnorm=="None") {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
      } else {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
      }))
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
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
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_continuous(input$xaxistype) +
      scale_y_continuous(trendy) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
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
      scale_x_continuous(paste(x.axis), label=comma)
      
      climate.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      qualitative.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      qualitative.time.series.point <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
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
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      Depth.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = Depth), lwd=input$pointsize) +
      scale_colour_gradientn("Depth", colours=rainbow(length(spectra.line.table$Depth))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      if (input$timecolour == "Black") {
          black.time.series
      } else if (input$timecolour == "Smooth") {
          smooth.time.series
      } else if (input$timecolour == "Selected") {
          ramp.time.series
      } else if (input$timecolour == "Cluster") {
          cluster.time.series
      } else if (input$timecolour == "Climate") {
          climate.time.series.line
      } else if (input$timecolour == "QualitativePoint") {
          qualitative.time.series.point
      } else if (input$timecolour == "QualitativeLine") {
          qualitative.time.series.line
      } else if (input$timecolour == "Depth") {
          Depth.time.series
      } else if (input$timecolour == "Area") {
          area.time.series
      }
      
      
      
  })
  
  
  observeEvent(input$timeseriesact2, {
      
      
  })
  output$timeseriesplot2 <- renderPlot({
      input$timeseriesact2
      isolate(print(plotInput3b()))
      
  })
  
  
  output$downloadPlot3b <- downloadHandler(
  
  
  
  filename = function() { paste(trendPlot(), '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput3b(), device="tiff", dpi=300, width=12, height=7)
  }
  )
  
  
  
  plotInput3c <- reactive({
      
      spectra.line.table <- ageData()
      
      x.axis <- if (input$xaxistype=="Depth") {
          paste("Depth (", input$lengthunit, ")", sep="", collapse="")
      } else if (input$xaxistype=="Age" && input$timetype=="BP"){
          paste("cal year BP")
      } else if (input$xaxistype=="Age" && input$timetype=="BC") {
          paste("cal year BC")
      } else if (input$axistype=="Age" && input$timetype=="AD") {
          paste("cal year AD")
      } else if (input$axistype=="Age" && input$timetype=="BC/AD") {
          paste("cal year BC/AD")
      }
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmm)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistype)], spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      
      trendy <-  as.vector((if(input$elementnorm=="None") {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
      } else {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
      }))
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
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
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_continuous(input$xaxistype) +
      scale_y_continuous(trendy) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
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
      scale_x_continuous(paste(x.axis), label=comma)
      
      climate.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      qualitative.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      qualitative.time.series.point <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
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
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      Depth.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = Depth), lwd=input$pointsize) +
      scale_colour_gradientn("Depth", colours=rainbow(length(spectra.line.table$Depth))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      if (input$timecolour == "Black") {
          black.time.series
      } else if (input$timecolour == "Smooth") {
          smooth.time.series
      } else if (input$timecolour == "Selected") {
          ramp.time.series
      } else if (input$timecolour == "Cluster") {
          cluster.time.series
      } else if (input$timecolour == "Climate") {
          climate.time.series.line
      } else if (input$timecolour == "QualitativePoint") {
          qualitative.time.series.point
      } else if (input$timecolour == "QualitativeLine") {
          qualitative.time.series.line
      } else if (input$timecolour == "Depth") {
          Depth.time.series
      } else if (input$timecolour == "Area") {
          area.time.series
      }
      
      
      
      
  })
  
  observeEvent(input$timeseriesact3, {
  })
  
  output$timeseriesplot3 <- renderPlot({
      input$timeseriesact3
      
      isolate(print(plotInput3c()))
      
      
  })
  
  output$downloadPlot3c <- downloadHandler(
  
  
  filename = function() { paste(trendPlot(), '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput3c(), device="tiff", dpi=300, width=12, height=7)
  }
  )
  
  
  plotInput3d <- reactive({
      
      spectra.line.table <- ageData()
      
      x.axis <- if (input$xaxistype=="Depth") {
          paste("Depth (", input$lengthunit, ")", sep="", collapse="")
      } else if (input$xaxistype=="Age" && input$timetype=="BP"){
          paste("cal year BP")
      } else if (input$xaxistype=="Age" && input$timetype=="BC") {
          paste("cal year BC")
      } else if (input$axistype=="Age" && input$timetype=="AD") {
          paste("cal year AD")
      } else if (input$axistype=="Age" && input$timetype=="BC/AD") {
          paste("cal year BC/AD")
      }
      
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmm)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistype)], spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      
      trendy <-  as.vector((if(input$elementnorm=="None") {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
      } else {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
      }))
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
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
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_continuous(input$xaxistype) +
      scale_y_continuous(trendy) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
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
      scale_x_continuous(paste(x.axis), label=comma)
      
      climate.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      qualitative.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      qualitative.time.series.point <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
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
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      Depth.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = Depth), lwd=input$pointsize) +
      scale_colour_gradientn("Depth", colours=rainbow(length(spectra.line.table$Depth))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      if (input$timecolour == "Black") {
          black.time.series
      } else if (input$timecolour == "Smooth") {
          smooth.time.series
      } else if (input$timecolour == "Selected") {
          ramp.time.series
      } else if (input$timecolour == "Cluster") {
          cluster.time.series
      } else if (input$timecolour == "Climate") {
          climate.time.series.line
      } else if (input$timecolour == "QualitativePoint") {
          qualitative.time.series.point
      } else if (input$timecolour == "QualitativeLine") {
          qualitative.time.series.line
      } else if (input$timecolour == "Depth") {
          Depth.time.series
      } else if (input$timecolour == "Area") {
          area.time.series
      }
      
      
      
      
  })
  
  observeEvent(input$timeseriesact4, {
      
  })
  
  
  
  output$timeseriesplot4 <- renderPlot({
      input$timeseriesact4
      isolate(print(plotInput3d()))
      
  })
  
  output$downloadPlot3d <- downloadHandler(
  
  
  
  filename = function() { paste(trendPlot(), '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput3d(), device="tiff", dpi=300, width=12, height=7)
  }
  )
  
  
  
  
  
  plotInput3e <- reactive({
      
      spectra.line.table <- ageData()
      
      
      x.axis <- if (input$xaxistype=="Depth") {
          paste("Depth (", input$lengthunit, ")", sep="", collapse="")
      } else if (input$xaxistype=="Age" && input$timetype=="BP"){
          paste("cal year BP")
      } else if (input$xaxistype=="Age" && input$timetype=="BC") {
          paste("cal year BC")
      } else if (input$axistype=="Age" && input$timetype=="AD") {
          paste("cal year AD")
      } else if (input$axistype=="Age" && input$timetype=="BC/AD") {
          paste("cal year BC/AD")
      }
      
      
      spectra.line.table.norm <- data.frame(spectra.line.table, null)
      colnames(spectra.line.table.norm) <- c(names(spectra.line.table), "None")
      spectra.line.table.norm
      
      #interval <- unique.spec*as.numeric(input$intervalmm)+as.numeric(input$startmm)
      
      #spectra.timeseries.table <- data.frame(interval, spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth)
      #colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth")
      
      spectra.timeseries.table <- data.frame(spectra.line.table[c(input$xaxistype)], spectra.line.table[c(input$elementtrend)]/spectra.line.table.norm[c(input$elementnorm)], spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(spectra.timeseries.table) <- c("Interval", "Selected", "Cluster", "Qualitative", "Depth", "Climate")
      
      
      trendy <-  as.vector((if(input$elementnorm=="None") {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), " Counts per Second")), sep=",", collapse="")
      } else {
          paste(gsub("[.]", "", c(substr(input$elementtrend, 1, 2), "/", substr(input$elementnorm, 1, 2))), sep=",", collapse="")
      }))
      
      
      
      
      
      
      
      
      
      
      
      
      
      black.time.series <- qplot(Interval, SMA(Selected, input$smoothing), input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_line(colour = "black", lwd=input$linesize) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      smooth.time.series <- qplot(spectra.timeseries.table$Interval, SMA(spectra.timeseries.table$Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="point") +
      theme_light() +
      stat_smooth() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      ramp.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
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
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      area.time.series <- ggplot(spectra.timeseries.table, aes(Interval)) +
      theme_classic() +
      geom_area(aes(y=Selected, colour="grey60", fill="grey60"), alpha=0.6) +
      scale_x_continuous(input$xaxistype) +
      scale_y_continuous(trendy) +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      
      cluster.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
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
      scale_x_continuous(paste(x.axis), label=comma)
      
      climate.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table, colour = as.factor(Climate)) +
      scale_colour_discrete("Climatic Period") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      qualitative.time.series.line <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table, colour = as.factor(Qualitative)) +
      scale_colour_discrete("Qualitative") +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      qualitative.time.series.point <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
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
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      Depth.time.series <- qplot(Interval, SMA(Selected, input$smoothing), xlab = input$xaxistype, ylab = trendy, geom="line", data = spectra.timeseries.table) +
      geom_point(aes(colour = Depth), lwd=input$pointsize) +
      scale_colour_gradientn("Depth", colours=rainbow(length(spectra.line.table$Depth))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15)) +
      scale_x_continuous(paste(x.axis), label=comma)
      
      
      if (input$timecolour == "Black") {
          black.time.series
      } else if (input$timecolour == "Smooth") {
          smooth.time.series
      } else if (input$timecolour == "Selected") {
          ramp.time.series
      } else if (input$timecolour == "Cluster") {
          cluster.time.series
      } else if (input$timecolour == "Climate") {
          climate.time.series.line
      } else if (input$timecolour == "QualitativePoint") {
          qualitative.time.series.point
      } else if (input$timecolour == "QualitativeLine") {
          qualitative.time.series.line
      } else if (input$timecolour == "Depth") {
          Depth.time.series
      } else if (input$timecolour == "Area") {
          area.time.series
      }
      
      
      
      
  })
  
  observeEvent(input$timeseriesact5, {
      
      
  })
  
  output$timeseriesplot5 <- renderPlot({
      input$timeseriesact5
      
      
      
      isolate(print(plotInput3e()))
      
  })
  
  
  output$downloadPlot3e <- downloadHandler(
  
  
  
  filename = function() { paste(trendPlot(), '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput3e(), device="tiff", dpi=300, width=12, height=7)
  }
  )
  
  
  plotInput4 <- reactive({
      
      
      spectra.line.table <- ageData()
      
      unique.spec <- seq(1, length(spectra.line.table$Spectrum), 1)
      null <- rep(1, length(unique.spec))
     
      
      first.ratio <-spectra.line.table[input$elementratioa]
      second.ratio <- spectra.line.table[input$elementratiob]
      third.ratio <- spectra.line.table[input$elementratioc]
      fourth.ratio <- spectra.line.table[input$elementratiod]
      
      first.ratio.norm <- first.ratio/sum(first.ratio)
      second.ratio.norm <- second.ratio/sum(second.ratio)
      third.ratio.norm <- third.ratio/sum(third.ratio)
      fourth.ratio.norm <- fourth.ratio/sum(fourth.ratio)
      
      
      ratio.frame <- data.frame(first.ratio, second.ratio, third.ratio, fourth.ratio, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(ratio.frame) <- gsub("[.]", "", c(substr(input$elementratioa, 1, 2), substr(input$elementratiob, 1, 2), substr(input$elementratioc, 1, 2), substr(input$elementratiod, 1, 2), "Cluster", "Qualitative", "Depth", "Climate"))
      
      
      ratio.names.x <- c(names(ratio.frame[1]), "/", names(ratio.frame[2]))
      ratio.names.y <- c(names(ratio.frame[3]), "/", names(ratio.frame[4]))
      
      ratio.names.x <- paste(ratio.names.x, sep=",", collapse="")
      ratio.names.y <- paste(ratio.names.y, sep=",", collapse="")
      
      
      
      
      black.ratio.plot <- qplot(ratio.frame[,1]/ratio.frame[,2], ratio.frame[,3]/ratio.frame[,4], xlab = ratio.names.x, ylab = ratio.names.y ) +
      geom_point(lwd=input$spotsize2) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      cluster.ratio.plot <- qplot(ratio.frame[,1]/ratio.frame[,2], ratio.frame[,3]/ratio.frame[,4], xlab = ratio.names.x, ylab = ratio.names.y ) +
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
      theme(legend.text=element_text(size=15))
      
      cluster.ratio.ellipse.plot <- qplot(ratio.frame[,1]/ratio.frame[,2], ratio.frame[,3]/ratio.frame[,4], xlab = ratio.names.x, ylab = ratio.names.y ) +
      stat_ellipse(aes(ratio.frame[,1]/ratio.frame[,2], ratio.frame[,3]/ratio.frame[,4], colour=as.factor(ratio.frame$Cluster))) +
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
      theme(legend.text=element_text(size=15))
      
      
      climate.ratio.plot <- qplot(ratio.frame[,1]/ratio.frame[,2], ratio.frame[,3]/ratio.frame[,4], xlab = ratio.names.x, ylab = ratio.names.y ) +
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
      theme(legend.text=element_text(size=15))
      
      climate.ratio.ellipse.plot <- qplot(ratio.frame[,1]/ratio.frame[,2], ratio.frame[,3]/ratio.frame[,4], xlab = ratio.names.x, ylab = ratio.names.y ) +
      stat_ellipse(aes(ratio.frame[,1]/ratio.frame[,2], ratio.frame[,3]/ratio.frame[,4], colour=as.factor(ratio.frame$Climate))) +
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
      theme(legend.text=element_text(size=15))

      
      qualitative.ratio.plot <- qplot(ratio.frame[,1]/ratio.frame[,2], ratio.frame[,3]/ratio.frame[,4], xlab = ratio.names.x, ylab = ratio.names.y ) +
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
      theme(legend.text=element_text(size=15))
      
      qualitative.ratio.ellipse.plot <- qplot(ratio.frame[,1]/ratio.frame[,2], ratio.frame[,3]/ratio.frame[,4], xlab = ratio.names.x, ylab = ratio.names.y ) +
      stat_ellipse(aes(ratio.frame[,1]/ratio.frame[,2], ratio.frame[,3]/ratio.frame[,4], colour=as.factor(ratio.frame$Qualitative))) +
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
      theme(legend.text=element_text(size=15))
      
      quanitative.ratio.plot <- qplot(ratio.frame[,1]/ratio.frame[,2], ratio.frame[,3]/ratio.frame[,4], xlab = ratio.names.x, ylab = ratio.names.y ) +
      geom_point(aes(colour=ratio.frame$Depth), size=input$spotsize2) +
      scale_colour_gradientn("Depth", colours=rainbow(length(ratio.frame$Depth))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      
      
      
      
      
      
      
      
      
      
      if (input$ratiocolour == "Black" && input$elipseplotnorm==FALSE) {
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
      } else if (input$ratiocolour == "Depth" && input$elipseplot2==FALSE ) {
          quanitative.ratio.plot
      } else if (input$ratiocolour == "Depth" && input$elipseplot2==TRUE) {
          quanitative.ratio.plot
      }
      
  })
  
  
  output$elementratiotimeseries <- renderPlot({
      print(plotInput4())
      
      
  })
  
  ratioTerm <- reactive({
      
      ratio.names <- paste(c(c(substr(input$elementratioa, 1,2), "-", substr(input$elementratiob, 1, 2)), "_", c(substr(input$elementratioc,1,2), "-", substr(input$elementratiod,1,2), "_RatioPlot")), collapse="")
      ratio.label <- paste(c(input$projectname, "_", ratio.names), collapse='')
      ratio.label
  })
  
  output$downloadPlot4 <- downloadHandler(
  
  
  filename = function() { paste(ratioTerm(), '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput4(), device="tiff", dpi=300, width=12, height=7)
  }
  )
  
  
  
  
  
  
  plotInput5 <- reactive({
      
      
      spectra.line.table <- ageData()

      
      
      unique.spec <- seq(1, length(spectra.line.table$Spectrum), 1)
      null <- rep(1, length(unique.spec))
      
      first.axis <- spectra.line.table[input$axisa]
      second.axis <- spectra.line.table[input$axisb]
      third.axis <- spectra.line.table[input$axisc]
      
      first.axis.norm <- first.axis/sum(first.axis)
      second.axis.norm <- second.axis/sum(second.axis)
      third.axis.norm <- third.axis/sum(third.axis)
      
      axis.frame <- data.frame(first.axis, second.axis, third.axis, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(axis.frame) <- gsub("[.]", "", c(substr(input$axisa, 1, 2), substr(input$axisb, 1, 2), substr(input$axisc, 1, 2), "Cluster", "Qualitative", "Depth", "Climate"))
      
      axis.frame.norm <- data.frame(first.axis.norm, second.axis.norm, third.axis.norm, spectra.line.table$Cluster, spectra.line.table$Qualitative, spectra.line.table$Depth, spectra.line.table$Climate)
      colnames(axis.frame.norm) <- gsub("[.]", "", c(substr(input$axisa, 1, 2), substr(input$axisb, 1, 2), substr(input$axisc, 1, 2), "Cluster", "Qualitative", "Depth", "Climate"))
      
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
      scale_shape_manual("Climatic Period", values=1:nlevels(axis.frame$Climte)) +
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
      
      ternaryplotDepth <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
      geom_point(aes(colour = Depth), size=input$ternpointsize+1) +
      geom_point(size=input$ternpointsize-2) +
      scale_colour_gradientn("Depth", colours=rainbow(length(axis.frame$Depth))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotquanitativeellipse <- ggtern(data=axis.frame, aes_string(x = colnames(axis.frame)[1], y = colnames(axis.frame)[2], z = colnames(axis.frame)[3])) +
      geom_density_tern() +
      geom_point(aes(colour = Depth), size=input$ternpointsize) +
      scale_colour_gradientn("Depth", colours=rainbow(length(axis.frame$Depth))) +
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
      
      ternaryplotDepth.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
      geom_point(aes(colour = Depth), size=input$ternpointsize+1) +
      geom_point(size=input$ternpointsize-2) +
      scale_colour_gradientn("Depth", colours=rainbow(length(axis.frame$Depth))) +
      theme_light() +
      theme(axis.text.x = element_text(size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      theme(axis.title.x = element_text(size=15)) +
      theme(axis.title.y = element_text(size=15, angle=90)) +
      theme(plot.title=element_text(size=20)) +
      theme(legend.title=element_text(size=15)) +
      theme(legend.text=element_text(size=15))
      
      ternaryplotquanitativeellipse.norm <- ggtern(data=axis.frame.norm, aes_string(x = colnames(axis.frame.norm)[1], y = colnames(axis.frame.norm)[2], z = colnames(axis.frame.norm)[3])) +
      geom_density_tern() +
      geom_point(aes(colour = Depth), size=input$ternpointsize) +
      scale_colour_gradientn("Depth", colours=rainbow(length(axis.frame$Depth))) +
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
          ternaryplotDepth
      } else if (input$ternarycolour == "Depth" && input$terndensityplot==TRUE && input$ternnormplot==FALSE) {
          ternaryplotquanitativeellipse
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
          ternaryplotDepth.norm
      } else if (input$ternarycolour == "Depth" && input$terndensityplot==TRUE && input$ternnormplot==TRUE) {
          ternaryplotquanitativeellipse.norm
      }
      
      
  })
  
  output$ternaryplot <- renderPlot({
      
      print(plotInput5())
      
  })
  
  axisTerm <- reactive({
      axis.names.tern <- paste(gsub("[.]", "-", c(substr(input$axisa, 1, 2), substr(input$axisb, 1, 2), substr(input$axisc, 1, 2), "Ternary")), collapse='')
      axis.labels <- paste(c(input$projectname, "_", axis.names.tern), collapse='')
      axis.labels
  })
  
  output$downloadPlot5 <- downloadHandler(
  
  
  
  filename = function() { paste(axisTerm(), '.tiff', sep='') },
  content = function(file) {
      ggsave(file,plotInput5(), device="tiff", dpi=300, width=12, height=7)
  }
  )
  
  
  
    })
    
})








