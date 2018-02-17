# SSM Vote Map
# server.R

library(shiny)
library(leaflet)
library(data.table)
library(rgdal)
library(rgeos)
library(htmltools)
library(XML)
library(RCurl)
library(ggplot2)

ssmVoteMap <- readOGR("./data","ssmVoteMap")

subMap <- readOGR("./data","SSC_2011_AUST")

subNm <- readOGR("./data","SSC_point")

cutval <- c(25,35,45,49.9999,50.00001,55,65,75,85)
voteCols <- c("#1f78b4","#69a5cd","#b4d2e6","#ffffff","#e7b8df","#cf72bf","#b82ca0","#81008a")
rpal <- colorBin(voteCols, 25:85, bins=cutval)
ssmVoteMap$popupval <- paste("<b>",ssmVoteMap$CED_NAME17,"</b>  -  ",ssmVoteMap$Member," (",ssmVoteMap$Party,")</br>",
                          "Yes: ",ssmVoteMap$Yes_pc,"%   ",ssmVoteMap$Yes_no,"</br>",
                          "No: ",ssmVoteMap$No_pc,"%   ",ssmVoteMap$No_no,"</br>",
                          "Total Voters: ",ssmVoteMap$Respond_pc,"%   ",ssmVoteMap$Respond_no,"</br>",
                          "Total Eligable Voters: ",ssmVoteMap$Total_no_1,
                          sep="")


participation <- fread("./data/participation.csv")
participation[,Demographic:=as.factor(Demographic)]

religion <- fread("./data/religion.csv")
age <- fread("./data/age.csv")
marriage <- fread("./data/marriage.csv")

census <- rbind(age,religion,marriage)

shinyServer(function(input, output) {
   
    output$mymap <- renderLeaflet({
        

        leaflet() %>%
            addProviderTiles("CartoDB.Positron",
                             options = providerTileOptions(noWrap = TRUE, opacity=0.7)      ) %>%
            addPolygons(group="ssmVoteMapG", 
                        data=ssmVoteMap, 
                        fill = TRUE, 
                        stroke = TRUE, 
                        weight = 1.75,
                        color="grey", 
                        fillColor = ~rpal(Yes_pc), 
                        fillOpacity = .9, 
                        smoothFactor=1,
                        layerId = ~CED_NAME17,
                        label =  lapply(ssmVoteMap$popupval,HTML),
                        labelOptions = labelOptions(direction='auto')) %>%
            # addPolygons(group="Suburb Outline", data=subMap, fill=FALSE, stroke=TRUE, weight=1, color="#56c556",
            #             opacity=.5, label=NULL, smoothFactor=1.5) %>%
            # addLabelOnlyMarkers(group="Suburb Name",data=subNm, label=~SSC_NAME,
            #                     clusterOptions = markerClusterOptions(),
            #                     labelOptions = labelOptions(clickable = FALSE, noHide = TRUE, offset = c(0,0),
            #                                                 textOnly = TRUE,
            #                                                 style = list(
            #                                                     "color" = "#3d8d3d"
            #                                                     ))) %>%
            # addLayersControl(
            #                  overlayGroups = c("Suburb Outline","Suburb Name"),
            #                  options = layersControlOptions(collapsed = FALSE)) %>%
            # hideGroup("Suburb Outline") %>%
            # hideGroup("Suburb Name") %>%
            addLegend(position="bottomright",colors=voteCols[voteCols!="#ffffff"],opacity=0.9, 
                      labels=c("25-35","35-45","45-50","50-55","55-65","65-75","75-85"),
                      title="Percentage YES vote") %>%
            #setView(lng = 140, lat = -25, zoom = 8)
            setView(lng = 140, lat = -25, zoom = 4)
            
    })
    
    observeEvent(input$loadSuburb, {
        
        withProgress(message = 'Loading map layers', value = .4, {
        
            leafletProxy("mymap") %>%
                addPolygons(group="Suburb Outline", data=subMap, fill=FALSE, stroke=TRUE, weight=1, color="#56c556",
                            opacity=.5, label=NULL, smoothFactor=1.5) %>%
                addLabelOnlyMarkers(group="Suburb Name",data=subNm, label=~SSC_NAME,
                                    clusterOptions = markerClusterOptions(),
                                    labelOptions = labelOptions(clickable = FALSE, noHide = TRUE, offset = c(0,0),
                                                                textOnly = TRUE,
                                                                style = list(
                                                                    "color" = "#3d8d3d"
                                                                ))) %>%
                addLayersControl(
                    overlayGroups = c("Suburb Outline","Suburb Name"),
                    options = layersControlOptions(collapsed = FALSE)) %>%
                hideGroup("Suburb Outline") %>%
                hideGroup("Suburb Name")
            
        incProgress(.2, detail = "Rendering layers")
            Sys.sleep(.7)
        incProgress(.3, detail = "Layers loaded")
            Sys.sleep(.7)
        })

    }, once = TRUE)
    
    # observeEvent(input$`Suburb Outline`, {
    #     if(input$`Suburb Outline` == TRUE) {
    #         leafletProxy("mymap") %>%
    #             addPolygons(group="Suburb Outline", data=subMap, fill=FALSE, stroke=TRUE, weight=1, color="#56c556",
    #                         opacity=.5, label=NULL, smoothFactor=1.5)
    #     }
    # })

    
    output$partPlot <- renderPlot({
        
        
        ggplot(participation[toupper(Area)=="TOTAL"],aes(x=Demographic,y=PartRate)) +
            geom_bar(stat="identity", fill="dark green", alpha=.8) +
            geom_text(aes(label=paste(PartRate,"%  ",prettyNum(EligPart,big.mark=","),sep="")), nudge_y = -1.5, size=3, angle=90, hjust = 1) +
            labs(title=paste("Survey Participation - Total"), x="Age Demographic", y="Participation Rate (%)") +
            theme_minimal() +
            theme(axis.text.x=element_text(hjust=1,angle=70, size=rel(1.2)),
                  panel.grid.major.x = element_blank()) +
            ylim(c(0,100))
    })
    
    output$censusPlot <- renderPlot({
        
        ggplot(census[dataset==input$dataset],aes(x=GroupNm, y=Persons)) +
            geom_bar(stat="sum") +
            theme_bw() +
            theme(axis.text.x = element_text(angle=90, hjust=1),
                  legend.position="none") +
            labs(x=input$dataset)
    
    })
        
    observeEvent(input$mymap_shape_click, {
        
        Click <- input$mymap_shape_click
        Elect <- Click$id
        output$partPlot <- renderPlot({


            ggplot(participation[toupper(Area)==toupper(Elect)],aes(x=Demographic,y=PartRate)) +
                geom_bar(stat="identity", fill="dark green", alpha=.8) +
                geom_text(aes(label=paste(PartRate,"%  ",prettyNum(EligPart,big.mark=","),sep="")), nudge_y = -1.5, size=3, angle=90, hjust = 1) +
                labs(title=paste("Survey Participation -",Elect), x="Age Demographic", y="Participation Rate (%)") +
                theme_minimal() +
                theme(axis.text.x=element_text(hjust=1,angle=70, size=rel(1.2)),
                      panel.grid.major.x = element_blank()) +
                ylim(c(0,100))
        })
        
        CED_ID <- ssmVoteMap@data$CED_CODE17[ssmVoteMap@data$CED_NAME17==Elect]

        output$censusPlot <- renderPlot({
            
            
            ggplot(census[CED_ID==CED_NO & dataset==input$dataset],aes(x=GroupNm, y=Persons)) +
                geom_bar(stat="sum") +
                #facet_wrap("rowNo", scale="free_x", ncol=1) +
                theme_bw() +
                theme(axis.text.x = element_text(angle=90, hjust=1),
                      legend.position="none") +
                labs(x=input$dataset)
            
        })
        
        

    })
    

    
    # observe({
    #     if (!is.null(input$'Suburb Name')){
    #         if (input$'Suburb Name' == TRUE){ # how to get value if layercontrol is clicked?
    #             if (input$map_zoom > 10) {
    #                 leafletProxy("mymap") %>%
    #                     addLabelOnlyMarkers(group="Suburb Name",data=subNm, label=~SSC_NAME,
    #                                         labelOptions = labelOptions(clickable = FALSE, noHide = TRUE, offset = c(0,0),
    #                                                                     textOnly = TRUE,
    #                                                                     style = list(
    #                                                                         "color" = "#3d8d3d"
    #                                                                     )))
    #             }else{
    #                 leafletProxy("mymap") %>% hideGroup("Suburb Name")
    #             }
    #         }
    #     }
    # })
    
    
    # style = list(
    #     "color" = "red",
    #     "font-family" = "serif",
    #     "font-style" = "italic",
    #     "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
    #     "font-size" = "12px",
    #     "border-color" = "rgba(0,0,0,0.5)"
    # )
    
    # observeEvent(input$suburbOutline, {
    #     if(input$suburbOutline==TRUE) {
    #         leafletProxy("mymap") %>%
    #                 addPolygons(layerId=subMap, data=subMap, fill=FALSE, stroke=TRUE, weight=1, color="#56c556", opacity=.5, label= NULL)
    #     } else {
    #         leafletProxy("mymap") %>%
    #             addPolygons(layerId=subMap, fill=FALSE, stroke=TRUE, weight=1, color="#56c556", opacity=.5, label= NULL)
    #     }
    # })
    # 
    # observeEvent(input$suburbNm, {
    #     leafletProxy("mymap") %>%
    #         addLabelOnlyMarkers(data=subNm,color="#3d8d3d")
    # })
  
})
