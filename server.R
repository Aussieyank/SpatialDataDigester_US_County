# ==========================
# > ?reactiveFileReader
# > ?reactivePoll
# > ?reactiveFileReader
# ==========================

function(input, output, session) {
  
  
  # shared_mydf_ct <- SharedData$new(mydf_ct)
  # print(shared_mydf_ct$data)
  
  # ======================================================================================
  # =====================  Core computational piece for the map  =========================
  # =====================                  ---                   =========================
  # =====================  filtering and weighted average calc   =========================
  # ======================================================================================
  
  # ==================use normalized value here=====================
  weighted_val <- eventReactive(input$do, {
    
    # =====================
    # 1) observe check boxes (for debug)
    
    # if(input$ck1) print("box 1 checked") else print("box 1 NOT checked")
    # if(input$ck2) print("box 2 checked") else print("box 2 NOT checked")
    # if(input$ck3) print("box 3 checked") else print("box 3 NOT checked")
    # box_vec = c(input$ck1, input$ck2, input$ck3, input$ck4, input$ck5, input$ck6, input$ck7)                                                                       ######
    box_vec = c(input$ck1, input$ck2, input$ck3, input$ck4, input$ck5, input$ck6, input$ck7,                    input$ck100)                                                                       ######
    i_dyn   = 8
    
    
    # flip values following user's input
    radio_chk = c(input$radio2 == 2, input$radio7 == 2)
    cols_flip = c(2,7)[radio_chk]
    
    if (length(cols_flip)!=0) mydf[,cols_flip] = scalar - mydf[,cols_flip]
    
    # print(summary(box_vec))   
    # print(str(box_vec))
    # print(class(box_vec))
    
    
    # 2) form a vector "colskeep" with checked boxes if 1 and 3 are checked, return c(1,3), if none return NULL
    
    # print(which(box_vec))
    
    colskeep = which(box_vec)
    # print(c(as.numeric(input$w1),as.numeric(input$w2),as.numeric(input$w3)))
    # print(c(as.numeric(input$w1),as.numeric(input$w2),as.numeric(input$w3))[colskeep])
    # print(length(colskeep))
    # print(n(colskeep))
    
    wtskeep = c(as.numeric(input$w1),as.numeric(input$w2),as.numeric(input$w3),
                as.numeric(input$w4),as.numeric(input$w5),as.numeric(input$w6),
                # as.numeric(input$w7))[colskeep]                                                                                                                     #####
                as.numeric(input$w7),                                                                          as.numeric(input$w100))[colskeep]                                                                                                                     #####
    # wtskeep = as.numeric(box_vec)[colskeep]

    # 3) wts = matrix(c(as.numeric(input$w1),as.numeric(input$w2),as.numeric(input$w3)),  3,1)  # checkbox input will filter out things
    wts = matrix(wtskeep, length(colskeep),1)
    
    # cat('wts', wts, '\n')
    
    # 4) then get the ranges from all sliders
    
    # print(rbind(input$sl1,input$sl2,input$sl3))
    
    ranges = rbind(input$sl1,input$sl2,input$sl3,input$sl4,input$sl5,input$sl6,input$sl7)   # each row contains the range for the corresponding variable           #####

    # ======================================================================
    # add distance calculation result to mydf as a column
    latlonclk = rbind(c(-90, 40)) # default if no clk
    latlonclk = clicklatlon()
    
    
    cat('I am in weighted_val(), reading xy() value', latlonclk)
    print(dim(latlonclk))

    
    # distance_vec = gcd.slc(lon0, lat0, dummy$lon, dummy$lat) 
    # ======================== make a checkbox for mouse lat lon input === make lat lon a text output to panel ====
    dist_vec = NULL
    radar_vars = c(names(dummy2), 'distance')
    if(input$ck100) {
      dist_vec = distm (lon_lat_county_mat, latlonclk, fun = distHaversine)
      dist_vec = normalize(dist_vec) * scalar       # normalize to [0,scalar],    scalar is saved in global.R
      if (input$radio100 == 1) dist_vec = scalar - dist_vec                  # check user input, this needs to be changed accordingly.
    }
    # print(head(dist_vec))
    # 
    # mydf_w_dist     = cbind(mydf,dist_vec)
    # print(names(mydf_w_dist))
    # 
    
    
    
    # ======================================================================
    
    
    # get 2 vectors of row numbers:   rowskeep,   rowsout
    # filtered mydf will be a n(rowskeep)-by-n(colskeep) matrix, 
    
    # dumb version
    i=1
    keeper = rep(TRUE, nrow(dummy2))
    totchk = length(colskeep)
    print(totchk)
    while(totchk) {
      if (colskeep[i] != i_dyn) {
        keeper = keeper & (dummy2[,colskeep[i]]>=ranges[colskeep[i],1] & dummy2[,colskeep[i]]<=ranges[colskeep[i],2])
      }
      i = i+1
      if(i>totchk) break
    }
    
    # print(which(keeper))
    # print(sum(keeper))
    rowskeep = which(keeper)
    rowsout  = which(!keeper)
    
    # weighted_val = df %*% weight

    # as.matrix(mydf) %*% wts / sum(wts)  # this is the weighted sum, WITHOUT filtering
    # as.matrix(mydf[,colskeep]) %*% wts / sum(wts)  # this is the weighted sum, with col filtering
    
    # mydf_rowskeep = mydf[rowskeep,]
    
    weighted_score =  rep(NaN, nrow(dummy2))
    # weighted_score[rowskeep] =     as.matrix(mydf[rowskeep,][,colskeep]) %*% wts / sum(wts)  # this is the weighted sum, with col and row filtering
    weighted_score[rowskeep] =     cbind(as.matrix(mydf[rowskeep,]),dist_vec[rowskeep,])[,colskeep] %*% wts / sum(wts)  # this is the weighted sum, with col and row filtering
    

    # print(names(dummy2)[box_vec])
    # print(wts)
    PieInput = data.frame("Parameters" = radar_vars[box_vec],
               "Weights" = wts)
    # print(PieInput)
    
    
    list(v = weighted_score,i = rowskeep, p = PieInput)
    
    # The output needs to have the same number of rows 
    #weighted_val(rowskeep) = 
    #weighted_val(rowsout)  = NA
    
    # =====================

  }) # end of weighted_val
  # ===============================================================
  
  # ======================================================================================
  # ================                                                  ====================
  # ================   Filtering and weighted average function ENDS   ====================
  # ================                                                  ====================
  # ======================================================================================
  
  
  output$out <- renderPrint({
    validate(need(input$map_shape_click, FALSE))                                           # this renders current mouse-clicked lat lng to a text box
    str(input$map_shape_click)
  })
  
  
  output$pie <- renderGvis({
    gvisPieChart(weighted_val()$p, options=list(title ="Input Categories and Weights"))    # pie chart rendering
    # gvisPieChart(weighted_val()$p, options=list(width=400, height=450))
  })
  
  
  # Format popup data for leaflet map.
  popup_dat <- reactive({
    paste0("<strong>County: </strong>",
           leafmap$county_state,
           "<br><strong>Score 0-10: </strong>",
           weighted_val()$v,
           "<br><strong>Air Quality: </strong>",
           leafmap$airqlty,                                                                # leaflet popup info preparation
           "<br><strong>VoteDiff: </strong>",
           leafmap$perdiff,
           "<br><strong>Income ($/yr): </strong>",
           leafmap$median_household_income_2015,
           "<br><strong>Cost ($/yr): </strong>",
           leafmap$twoatwoc,
           "<br><strong>Income to Cost Ratio: </strong>",
           leafmap$r_inc_cos,
           "<br><strong>Crime Rate: </strong>",
           leafmap$crime_rate_per_100k,
           "<br><strong>Population Density: </strong>",
           leafmap$pop_den_log                                                                                                      #############
    )
  }) # end of popup_dat

  
  output$map <- renderLeaflet({
    # data <- popup_weight()
    leaflet() %>% 
      # addProviderTiles("Thunderforest.Transport") %>%      #Thunderforest.Transport   Stamen.TonerLite
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png") %>%
      setView(lng = -91.5, lat = 35, zoom = 5)
    # proxy <- leafletProxy("map", data = leafmap)
    # print(str(proxy))
  }) # end of renderLeaflet
  

  observeEvent(input$do,{
    
    # data <- popup_weight()
    proxy <- leafletProxy("map")
    # print(class(proxy))
    
    
    # pal <- colorQuantile("YlOrRd", NULL, n = 20)
    pal <- colorQuantile("Spectral", NULL, n = 20, reverse = TRUE,na.color = "transparent")
    # pal <- colorBin("Spectral", c(0,10), bins = 11, pretty = TRUE, na.color = "transparent", alpha = FALSE, reverse = TRUE)
    # pal <- colorBin("RdBu", c(-10,10), bins = 11, pretty = TRUE, na.color = "#808080", alpha = FALSE, reverse = FALSE)
    if (input$ck2 == TRUE & !any(input$ck1,input$ck3,input$ck4,input$ck5,input$ck6,input$ck7,input$ck100)) {
      color_val = dummy2[,2]
      pal <- colorBin("RdBu", c(-1,1), bins = 11, na.color = "transparent", alpha = FALSE, reverse = FALSE)
    } else color_val = as.vector(weighted_val()$v)
    
    isolate({
      
      # print((data$weighted_val))
      proxy %>%
        clearShapes() %>%
        addPolygons(data=leafmap,
                    fillColor = pal(color_val),
                    fillOpacity = 0.8,
                    color = "#BDBDC3",
                    weight = 1,
                    popup = popup_dat())%>%
        addLegend("bottomleft", pal=pal, values=color_val, title="color by title",
                  layerId="colorLegend")
      
    }) # end of isolate

  }) # end of observe
  
  
  observe({
    s = input$leafmaptable_rows_selected
    
    markers = as.data.frame(leafmap)[weighted_val()$i,][s,c("lat","lon")]
    # print(markers)
    proxy <- leafletProxy("map")
    proxy %>% 
      clearMarkers() %>% 
      addMarkers(data=leafmap,lng = markers[,2], lat =markers[,1])
      
    
    
    
    
  })
  
  clicklatlon <- function () {                          # this works
    # clicklonlat <-- eventReactive(input$map_shape_click,{        # this doe NOT work
    # clicklonlat <-- reactive({                                   # this doe NOT work
    # observe({                                               # this works
    # observeEvent(input$map_shape_click,{                   # this works
    
    # print(map_click_info)
    
    # lat0 = input$map_shape_click$lat
    # lon0 = input$map_shape_click$lng
    # 
    # print(lon0)
    # print(lat0)
    
    xy = rbind(c(input$map_shape_click$lng, input$map_shape_click$lat))

  }
  
 
 
 
 output$click_lat_lon = renderPrint({
   if (!is.null(input$map_shape_click)) {
     cat(input$map_shape_click$lat, input$map_shape_click$lng)                           # observe mouse click, render lat lng as text
   }
 })
 
 
 observe({                                                                                # observe mouse click, and put a marker on map
   s = input$map_shape_click
   if(input$ck100 & (!is.null(s))) {
     myUrl = 'http://www.clker.com/cliparts/5/4/e/f/133831998776815806Red%20Heart.svg'    # heart
     if (input$radio100 == 2) myUrl = 'http://cdn.mysitemyway.com/etc-mysitemyway/icons/legacy-previews/icons-256/blue-jelly-icons-culture/024890-blue-jelly-icon-culture-heart-broken1-sc44.png'
     
     proxy <- leafletProxy("map",data=leafmap)                                            
     proxy %>%
       clearMarkers() %>%
       addMarkers(lng = s$lng, lat = s$lat,
                  icon = icons(iconUrl = myUrl,
                               iconWidth = 30, iconHeight = 30)
                  )
   }


 })


  # output$radar <- renderChartJSRadar({
  output$radar <- renderUI({


    # print("renderChartJSRadar")
    # print(dummy_norm)

    # this IS the mydf data frame!!!!!
    # flip values following user's input
    radio_chk = c(input$radio2 == 2, input$radio7 == 2)
    cols_flip = c(2,7)[radio_chk]
    if (length(cols_flip)!=0) mydf[,cols_flip] = scalar - mydf[,cols_flip]                  # render radar chart

    radardf = mydf
    radardf$score = weighted_val()$v

    radardf$county_names = dummy$county_state

    radardf <- radardf %>%
      filter(!is.na(score)) %>%
        arrange(desc(score))

    radardf_plot <- radardf[,c(1,2,3,4,5,6,7)]                                                                                                                  ####### expand


    # print(names(radardf))
    # print(head(radardf_plot))
    # print(t(head(radardf_plot)))

    scores_top = as.data.frame(t(head(radardf_plot,3)))                                        #
    names(scores_top) = head(radardf$county_names,3)
    labs = c("air","vote","income","cost","income/cost","crime rate","pop den")                                                                                ####### this needs to be generalized
    # print("here")
    # print(scores)
    # print(dd)
    
    # quantile data
    qt = lapply(mydf, quantile, name=FALSE, na.rm=TRUE)
    dd  <-  as.data.frame(matrix(unlist(qt), nrow=length(unlist(qt[1]))))
    
    scores_qt = as.data.frame(t(dd[2:4,]))
    names(scores_qt) = c("25%", "median", "75%")
    
    
    
    scores_plot = cbind(scores_top, scores_qt)
    
    tagList(chartJSRadar(scores_plot, labs , maxScale = NULL))


    # ===================================================================================
    # # print(weighted_val()$i)
    #
    # #
    # chartJSRadar(skills, main = "Data Science Radar")
    # ===================================================================================

  }) # end of renderChartJSRadar

  output$leafmaptable = DT::renderDataTable({
    as.data.frame(leafmap)[weighted_val()$i,]                                             # render FILTERED data table for tab 2 -- sliders on tab 1 DO INTERACT with tab 3 data
  })
  
  # output$corr <- renderUI({
  #   
  # })
  
  
  
  
  # ======================================================================================
  # ====================          Map related logic ENDS         =========================
  # ====================                                         =========================
  # ====================          Corr Plot Logic STARTS         =========================
  # ======================================================================================
  
  
  
  
  dataset <- reactive({
    dummy
  })
  
  numericColumns <- reactive({
    df <- dataset()
    colnames(df)[sapply(df, is.numeric)]                                                  # get col names
  })
  
  correlation <- reactive({
    data <- dataset()
    variables <- input$variables
    if(is.null(data) || !length(intersect(variables, colnames(data)))) {
      NULL
    } else {
      cor(dataset()[,input$variables], use = input$corUse, method = input$corMethod)      # computational core -- corr calc
    }
  })
  
  sigConfMat <- reactive({
    val <- correlation()
    if(!is.null(val))
      corTest(val, input$confLevel)                                                        # calling correlation function, send to corTest then calc for p-value and CI
  })
  
  ## Data and Correlation Validation and UI Updates ##########
  
  #Update hclust rect max
  observe({
    val <- correlation()
    if(!is.null(val))
      updateNumericInput(session, "plotHclustAddrect", max = nrow(val))
  })
  
  #Update variable selection
  observe({
    updateCheckboxGroupInput(session, "variablesCheckbox", choices = numericColumns(), selected = numericColumns())
    
    updateSelectInput(session, "variables", choices = numericColumns(), selected = numericColumns())
  })
  
  #Link Variable Selection
  observe({
    if(input$variablesStyle == "Checkbox") {
      updateCheckboxGroupInput(session, "variablesCheckbox", selected = isolate(input$vairables))
    }
  })
  observe({
    updateSelectInput(session, "variables", selected = input$variablesCheckbox)
  })
  
  output$warning <- renderUI({
    val <- correlation()
    if(is.null(val)) {
      tags$i("Waiting for data input...")
    } else {
      isNA <- is.na(val)
      if(sum(isNA)) {
        tags$div(
          tags$h4("Warning: The following pairs in calculated correlation have been converted to zero because they produced NAs!"),
          helpText("Consider using an approriate NA Action to exclude missing data"),
          renderTable(expand.grid(attr(val, "dimnames"))[isNA,]))
      }
    }
  })
  
  ## Correlation Plot ####################################
  
  output$corrPlot <- renderPlot({
    val <- correlation()
    if(is.null(val)) return(NULL)
    
    val[is.na(val)] <- 0
    args <- list(val,
                 order = if(input$plotOrder == "manual") "original" else input$plotOrder, 
                 hclust.method = input$plotHclustMethod, 
                 addrect = input$plotHclustAddrect,
                 
                 p.mat = sigConfMat()[[1]],
                 sig.level = if(input$sigTest) input$sigLevel else NULL,
                 insig = if(input$sigTest) input$sigAction else NULL,
                 
                 lowCI.mat = sigConfMat()[[2]],
                 uppCI.mat = sigConfMat()[[3]],
                 plotCI = if(input$showConf) input$confPlot else "n")
    
    if(input$showConf) {
      do.call(corrplot, c(list(type = input$plotType), args))
    } else if(input$plotMethod == "mixed") {
      do.call(corrplot.mixed, c(list(lower = input$plotLower,
                                     upper = input$plotUpper),
                                args))
    } else {
      do.call(corrplot, c(list(method = input$plotMethod, type = input$plotType), args))
    }
  }) # end of renderPlot to output$corrPlot
  
  
  
  
  # ======================================================================================
  # ====================                                         =========================
  # ====================          Corr Plot Logic Ends           =========================
  # ====================                                         =========================
  # ======================================================================================
  
  
  
  
  
}