library(shiny)

# Plots a breakdown of total jobs per year
shinyServer(function(input, output, session) {
    
    # REACTIVES ====
    ## Dropdown update ====
    
    observe({
        updated_job_categories <-
        job_category_ratios %>% 
            filter(cluster %in% input$cluster) %>% 
            pull(job_category)
        
        updatePickerInput(inputId = "job_category", 
                          choices = updated_job_categories, 
                          selected = updated_job_categories[1], 
                          session = session)
        
    })
    
    ## Main plot ====
    plot_jobs = reactive({
        
        jobs_plot <-
        job_opportunities %>% 
            filter(year>=input$year_from, year<=input$year_to) %>% 
            mutate(text = glue::glue("{round(jobs_base/1e3)} K")) %>% 
            ggplot(aes(x = year, y = jobs_base)) +
            geom_col(aes(x = year, y = working_pop), fill = "#53868B") +
            geom_line() +
            geom_ribbon(aes(ymin = jobs_worst, ymax = jobs_best, alpha = 0.3), 
                        fill = "grey70", 
                        show.legend = FALSE) +
            scale_y_continuous(breaks = seq(0, 2000000, 250000), 
                               minor_breaks = NULL, 
                               labels = scales::comma_format()) +
            scale_x_continuous(labels = seq(input$year_from, input$year_to,1), 
                               breaks = seq(input$year_from, input$year_to,1), 
                               minor_breaks = NULL) +
            geom_text(aes(label = text), check_overlap = TRUE) +
            geom_text(aes(label = glue::glue("{round(working_pop/1e6,2)} M"), 
                          y = working_pop)) +
            labs(title = "Job Opportunities in Mombasa County ('000)",
                 subtitle = "Showing total number of jobs against the working population (15-64 yrs, millions)",
                 x = NULL,
                 y = "Job Opportunities")
            
        return(list(jobs_plot = jobs_plot))
    })
    
    ## Main table  ====
    # This table displays jobs created based on the choices selected
    total_jobs = reactive({
        
        cluster <-
            cluster_ratios %>% 
            filter(cluster %in% input$cluster) %>% 
            summarise(ratio = sum(ratio)) %>% 
            pull(ratio)
        
        sector <-
            sector_ratios %>% 
            filter(sector %in% input$sector) %>% 
            summarise(ratio = sum(ratio)) %>% 
            pull(ratio)
        
        org_size <-
            org_size_ratios %>% 
            filter(size %in% input$org_size) %>% 
            summarise(ratio = sum(ratio)) %>% 
            pull(ratio)
        
        job_category <-
            job_category_ratios %>% 
            filter(cluster %in% input$cluster) %>% 
            filter(job_category %in% input$job_category) %>% 
            summarise(ratio = sum(ratio)) %>% 
            pull(ratio)
        
        # Applying ratios
        multiplier = cluster*sector*org_size*cluster*job_category
        multiplier
        
        jobs_created <-
            job_opportunities %>% 
            filter(year>=input$year_from, year<=input$year_to) %>% 
            select(year, jobs_base) %>% 
            mutate(jobs_created = jobs_base*multiplier,
                   jobs_created = round(jobs_created)) %>% 
            rename(all_jobs = jobs_base,
                   select_criteria = jobs_created) %>% 
            set_names(str_to_upper(colnames(.)))
        
        return(list(jobs_created=jobs_created))
        
    })
    
    ## All jobs ====
    summary_stats = reactive({
        jobs_base <-
            job_opportunities %>% 
            filter(year>=input$year_from, year<=input$year_to) %>% 
            pull(jobs_base)
        
        jobs_best <-
            job_opportunities %>% 
            filter(year>=input$year_from, year<=input$year_to) %>% 
            pull(jobs_best)
        
        jobs_worst <-
            job_opportunities %>% 
            filter(year>=input$year_from, year<=input$year_to) %>% 
            pull(jobs_worst)
        
        jobs_base <- jobs_base[length(jobs_base)] - jobs_base[1]
        jobs_best <- jobs_best[length(jobs_best)] - jobs_best[1]
        jobs_worst <- jobs_worst[length(jobs_worst)] - jobs_worst[1]
        
        jobs_best <- ifelse(test = is.na(jobs_best), yes = jobs_base, no = jobs_best)
        jobs_worst <- ifelse(test = is.na(jobs_worst), yes = jobs_base, no = jobs_worst)
        
        
        return(list(jobs_base = jobs_base,
                    jobs_best = jobs_best,
                    jobs_worst = jobs_worst))
    })

    # OUTPUTS ====
    ## Main plot ====
    output$jobs_plot <- renderPlot(plot_jobs()$jobs_plot)
    
    ## Main table ====
    output$jobs_created <- renderDataTable(server = FALSE,{
        datatable(data = total_jobs()$jobs_created,
                  rownames = FALSE,
                  extensions = "Buttons",
                  options = list(dom = "Btpli",
                                 buttons = c("excel", "pdf")))
    })
    
    ## Summary Stats ====
    output$jobs_base <- renderInfoBox({
        infoBox(title = "All Jobs",
                value = comma(x = summary_stats()$jobs_base,
                              accuracy = 1))
        })
    
    
    output$jobs_best <- renderInfoBox({
        infoBox(title = "Best Case",
                value = comma(x = summary_stats()$jobs_best, 
                              accuracy = 1))
    })
    
    
    output$jobs_worst <- renderInfoBox({
        infoBox(title = "Worst Case",
                value = comma(x = summary_stats()$jobs_worst,
                              accuracy = 1))
    })
    
    
})
