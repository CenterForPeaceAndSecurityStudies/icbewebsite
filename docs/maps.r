library(flextable)
library(tidyverse)
library(googlesheets4)

library(tidyverse)
library(ggraph)
#library(edgebundle)
library(igraph)
library(stringr)
library(svglite)
icb_crises <-readRDS(paste0(here::here(), "/docs/icb1v14.Rds") )
conflict_data_long_singlecode_icb <- readRDS("/mnt/8tb_a/rwd_github_private/icbe/docs/conflict_data_long_singlecode_icb.rds") %>% separate(dataset_identifier, c("a", "b"), sep='_') %>% mutate(crisno=b %>% as.numeric())

crises <- icb_crises$crisno %>% unique()
show_plot <- function(x){
  plot_path <- paste0('metro_plots/',basename(x))
  cat(sep="",
      '\n\n<img src="',plot_path,'" loading="lazy" >\n\n'
      )
}
map <- function(crisis, fromscratch=T){

  i=which(icb_crises$crisno==crisis)
  section <- paste0(icb_crises[i,]$crisname %>% stringr::str_to_title()," (", icb_crises[i,]$yrtrig ,"-",icb_crises[i,]$yrterm,")", " [#", icb_crises[i,]$crisno,"]")

  save_file_ft <- paste0("/mnt/8tb_a/rwd_github_private/ICBEdataset/paper/tables/sentence_tables/ft_sentence_table_",crisis,".Rds") #We pull from the icb paper folder for now

  save_file_p <- paste0("/mnt/8tb_a/rwd_github_private/ICBEdataset/paper/figures/metro_plots/p_metro_plot_",crisis,".Rds")
  save_file_png <- paste0("/mnt/8tb_a/rwd_github_private/icbe/docs/metro_plots/p_metro_plot_",crisis,".svg")

  save_file_mids_p <- paste0("/mnt/8tb_a/rwd_github_private/ICBEdataset/paper/figures/metro_plots/p_mids_metro_plot_",crisis,".Rds")
  save_file_mids_png <- paste0("/mnt/8tb_a/rwd_github_private/icbe/docs/metro_plots/p_mids_metro_plot_",crisis,".svg")

  save_file_icb_p <- paste0("/mnt/8tb_a/rwd_github_private/ICBEdataset/paper/figures/metro_plots/p_icb_metro_plot_",crisis,".Rds")
  save_file_icb_png <- paste0("/mnt/8tb_a/rwd_github_private/icbe/docs/metro_plots/p_icb_metro_plot_",crisis,".svg")

  plot_problem <- paste0('https://github.com/CenterForPeaceAndSecurityStudies/icbe/issues/new?assignees=&labels=&template=bug_report.md&title=Crisis+%23+',i,'+Plot+%28Problem Here%29')
  table_problem <- paste0('https://github.com/CenterForPeaceAndSecurityStudies/icbe/issues/new?assignees=&labels=&template=bug_report.md&title=Crisis+%23+',i,'+Table+%28Sentence %23s Here%29++%28Problem Here%29')

  #We open a tabset in the source file
  #If the case is missing for some reason skip it
  if(file.exists(save_file_p)){
    ft_full_case <- readRDS(save_file_ft)
    #cat(sprintf(template, section))
    if( crisis==196){ cat("\n\n#### ",section,"  {.active .tabset}\n\n") } else {
      cat("\n\n#### ",section,"  {.tabset .tabset-fade .tabset-pills }\n\n")
    }

    if(fromscratch){
      p_metro_plot <- readRDS(save_file_p)
      plot_height <- p_metro_plot$sentence_count
      p_width=max(length(p_metro_plot$actor_colors)*1.5,14)
      p_height=max(p_metro_plot$sentence_count/2,6)
      svglite(save_file_png, #because this is a multiple and not a discrete amount it ends up adding too much padding to very long
              width =p_width , height =p_height
              #dpi = 1200
      )
      plot(
           p_metro_plot + theme(plot.margin=unit(c(0,0,0,0),"in"))+
             scale_x_continuous(expand = expansion(add = c(50, 50))) +
             scale_y_continuous(expand = expansion(add = c(50, 50)))
           )
      invisible(dev.off())

    }

    cat('\n\n##### ICBe (ours) {.tabset .tabset-fade .tabset-pills} \n\n')
      cat('\n\n###### Crisis Map\n\n')
      cat('\n[Report Plot Problem](',plot_problem,'){target="_blank"}')
      cat(' \n \n')
      show_plot(save_file_png)
      cat(' \n \n')
      #cat('</details>')

      #These are huge,
      #cat('<details><summary>ICBe Narrative</summary>')
      cat('###### Narrative and Codings\n\n')
        cat('\n[Report Table Problem](',table_problem,'){target="_blank"}')
        #ft_full_case %>%
        #  flextable::width(j = 2, width=5, unit = "in") %>%
        #  flextable::width(j = 3, width=5, unit = "in") %>% flextable_to_rmd()
        cat(' \n \n')
        #cat('</details>')

    #ICB Dyadic
    if(file.exists(save_file_icb_p)){
      cat('\n\n##### ICB \n\n')
      #cat('<details><summary>ICB Dyadic Crisis Map</summary>')
      if(fromscratch){
        p_icb_metro_plot <- readRDS(save_file_icb_p)
        plot_icb_height <- p_icb_metro_plot$sentence_count
        p_width=max(length(p_icb_metro_plot$actor_colors)*1.5,14)
        p_height=min(max(p_icb_metro_plot$sentence_count/2,6),12) #cap it at 49 inches long
        svglite(save_file_icb_png, #because this is a multiple and not a discrete amount it ends up adding too much padding to very long
                width =p_width , height =p_height
                #dpi = 1200
        )
        plot(
          p_icb_metro_plot + theme(plot.margin=unit(c(0,0,0,0),"in"))+
            scale_x_continuous(expand = expansion(add = c(10, 15))) +
            scale_y_continuous(expand = expansion(add = c(100, 100)))
        )
        invisible(dev.off())

      }
      show_plot(save_file_icb_png)
      #cat('</details>')
    }

    #Phoenix
    save_file_phoenix_p <- paste0("/mnt/8tb_a/rwd_github_private/ICBEdataset/paper/figures/metro_plots/p_phoenix_metro_plot_",crisis,".Rds")
    save_file_phoenix_png <- paste0("/mnt/8tb_a/rwd_github_private/icbe/docs/metro_plots/p_phoenix_metro_plot_",crisis,".svg")
    if(file.exists(save_file_phoenix_p)){
      #cat('<details><summary>Phoenix Crisis Map</summary>')
      cat('\n\n##### Phoenix \n\n')
      if(fromscratch){
        p_phoenix_metro_plot <- readRDS(save_file_phoenix_p)
        plot_phoenix_height <- p_phoenix_metro_plot$sentence_count
        p_width=max(length(p_phoenix_metro_plot$actor_colors)*1.5,14)
        p_height=min(max( (p_phoenix_metro_plot$data$y %>% unique() %>% length()) /2,6),40) #cap it at 30 inches long
        vertical_offset= (p_phoenix_metro_plot$data$y %>% range() %>% abs() %>% diff() %>% abs())/(p_phoenix_metro_plot$data$y %>% unique() %>% length())   #offsets are kind of complicated bc of the graph
        horizontal_offset= (p_phoenix_metro_plot$data$x %>% range() %>% abs() %>% diff() %>% abs())/length(p_phoenix_metro_plot$actor_colors)

        svglite(save_file_phoenix_png, #because this is a multiple and not a discrete amount it ends up adding too much padding to very long
                width =p_width , height =p_height
                #dpi = 1200
        )
        plot(
          p_phoenix_metro_plot +
                     theme(plot.margin=unit(c(0,0,0,0),"in")) +
                     scale_x_continuous(expand = expansion(add = c(horizontal_offset/2,horizontal_offset/2)) ) +
                     scale_y_continuous(expand = expansion(add = c(0, vertical_offset*3   ) ) )
        )
        invisible(dev.off())

      }
      show_plot(save_file_phoenix_png)
      #cat('</details>')
    }

    #terrier
    save_file_terrier_p <- paste0("/mnt/8tb_a/rwd_github_private/ICBEdataset/paper/figures/metro_plots/p_terrier_metro_plot_",crisis,".Rds")
    save_file_terrier_png <- paste0("/mnt/8tb_a/rwd_github_private/icbe/docs/metro_plots/p_terrier_metro_plot_",crisis,".svg")
    if(file.exists(save_file_terrier_p)){
      cat('\n\n##### Terrier Crisis Map\n\n')
      if(fromscratch){
        p_metro_plot <- readRDS(save_file_terrier_p)
        plot_icews_height <- p_metro_plot$sentence_count
        p_width=max(length(p_metro_plot$actor_colors)*1.5,14)
        p_height=min(max((p_metro_plot$data$y %>% unique() %>% length()) /2,6),60) #cap it at 60 inches long
        vertical_offset= (p_metro_plot$data$y %>% range() %>% abs() %>% diff() %>% abs())/(p_metro_plot$data$y %>% unique() %>% length())   #offsets are kind of complicated bc of the graph
        horizontal_offset= (p_metro_plot$data$x %>% range() %>% abs() %>% diff() %>% abs())/length(p_metro_plot$actor_colors)
        svglite(save_file_terrier_png, #because this is a multiple and not a discrete amount it ends up adding too much padding to very long
                width =p_width , height =p_height
                #dpi = 1200
        )
        plot(
          p_metro_plot +
            theme(plot.margin=unit(c(0,0,0,0),"in")) +
            scale_x_continuous(expand = expansion(add = c(horizontal_offset/2,horizontal_offset/2)) ) +
            scale_y_continuous(expand = expansion(add = c(0, vertical_offset*4   ) ) )
        )
        invisible(dev.off())

      }
      show_plot(save_file_terrier_png)
    }

    #ICEWS
    save_file_icews_p <- paste0("/mnt/8tb_a/rwd_github_private/ICBEdataset/paper/figures/metro_plots/p_icews_metro_plot_",crisis,".Rds")
    save_file_icews_png <- paste0("/mnt/8tb_a/rwd_github_private/icbe/docs/metro_plots/p_icews_metro_plot_",crisis,".svg")
    if(file.exists(save_file_icews_p)){
      cat('\n\n##### ICEWS \n\n')
      if(fromscratch){
        p_icews_metro_plot <- readRDS(save_file_icews_p)
        plot_icews_height <- p_icews_metro_plot$sentence_count
        p_width=max(length(p_icews_metro_plot$actor_colors)*1.5,14)
        p_height=min(max((p_icews_metro_plot$data$y %>% unique() %>% length()) /2,6),60) #cap it at 60 inches long
        vertical_offset= (p_icews_metro_plot$data$y %>% range() %>% abs() %>% diff() %>% abs())/(p_icews_metro_plot$data$y %>% unique() %>% length())   #offsets are kind of complicated bc of the graph
        horizontal_offset= (p_icews_metro_plot$data$x %>% range() %>% abs() %>% diff() %>% abs())/length(p_icews_metro_plot$actor_colors)
        svglite(save_file_icews_png, #because this is a multiple and not a discrete amount it ends up adding too much padding to very long
                width =p_width , height =p_height
                #dpi = 1200
        )
        plot(
          p_icews_metro_plot +
            theme(plot.margin=unit(c(0,0,0,0),"in")) +
            scale_x_continuous(expand = expansion(add = c(horizontal_offset/2,horizontal_offset/2)) ) +
            scale_y_continuous(expand = expansion(add = c(0, vertical_offset*4   ) ) )
        )
        invisible(dev.off())

      }
      show_plot(save_file_icews_png)
    }


    #Mids
    if(file.exists(save_file_mids_p)){
      #cat('<details><summary>MIDs Crisis Map</summary>')
      cat('\n\n##### MIDs \n\n')
      if(fromscratch){
        p_mids_metro_plot <- readRDS(save_file_mids_p)
        plot_mids_height <- p_mids_metro_plot$sentence_count
        p_width=max(length(p_mids_metro_plot$actor_colors)*1.5,14)
        p_height=min(max(p_mids_metro_plot$sentence_count/2,6),12) #cap it at 49 inches long

        svglite(save_file_mids_png, #because this is a multiple and not a discrete amount it ends up adding too much padding to very long
                width =p_width , height =p_height
                #dpi = 1200
        )
        plot(
          p_mids_metro_plot +
            theme(plot.margin=unit(c(0,0,0,0),"in"))+
            scale_x_continuous(expand = expansion(add = c(10,15))) +
            scale_y_continuous(expand = expansion(add = c(100, 100)))
        )
        invisible(dev.off())
      }
      show_plot(save_file_mids_png)
      #cat('</details>')
    }

    #Mids Incidents
    save_file_mids_incidents_p <- paste0("/mnt/8tb_a/rwd_github_private/ICBEdataset/paper/figures/metro_plots/p_mids_incidents_metro_plot_",crisis,".Rds")
    save_file_mids_incidents_png <- paste0("/mnt/8tb_a/rwd_github_private/icbe/docs/metro_plots/p_mids_incidents_metro_plot_",crisis,".svg")
    if(file.exists(save_file_mids_incidents_p)){
      cat('\n\n##### MIDs Incidents \n\n')
      if(fromscratch){
        p_metro_plot <- readRDS(save_file_mids_incidents_p)
        p_width=max(length(p_metro_plot$actor_colors)*1.5,14)
        p_height=min(max((p_metro_plot$data$y %>% unique() %>% length()) /2,6),60) #cap it at 60 inches long
        vertical_offset= (p_metro_plot$data$y %>% range() %>% abs() %>% diff() %>% abs())/(p_metro_plot$data$y %>% unique() %>% length())   #offsets are kind of complicated bc of the graph
        horizontal_offset= (p_metro_plot$data$x %>% range() %>% abs() %>% diff() %>% abs())/length(p_metro_plot$actor_colors)
        svglite(save_file_mids_incidents_png, #because this is a multiple and not a discrete amount it ends up adding too much padding to very long
                width =p_width , height =p_height
                #dpi = 1200
        )
        plot(
          p_metro_plot +
            theme(plot.margin=unit(c(0,0,0,0),"in")) +
            scale_x_continuous(expand = expansion(add = c(horizontal_offset/2,horizontal_offset/2)) ) +
            scale_y_continuous(expand = expansion(add = c(0, vertical_offset*1   ) ) )
        )
        invisible(dev.off())
      }
      show_plot(save_file_mids_incidents_png)
      #cat('</details>')
    }


    #UCDP-GED Incidents
    save_file_ucdp_p <- paste0("/mnt/8tb_a/rwd_github_private/ICBEdataset/paper/figures/metro_plots/p_ucdp_metro_plot_",crisis,".Rds")
    save_file_ucdp_png <- paste0("/mnt/8tb_a/rwd_github_private/icbe/docs/metro_plots/p_ucdp_metro_plot_",crisis,".svg")
    if(file.exists(save_file_ucdp_p)){
      cat('\n\n##### UCDP-GED Crisis Map\n\n')
      if(fromscratch){
        p_metro_plot <- readRDS(save_file_ucdp_p)
        p_width=max(length(p_metro_plot$actor_colors)*1.5,14)
        p_height=min(max((p_metro_plot$data$y %>% unique() %>% length()) /2,6),60) #cap it at 60 inches long
        vertical_offset= (p_metro_plot$data$y %>% range() %>% abs() %>% diff() %>% abs())/(p_metro_plot$data$y %>% unique() %>% length())   #offsets are kind of complicated bc of the graph
        horizontal_offset= (p_metro_plot$data$x %>% range() %>% abs() %>% diff() %>% abs())/length(p_metro_plot$actor_colors)
        svglite(save_file_ucdp_png, #because this is a multiple and not a discrete amount it ends up adding too much padding to very long
                width =p_width , height =p_height
                #dpi = 1200
        )
        plot(
          p_metro_plot +
            theme(plot.margin=unit(c(0,0,0,0),"in")) +
            scale_x_continuous(expand = expansion(add = c(horizontal_offset/2,horizontal_offset/2)) ) +
            scale_y_continuous(expand = expansion(add = c(0, vertical_offset*1   ) ) )
        )
        invisible(dev.off())
      }
      show_plot(save_file_ucdp_png)
      #cat('</details>')
    }

    #skip wikipedia temporarily
    #wikipedia_links <- conflict_data_long_singlecode_icb %>% filter(crisno==crisis) %>% pull(wiki_en_link)
    #if(length(wikipedia_links)>0 ){
    #  cat('\n\n#### Wikipedia Page \n\n')
    #  cat(glue('<iframe src="',{wikipedia_links[1]},'" loading="lazy" height="800" width="100%" style="border: 1px solid #464646;" allowfullscreen="" allow="autoplay" data-external="1"></iframe>'))
    #}

    #cat("\n\n### {-}\n\n")
    #cat("\n\n## {-}\n\n")


  } else{
    cat('\nCase Missing\n')
    cat(' \n \n')
  }

}
