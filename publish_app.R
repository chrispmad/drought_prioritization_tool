# Check to see if a previous attempt to automatically
# publish this app has failed; if so, don't attempt any of the stuff below,
# as that will very likely fail again.

setwd(here::here())

print("Beginning republishing")

if(!dir.exists('publishing_results')){
  dir.create('publishing_results')
}

if(!file.exists(paste0('publishing_results/publishing_results_',Sys.Date(),'_errored.csv'))){
  
  publishing_results = data.frame(
    publish_succeeded = F,
    error_at = 'None',
    error = FALSE
  )
  
  file.remove('app/www/ref_docs/EFN_Hydrology_Paper_New.docx')
  file.remove('app/www/ref_docs/Figure Captions Hydrology_Main.docx')
  file.remove('app/www/ref_docs/Hydrology_Sup_Figures.docx')
  
  print('Removed old version of 3 word docs.')
  
  # Update the EFN Hydrology Paper document
  tryCatch(
    file.copy(
      from = "J:/2 SCIENCE - Environmental Flow Needs/EFN_Hydrology_Paper_New/EFN_Hydrology_Paper_New.docx",
      to = 'app/www/ref_docs/EFN_Hydrology_Paper_New.docx'
    ),
    error = function(e) {
      publishing_results$error_at = 'copying_EFN_Hydrology_Paper_New.docx'
      publishing_results$error = TRUE
    }
  )
  print('Copied new version of EFN Hydrology Paper into app folder.')
  
  # Update the figure caption word document
  tryCatch(
    file.copy(
      from = "J:/2 SCIENCE - Environmental Flow Needs/EFN_Hydrology_Paper_New/Figure Captions Hydrology_Main^.docx",
      to = 'app/www/ref_docs/Figure Captions Hydrology_Main.docx'
    ),
    error = function(e) {
      publishing_results$error_at = 'copying_figure_captions'
      publishing_results$error = TRUE
    }
  )
  print('Copied new version of figure captions into app folder.')
  
  # Update the EFN Hydrology Paper document
  tryCatch(
    file.copy(
      from = "J:/2 SCIENCE - Environmental Flow Needs/EFN_Hydrology_Paper_New/Hydrology_Sup_Figures.docx",
      to = 'app/www/ref_docs/Hydrology_Sup_Figures.docx'
    ),
    error = function(e) {
      publishing_results$error_at = 'copying_Hydrology_Sup_Figures.docx'
      publishing_results$error = TRUE
    }
  )
  print('Copied new version of EFN Hydrology Supplmental Figures into app folder.')
  
  if(!publishing_results$error){
    # Publish app to Shinyapps.io
    tryCatch(
      rsconnect::deployApp(
        appDir = 'app/',
        appTitle = 'PtolemyTool',
        account = 'bcgov-env'
      ),
      error = function(e) {
        publishing_results$error_at = 'publishing'
        publishing_results$error = TRUE
      }
    )
  }
  
  if(publishing_results$error){
    write.csv(publishing_results,
              paste0('publishing_results/publishing_results_',Sys.Date(),'_errored.csv'))
  } else {
    write.csv(publishing_results,
              paste0('publishing_results/publishing_results_',Sys.Date(),'.csv'))
  }
}

print(paste0('Republishing of app complete at ', Sys.time()))
