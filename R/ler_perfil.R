# funcao ler pdf perfil modificado ----------------------------------------

ler_perfil <- function(
   pasta,
   var = c('Ra', 'Rq', 'Rz', 'Rda', 'Rmr', 'Rsk', 'Rku'),
   und = c('µm', 'µm', 'µm', '°', '%', '', '')
) {
   id <-
      pasta %>%
      stringr::str_split('/') %>%
      '[['(1) %>%
      tail(1) %>%
      stringr::str_replace('Perfil modificado', '') %>%
      stringr::str_trim('both')
   arquivos <- list.files(pasta)
   dat <- matrix(
      numeric(),
      nrow = length(arquivos),
      ncol = length(var),
      dimnames = list(NULL, var)
   )
   for (i in 1:length(arquivos)) {
      txt <-
         file.path(pasta, arquivos[i]) %>%
         pdftools::pdf_text() %>%
         stringr::str_replace_all(',', '.') %>%
         stringr::str_split('\n') %>%
         `[[`(1)
      for (j in 1:length(var)) {
         loc <-
            txt %>%
            stringr::str_detect(paste0('\\b', var[j],'\\b'))
         pos_var <- stringr::str_locate(txt[loc], var[j])
         if (und[j] == '') {
            pos_und <- matrix(min(35, nchar(txt[loc]) + 1), 1, 2)
         } else {
            pos_und <- stringr::str_locate(txt[loc], und[j])
         }
         dat[i, j] <-
            txt[loc] %>%
            stringr::str_sub(
               start = pos_var[2] + 1,
               end = pos_und[1] - 1
            ) %>%
            stringr::str_replace_all('[^0-9.]', '') %>%
            stringr::str_trim('both') %>%
            as.numeric
         txt[loc] <-
            paste0(
               txt[loc] %>% stringr::str_sub(1, pos_var[1]  - 1),
               txt[loc] %>% stringr::str_sub(pos_und[2] + 1)
            )
      }
   }
   media <- apply(dat, 2, mean, na.rm = T)

   return(data.frame(cp = id, t(media)) %>% tibble::as_tibble())
}
