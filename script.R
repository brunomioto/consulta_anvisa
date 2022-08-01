
# packages ----------------------------------------------------------------

library(httr)
library(dplyr)

# api ---------------------------------------------------------------------

#maquira 05823205000190
#ingamed 04037992000344
#pfizer  46070868003699
#3m      45985371000108


consulta_anvisa <- function(cnpj){

# Parâmetros
query <- list(
  "count" = 200,
  "filter[cnpj]" = cnpj,
  "page" = 1
)

# Fazer requisição
resp <- httr::GET(
  "https://consultas.anvisa.gov.br/api/consulta/saude",
  query = query,
  httr::add_headers("Authorization" = "Guest")
)

resultado <- httr::content(resp)

df <- data.frame(matrix(unlist(resultado$content), nrow=length(resultado$content), byrow=TRUE)) %>% 
  dplyr::rename("processo" = 1,
                "cnpj" = 2,
                "razao_social" = 3,
                "produto" = 4,
                "negativo" = 5,
                "registro" = 6,
                "situacao" = 7)


return(df)
}

empresas <- c("05823205000190", #maquira
              "04037992000344" #ingamed
              )

purrr::map_df(.x = empresas,.f =  consulta_anvisa) %>% 
  View()



# test cnpj ---------------------------------------------------------------


#maquira 05823205000190
#ingamed 04037992000344
#pfizer  46070868003699
#3m      45985371000108


# Parâmetros
query <- list(
  "count" = 500,
  "filter[cnpj]" = "05823205000190",
  "page" = 1
)

# Fazer requisição
resp <- httr::GET(
  "https://consultas.anvisa.gov.br/api/consulta/saude",
  query = query,
  httr::add_headers("Authorization" = "Guest")
)

resultado <- httr::content(resp)


df <- data.frame(matrix(unlist(resultado$content), nrow=length(resultado$content), byrow=TRUE))

df %>% 
  View()
               