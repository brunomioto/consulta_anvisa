library(dplyr)
library(httr2)
library(lubridate)
library(glue)
library(readr)
library(writexl)

url <- "https://consultas.anvisa.gov.br/api/fila/?filter%5Barea%5D=8&filter%5Bfila%5D=285&filter%5Bsubfila%5D=198"
# 
req <- request(url)

req2 <- req |> 
  req_headers(
    Accept = "application/json, text/plain, */*",
    `Accept-Encoding` = "gzip, deflate, br, zstd",
    `Accept-Language` = "en-US,en;q=0.9,pt-BR;q=0.8,pt;q=0.7",
    Authorization = "Guest",
    `Cache-Control` = "no-cache",
    Connection = "keep-alive",
    Cookie = "_TRAEFIK_BACKEND=http://10.0.7.88:8080; _cfuvid=KxVSK12YyGF0zspcEUzy6I2kWImCuhIuQOt54SZYGRc-1735826032757-0.0.1.1-604800000",
    Host = "consultas.anvisa.gov.br",
    `If-Modified-Since` = "Mon, 26 Jul 1997 05:00:00 GMT",
    Pragma = "no-cache",
    Referer = "https://consultas.anvisa.gov.br/",
    `Sec-Fetch-Dest` = "empty",
    `Sec-Fetch-Mode` = "cors",
    `Sec-Fetch-Site` = "same-origin",
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36 OPR/114.0.0.0",
    `sec-ch-ua` = "\"Chromium\";v=\"128\", \"Not;A=Brand\";v=\"24\", \"Opera GX\";v=\"114\"",
    `sec-ch-ua-mobile` = "?0",
    `sec-ch-ua-platform` = "\"Windows\""
  )

resp <- req2 |> 
  req_perform() |> 
  resp_body_json()


lista_materiais <- data.frame(matrix(unlist(resp), nrow=length(resp), byrow=TRUE)) |> 
  tibble() |> 
  rename(
    "ordem_analise" = 1,
    "data_entrada" = 2,
    "expediente_numero" = 3,
    "codigo_assunto" = 4,
    "descricao_assunto" = 5,
    "processo_numero" = 6,
    "data_atualizacao" = 7,
    "processo_texto" = 8,
    "expediente_texto" = 9
  ) |> 
  mutate(data_entrada = ymd_hms(data_entrada),
         data_atualizacao = ymd_hms(data_atualizacao),
         ordem_analise = as.numeric(ordem_analise))



# |> 
  # select(
  #   ordem_analise, data_entrada,
  #   processo_texto, expediente_texto, 
  #   codigo_assunto, descricao_assunto,
  #   data_atualizacao
  # )





# empresas ----------------------------------------------------------------


url_empresas <- "https://consultas.anvisa.gov.br/api/documento/tecnico?column=&count=20000&filter%5Bassunto%5D=80256&order=asc&page=1"

req_empresas <- request(url_empresas)

req_empresas2 <- req_empresas |> 
  req_headers(
    Accept = "application/json, text/plain, */*",
    `Accept-Encoding` = "gzip, deflate, br, zstd",
    `Accept-Language` = "en-US,en;q=0.9,pt-BR;q=0.8,pt;q=0.7",
    Authorization = "Guest",
    `Cache-Control` = "no-cache",
    Connection = "keep-alive",
    Cookie = "_TRAEFIK_BACKEND=http://10.0.7.88:8080; _cfuvid=KxVSK12YyGF0zspcEUzy6I2kWImCuhIuQOt54SZYGRc-1735826032757-0.0.1.1-604800000",
    Host = "consultas.anvisa.gov.br",
    `If-Modified-Since` = "Mon, 26 Jul 1997 05:00:00 GMT",
    Pragma = "no-cache",
    Referer = "https://consultas.anvisa.gov.br/",
    `Sec-Fetch-Dest` = "empty",
    `Sec-Fetch-Mode` = "cors",
    `Sec-Fetch-Site` = "same-origin",
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36 OPR/114.0.0.0",
    `sec-ch-ua` = "\"Chromium\";v=\"128\", \"Not;A=Brand\";v=\"24\", \"Opera GX\";v=\"114\"",
    `sec-ch-ua-mobile` = "?0",
    `sec-ch-ua-platform` = "\"Windows\""
  )


resp_empresas <- req_empresas2 |> 
  req_perform() |> 
  resp_body_json()



lista_empresas <- data.frame(matrix(unlist(resp_empresas$content),
                                    nrow=length(resp_empresas$content),
                                    byrow=TRUE)) |> 
  tibble() |> 
  # mutate(ordem = row_number()) |> 
  rename(cnpj_numero = 1,
         razao_social = 2,
         processo_numero = 3,
         expediente_numero = 5,
         assunto_numero = 6,
         assunto_texto = 7,
         situacao = 8)  |> 
  select(-c(4,9))#|> 
  # dplyr::relocate(ordem)



# junta tudo --------------------------------------------------------------

lista_geral <- lista_materiais |> 
  left_join(lista_empresas,
            by = c("expediente_numero", "processo_numero")) |> 
  mutate(dias_analise = lubridate::interval(data_entrada, data_atualizacao)%/% days(1),
         expediente_numero = as.numeric(expediente_numero),
         processo_numero = as.numeric(processo_numero))

readr::write_csv(lista_geral, glue::glue("data/lista_geral_ultima.csv"))

ultima_data <- lista_geral |> 
  # select(data_atualizacao) |>  |> 
  distinct(data_atualizacao) |> 
  arrange(desc(data_atualizacao)) |> 
  slice_head(n=1) |> 
  pull()
ultima_data_format <- format(lubridate::ymd_hms(ultima_data), "%Y_%m_%d")
writexl::write_xlsx(lista_geral, glue::glue("data/lista_geral_{ultima_data_format}.xlsx"))
readr::write_csv(lista_geral, glue::glue("data/lista_geral_{ultima_data_format}.csv"))

data_anterior_format <- format(lubridate::ymd_hms(ultima_data)-days(1), "%Y_%m_%d")


data_anterior <- readr::read_csv(glue::glue("https://raw.githubusercontent.com/brunomioto/consulta_anvisa/refs/heads/master/data/lista_geral_ultima.csv?token=GHSAT0AAAAAACOVZFFBWUXEWT6S5QTQNTLGZ3W4JZA"))

# data_anterior <- readr::read_csv(glue::glue("data/lista_geral_{data_anterior_format}.csv"))


dataset_anterior <- data_anterior |> 
  select(ordem_analise_anterior = ordem_analise,
         data_entrada_anterior = data_entrada,
         data_atualizacao_anterior = data_atualizacao,
         expediente_numero,
         processo_numero,
         razao_social) |> 
  mutate(expediente_numero = as.numeric(expediente_numero),
         processo_numero = as.numeric(processo_numero))
  
lista_geral_diff <- lista_geral |> 
  left_join(dataset_anterior) |> 
  mutate(diff_ordem = ordem_analise - ordem_analise_anterior)

writexl::write_xlsx(lista_geral_diff, glue::glue("data/lista_geral_diff.xlsx"))
readr::write_csv(lista_geral_diff, glue::glue("data/lista_geral_diff.csv"))




# maquira <- bbb |> 
#   filter(cnpj_numero == "05823205000190")


# oi <- bbb


# library(ggplot2)
# oi |> 
#   ggplot(aes(x = ordem_analise, y = dias_analise))+
#   geom_point()
# 
# 
# 
# oi |> 
#   mutate(hora = lubridate::hour(data_entrada)) |> 
#   View()
#   ggplot(aes(x = hora))+
#   geom_histogram(binwidth = 1)
  # select(data_entrada, hora)
