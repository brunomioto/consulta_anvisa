
<!-- README.md is generated from README.Rmd. Please edit that file -->

# consulta_anvisa

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Este projeto realiza consultas às APIs da ANVISA para obter e processar
dados relacionados a materiais e empresas, exportando os resultados em
formatos CSV e XLSX.

O script principal (script.R) executa as seguintes etapas:

- Consulta de Materiais: Acessa a API da ANVISA para obter uma lista de
  materiais, incluindo informações como ordem de análise, data de
  entrada, número do expediente, código e descrição do assunto, número
  do processo, data de atualização e textos relacionados ao processo e
  expediente.

- Consulta de Empresas: Acessa outra API da ANVISA para obter uma lista
  de empresas, incluindo CNPJ, razão social, número do processo, número
  do expediente, número e texto do assunto, e situação.

- Processamento de Dados: Os dados de materiais e empresas são
  combinados com base nos números de expediente e processo. São
  calculados o tempo de análise em dias e outras métricas relevantes.

- Exportação de Dados: Os resultados são exportados para o diretório
  data/ nos formatos CSV e XLSX, com nomes de arquivos que indicam a
  data da última atualização.
