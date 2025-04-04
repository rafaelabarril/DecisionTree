% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hello.R
\name{classificar}
\alias{classificar}
\title{Classificar}
\usage{
classificar(
  dados,
  coluna_resposta,
  novas_informacoes,
  g_info_min = 0.8,
  profundidade_max = 3
)
}
\arguments{
\item{dados}{Banco de dados.}

\item{coluna_resposta}{Coluna resposta do banco do tipo "categórica".}

\item{novas_informacoes}{Banco de dados com informações para serem classificadas.}

\item{g_info_min}{Valor mínimo para o ganho de informação.}

\item{profundidade_max}{Profundidade máxima dos ramos.}
}
\value{
Categoria predita.
}
\description{
Classificar
}
\examples{
classificar(iris[-c(1,25,56,78,115),], "Species", iris[c(1,25,56,78,115),], g_info_min = 0.5, profundidade_max = 3)


}
