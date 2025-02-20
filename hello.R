#' @title Organizar Banco
#' @param banco Banco de Dados.
#' @param coluna_resposta Coluna resposta do banco do tipo "categórica".
#' @return Banco de Dados com a coluna resposta sendo a primeira coluna do banco.
#'
#'
organizar_banco <- function(banco, coluna_resposta){
  return(banco[, c(coluna_resposta, setdiff(names(banco), coluna_resposta))])
}

#' @title Entropia
#' @param coluna_resposta Coluna resposta do banco do tipo "categórica".
#' @return Lista com o ganho de informação na coluna resposta e as probabilidades de cada categoria (p-chapéu).
#'
#' @export
entropia <- function(coluna_resposta) {
  if (length(coluna_resposta) == 0) return(0.0)
  p_chapeu <- table(coluna_resposta) / length(coluna_resposta)
  ent <- -sum(ifelse(p_chapeu == 0, 0, p_chapeu * log2(p_chapeu)))
  return(list(entropia = ent, probabilidades = as.numeric(p_chapeu)))
}

#' @title Ganho de Informação
#' @param corte Valor em que será realizado o corte.
#' @param coluna_resposta Coluna resposta do banco do tipo "categórica".
#' @param coluna_corte Coluna do banco em que será realizado o corte.
#' @return Ganho de informação obtido.
#'
#' @export
ganho_inf <- function(corte, coluna_resposta, coluna_corte){
  e1 <- entropia(coluna_resposta[coluna_corte <= corte])[[1]]
  e2 <- entropia(coluna_resposta[coluna_corte > corte])[[1]]
  entropia(coluna_resposta)[[1]] - (e1*mean(coluna_corte <= corte) + e2*mean(coluna_corte > corte))
}

#' @title Otimizar
#' @param coluna_resposta Coluna resposta do banco do tipo "categórica".
#' @param coluna_corte Coluna do banco em que será realizado o corte.
#' @return Lista com o ganho de informação obtido, valor do corte e probabilidades.
#'
otimizar <- function(coluna_resposta, coluna_corte){
  candidato <- unique(coluna_corte)
  info <- 0
  corte <- 0
  indice <- 0
  for (i in 1:length(candidato)){
    valor <- ganho_inf(candidato[i], coluna_resposta, coluna_corte)
    if (valor > info){
      indice <- i
      info <- valor
      corte <- candidato[i]
    }
  }
  p_chapeu1 <- table(coluna_resposta[coluna_corte <= corte])/sum(coluna_corte <= corte)
  p_chapeu2 <- table(coluna_resposta[coluna_corte > corte])/sum(coluna_corte > corte)
  return(list(ganho_informacao = info, corte = corte, p_menor = p_chapeu1, p_maior = p_chapeu2))#, which(cut_column < corte), which(cut_column >= corte)))
}

#' @title Avaliar
#' @param dados Banco de dados.
#' @return Lista com ganho de informação, valor do corte, coluna do corte e probabilidades.
#'
avaliar <- function(dados){
  info <- 0
  corte <- 0
  p_chapeu1 <- 0
  p_chapeu2 <- 0
  nome_coluna <- ""
  if (nrow(dados) <= 1){
    return(NULL)
  }
  for (i in 2:ncol(dados)){
    otim_coluna <- otimizar(dados[,1], dados[,i])
    if (otim_coluna[[1]] > info){
      info <- otim_coluna[[1]]
      corte <- otim_coluna[[2]]
      p_chapeu1 <- otim_coluna[[3]]
      p_chapeu2 <- otim_coluna[[4]]
      nome_coluna <- names(dados)[i]
    }
  }
  resultado <- list(info, corte, nome_coluna, p_chapeu1, p_chapeu2)
  return(resultado)
}

#' @title Subtabelas
#' @param dados Banco de dados.
#' @return Lista com as duas tabelas obtidas com o corte, nome da coluna em que foi feito o corte e valor do corte.
#'
subtabela <- function(dados){
  avaliar_resultado <- avaliar(dados)
  nome_coluna <- avaliar_resultado[[3]]
  corte <- avaliar_resultado[[2]]
  sub1 <- subset(dados, dados[,nome_coluna] <= corte)
  sub2 <- subset(dados, dados[,nome_coluna] > corte)
  return(list(sub1, sub2, nome_coluna, corte))}

#' @title Constroi filho
#' @param dados Banco de dados.
#' @param coluna_resposta Coluna resposta do banco do tipo "categórica".
#' @param g_info_min Valor mínimo para o ganho de informação.
#' @param profundidade_max Profundidade máxima dos ramos.
#'
#' @export
constroi_filho <- function(dados, coluna_resposta, g_info_min = 0.5, profundidade_max = 3, contador = 0){
  if (!(coluna_resposta %in% colnames(dados))){
    stop ("Erro: Coluna resposta não encontrada.")
  }
  dados <- organizar_banco(dados, coluna_resposta)
  dado <- avaliar(dados)
  if (is.null(dado)){
    return(NULL)
  }
  if(dado[[1]] == 0){
    lista = list(p_chapeu = table(dados[,1]) / length(dados[,1]))
  }
  else if(dado[[1]] < g_info_min){
    lista <- list("corte" = list("valor" = dado[[2]], "variavel" = dado[[3]]), "menor" = list("p_chapeu" = as.numeric(dado[[4]])), "maior" = list("p_chapeu" = as.numeric(dado[[5]])))
  }
  else if(contador > profundidade_max){
    lista <- list("corte" = list("valor" = dado[[2]], "variavel" = dado[[3]]), "menor" = list("p_chapeu" = as.numeric(dado[[4]])), "maior" = list("p_chapeu" = as.numeric(dado[[5]])))
    return(lista)
  } else {
    subtabela_fora <- subtabela(dados)
    lista <- list("corte" = list( "valor" = subtabela_fora[[4]], "variavel" = subtabela_fora[[3]]),
                  "menor" = constroi_filho(subtabela_fora[[1]], coluna_resposta, g_info_min, profundidade_max, contador + 1),
                  "maior" = constroi_filho(subtabela_fora[[2]], coluna_resposta, g_info_min, profundidade_max, contador + 1))
  }
  return(lista)
}

#' @title Classificar
#' @param dados Banco de dados.
#' @param coluna_resposta Coluna resposta do banco do tipo "categórica".
#' @param novas_informacoes Banco de dados com informações para serem classificadas.
#' @param g_info_min Valor mínimo para o ganho de informação.
#' @param profundidade_max Profundidade máxima dos ramos.
#' @return Categoria predita.
#' @examples
#' classificar(iris[-c(1,25,56,78,115),], "Species", iris[c(1,25,56,78,115),], g_info_min = 0.5, profundidade_max = 3)
#'
#'
#' @export
classificar <- function(dados, coluna_resposta, novas_informacoes, g_info_min = 0.8, profundidade_max = 3) {
  arvore <- constroi_filho(dados, coluna_resposta)
  niveis_resposta <- levels(factor(dados[, coluna_resposta]))
  p_chapeu <- matrix(0, nrow = nrow(novas_informacoes), ncol = length(niveis_resposta))
  colnames(p_chapeu) <- niveis_resposta
  for (i in 1:nrow(novas_informacoes)) {
    novo_dado <- novas_informacoes[i, , drop = FALSE]
    no <- arvore
    while (is.null(no$p_chapeu)) {
      if (novo_dado[, no$corte[["variavel"]]] > no$corte[["valor"]]) {
        no <- no$maior
      } else {
        no <- no$menor
      }
    }
    p_chapeu[i, ] <- no$p_chapeu
  }
  predicoes <- apply(p_chapeu, 1, function(x) colnames(p_chapeu)[which.max(x)])

  return(predicoes)
}

#' @title Hold-Out
#' @param dados Banco de dados.
#' @param coluna_resposta Coluna resposta do banco do tipo "categórica".
#' @param n_linhas_treino Número de linhas do banco para serem usadas como treino.
#' @param g_info_min Valor mínimo para o ganho de informação.
#' @param profundidade_max Profundidade máxima dos ramos.
#' @return Lista com matriz confusão, acuracia, recall e precisão.
#' @examples
#'  avaliar_arvore_Hold_Out(iris, "Species", 50, g_info_min = 0.05, profundidade_max = 3)
#'
#' @export

avaliar_arvore_Hold_Out <- function(data, coluna_resposta, n_linhas_treino, g_info_min = 0.8, profundidade_max = 3) {
  data <- organizar_banco(data, coluna_resposta)
  n_linhas <- n_linhas_treino
  amostra <- sample(1:nrow(data), n_linhas)
  dadinhos_treino <- data[amostra, ]
  dadinhos_teste <- data[-amostra, ]
  arvore <- constroi_filho(dadinhos_treino, coluna_resposta, g_info_min, profundidade_max)
  novas_informacoes <- dadinhos_teste[,-1]
  p_chapeu <- matrix(0, nrow = nrow(novas_informacoes), ncol = length(unique(data[, coluna_resposta])))
  colnames(p_chapeu) <- levels(factor(data[, coluna_resposta]))
  for (i in 1:nrow(novas_informacoes)) {
    novo_dado <- novas_informacoes[i, , drop = FALSE]
    no <- arvore
    while (is.null(no$p_chapeu)) {
      if (novo_dado[, no$corte[["variavel"]]] > no$corte[["valor"]]) {
        no <- no$maior
      } else {
        no <- no$menor
      }
    }
    p_chapeu[i, ] <- no$p_chapeu
  }
  predicoes <- apply(p_chapeu, 1, function(x) colnames(p_chapeu)[which.max(x)])
  print("predito")
  print(predicoes)
  verdadeiro <- dadinhos_teste[, 1]
  matriz_confusao <- table(predito = predicoes, verdadeiro = verdadeiro)
  acuracia <- sum(diag(matriz_confusao)) / sum(matriz_confusao)

  recall <- sapply(levels(factor(data[, coluna_resposta])), function(classe){
    matriz_confusao[classe, classe] / sum(matriz_confusao[, classe])})

  precisao <- sapply(seq_along(levels(factor(data[, coluna_resposta]))), function(classe) {
    matriz_confusao[classe, classe] / sum(matriz_confusao[classe, ])
  })

  return(list(matriz_confusao = matriz_confusao, acuracia = acuracia, recall = recall, precisao = precisao
  ))
}

#' @title K-Fold
#' @param dados Banco de dados.
#' @param coluna_resposta Coluna resposta do banco do tipo "categórica".
#' @param g_info_min Valor mínimo para o ganho de informação.
#' @param profundidade_max Profundidade máxima dos ramos.
#' @return Lista com acuracia, recall e precisão.
#'
#' @examples
#'  avaliar_arvore_K_fold(iris, "Species", g_info_min = 0.05)
#'
#' @export

avaliar_arvore_K_fold <- function(data, coluna_resposta, g_info_min = 0.025,
                                   profundidade_max = 3, valor_p_chapeu = 0.5) {
  data <- organizar_banco(data, coluna_resposta)
  data[,coluna_resposta] <- factor(data[,coluna_resposta])
  amostra <- sample(seq_len(nrow(data))%%10 + 1, replace = FALSE)
  result_acuracia <- c()
  result_recall <- c()
  result_precisao <- c()

  niveis_resposta <- levels(factor(data[, coluna_resposta]))

  for (j in 1:10) {
    dadinhos1 <- data[amostra != j, ]  # treino
    dadinhos2 <- data[amostra == j, ] # teste

    arvore <- constroi_filho(dadinhos1, coluna_resposta)

    novas_informacoes <- dadinhos2[, -1, drop = FALSE]
    p_chapeu <- matrix(0, nrow = nrow(novas_informacoes), ncol = length(niveis_resposta))
    colnames(p_chapeu) <- niveis_resposta

    for (i in 1:nrow(novas_informacoes)) {
      novo_dado <- novas_informacoes[i, , drop = FALSE]
      no <- arvore
      while (is.null(no$p_chapeu)) {
        if (novo_dado[, no$corte[["variavel"]]] > no$corte[["valor"]]) {
          no <- no$maior
        } else {
          no <- no$menor
        }
      }
      p_chapeu[i, ] <- no$p_chapeu
    }

    predicoes <- apply(p_chapeu, 1, function(x) colnames(p_chapeu)[which.max(x)])
    verdadeiro <- factor(dadinhos2[, coluna_resposta], levels = niveis_resposta)
    matriz_confusao <- table(factor(predicoes, levels = niveis_resposta), verdadeiro)

    acuracia <- sum(diag(matriz_confusao)) / sum(matriz_confusao)

    recall <- sapply(niveis_resposta, function(classe) {
      matriz_confusao[classe, classe] / sum(matriz_confusao[, classe])

    })

    precisao <- sapply(niveis_resposta, function(classe) {
      matriz_confusao[classe, classe] / sum(matriz_confusao[classe, ])
    })

    result_acuracia <- append(result_acuracia, acuracia)
    result_recall <- append(result_recall, mean(recall, na.rm = TRUE))
    result_precisao <- append(result_precisao, mean(precisao, na.rm = TRUE))
  }

  return(list(
    acuracia = mean(result_acuracia),
    recall = mean(result_recall),
    precisao = mean(result_precisao)
  ))
}
