# ============================================================
# 01_build_bases.R
# ConstruC'C#o das bases:
# 1. escolas_ativas
# 2. ideb_painel
# 3. censo_informatizacao_painel
# 4. painel_ideb_informatizacao
# ============================================================

# ------------------------------------------------------------
# 0. Pacotes
# ------------------------------------------------------------

library(data.table)
library(janitor)
library(glue)
library(basedosdados)

data.table::setDTthreads(
  max(1L, parallel::detectCores(logical = TRUE) - 1L)
)

# ------------------------------------------------------------
# 1. ConfiguraC'C5es gerais
# ------------------------------------------------------------

dir_dados <- "C:\\Users\\Felipe\\Downloads\\aps\\escolas_ativas"

dir_output <- "C:\\Users\\Felipe\\OneDrive\\Documentos\\GitHub\\projeto-aps\\outputs"

if (!dir.exists(dir_output)) {
  dir.create(dir_output, recursive = TRUE)
}

billing_project_id <- "trabalho-aps-500113"

uf_foco <- "SP"

ano_inicio <- 2015L
ano_fim <- 2025L

anos_ideb_ciclo <- seq(2015L, 2025L, by = 2L)

variavel_tratamento_did <- "trat_informatizacao_pedagogica"

arquivos_endereco <- list.files( 
  path = dir_dados,
  pattern = "^ENDERECO_ESCOLAS.*\\.csv$",
  full.names = TRUE
) |> sort()

arquivo_inse <- list.files(
  path = dir_dados,
  pattern = "^INSE_2019_ESCOLAS.*\\.csv$",
  full.names = TRUE
) |> sort()

arquivos_cmsp <- list.files(
  path = dir_dados,
  pattern = "^TB_CMSP.*\\.csv$",
  full.names = TRUE
) |> sort()

if (length(arquivos_endereco) == 0) {
  stop("Nenhum arquivo ENDERECO_ESCOLAS foi encontrado.")
}

# ------------------------------------------------------------
# 2. FunC'C5es auxiliares
# ------------------------------------------------------------

limpar_nomes <- function(nomes) {
  nomes <- enc2utf8(nomes)
  nomes <- gsub("^\\ufeff", "", nomes)
  nomes <- gsub("^C/B;B?", "", nomes)
  janitor::make_clean_names(nomes)
}

mapear_colunas <- function(arquivo) {
  cabecalho <- data.table::fread(
    file = arquivo,
    nrows = 0,
    sep = "auto",
    encoding = "Latin-1",
    showProgress = FALSE
  )
  
  data.table(
    original = names(cabecalho),
    clean = limpar_nomes(names(cabecalho))
  )
}

ler_csv_dt <- function(arquivo, select_clean = NULL) {
  
  mapa <- mapear_colunas(arquivo)
  
  if (!is.null(select_clean)) {
    cols_para_ler <- mapa[clean %in% select_clean, original]
    
    if (length(cols_para_ler) == 0) {
      stop(paste0(
        "Nenhuma coluna esperada foi encontrada em: ",
        basename(arquivo)
      ))
    }
    
    dt <- data.table::fread(
      file = arquivo,
      sep = "auto",
      encoding = "Latin-1",
      colClasses = "character",
      select = cols_para_ler,
      na.strings = c("", "NA", "NaN", "NULL"),
      showProgress = FALSE
    )
    
  } else {
    
    dt <- data.table::fread(
      file = arquivo,
      sep = "auto",
      encoding = "Latin-1",
      colClasses = "character",
      na.strings = c("", "NA", "NaN", "NULL"),
      showProgress = FALSE
    )
  }
  
  data.table::setnames(dt, limpar_nomes(names(dt)))
  dt
}

pegar_coluna <- function(dt, nomes) {
  nomes_existentes <- nomes[nomes %in% names(dt)]
  
  if (length(nomes_existentes) == 0) {
    return(rep(NA_character_, nrow(dt)))
  }
  
  lista <- lapply(nomes_existentes, function(nm) as.character(dt[[nm]]))
  
  if (length(lista) == 1) {
    lista[[1]]
  } else {
    do.call(data.table::fcoalesce, lista)
  }
}

txt <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("\\s+", " ", trimws(x))
  x[x == ""] <- NA_character_
  x
}

normalizar_codigo <- function(x) {
  x <- txt(x)
  x <- gsub("\\.0$", "", x)
  x <- gsub("[^0-9]", "", x)
  x <- sub("^0+", "", x)
  x[x == ""] <- NA_character_
  x
}

num_br <- function(x) {
  x <- txt(x)
  x <- gsub(",", ".", x)
  x <- gsub("[^0-9\\.\\-]", "", x)
  suppressWarnings(as.numeric(x))
}

num_bd <- function(x) {
  x <- as.character(x)
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}

binario_bd <- function(x) {
  x_chr <- toupper(trimws(as.character(x)))
  
  saida <- fifelse(
    x_chr %in% c("1", "SIM", "S", "TRUE", "T"),
    1L,
    fifelse(
      x_chr %in% c("0", "NAO", "NCO", "N", "FALSE", "F"),
      0L,
      NA_integer_
    )
  )
  
  x_num <- suppressWarnings(as.numeric(gsub(",", ".", x_chr)))
  
  saida <- fifelse(
    is.na(saida) & !is.na(x_num) & x_num > 0,
    1L,
    saida
  )
  
  saida <- fifelse(
    is.na(saida) & !is.na(x_num) & x_num == 0,
    0L,
    saida
  )
  
  fifelse(is.na(saida), 0L, saida)
}

data_br <- function(x) {
  x <- txt(x)
  
  saida <- data.table::as.IDate(rep(NA_character_, length(x)))
  
  formatos <- c(
    "%Y-%m-%d",
    "%d/%m/%Y",
    "%d-%m-%Y",
    "%Y%m%d"
  )
  
  for (fmt in formatos) {
    idx <- is.na(saida) & !is.na(x)
    
    if (any(idx)) {
      saida[idx] <- suppressWarnings(
        data.table::as.IDate(x[idx], format = fmt)
      )
    }
  }
  
  saida
}

extrair_ano <- function(arquivo) {
  ano <- regmatches(
    basename(arquivo),
    regexpr("20\\d{2}", basename(arquivo))
  )
  
  if (length(ano) == 0) {
    NA_integer_
  } else {
    as.integer(ano)
  }
}

primeira_data <- function(x) {
  if (all(is.na(x))) {
    return(data.table::as.IDate(NA_character_))
  }
  
  min(x, na.rm = TRUE)
}

primeiro_ano_ideb_compativel <- function(ano_real, anos_ciclo = anos_ideb_ciclo) {
  if (is.na(ano_real)) return(NA_integer_)
  
  possiveis <- anos_ciclo[anos_ciclo >= ano_real]
  
  if (length(possiveis) == 0) {
    return(NA_integer_)
  }
  
  min(possiveis)
}

montar_array_sql <- function(ids) {
  ids <- unique(ids[!is.na(ids)])
  paste0("'", ids, "'", collapse = ", ")
}

# ------------------------------------------------------------
# 3. ENDERECO_ESCOLAS: base escola-level
# ------------------------------------------------------------

padronizar_endereco <- function(arquivo) {
  
  message("Lendo endereC'o: ", basename(arquivo))
  
  cols_endereco <- c(
    "de",
    "diretoria",
    "diretoria_ensino",
    "mun",
    "municipio",
    "distr",
    "distrito",
    "cod_esc",
    "codigo_escola",
    "codigo_da_escola",
    "codescmec",
    "codigo_mec",
    "cod_mec",
    "nomesc",
    "nome_escola",
    "nome_da_escola",
    "situacao",
    "codsit",
    "situacao_funcionamento",
    "cep",
    "baiesc",
    "bairro",
    "zona",
    "localizacao",
    "ds_latitude",
    "latitude",
    "ds_longitude",
    "longitude"
  )
  
  dt <- ler_csv_dt(arquivo, select_clean = cols_endereco)
  
  out <- data.table(
    ano_endereco = extrair_ano(arquivo),
    
    codigo_escola = normalizar_codigo(
      pegar_coluna(dt, c("cod_esc", "codigo_escola", "codigo_da_escola"))
    ),
    
    codigo_escola_mec = normalizar_codigo(
      pegar_coluna(dt, c("codescmec", "codigo_mec", "cod_mec"))
    ),
    
    nome_escola = txt(
      pegar_coluna(dt, c("nomesc", "nome_escola", "nome_da_escola"))
    ),
    
    distrito = txt(
      pegar_coluna(dt, c("distr", "distrito"))
    ),
    
    bairro = txt(
      pegar_coluna(dt, c("baiesc", "bairro"))
    ),
    
    municipio = txt(
      pegar_coluna(dt, c("mun", "municipio"))
    ),
    
    diretoria = txt(
      pegar_coluna(dt, c("de", "diretoria", "diretoria_ensino"))
    ),
    
    localizacao_original = txt(
      pegar_coluna(dt, c("zona", "localizacao"))
    ),
    
    cep = txt(
      pegar_coluna(dt, c("cep"))
    ),
    
    latitude = num_br(
      pegar_coluna(dt, c("ds_latitude", "latitude"))
    ),
    
    longitude = num_br(
      pegar_coluna(dt, c("ds_longitude", "longitude"))
    ),
    
    situacao_original = txt(
      pegar_coluna(dt, c("situacao", "codsit", "situacao_funcionamento"))
    )
  )
  
  out <- out[!is.na(codigo_escola)]
  
  out[
    ,
    localizacao := fcase(
      grepl("URB", toupper(localizacao_original)) | localizacao_original == "1",
      "Urbana",
      
      grepl("RUR", toupper(localizacao_original)) | localizacao_original == "2",
      "Rural",
      
      default = localizacao_original
    )
  ]
  
  out[
    ,
    situacao_funcionamento := fcase(
      toupper(situacao_original) %in% c("ATIVA", "ATIVO", "1", "EM ATIVIDADE"),
      "Ativa",
      
      toupper(situacao_original) %in% c(
        "INATIVA", "INATIVO", "0", "EXTINTA", "EXTINTO", "PARALISADA"
      ),
      "Inativa",
      
      default = situacao_original
    )
  ]
  
  out[
    ,
    completude :=
      fifelse(!is.na(codigo_escola_mec), 1L, 0L) +
      fifelse(!is.na(nome_escola), 1L, 0L) +
      fifelse(!is.na(distrito), 1L, 0L) +
      fifelse(!is.na(bairro), 1L, 0L) +
      fifelse(!is.na(municipio), 1L, 0L) +
      fifelse(!is.na(diretoria), 1L, 0L) +
      fifelse(!is.na(localizacao), 1L, 0L) +
      fifelse(!is.na(cep), 1L, 0L) +
      fifelse(!is.na(latitude), 1L, 0L) +
      fifelse(!is.na(longitude), 1L, 0L)
  ]
  
  out[
    ,
    .(
      ano_endereco,
      codigo_escola,
      codigo_escola_mec,
      nome_escola,
      distrito,
      bairro,
      municipio,
      diretoria,
      localizacao,
      cep,
      latitude,
      longitude,
      situacao_funcionamento,
      completude
    )
  ]
}

enderecos_bruto <- data.table::rbindlist(
  lapply(arquivos_endereco, padronizar_endereco),
  use.names = TRUE,
  fill = TRUE
)

data.table::setorder(
  enderecos_bruto,
  codigo_escola,
  -ano_endereco,
  -completude
)

base_escolas <- unique(
  enderecos_bruto,
  by = "codigo_escola"
)

base_escolas <- base_escolas[
  situacao_funcionamento == "Ativa" | is.na(situacao_funcionamento)
]

base_escolas <- base_escolas[
  ,
  .(
    codigo_escola,
    codigo_escola_mec,
    nome_escola,
    distrito,
    bairro,
    municipio,
    diretoria,
    localizacao,
    cep,
    latitude,
    longitude
  )
]

# ------------------------------------------------------------
# 4. INSE
# ------------------------------------------------------------

criar_inse <- function(arquivo_inse) {
  
  if (length(arquivo_inse) == 0) {
    warning("Nenhum arquivo INSE encontrado. nivel_socioeconomico ficarC! vazio.")
    
    return(data.table(
      codigo_escola = character(),
      nivel_socioeconomico = character()
    ))
  }
  
  arquivo <- arquivo_inse[1]
  
  message("Lendo INSE: ", basename(arquivo))
  
  mapa <- mapear_colunas(arquivo)
  
  colunas_codigo <- c(
    "cod_esc",
    "codigo_escola",
    "codigo_da_escola",
    "co_entidade",
    "id_escola"
  )
  
  possiveis_colunas_inse <- c(
    "nivel_socioeconomico_dos_alunos",
    "nivel_socioeconomico",
    "nivel_socioeconomico_inse",
    "inse",
    "indicador_de_nivel_socioeconomico"
  )
  
  coluna_inse <- mapa[
    clean %in% possiveis_colunas_inse,
    clean
  ][1]
  
  if (is.na(coluna_inse)) {
    coluna_inse <- mapa[
      grepl("socioeconomico|inse", clean, ignore.case = TRUE),
      clean
    ][1]
  }
  
  if (is.na(coluna_inse)) {
    warning("NC#o foi possC-vel identificar a coluna do INSE.")
    
    return(data.table(
      codigo_escola = character(),
      nivel_socioeconomico = character()
    ))
  }
  
  dt <- ler_csv_dt(
    arquivo,
    select_clean = unique(c(colunas_codigo, coluna_inse))
  )
  
  inse <- data.table(
    codigo_escola = normalizar_codigo(
      pegar_coluna(dt, colunas_codigo)
    ),
    nivel_socioeconomico = txt(dt[[coluna_inse]])
  )
  
  inse <- inse[!is.na(codigo_escola)]
  
  inse[
    ,
    flag_inse_preenchido := fifelse(!is.na(nivel_socioeconomico), 1L, 0L)
  ]
  
  data.table::setorder(
    inse,
    codigo_escola,
    -flag_inse_preenchido
  )
  
  inse <- unique(
    inse,
    by = "codigo_escola"
  )
  
  inse[
    ,
    .(
      codigo_escola,
      nivel_socioeconomico
    )
  ]
}

inse_2019 <- criar_inse(arquivo_inse)

# ------------------------------------------------------------
# 5. CMSP escola-level
# ------------------------------------------------------------

cmsp_vazio <- function() {
  data.table(
    codigo_escola = character(),
    tratamento_cmsp = integer(),
    tratamento_cmsp_periodo = data.table::as.IDate(character()),
    tratamento_cmsp_intensidade = numeric()
  )
}

agregar_cmsp_arquivo <- function(arquivo) {
  
  message("Lendo CMSP: ", basename(arquivo))
  
  cols_cmsp <- c(
    "codigo_escola",
    "cod_escola",
    "cod_esc",
    "codigo_da_escola",
    "data",
    "tempo_de_sessao",
    "tempo_de_sessao_s",
    "tempo_sessao"
  )
  
  dt <- tryCatch(
    ler_csv_dt(arquivo, select_clean = cols_cmsp),
    error = function(e) {
      warning(
        "Arquivo CMSP ignorado: ",
        basename(arquivo),
        " | ",
        conditionMessage(e)
      )
      return(NULL)
    }
  )
  
  if (is.null(dt) || nrow(dt) == 0) {
    return(cmsp_vazio()[, tratamento_cmsp := NULL])
  }
  
  out <- data.table(
    codigo_escola = normalizar_codigo(
      pegar_coluna(dt, c("codigo_escola", "cod_escola", "cod_esc", "codigo_da_escola"))
    ),
    data_sessao = data_br(
      pegar_coluna(dt, c("data"))
    ),
    tempo_sessao = num_br(
      pegar_coluna(dt, c("tempo_de_sessao", "tempo_de_sessao_s", "tempo_sessao"))
    )
  )
  
  out <- out[!is.na(codigo_escola)]
  
  if (nrow(out) == 0) {
    return(cmsp_vazio()[, tratamento_cmsp := NULL])
  }
  
  out[
    ,
    .(
      tratamento_cmsp_periodo = primeira_data(data_sessao),
      tratamento_cmsp_intensidade = sum(tempo_sessao, na.rm = TRUE)
    ),
    by = codigo_escola
  ]
}

if (length(arquivos_cmsp) > 0) {
  
  cmsp_por_arquivo <- data.table::rbindlist(
    lapply(arquivos_cmsp, agregar_cmsp_arquivo),
    use.names = TRUE,
    fill = TRUE
  )
  
  if (nrow(cmsp_por_arquivo) > 0) {
    
    cmsp_escola <- cmsp_por_arquivo[
      ,
      .(
        tratamento_cmsp = 1L,
        tratamento_cmsp_periodo = primeira_data(tratamento_cmsp_periodo),
        tratamento_cmsp_intensidade = sum(tratamento_cmsp_intensidade, na.rm = TRUE)
      ),
      by = codigo_escola
    ]
    
  } else {
    cmsp_escola <- cmsp_vazio()
  }
  
} else {
  
  warning("Nenhum arquivo CMSP encontrado. tratamento_cmsp ficarC! igual a 0.")
  cmsp_escola <- cmsp_vazio()
}

# ------------------------------------------------------------
# 5. CMSP escola-level
# ------------------------------------------------------------

cmsp_vazio <- function() {
  data.table(
    codigo_escola = character(),
    tratamento_cmsp = integer(),
    tratamento_cmsp_periodo = data.table::as.IDate(character()),
    tratamento_cmsp_intensidade = numeric()
  )
}

agregar_cmsp_arquivo <- function(arquivo) {
  
  message("Lendo CMSP: ", basename(arquivo))
  
  cols_cmsp <- c(
    "codigo_escola",
    "cod_escola",
    "cod_esc",
    "codigo_da_escola",
    "data",
    "tempo_de_sessao",
    "tempo_de_sessao_s",
    "tempo_sessao"
  )
  
  dt <- tryCatch(
    ler_csv_dt(arquivo, select_clean = cols_cmsp),
    error = function(e) {
      warning(
        "Arquivo CMSP ignorado: ",
        basename(arquivo),
        " | ",
        conditionMessage(e)
      )
      return(NULL)
    }
  )
  
  if (is.null(dt) || nrow(dt) == 0) {
    return(cmsp_vazio()[, tratamento_cmsp := NULL])
  }
  
  out <- data.table(
    codigo_escola = normalizar_codigo(
      pegar_coluna(dt, c("codigo_escola", "cod_escola", "cod_esc", "codigo_da_escola"))
    ),
    data_sessao = data_br(
      pegar_coluna(dt, c("data"))
    ),
    tempo_sessao = num_br(
      pegar_coluna(dt, c("tempo_de_sessao", "tempo_de_sessao_s", "tempo_sessao"))
    )
  )
  
  out <- out[!is.na(codigo_escola)]
  
  if (nrow(out) == 0) {
    return(cmsp_vazio()[, tratamento_cmsp := NULL])
  }
  
  out[
    ,
    .(
      tratamento_cmsp_periodo = primeira_data(data_sessao),
      tratamento_cmsp_intensidade = sum(tempo_sessao, na.rm = TRUE)
    ),
    by = codigo_escola
  ]
}

if (length(arquivos_cmsp) > 0) {
  
  cmsp_por_arquivo <- data.table::rbindlist(
    lapply(arquivos_cmsp, agregar_cmsp_arquivo),
    use.names = TRUE,
    fill = TRUE
  )
  
  if (nrow(cmsp_por_arquivo) > 0) {
    
    cmsp_escola <- cmsp_por_arquivo[
      ,
      .(
        tratamento_cmsp = 1L,
        tratamento_cmsp_periodo = primeira_data(tratamento_cmsp_periodo),
        tratamento_cmsp_intensidade = sum(tratamento_cmsp_intensidade, na.rm = TRUE)
      ),
      by = codigo_escola
    ]
    
  } else {
    cmsp_escola <- cmsp_vazio()
  }
  
} else {
  
  warning("Nenhum arquivo CMSP encontrado. tratamento_cmsp ficarC! igual a 0.")
  cmsp_escola <- cmsp_vazio()
}

# ------------------------------------------------------------
# 6. Tabela escolas_ativas
# ------------------------------------------------------------

escolas_ativas <- merge(
  base_escolas,
  inse_2019,
  by = "codigo_escola",
  all.x = TRUE,
  sort = FALSE
)

escolas_ativas <- merge(
  escolas_ativas,
  cmsp_escola,
  by = "codigo_escola",
  all.x = TRUE,
  sort = FALSE
)

escolas_ativas[
  is.na(tratamento_cmsp),
  tratamento_cmsp := 0L
]

escolas_ativas[
  is.na(tratamento_cmsp_intensidade),
  tratamento_cmsp_intensidade := 0
]

escolas_ativas[
  ,
  ano_primeiro_cmsp_real := fifelse(
    tratamento_cmsp == 1L & !is.na(tratamento_cmsp_periodo),
    as.integer(format(tratamento_cmsp_periodo, "%Y")),
    NA_integer_
  )
]

escolas_ativas[
  ,
  g_cmsp := vapply(
    ano_primeiro_cmsp_real,
    primeiro_ano_ideb_compativel,
    integer(1)
  )
]

escolas_ativas[
  tratamento_cmsp == 0L | is.na(g_cmsp),
  g_cmsp := 0L
]

escolas_ativas[
  ,
  grupo_cmsp := fifelse(
    tratamento_cmsp == 1L,
    "tratada_cmsp",
    "nunca_tratada_cmsp"
  )
]

escolas_ativas <- escolas_ativas[
  ,
  .(
    codigo_escola,
    codigo_escola_mec,
    nome_escola,
    distrito,
    bairro,
    municipio,
    diretoria,
    localizacao,
    cep,
    latitude,
    longitude,
    nivel_socioeconomico,
    tratamento_cmsp,
    tratamento_cmsp_periodo,
    tratamento_cmsp_intensidade,
    ano_primeiro_cmsp_real,
    g_cmsp,
    grupo_cmsp
  )
]

# ------------------------------------------------------------
# 7. PreparaC'C#o dos IDs para Base dos Dados
# ------------------------------------------------------------

escolas_bd <- copy(escolas_ativas)

escolas_bd[
  ,
  id_escola_bd := normalizar_codigo(codigo_escola_mec)
]

escolas_bd[
  is.na(id_escola_bd),
  id_escola_bd := normalizar_codigo(codigo_escola)
]

escolas_bd <- escolas_bd[
  !is.na(id_escola_bd),
  .(
    codigo_escola_sp = normalizar_codigo(codigo_escola),
    codigo_escola_mec = normalizar_codigo(codigo_escola_mec),
    id_escola_bd
  )
]

escolas_bd <- unique(escolas_bd, by = "id_escola_bd")

ids_escolas_bd <- escolas_bd[
  ,
  unique(id_escola_bd)
]

if (length(ids_escolas_bd) == 0) {
  stop("Nenhum cC3digo de escola vC!lido foi encontrado para consulta na Base dos Dados.")
}

ids_sql <- montar_array_sql(ids_escolas_bd)

# ------------------------------------------------------------
# 8. Consulta IDEB
# ------------------------------------------------------------

query_ideb <- glue("
  WITH escolas_base AS (
    SELECT id_escola
    FROM UNNEST([{ids_sql}]) AS id_escola
  )

  SELECT
    CAST(t.id_escola AS STRING) AS codigo_escola,
    CAST(t.ano AS INT64) AS ano,
    CAST(t.anos_escolares AS STRING) AS anos_escolares,

    t.ideb,
    t.indicador_rendimento,
    t.nota_saeb_matematica,
    t.nota_saeb_lingua_portuguesa,
    t.nota_saeb_media_padronizada

  FROM `basedosdados.br_inep_ideb.escola` AS t

  INNER JOIN escolas_base AS e
    ON CAST(t.id_escola AS STRING) = e.id_escola

  WHERE t.sigla_uf = '{uf_foco}'
    AND CAST(t.ano AS INT64) BETWEEN {ano_inicio} AND {ano_fim}
")

message("Consultando IDEB na Base dos Dados...")

ideb_painel <- basedosdados::read_sql(
  query = query_ideb,
  billing_project_id = billing_project_id
)

setDT(ideb_painel)
setnames(ideb_painel, janitor::make_clean_names(names(ideb_painel)))

ideb_painel[
  ,
  codigo_escola := normalizar_codigo(codigo_escola)
]

ideb_painel[
  ,
  ano := as.integer(ano)
]

ideb_painel[
  ,
  `:=`(
    ideb = num_bd(ideb),
    indicador_rendimento = num_bd(indicador_rendimento),
    nota_saeb_matematica = num_bd(nota_saeb_matematica),
    nota_saeb_lingua_portuguesa = num_bd(nota_saeb_lingua_portuguesa),
    nota_saeb_media_padronizada = num_bd(nota_saeb_media_padronizada),
    anos_escolares = as.character(anos_escolares)
  )
]

ideb_painel[
  grepl("inic", anos_escolares, ignore.case = TRUE),
  anos_escolares := "anos_iniciais"
]

ideb_painel[
  grepl("fin", anos_escolares, ignore.case = TRUE),
  anos_escolares := "anos_finais"
]

ideb_painel[
  grepl("medio|mC)dio", anos_escolares, ignore.case = TRUE),
  anos_escolares := "ensino_medio"
]

ideb_painel <- unique(
  ideb_painel,
  by = c("codigo_escola", "ano", "anos_escolares")
)

setorder(
  ideb_painel,
  codigo_escola,
  anos_escolares,
  ano
)

ideb_painel <- ideb_painel[
  ,
  .(
    codigo_escola,
    ano,
    anos_escolares,
    ideb,
    indicador_rendimento,
    nota_saeb_matematica,
    nota_saeb_lingua_portuguesa,
    nota_saeb_media_padronizada
  )
]

# ------------------------------------------------------------
# 9. Consulta Censo Escolar: informatizaC'C#o
# ------------------------------------------------------------

query_censo <- glue("
  WITH escolas_base AS (
    SELECT id_escola
    FROM UNNEST([{ids_sql}]) AS id_escola
  )

  SELECT
    CAST(t.id_escola AS STRING) AS codigo_escola,
    CAST(t.ano AS INT64) AS ano,

    t.internet_alunos,
    t.laboratorio_informatica,

    t.quantidade_computador_aluno,
    t.desktop_aluno,
    t.quantidade_desktop_aluno,
    t.computador_portatil_aluno,
    t.quantidade_computador_portatil_aluno,
    t.tablet_aluno,
    t.quantidade_tablet_aluno,

    t.acesso_internet_computador,
    t.acesso_internet_dispositivo_pessoal

  FROM `basedosdados.br_inep_censo_escolar.escola` AS t

  INNER JOIN escolas_base AS e
    ON CAST(t.id_escola AS STRING) = e.id_escola

  WHERE t.sigla_uf = '{uf_foco}'
    AND CAST(t.ano AS INT64) BETWEEN {ano_inicio} AND {ano_fim}
")

message("Consultando Censo Escolar na Base dos Dados...")

censo_raw <- basedosdados::read_sql(
  query = query_censo,
  billing_project_id = billing_project_id
)

setDT(censo_raw)
setnames(censo_raw, janitor::make_clean_names(names(censo_raw)))

censo_raw[
  ,
  codigo_escola := normalizar_codigo(codigo_escola)
]

censo_raw[
  ,
  ano := as.integer(ano)
]

censo_raw <- unique(
  censo_raw,
  by = c("codigo_escola", "ano")
)

# ------------------------------------------------------------
# 10. Indicadores anuais de informatizaC'C#o
# ------------------------------------------------------------

censo_informatizacao_painel <- copy(censo_raw)

censo_informatizacao_painel[
  ,
  `:=`(
    internet_alunos_bin = binario_bd(internet_alunos),
    laboratorio_informatica_bin = binario_bd(laboratorio_informatica),
    
    quantidade_computador_aluno_num = num_bd(quantidade_computador_aluno),
    desktop_aluno_bin = binario_bd(desktop_aluno),
    quantidade_desktop_aluno_num = num_bd(quantidade_desktop_aluno),
    
    computador_portatil_aluno_bin = binario_bd(computador_portatil_aluno),
    quantidade_computador_portatil_aluno_num = num_bd(quantidade_computador_portatil_aluno),
    
    tablet_aluno_bin = binario_bd(tablet_aluno),
    quantidade_tablet_aluno_num = num_bd(quantidade_tablet_aluno)
  )
]

censo_informatizacao_painel[
  ,
  trat_internet_alunos := internet_alunos_bin
]

censo_informatizacao_painel[
  ,
  trat_lab_info := laboratorio_informatica_bin
]

censo_informatizacao_painel[
  ,
  trat_dispositivo_aluno := fifelse(
    quantidade_computador_aluno_num > 0 |
      desktop_aluno_bin == 1L |
      quantidade_desktop_aluno_num > 0 |
      computador_portatil_aluno_bin == 1L |
      quantidade_computador_portatil_aluno_num > 0 |
      tablet_aluno_bin == 1L |
      quantidade_tablet_aluno_num > 0,
    1L,
    0L
  )
]

censo_informatizacao_painel[
  ,
  trat_informatizacao_basica := fifelse(
    trat_internet_alunos == 1L | trat_lab_info == 1L,
    1L,
    0L
  )
]

censo_informatizacao_painel[
  ,
  trat_informatizacao_pedagogica := fifelse(
    trat_internet_alunos == 1L & trat_dispositivo_aluno == 1L,
    1L,
    0L
  )
]

censo_informatizacao_painel[
  ,
  trat_informatizacao_completa := fifelse(
    trat_internet_alunos == 1L &
      trat_lab_info == 1L &
      trat_dispositivo_aluno == 1L,
    1L,
    0L
  )
]

# ------------------------------------------------------------
# 11. ClassificaC'C#o dinC"mica do tratamento de informatizaC'C#o
# ------------------------------------------------------------

if (!variavel_tratamento_did %in% names(censo_informatizacao_painel)) {
  stop("A variC!vel definida em variavel_tratamento_did nC#o existe no painel do Censo.")
}

censo_informatizacao_painel[
  ,
  trat_did := get(variavel_tratamento_did)
]

setorder(
  censo_informatizacao_painel,
  codigo_escola,
  ano
)

status_tratamento_escola <- censo_informatizacao_painel[
  ,
  {
    dt_escola <- .SD[!is.na(ano)]
    
    anos_obs <- dt_escola$ano
    trat_obs <- dt_escola$trat_did
    
    if (length(anos_obs) == 0) {
      
      .(
        ano_primeiro_trat_real = NA_integer_,
        g_info = 0L,
        ever_treated_info = 0L,
        always_treated_info = 0L,
        left_censored_info = 0L,
        reverte_tratamento_info = 0L,
        grupo_informatizacao = "sem_informacao"
      )
      
    } else {
      
      ano_min_obs <- min(anos_obs, na.rm = TRUE)
      
      ever <- as.integer(any(trat_obs == 1L, na.rm = TRUE))
      
      ano_primeiro_real <- if (ever == 1L) {
        min(anos_obs[trat_obs == 1L], na.rm = TRUE)
      } else {
        NA_integer_
      }
      
      left <- as.integer(
        ever == 1L &&
          !is.na(ano_primeiro_real) &&
          ano_primeiro_real == ano_min_obs
      )
      
      always <- as.integer(
        length(trat_obs) > 0 &&
          all(trat_obs == 1L, na.rm = TRUE)
      )
      
      reverte <- if (ever == 1L && !is.na(ano_primeiro_real)) {
        as.integer(any(
          trat_obs[anos_obs > ano_primeiro_real] == 0L,
          na.rm = TRUE
        ))
      } else {
        0L
      }
      
      g <- if (ever == 1L) {
        primeiro_ano_ideb_compativel(ano_primeiro_real, anos_ideb_ciclo)
      } else {
        0L
      }
      
      if (is.na(g)) {
        g <- 0L
      }
      
      grupo <- fcase(
        ever == 0L,
        "nunca_informatizada",
        
        always == 1L,
        "sempre_informatizada",
        
        left == 1L,
        "left_censored",
        
        reverte == 1L,
        "se_tornou_informatizada_com_reversao",
        
        ever == 1L,
        "se_tornou_informatizada",
        
        default = "sem_classificacao"
      )
      
      .(
        ano_primeiro_trat_real = as.integer(ano_primeiro_real),
        g_info = as.integer(g),
        ever_treated_info = ever,
        always_treated_info = always,
        left_censored_info = left,
        reverte_tratamento_info = reverte,
        grupo_informatizacao = grupo
      )
    }
  },
  by = codigo_escola
]

censo_informatizacao_painel <- merge(
  censo_informatizacao_painel,
  status_tratamento_escola,
  by = "codigo_escola",
  all.x = TRUE,
  sort = FALSE
)

censo_informatizacao_painel[
  ,
  post_info := fifelse(
    ever_treated_info == 1L & g_info > 0L & ano >= g_info,
    1L,
    0L
  )
]

censo_informatizacao_painel[
  ,
  tempo_relativo_info := fifelse(
    ever_treated_info == 1L & g_info > 0L,
    ano - g_info,
    NA_integer_
  )
]

censo_informatizacao_painel <- censo_informatizacao_painel[
  ,
  .(
    codigo_escola,
    ano,
    
    trat_internet_alunos,
    trat_lab_info,
    trat_dispositivo_aluno,
    trat_informatizacao_basica,
    trat_informatizacao_pedagogica,
    trat_informatizacao_completa,
    
    ano_primeiro_trat_real,
    g_info,
    ever_treated_info,
    post_info,
    tempo_relativo_info,
    reverte_tratamento_info,
    always_treated_info,
    left_censored_info,
    grupo_informatizacao
  )
]

setorder(
  censo_informatizacao_painel,
  codigo_escola,
  ano
)

# ------------------------------------------------------------
# 12. Painel final IDEB + informatizaC'C#o + CMSP + metadados
# ------------------------------------------------------------

painel_ideb_informatizacao <- merge(
  ideb_painel,
  censo_informatizacao_painel,
  by = c("codigo_escola", "ano"),
  all.x = TRUE,
  sort = FALSE
)

escolas_meta_bd <- copy(escolas_ativas)

escolas_meta_bd[
  ,
  codigo_escola_sp := normalizar_codigo(codigo_escola)
]

escolas_meta_bd[
  ,
  codigo_escola := normalizar_codigo(codigo_escola_mec)
]

escolas_meta_bd[
  is.na(codigo_escola),
  codigo_escola := codigo_escola_sp
]

escolas_meta_bd <- escolas_meta_bd[
  ,
  .(
    codigo_escola,
    codigo_escola_sp,
    nome_escola,
    distrito,
    bairro,
    municipio,
    diretoria,
    localizacao,
    cep,
    latitude,
    longitude,
    nivel_socioeconomico,
    tratamento_cmsp,
    tratamento_cmsp_periodo,
    tratamento_cmsp_intensidade,
    ano_primeiro_cmsp_real,
    g_cmsp,
    grupo_cmsp
  )
]

escolas_meta_bd <- unique(
  escolas_meta_bd,
  by = "codigo_escola"
)

painel_ideb_informatizacao <- merge(
  painel_ideb_informatizacao,
  escolas_meta_bd,
  by = "codigo_escola",
  all.x = TRUE,
  sort = FALSE
)

painel_ideb_informatizacao[
  ,
  post_cmsp := fifelse(
    tratamento_cmsp == 1L & g_cmsp > 0L & ano >= g_cmsp,
    1L,
    0L
  )
]

painel_ideb_informatizacao[
  ,
  tempo_relativo_cmsp := fifelse(
    tratamento_cmsp == 1L & g_cmsp > 0L,
    ano - g_cmsp,
    NA_integer_
  )
]

setorder(
  painel_ideb_informatizacao,
  codigo_escola,
  anos_escolares,
  ano
)

# ------------------------------------------------------------
# 13. DiagnC3sticos rC!pidos
# ------------------------------------------------------------

diagnostico_bases <- data.table(
  base = c(
    "escolas_ativas",
    "ideb_painel",
    "censo_informatizacao_painel",
    "painel_ideb_informatizacao"
  ),
  linhas = c(
    nrow(escolas_ativas),
    nrow(ideb_painel),
    nrow(censo_informatizacao_painel),
    nrow(painel_ideb_informatizacao)
  ),
  escolas = c(
    uniqueN(escolas_ativas$codigo_escola),
    uniqueN(ideb_painel$codigo_escola),
    uniqueN(censo_informatizacao_painel$codigo_escola),
    uniqueN(painel_ideb_informatizacao$codigo_escola)
  )
)

duplicidades_painel_final <- painel_ideb_informatizacao[
  ,
  .N,
  by = .(codigo_escola, ano, anos_escolares)
][N > 1]

if (nrow(duplicidades_painel_final) > 0) {
  warning("HC! duplicidades em codigo_escola + ano + anos_escolares no painel final.")
}

# ------------------------------------------------------------
# 14. ExportaC'C#o dos outputs
# ------------------------------------------------------------

# RDS: preferC-vel para carregar no RMarkdown preservando tipos
saveRDS(
  escolas_ativas,
  file = file.path(dir_output, "escolas_ativas.rds")
)

saveRDS(
  ideb_painel,
  file = file.path(dir_output, "ideb_painel.rds")
)

saveRDS(
  censo_informatizacao_painel,
  file = file.path(dir_output, "censo_informatizacao_painel.rds")
)

saveRDS(
  painel_ideb_informatizacao,
  file = file.path(dir_output, "painel_ideb_informatizacao.rds")
)

saveRDS(
  diagnostico_bases,
  file = file.path(dir_output, "diagnostico_bases.rds")
)

# CSV: C:til para auditoria externa
data.table::fwrite(
  escolas_ativas,
  file = file.path(dir_output, "escolas_ativas.csv"),
  sep = ";",
  bom = TRUE
)

data.table::fwrite(
  ideb_painel,
  file = file.path(dir_output, "ideb_painel.csv"),
  sep = ";",
  bom = TRUE
)

data.table::fwrite(
  censo_informatizacao_painel,
  file = file.path(dir_output, "censo_informatizacao_painel.csv"),
  sep = ";",
  bom = TRUE
)

data.table::fwrite(
  painel_ideb_informatizacao,
  file = file.path(dir_output, "painel_ideb_informatizacao.csv"),
  sep = ";",
  bom = TRUE
)

data.table::fwrite(
  diagnostico_bases,
  file = file.path(dir_output, "diagnostico_bases.csv"),
  sep = ";",
  bom = TRUE
)

if (nrow(duplicidades_painel_final) > 0) {
  data.table::fwrite(
    duplicidades_painel_final,
    file = file.path(dir_output, "duplicidades_painel_final.csv"),
    sep = ";",
    bom = TRUE
  )
}

message("Bases geradas com sucesso em: ", dir_output)
print(diagnostico_bases)