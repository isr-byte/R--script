#UNIVERSIDADE FEDERAL DA PARAÍBA
#CENTRO DE CIÊNCIAS DA SAÚDE
#DEPARTAMENTO DE NUTRIÇÃO

#AUTORA: IZABELE DA SILVA ROCHA
#ORIENTADOR: PROF. DR. SÁVIO MARCELINO GOMES

#TÍTULO: VIOLÊNCIA,DEPRESSÃO E EXCESSO DE PESO EM MULHERES BRASILEIRAS,CONFORME A ORIENTAÇÃO SEXUAL 

#JOÃO PESSOA - PARAÍBA - BRASIL / 2026

#-------------------------------------------------------------------------------
#           EXTRAÇÃO, LIMPEZA E ANÁLISE DE DADOS DA PNS (2019)
#-------------------------------------------------------------------------------
# 1 - Seleção de variáveis com base no dicionário de variáveis da PNS:

variáveis <- c("C006","C008", "Y008","V00291","Q092",
               
               "C009","V0001","V0026","D00901","VDF003",
               
               "V00201","V00202","V00203","V00204","V00205",
               "V01401","V01402","V01403","V01404","V01405", 
               "V02701","V02702",
               
               "P035", "W00103", "W00203", "P00104", "P00404",
               "VDDATA", "C00703", "C00702", "C00701")

#-------------------------------------------------------------------------------
#C006 - GÊNERO/SEXO
#Y008 - ORIENTAÇÃO SEXUAL
#V00291 - PESO RESIDENTE SELECIONADO COM CALIBRAÇÃO
#Q092 - DEPRESSÃO
#P035 - ATIVIDADE FÍSICA (DIAS)
#-------------------------------------------------------------------------------
#VARIÁVEIS SOCIODEMOGRÁFICAS:
#C009 - RAÇA/COR DE PELE
#V0001 - UNIDADE DE FEDERAÇÃO
#V0026 - SITUAÇÃO CENSITÁRIA
#D00901 - NÍVEL EDUCACIONAL
#VDF003 - RENDA FAMILIAR PER CAPITA
#C008 - IDADE DO RESIDENTE em anos
#C00703 - ANO DE NASCIMENTO
#C00702 - MÊS DE NASCIMENTO
#C00701 - DIA DE NASCIMENTO
#-------------------------------------------------------------------------------
#VARIÁVEIS DE VIOLÊNCIA:
#VIOLÊNCIA PSICOLÓGICA:
#V00201 - Nos últimos doze meses, alguém: Te ofendeu, humilhou ou ridicularizou na frente de outras pessoas?
#V00202 - Nos últimos doze meses, alguém: Gritou com você ou te xingou?
#V00203 - Nos últimos doze meses, alguém: Usou redes sociais ou celular para ameaçar, ofender,xingar ou expor imagens suas sem o seu consentimento?
#V00204 - Nos últimos doze meses, alguém: Te ameaçou de ferir ou machucar alguém importante para você?
#V00205 - Nos últimos doze meses, alguém: Destruiu alguma coisa sua de propósito?
#VIOLÊNCIA FÍSICA:
#V01401 - Nos últimos doze meses, alguém: Te deu um tapa ou uma bofetada?
#V01402 - Nos últimos doze meses, alguém: Te empurrou, segurou com força ou jogou algo em você com a intenção de machucar?
#V01403 - Nos últimos doze meses, alguém: Te deu um soco, chutou ou arrastou pelo cabelo?
#V01404 - Nos últimos doze meses, alguém: Tentou ou efetivamente estrangulou,asfixiou ou te queimou de propósito?
#V01405 - Nos últimos doze meses, alguém: Te ameaçou ou feriu com uma faca,arma de fogo ou alguma outra arma ou objeto?
#VIOLÊNCIA SEXUAL:
#V02701 - Nos últimos doze meses, alguém: tocou, manipulou, beijou ou expôs partes do seu corpo contra sua vontade?
#V02702 - Nos últimos doze meses, alguém: Te ameaçou ou forçou a ter relações sexuais ou quaisquer outros atos sexuais contra sua vontade?
#-------------------------------------------------------------------------------
#VARIÁVEIS ANTROPOMÉTRICAS:
#W00103 - peso em kg (aferido)
#W00203 - altura em cm (aferida)
#P00104 - peso em kg (autorreferido)
#P00404 - altura em cm (autorreferida)
#-------------------------------------------------------------------------------

# 2 - Extração de dados brutos do site do Instituto Brasileiro de Geografia e Estatística (IBGE):

install.packages("PNSIBGE")
library(PNSIBGE)

help("get_pns")
dadosPNS_brutos <- get_pns(year=2019, vars=variáveis, design=F)

#-------------------------------------------------------------------------------

# 3 - Modificação das variáveis e estratificação dos conjuntos de dados: 

# 3.1 - Agrupamento da variável Educação:

dadosPNS_brutos$estudo <- factor(dadosPNS_brutos$D00901, 
                                 labels = c("Ensino_Fundamental_Incompleto","Ensino_Fundamental_Incompleto",
                                            "Ensino_Fundamental_Incompleto","Ensino_Fundamental_Incompleto",
                                            "Ensino_Fundamental_Completo","Ensino_Fundamental_Completo", 
                                            "Ensino_Fundamental_Completo","Ensino_Fundamental_Completo", 
                                            "Ensino_Médio", "Ensino_Médio", "Ensino_Médio",
                                            "Ensino_Superior", "Ensino_Superior", "Ensino_Superior", 
                                            "Ensino_Superior", "NA","NA") , 
                                 levels = c("Creche", "Pré-escola", "Classe de alfabetização – CA", 
                                            "Alfabetização de jovens e adultos","Antigo primário (elementar)",
                                            "Antigo ginasial (médio 1º ciclo)", "Regular do ensino fundamental
             ou do 1º grau", "Educação de jovens e adultos (EJA) ou supletivo 
             do ensino fundamental", "Antigo científico, clássico etc. 
             (médio 2º ciclo)","Regular do ensino médio ou do 2º grau", 
                                            "Educação de jovens e adultos (EJA) ou supletivo do ensino médio", 
                                            "Superior – graduação", "Especialização de nível superior (duração
             mínima de 360 horas)","Mestrado", "Doutorado", 
                                            "Ignorado", "Não aplicável"))

# 3.1.1 - Categoria de referência
dadosPNS_brutos$estudo <- relevel(dadosPNS_brutos$estudo, 
                                  ref = "Ensino_Superior")

table(dadosPNS_brutos$estudo)
dadosPNS_brutos$estudo <- droplevels(dadosPNS_brutos$estudo)

#-------------------------------------------------------------------------------
# 3.2 - Agrupamento da variável Unidade da Federação em Macrorregiões:

dadosPNS_brutos$regiao <- factor(dadosPNS_brutos$V0001, 
                                 labels = c("Norte","Norte","Norte","Norte","Norte","Norte","Norte",
                                            "Nordeste","Nordeste","Nordeste","Nordeste","Nordeste","Nordeste",
                                            "Nordeste","Nordeste","Nordeste", 
                                            "Sudeste","Sudeste","Sudeste","Sudeste",
                                            "Sul","Sul","Sul",
                                            "Centro-Oeste","Centro-Oeste","Centro-Oeste","Centro-Oeste"), 
                                 levels = c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins",
                                            "Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba",
                                            "Pernambuco","Alagoas","Sergipe","Bahia",
                                            "Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo",
                                            "Paraná","Santa Catarina","Rio Grande do Sul",
                                            "Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal"))

# 3.2.1 - Categoria de referência
dadosPNS_brutos$regiao <- relevel(dadosPNS_brutos$regiao, ref = "Sudeste")

table(dadosPNS_brutos$regiao)

#-------------------------------------------------------------------------------
# 3.3 - Agrupamento da variável raça/cor de pele:

dadosPNS_brutos$cor <- factor(dadosPNS_brutos$C009, 
                              labels = c("Brancos","Nao_brancos","Nao_brancos","Nao_brancos","Nao_brancos","NA","NA"),
                              levels = c("Branca","Preta","Amarela","Parda","Indígena","Ignorada","Não aplicável"))

# 3.3.1 - Categoria de referência
dadosPNS_brutos$cor <- relevel(dadosPNS_brutos$cor, ref = "Brancos")

table(dadosPNS_brutos$cor)
dadosPNS_brutos$cor <- droplevels(dadosPNS_brutos$cor)

#-------------------------------------------------------------------------------

# CONFERÊNCIA: tabela_orientação_sexual_geral:
table(dadosPNS_brutos$Y008)

# CONFERÊNCIA: tabela_orientação_mulheres:
table(dadosPNS_brutos$Y008[dadosPNS_brutos$C006 == "Mulher"])

# remoção de "Outra orientação", "não sabe" e "recusou-se"

dadosPNS_brutos <- subset(dadosPNS_brutos,!Y008 %in% 
                            c("Outra orientação", "Não sabe", "Recusou-se a responder"))

dadosPNS_brutos$Y008 <- droplevels(dadosPNS_brutos$Y008)


# 3.4 - Agrupamento Orientação sexual:

dadosPNS_brutos$orientacao_sexual <- factor(dadosPNS_brutos$Y008, 
                                            labels = c("Heterossexuais", "Bissexuais", "Homossexuais"),
                                            levels = c("Heterosexual","Bissexual","Homosexual"))

# 3.4.1 - Categoria de referência
dadosPNS_brutos$orientacao_sexual <- relevel(dadosPNS_brutos$Y008, ref = "Heterossexuais")

table(dadosPNS_brutos$orientacao_sexual)
table(dadosPNS_brutos$orientacao_sexual[dadosPNS_brutos$C006 == "Mulher"])

#-------------------------------------------------------------------------------

# 3.5 - Agrupamento da variável rendimento familiar per capita:

# 3.5.1 - definindo o valor de salário mínimo (EM 2019):

salario_minimo <- 998

# 3.5.2 - criando a variável de classificação de faixa salarial:

dadosPNS_brutos$faixa_rendimento_percapita <- cut(dadosPNS_brutos$VDF003, 
                                                  breaks = c(-Inf, salario_minimo, 3 * salario_minimo, Inf),
                                                  labels = c("menos de 1 salário mínimo", "de 1 a 3 salários mínimos", 
                                                             "acima de 3 salários mínimos"), right = FALSE)

# menos de 1 salário mínimo:0 a 998,00 reais
# de 1 a 3 salários mínimos:999,00 a 2994,00 reais
# mais de 3 salários mínimos: acima de 2994,00 reais.

# 3.5.3 - transformando em fator:
dadosPNS_brutos$faixa_rendimento_percapita <- 
  factor(dadosPNS_brutos$faixa_rendimento_percapita, 
         labels = c("<1_SM", ">1<3_SM", ">3_SM","NA", "NA"),
         levels = c("menos de 1 salário mínimo", "de 1 a 3 salários mínimos",
                    "acima de 3 salários mínimos", "Ignorada", "Não aplicável"))

# 3.5.4 - Categoria de referência
dadosPNS_brutos$faixa_rendimento_percapita <- relevel(dadosPNS_brutos$
                                                        faixa_rendimento_percapita, ref = ">3_SM")


table(dadosPNS_brutos$faixa_rendimento_percapita)

dadosPNS_brutos$faixa_rendimento_percapita <- droplevels(dadosPNS_brutos$faixa_rendimento_percapita)

#-------------------------------------------------------------------------------

# 3.6 - Agrupamento da variável violência:

# 3.6.1 - condicionamento da presença de qualquer episódio de violência:

dadosPNS_brutos$sofreu_qualquer_violencia <- ifelse(
  rowSums(dadosPNS_brutos[, c("V00201","V00202","V00203","V00204","V00205")]== "Sim", na.rm = TRUE) > 0 |
    rowSums(dadosPNS_brutos[, c("V01401","V01402","V01403","V01404","V01405")]== "Sim", na.rm = TRUE) > 0 |
    rowSums(dadosPNS_brutos[, c("V02701","V02702")]== "Sim", na.rm = TRUE) > 0,"Sim","Não")

table(dadosPNS_brutos$sofreu_qualquer_violencia)

# 3.6.2 - condicionamento da presença de episódio de violência psicológica:

dadosPNS_brutos$sofreu_violencia_psicologica <- ifelse(rowSums(dadosPNS_brutos[,
                    c("V00201","V00202","V00203","V00204","V00205")] == "Sim") > 0 , "Sim","Não")

table(dadosPNS_brutos$sofreu_violencia_psicologica)

# 3.6.3 - condicionamento da presença de episódio de violência física:

dadosPNS_brutos$sofreu_violencia_fisica <- ifelse(rowSums(dadosPNS_brutos[, 
              c("V01401","V01402","V01403","V01404","V01405")] == "Sim") > 0 , "Sim","Não")

table(dadosPNS_brutos$sofreu_violencia_fisica)

# 3.6.4 - condicionamento da presença de episódio de violência sexual:

dadosPNS_brutos$sofreu_violencia_sexual <- ifelse(rowSums(dadosPNS_brutos[, 
                                      c("V02701","V02702")] == "Sim") > 0 , "Sim","Não")

table(dadosPNS_brutos$sofreu_violencia_sexual)

#-------------------------------------------------------------------------------

# 3.7- Agrupamento da variável Atividade física:

# 3.7.1 - Classificação do nível de atividade física com base no IPAQ:

dadosPNS_brutos$treino <- ifelse(dadosPNS_brutos$P035 == 0, "sedentario",
                                 ifelse(dadosPNS_brutos$P035 %in% c(1, 2), "irregularmente_ativo",
                                        ifelse(dadosPNS_brutos$P035 >= 3, "ativo", 
                                               ifelse(dadosPNS_brutos$P035 >= 5, "muito_ativo",NA))))

# 3.7.2 - Transformação dos níveis em fator:

dadosPNS_brutos$treino <- factor(dadosPNS_brutos$treino, 
                                 labels = c("sedentario", "irregularmente_ativo", "ativo","muito_ativo",
                                            "NA", "NA"),
                                 levels = c("sedentario", "irregularmente_ativo","ativo", "muito_ativo","Ignorada", 
                                            "Não aplicável"))

# 3.7.3 - Categoria de referência
dadosPNS_brutos$treino <- relevel(dadosPNS_brutos$treino, ref = "ativo")

table(dadosPNS_brutos$treino)

dadosPNS_brutos$treino <- droplevels(dadosPNS_brutos$treino)

#-------------------------------------------------------------------------------

# 3.8 - ESTADO NUTRICIONAL PNS (DADOS AUTORREFERIDOS)

library(dplyr)
library(lubridate)
library(zscorer)

# 3.8.1 – Cálculo da idade em anos e meses

dadosPNS_brutos <- dadosPNS_brutos %>% mutate(
  data_nascimento = make_date(C00703, C00702, C00701),
  data_arquivo = ymd(VDDATA),
  idade_anos = interval(data_nascimento, data_arquivo) %/% years(1),
  idade_meses = interval(data_nascimento, data_arquivo) %/% months(1))


# 3.8.2 – Categorização da faixa etária

dadosPNS_brutos <- dadosPNS_brutos %>%
  mutate(faixa_etaria = case_when(
    idade_anos >= 18 & idade_anos < 20 ~ "Adolescente",
    idade_anos >= 20 & idade_anos < 60 ~ "Adulto",
    idade_anos >= 60 ~ "Idoso",
    TRUE ~ NA_character_))

table(dadosPNS_brutos$faixa_etaria)

# 3.8.3 – Tabela de frequência absoluta e relativa das faixas etárias

tabela_faixa_etaria <- dadosPNS_brutos %>%
  filter(!is.na(faixa_etaria)) %>%
  count(faixa_etaria) %>%
  mutate(percentual = round(100 * n / sum(n), 2))

print(tabela_faixa_etaria)

# 3.8.4 – Conversão de altura para metros 

dadosPNS_brutos <- dadosPNS_brutos %>%
  mutate(altura_metros = P00404 / 100,peso_kg = P00104)

# 3.8.5 – Cálculo de IMC para adultos

dadosPNS_brutos <- dadosPNS_brutos %>%
  mutate(imc_adulto = if_else(faixa_etaria %in% c("Adulto"),
                              peso_kg / (altura_metros^2),NA_real_ ))

# 3.8.6 – Classificação do estado nutricional de adultos

dadosPNS_brutos <- dadosPNS_brutos %>%
  mutate(
    estado_nutricional_adulto = case_when(
      faixa_etaria == "Adulto" & imc_adulto < 18.5 ~ "Baixo_peso",
      faixa_etaria == "Adulto" & imc_adulto >= 18.5 & imc_adulto < 25 ~ "Eutrofia",
      faixa_etaria == "Adulto" & imc_adulto >= 25 & imc_adulto < 30 ~ "Sobrepeso",
      faixa_etaria == "Adulto" & imc_adulto >= 30 ~ "Obesidade",
      TRUE ~ NA_character_))

# 3.8.7 – Cálculo do IMC para idosos

dadosPNS_brutos <- dadosPNS_brutos %>%
  mutate(imc_idoso = if_else(faixa_etaria %in% c("Idoso"),
                             peso_kg / (altura_metros^2),NA_real_))

# 3.8.8 – Classificão do estado nutricional de idoso

dadosPNS_brutos <- dadosPNS_brutos %>%
  mutate(estado_nutricional_idoso = case_when(
    faixa_etaria == "Idoso" & imc_idoso <= 23 ~ "Baixo_peso",
    faixa_etaria == "Idoso" & imc_idoso > 23 & imc_idoso < 27 ~ "Eutrofia",
    faixa_etaria == "Idoso" & imc_idoso >= 27 ~ "Sobrepeso",
    TRUE ~ NA_character_))

# 3.8.9 – Consolidação em única variável: “estado_nutricional”

dadosPNS_brutos <- dadosPNS_brutos %>%
  mutate(estado_nutricional = coalesce(
    estado_nutricional_adulto, estado_nutricional_idoso))

table(dadosPNS_brutos$estado_nutricional,dadosPNS_brutos$faixa_etaria)
table(dadosPNS_brutos$estado_nutricional,dadosPNS_brutos$faixa_etaria,dadosPNS_brutos$orientacao_sexual)


# 3.8.10 - Conversão para fator:

# Estado nutricional

dadosPNS_brutos$estado_nutricional<- 
  factor (dadosPNS_brutos$estado_nutricional, 
          labels = c ("Baixo_peso", "Eutrofia","Sobrepeso","Obesidade"),
          levels = c("Baixo_peso", "Eutrofia","Sobrepeso", "Obesidade"))

# categoria de referência
dadosPNS_brutos$estado_nutricional<- relevel(dadosPNS_brutos$
                                               estado_nutricional, ref = "Eutrofia")

table (dadosPNS_brutos$estado_nutricional)

# Estado nutricional binário

dadosPNS_brutos <- subset(dadosPNS_brutos,!estado_nutricional %in% 
                            c("Baixo_peso"))

dadosPNS_brutos$estado_nutricional <- droplevels(dadosPNS_brutos$estado_nutricional)

dadosPNS_brutos$estado_nutricional_binario <- 
  factor (dadosPNS_brutos$estado_nutricional, 
          labels = c ("Sem_excesso_de_peso","Com_excesso_de_peso",
                      "Com_excesso_de_peso"),
          levels = c("Eutrofia","Sobrepeso", "Obesidade"))

# 3.8.11 - Categoria de referência
dadosPNS_brutos$estado_nutricional_binario <- relevel(dadosPNS_brutos$
                                                        estado_nutricional_binario, ref = "Sem_excesso_de_peso")

table(dadosPNS_brutos$estado_nutricional_binario)
table(dadosPNS_brutos$estado_nutricional_binario[dadosPNS_brutos$C006 == "Mulher"])

#-------------------------------------------------------------------------------

# 3.9 - Agrupamento da variável Depressão:
table (dadosPNS_brutos$Q092)
dadosPNS_brutos$Q092 <- droplevels(dadosPNS_brutos$Q092)

# 3.9.1 Tranformando em fator
dadosPNS_brutos$depressao <- factor (dadosPNS_brutos$Q092, 
                                     labels = c ("Não","Sim"),
                                     levels = c("Não","Sim"))

# 3.9.2 - Categoria de referência 
dadosPNS_brutos$depressao <- relevel (dadosPNS_brutos$depressao, ref = "Não") 

table(dadosPNS_brutos$depressao)

# ------------------------------------------------------------------------------

# 4 - Criação do objeto do plano amostral atribuindo pesos correspondentes 
# ao design de amostragem complexo:

install.packages("survey")
library(survey)

plano_amostral <- svydesign(ids = ~UPA_PNS,strata = ~V0024,weights = ~V00281,
                            data = dadosPNS_brutos,nest = TRUE)

summary(plano_amostral) 


#Dividindo os dados por gênero:

men_plano_amostral <- subset(plano_amostral, subset = C006 == "Homem")

women_plano_amostral <- subset(plano_amostral, subset = C006 == "Mulher")

women_hetero <- subset(plano_amostral, subset = C006 == "Mulher" & orientacao_sexual == "Heterossexuais")

women_bi <- subset(plano_amostral, subset = C006 == "Mulher" & orientacao_sexual == "Bissexuais")

women_homo <- subset(plano_amostral, subset = C006 == "Mulher" & orientacao_sexual == "Homossexuais")

#-------------------------------------------------------------------------------
# 5 - ANÁLISES DESCRITIVAS
#-------------------------------------------------------------------------------

# Descrição dos dados socioeconômicos por gênero e orientação sexual:
# (Esta etapa deve ser repetida para todas as variáveis analisadas na Tabela 1)


# 5.1 - ESCOLARIDADE/ MULHER/ ORIENTAÇÃO SEXUAL

escolaridade <- svyby(~ estudo, ~ orientacao_sexual, women_plano_amostral, svymean, na.rm=T) #proporcao

escolaridade #retorna a variável dependente por gênero e orientação sexual

confint(escolaridade) #retorna o intervalo de confiança de 95%


# 5.2 - RAÇA COR/MULHER/ ORIENTAÇÃO SEXUAL

raçacor <- svyby(~ cor, ~ orientacao_sexual , women_plano_amostral, svymean, na.rm=T)

raçacor

confint(raçacor)


# 5.3 - REGIÃO/MULHER/ORIENTAÇÃO SEXUAL

regiaogeo <- svyby(~ regiao, ~ orientacao_sexual, women_plano_amostral, svymean, na.rm=T) 

regiaogeo

confint(regiaogeo)


# 5.4 - SITUAÇÃO CENSITÁRIA/MULHER/ORIENTAÇÃO SEXUAL

situação_censitária <- svyby(~ V0026, ~ orientacao_sexual , women_plano_amostral, svymean, na.rm=T) 

situação_censitária

confint(situação_censitária)


# 5.5 - RENDIMENTO PER CAPITA/MULHER/ORIENTAÇÃO SEXUAL

rendimentopercapita <- svyby(~ faixa_rendimento_percapita, ~ orientacao_sexual, 
                             women_plano_amostral, svymean, na.rm=T) 

rendimentopercapita

confint(rendimentopercapita)


# 5.6 - QUALQUER VIOLENCIA/MULHER/ORIENTAÇÃO SEXUAL

qualquerviolencia <- svyby(~ sofreu_qualquer_violencia, ~ orientacao_sexual, 
                           women_plano_amostral, svymean, na.rm=T) 

qualquerviolencia

confint(qualquerviolencia)


# 5.7 - VIOLENCIA PSICOLÓGICA/MULHER/ORIENTAÇÃO SEXUAL

violenciapsicologica <- svyby(~ sofreu_violencia_psicologica, ~ orientacao_sexual, 
                              women_plano_amostral, svymean, na.rm=T) 

violenciapsicologica

confint(violenciapsicologica)


# 5.8 - VIOLENCIA FÍSICA/MULHER/ORIENTAÇÃO SEXUAL

violenciafisica <- svyby(~ sofreu_violencia_fisica, ~ orientacao_sexual, 
                         women_plano_amostral, svymean, na.rm=T) 

violenciafisica

confint(violenciafisica)


# 5.9 - VIOLENCIA SEXUAL/MULHER/ORIENTAÇÃO SEXUAL

violenciasexual <- svyby(~ sofreu_violencia_sexual, ~ orientacao_sexual, 
                         women_plano_amostral, svymean, na.rm=T) 

violenciasexual

confint(violenciasexual)


# 5.10 - DEPRESSÃO/MULHER/ORIENTAÇÃO SEXUAL

Depressaoo <- svyby(~ depressao, ~ orientacao_sexual, women_plano_amostral, svymean, na.rm=T) 

Depressaoo

confint(Depressaoo)


# 5.11 - ATIVIDADE FÍSICA

atividadefisicasemanal <- svyby(~ treino, ~ orientacao_sexual, women_plano_amostral, 
                                svymean, na.rm=T) 

atividadefisicasemanal

confint(atividadefisicasemanal)


# 5.12 - ESTADO NUTRICIONAL

estadonutricional <- svyby(~ estado_nutricional, ~ orientacao_sexual, women_plano_amostral,
                           svymean, na.rm=T)

estadonutricional

confint(estadonutricional)


estadonutricional2 <- svyby(~ estado_nutricional, ~interaction(orientacao_sexual,faixa_etaria),
                            women_plano_amostral, svymean, na.rm=T)

estadonutricional2

confint(estadonutricional2)



estadonutricional3 <- svyby(~ estado_nutricional, ~ orientacao_sexual,
                            women_plano_amostral, svymean, na.rm=T)

estadonutricional3

confint(estadonutricional3)


estadonutricionalbinario <- svyby(~ estado_nutricional_binario, 
                                  ~interaction(orientacao_sexual,faixa_etaria),
                                  women_plano_amostral, svymean, na.rm=T)

estadonutricionalbinario

confint(estadonutricionalbinario)


estadonutricionalbinario2 <- svyby(~ estado_nutricional_binario, ~orientacao_sexual,
                                  women_plano_amostral, svymean, na.rm=T)

estadonutricionalbinario2

confint(estadonutricionalbinario2)


estadonutricionalbinario3 <- svyby(~ estado_nutricional_binario, 
                                  ~interaction(orientacao_sexual,depressao),
                                  women_plano_amostral, svymean, na.rm=T)

estadonutricionalbinario3

confint(estadonutricionalbinario3)


# 5.13 - FAIXA ETÁRIA

FAIXAETARIA <- svyby(~ faixa_etaria, ~ orientacao_sexual, women_plano_amostral,
                     svymean, na.rm=T)

FAIXAETARIA

confint(FAIXAETARIA)

#-------------------------------------------------------------------------------
# 6 - ANÁLISE BIVARIADA (QUI-QUADRADO)
#-------------------------------------------------------------------------------

options (scipen=999) # remoção de notação científica

# Teste de Qui-quadrado de Rao & Scott:

teste_chi1 <- svychisq(~ depressao + estado_nutricional_binario, 
                       design = women_plano_amostral,
                       statistic = "Chisq")

print(teste_chi1)


teste_chi2 <- svychisq(~ depressao + estado_nutricional_binario, 
                       design = women_hetero,
                       statistic = "Chisq")
print(teste_chi2)

teste_chi3 <- svychisq(~ depressao + estado_nutricional_binario, 
                       design = women_bi,
                       statistic = "Chisq")
print(teste_chi3)

teste_chi4 <- svychisq(~ depressao + estado_nutricional_binario, 
                       design = women_homo,
                       statistic = "Chisq")
print(teste_chi4)

#-------------------------------------------------------------------------------
# 7 - ANÁLISE MULTIVARIADA 
#------------------------------------------------------------------------------

# 7.1 - Regressão Logística

## MULHERES HETEROSSEXUAIS

# Modelo 1 (ESTADO NUTRICIONAL BINÁRIO E DEPRESSÃO)

mulheres_het_dep_peso <- svyglm(formula = estado_nutricional_binario ~ depressao + 
                                  treino + faixa_rendimento_percapita 
                                + regiao + cor + C008, design = women_hetero,
                                family = "binomial")

# Modelo 2 (ESTADO NUTRICIONAL BINÁRIO,DEPRESSÃO E VIOLENCIA psicológica)
mulheres_het_dep_peso_vp <- svyglm(formula = estado_nutricional_binario ~ depressao + sofreu_violencia_psicologica +
                                     treino + faixa_rendimento_percapita 
                                   + regiao + cor + C008, design = women_hetero,
                                   family = "binomial")

# Modelo 3 (ESTADO NUTRICIONAL BINÁRIO,DEPRESSÃO E VIOLENCIA fisica)
mulheres_het_dep_peso_vf <- svyglm(formula = estado_nutricional_binario ~ depressao + sofreu_violencia_fisica +
                                     treino + faixa_rendimento_percapita 
                                   + regiao + cor + C008, design = women_hetero,
                                   family = "binomial")

# Modelo 4 (ESTADO NUTRICIONAL BINÁRIO,DEPRESSÃO E VIOLENCIA sexual)
mulheres_het_dep_peso_vs <- svyglm(formula = estado_nutricional_binario ~ depressao + sofreu_violencia_sexual +
                                     treino + faixa_rendimento_percapita 
                                   + regiao + cor + C008, design = women_hetero,
                                   family = "binomial")

## MULHERES BISSEXUAIS 

# Modelo 5 (ESTADO NUTRICIONAL BINÁRIO E DEPRESSÃO)

mulheres_bi_dep_peso <- svyglm(formula = estado_nutricional_binario ~ depressao + 
                                 treino + faixa_rendimento_percapita 
                               + regiao + cor + C008, design = women_bi,
                               family = "binomial")

# Modelo 6 (ESTADO NUTRICIONAL BINÁRIO,DEPRESSÃO E VIOLENCIA psicologica)
mulheres_bi_dep_peso_vp <- svyglm(formula = estado_nutricional_binario ~ depressao + sofreu_violencia_psicologica +
                                    treino + faixa_rendimento_percapita 
                                  + regiao + cor + C008, design = women_bi,
                                  family = "binomial")

# Modelo 7 (ESTADO NUTRICIONAL BINÁRIO,DEPRESSÃO E VIOLENCIA fisica)
mulheres_bi_dep_peso_vf <- svyglm(formula = estado_nutricional_binario ~ depressao + sofreu_violencia_fisica +
                                    treino + faixa_rendimento_percapita 
                                  + regiao + cor + C008, design = women_bi,
                                  family = "binomial")

# Modelo 8 (ESTADO NUTRICIONAL BINÁRIO,DEPRESSÃO E VIOLENCIA sexual)
mulheres_bi_dep_peso_vs <- svyglm(formula = estado_nutricional_binario ~ depressao + sofreu_violencia_sexual +
                                    treino + faixa_rendimento_percapita 
                                  + regiao + cor + C008, design = women_bi,
                                  family = "binomial")

## MULHERES HOMOSSEXUAIS 

# Modelo 9 (ESTADO NUTRICIONAL BINÁRIO E DEPRESSÃO)

mulheres_homo_dep_peso <- svyglm(formula = estado_nutricional_binario ~ depressao + 
                                   treino + faixa_rendimento_percapita 
                                 + regiao + cor + C008, design = women_homo,
                                 family = "binomial")

# Modelo 10 (ESTADO NUTRICIONAL BINÁRIO,DEPRESSÃO E VIOLENCIA psicologica)
mulheres_homo_dep_peso_vp <- svyglm(formula = estado_nutricional_binario ~ depressao + sofreu_violencia_psicologica +
                                      treino + faixa_rendimento_percapita 
                                    + regiao + cor + C008, design = women_homo,
                                    family = "binomial") 

# Modelo 11 (ESTADO NUTRICIONAL BINÁRIO,DEPRESSÃO E VIOLENCIA fisica)
mulheres_homo_dep_peso_vf <- svyglm(formula = estado_nutricional_binario ~ depressao + sofreu_violencia_fisica +
                                      treino + faixa_rendimento_percapita 
                                    + regiao + cor + C008, design = women_homo,
                                    family = "binomial")

# Modelo 12 (ESTADO NUTRICIONAL BINÁRIO,DEPRESSÃO E VIOLENCIA sexual)
mulheres_homo_dep_peso_vs <- svyglm(formula = estado_nutricional_binario ~ depressao + sofreu_violencia_sexual +
                                      treino + faixa_rendimento_percapita 
                                    + regiao + cor + C008, design = women_homo,
                                    family = "binomial")

# 5.2 - Extração dos coeficientes e erros padrão/ Cálculos OR e IC95%/ Visualização

# Modelo 1

coef_est1 <- coef(mulheres_het_dep_peso)  # estimativas
OR1 <- exp(coef_est1)                     # odds ratios
IC1 <- confint(mulheres_het_dep_peso)     # intervalo de confiança
lower1 <- exp(IC1[, 1])
upper1 <- exp(IC1[, 2])
p_valor1 <- summary(mulheres_het_dep_peso)$coefficients[, 4]

resultados_OR1 <- data.frame(
  Termo = names(coef_est1),
  OR = round(OR1, 3),
  IC_2.5 = round(lower1, 3),
  IC_97.5 = round(upper1, 3),
  p_valor = round(p_valor1, 4))

print(resultados_OR1)

# Modelo 2

coef_est2 <- coef(mulheres_het_dep_peso_vp)  
OR2 <- exp(coef_est2)                      
IC2 <- confint(mulheres_het_dep_peso_vp)     
lower2 <- exp(IC2[, 1])
upper2 <- exp(IC2[, 2])
p_valor2 <- summary(mulheres_het_dep_peso_vp)$coefficients[, 4]

resultados_OR2 <- data.frame(
  Termo = names(coef_est2),
  OR = round(OR2, 3),
  IC_2.5 = round(lower2, 3),
  IC_97.5 = round(upper2, 3),
  p_valor = round(p_valor2, 4))

print(resultados_OR2)

# Modelo 3

coef_est3 <- coef(mulheres_het_dep_peso_vf)  
OR3 <- exp(coef_est3)                      
IC3 <- confint(mulheres_het_dep_peso_vf)     
lower3 <- exp(IC3[, 1])
upper3 <- exp(IC3[, 2])
p_valor3 <- summary(mulheres_het_dep_peso_vf)$coefficients[, 4]

resultados_OR3 <- data.frame(
  Termo = names(coef_est3),
  OR = round(OR3, 3),
  IC_2.5 = round(lower3, 3),
  IC_97.5 = round(upper3, 3),
  p_valor = round(p_valor3, 4))

print(resultados_OR3)

# Modelo 4

coef_est4 <- coef(mulheres_het_dep_peso_vs)  
OR4 <- exp(coef_est4)                      
IC4 <- confint(mulheres_het_dep_peso_vs)     
lower4 <- exp(IC4[, 1])
upper4 <- exp(IC4[, 2])
p_valor4 <- summary(mulheres_het_dep_peso_vs)$coefficients[, 4]

resultados_OR4 <- data.frame(
  Termo = names(coef_est4),
  OR = round(OR4, 3),
  IC_2.5 = round(lower4, 3),
  IC_97.5 = round(upper4, 3),
  p_valor = round(p_valor4, 4))

print(resultados_OR4)

# Modelo 5

coef_est5 <- coef(mulheres_bi_dep_peso)  
OR5 <- exp(coef_est5)                   
IC5 <- confint(mulheres_bi_dep_peso)     
lower5 <- exp(IC5[, 1])
upper5 <- exp(IC5[, 2])
p_valor5 <- summary(mulheres_bi_dep_peso)$coefficients[, 4]

resultados_OR5 <- data.frame(
  Termo = names(coef_est5),
  OR = round(OR5, 3),
  IC_2.5 = round(lower5, 3),
  IC_97.5 = round(upper5, 3),
  p_valor = round(p_valor5, 4))

print(resultados_OR5)

# Modelo 6

coef_est6 <- coef(mulheres_bi_dep_peso_vp)  
OR6 <- exp(coef_est6)                     
IC6 <- confint(mulheres_bi_dep_peso_vp)     
lower6 <- exp(IC6[, 1])
upper6 <- exp(IC6[, 2])
p_valor6 <- summary(mulheres_bi_dep_peso_vp)$coefficients[, 4]

resultados_OR6 <- data.frame(
  Termo = names(coef_est6),
  OR = round(OR6, 3),
  IC_2.5 = round(lower6, 3),
  IC_97.5 = round(upper6, 3),
  p_valor = round(p_valor6, 4))

print(resultados_OR6)

# Modelo 7

coef_est7 <- coef(mulheres_bi_dep_peso_vf)  
OR7 <- exp(coef_est7)                     
IC7 <- confint(mulheres_bi_dep_peso_vf)     
lower7 <- exp(IC7[, 1])
upper7 <- exp(IC7[, 2])
p_valor7 <- summary(mulheres_bi_dep_peso_vf)$coefficients[, 4]

resultados_OR7 <- data.frame(
  Termo = names(coef_est7),
  OR = round(OR7, 3),
  IC_2.5 = round(lower7, 3),
  IC_97.5 = round(upper7, 3),
  p_valor = round(p_valor7, 4))

print(resultados_OR7)

# Modelo 8

coef_est8 <- coef(mulheres_bi_dep_peso_vs)  
OR8 <- exp(coef_est8)                     
IC8 <- confint(mulheres_bi_dep_peso_vs)     
lower8 <- exp(IC8[, 1])
upper8 <- exp(IC8[, 2])
p_valor8 <- summary(mulheres_bi_dep_peso_vs)$coefficients[, 4]

resultados_OR8 <- data.frame(
  Termo = names(coef_est8),
  OR = round(OR8, 3),
  IC_2.5 = round(lower8, 3),
  IC_97.5 = round(upper8, 3),
  p_valor = round(p_valor8, 4))

print(resultados_OR8)


# Modelo 9

coef_est9 <- coef(mulheres_homo_dep_peso)  
OR9 <- exp(coef_est9)                     
IC9 <- confint(mulheres_homo_dep_peso)     
lower9 <- exp(IC9[, 1])
upper9 <- exp(IC9[, 2])
p_valor9 <- summary(mulheres_homo_dep_peso)$coefficients[, 4]

resultados_OR9 <- data.frame(
  Termo = names(coef_est9),
  OR = round(OR9, 3),
  IC_2.5 = round(lower9, 3),
  IC_97.5 = round(upper9, 3),
  p_valor = round(p_valor9, 4))

print(resultados_OR9)

# Modelo 10

coef_est10 <- coef(mulheres_homo_dep_peso_vp)  
OR10 <- exp(coef_est10)                       
IC10 <- confint(mulheres_homo_dep_peso_vp)     
lower10 <- exp(IC10[, 1])
upper10 <- exp(IC10[, 2])
p_valor10 <- summary(mulheres_homo_dep_peso_vp)$coefficients[, 4]

resultados_OR10 <- data.frame(
  Termo = names(coef_est10),
  OR = round(OR10, 3),
  IC_2.5 = round(lower10, 3),
  IC_97.5 = round(upper10, 3),
  p_valor = round(p_valor10, 4))

print(resultados_OR10)

# Modelo 11

coef_est11 <- coef(mulheres_homo_dep_peso_vf)  
OR11 <- exp(coef_est11)                       
IC11 <- confint(mulheres_homo_dep_peso_vf)     
lower11 <- exp(IC11[, 1])
upper11 <- exp(IC11[, 2])
p_valor11 <- summary(mulheres_homo_dep_peso_vf)$coefficients[, 4]

resultados_OR11 <- data.frame(
  Termo = names(coef_est11),
  OR = round(OR11, 3),
  IC_2.5 = round(lower11, 3),
  IC_97.5 = round(upper11, 3),
  p_valor = round(p_valor11, 4))

print(resultados_OR11)

# Modelo 12

coef_est12 <- coef(mulheres_homo_dep_peso_vs)  
OR12 <- exp(coef_est12)                       
IC12 <- confint(mulheres_homo_dep_peso_vs)     
lower12 <- exp(IC12[, 1])
upper12 <- exp(IC12[, 2])
p_valor12 <- summary(mulheres_homo_dep_peso_vs)$coefficients[, 4]

resultados_OR12 <- data.frame(
  Termo = names(coef_est12),
  OR = round(OR12, 3),
  IC_2.5 = round(lower12, 3),
  IC_97.5 = round(upper12, 3),
  p_valor = round(p_valor12, 4))

print(resultados_OR12)

