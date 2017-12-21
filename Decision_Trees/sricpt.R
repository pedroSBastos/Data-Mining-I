data <- read.csv("/home/bastos/Documentos/DM1/UCMFv2.csv")

data$IDADE[data$IDADE == "#VALUE!"] <- NA #Tem que se lhe diga
#IDADE >19
for(i in seq(data$IDADE)){
  if(!is.na(as.numeric(sub(",",".",as.character(data$IDADE[i]))))){
    if(as.numeric(sub(",",".",as.character(data$IDADE[i])))>19){
      data$Peso[i] <- NA
      data$Altura[i] <- NA
      data$IMC[i] <- NA
      data$Atendimento[i] <- NA
      data$DN[i] <- NA
      data$IDADE[i] <- NA
      data$Convenio[i] <- NA
      data$PULSOS[i] <- NA
      data$PA.SISTOLICA[i] <- NA
      data$PA.DIASTOLICA[i] <- NA
      data$PPA[i] <- NA
      data$NORMAL.X.ANORMAL[i] <- NA
      data$B2[i] <- NA
      data$SOPRO[i] <- NA
      data$FC[i] <- NA
      data$HDA.1[i] <- NA
      data$HDA2[i] <- NA
      data$SEXO[i] <- NA
      data$MOTIVO1[i] <- NA
      data$MOTIVO2[i] <- NA
    }
  }
}

#SEXO
data$SEXO[data$SEXO == "Masculino"] <- "M"
data$SEXO[data$SEXO == "masculino"] <- "M"
data$SEXO[data$SEXO == "Feminino"] <- "F"
data$SEXO[data$SEXO == "Indeterminado"] <- NA
data$SEXO[data$SEXO == ""] <- NA

#IDADE
data$IDADE[data$IDADE == ""] <- NA
for(i in seq(data$IDADE)){
  if(!is.na(as.numeric(sub(",",".",as.character(data$IDADE[i]))))){
    if(as.numeric(sub(",",".",as.character(data$IDADE[i])))<0){
      data$IDADE[i] <- NA
      data$Atendimento[i] <- NA
      data$DN[i] <- NA
    }
  }
}

#IMC entre 10 e 50 https://pt.wikipedia.org/wiki/%C3%8Dndice_de_massa_corporal
for(i in seq(data$IMC)){
  if(!is.na(as.numeric(sub(",",".",as.character(data$IMC[i]))))){
    if(as.numeric(sub(",",".",as.character(data$IMC[i])))<10){
      data$IMC[i] <- NA
      data$Peso[i] <- NA #aberracao nao pode estar viva retira peso e altura uma ou ambas deve estar errada
      data$Altura[i] <- NA
    }
  }
}
for(i in seq(data$IMC)){
  if(!is.na(as.numeric(sub(",",".",as.character(data$IMC[i]))))){
    if(as.numeric(sub(",",".",as.character(data$IMC[i])))>50){
      data$IMC[i] <- NA
      data$Peso[i] <- NA
      data$Altura[i] <- NA
    }
  }
}
data$IMC[data$IMC == ""] <- NA

#Peso
for(i in seq(data$Peso)){
  if(!is.na(as.numeric(sub(",",".",as.character(data$Peso[i]))))){
    if(as.numeric(sub(",",".",as.character(data$Peso[i])))<=0){
      data$Peso[i] <- NA
    }
  }
}
data$Peso[data$Peso == ""] <- NA

#Altura #recorde do guiness
for(i in seq(data$Altura)){
  if(!is.na(as.numeric(sub(",",".",as.character(data$Altura[i]))))){
    if(as.numeric(sub(",",".",as.character(data$Altura[i])))<24){
      data$Altura[i] <- NA
    }
  }
}
data$Altura[data$Altura == ""] <- NA

#PULSOS
data$PULSOS[data$PULSOS == "AMPLOS"] <- "Amplos"
data$PULSOS[data$PULSOS == "NORMAIS"] <- "Normais"
data$PULSOS[data$PULSOS == ""] <- NA

#PA.SISTOLICA 60-190 http://www.bloodpressureuk.org/BloodPressureandyou/Thebasics/Bloodpressurechart
for(i in seq(data$PA.SISTOLICA)){
  if(!is.na(as.numeric(sub(",",".",as.character(data$PA.SISTOLICA[i]))))){
    if(as.numeric(sub(",",".",as.character(data$PA.SISTOLICA[i])))<60){
      data$PA.SISTOLICA[i] <- NA
    }
  }
}
for(i in seq(data$PA.SISTOLICA)){
  if(!is.na(as.numeric(sub(",",".",as.character(data$PA.SISTOLICA[i]))))){
    if(as.numeric(sub(",",".",as.character(data$PA.SISTOLICA[i])))>190){
      data$PA.SISTOLICA[i] <- NA
    }
  }
}
data$PA.SISTOLICA[data$PA.SISTOLICA == ""] <- NA

#PA.DIASTOLICA 40-100 http://www.bloodpressureuk.org/BloodPressureandyou/Thebasics/Bloodpressurechart
for(i in seq(data$PA.DIASTOLICA)){
  if(!is.na(as.numeric(sub(",",".",as.character(data$PA.DIASTOLICA[i]))))){
    if(as.numeric(sub(",",".",as.character(data$PA.DIASTOLICA[i])))<40){
      data$PA.DIASTOLICA[i] <- NA
    }
  }
}
for(i in seq(data$PA.DIASTOLICA)){
  if(!is.na(as.numeric(sub(",",".",as.character(data$PA.DIASTOLICA[i]))))){
    if(as.numeric(sub(",",".",as.character(data$PA.DIASTOLICA[i])))>100){
      data$PA.DIASTOLICA[i] <- NA
    }
  }
}
data$PA.DIASTOLICA[data$PA.DIASTOLICA == ""] <- NA

#PPA 
data$PPA[data$PPA == "Não Calculado"] <- NA
data$PPA[data$PPA == "#VALUE!"] <- NA
data$PPA[data$PPA == ""] <- NA

#NORMAL.X.ANORMAL
data$NORMAL.X.ANORMAL[data$NORMAL.X.ANORMAL == "anormal"] <- "Anormal"
data$NORMAL.X.ANORMAL[data$NORMAL.X.ANORMAL == "Normais"] <- "Normal"
data$NORMAL.X.ANORMAL[data$NORMAL.X.ANORMAL == ""] <- NA

#SOPRO
data$SOPRO = as.character(data$SOPRO) #converte para caracter
data$SOPRO[data$SOPRO == "ausente"] <- "Ausente"
data$SOPRO[data$SOPRO == "contínuo"] <- "Contínuo"
data$SOPRO[data$SOPRO == "diastólico"] <- "Diastólico"
data$SOPRO[data$SOPRO == "sistólico"] <- "Sistólico"
data$SOPRO[data$SOPRO == ""] <- NA

#FC
data$FC = as.character(data$FC) #converte caracter
for(i in seq(data$FC)){
  m <- strsplit(as.character(data$FC[i]), split = "-")
  if(!i==7830){
    m <- as.numeric(unlist(m))
    data$FC[i] <- median(m)
  }
  else{
    m <- strsplit(as.character(data$FC[i]), split = " a ")
    m <- as.numeric(unlist(m))
    data$FC[i] <- median(m)
  }
}
data$FC = as.numeric(as.character(data$FC)) #converte numerico
#40-160
for(i in seq(data$FC)){
  if(!is.na(as.numeric(sub(",",".",as.character(data$FC[i]))))){
    if(as.numeric(sub(",",".",as.character(data$FC[i])))<40){
      data$FC[i] <- NA
    }
  }
}
for(i in seq(data$FC)){
  if(!is.na(as.numeric(sub(",",".",as.character(data$FC[i]))))){
    if(as.numeric(sub(",",".",as.character(data$FC[i])))>160){
      data$FC[i] <- NA
    }
  }
}
data$FC[data$FC == ""] <- NA

#HDA.1
data$HDA.1[data$HDA.1 == ""] <- NA

#HDA2
data$HDA2[data$HDA2 == ""] <- NA

#MOTIVO1
data$MOTIVO1[data$MOTIVO1 == ""] <- NA

#MOTIVO2
data$MOTIVO2[data$MOTIVO2 == ""] <- NA

#Convenio
data$Convenio[data$Convenio == ""] <- NA

#DN
data$DN[data$DN == ""] <- NA

#Atendimento
data$Atendimento[data$Atendimento == ""] <- NA

#IMC-Calculo
#subset(data, ! IMC == round(as.numeric(as.character(Peso))/((as.numeric(Altura)/100)^2)))

#Datas-correcao inteiro -> data
  #Atendimento
for(i in seq(data$Atendimento)){
  if(is.na(as.Date(as.character(data$Atendimento[i]),format="%d/%m/%y"))){
    data$Atendimento[i] <- as.factor(format(as.Date(as.numeric(as.character(data$Atendimento[i])), origin="1899-12-30"), format="%d/%m/%y"))
  }
}
  #DN
for(i in seq(data$DN)){
  if(is.na(as.Date(as.character(data$DN[i]),format="%d/%m/%y"))){
    data$DN[i] <- format(as.Date(as.numeric(as.character(data$DN[i])), origin="1899-12-30"), format="%d/%m/%y")
  }
}

#B2
data$B2[data$B2 == ""] <- NA

#Limpa idades devido a ultima funcao
for(i in seq(data$IDADE)){
    if(is.na(data$IDADE[i]) || is.na(data$DN[i]) || is.na(data$Atendimento[i])){ 
      data$IDADE[i] <- NA
      data$Atendimento[i] <- NA
      data$DN[i] <- NA
    }
}

#remove linhas com mais de 11 NA's (mais de 50%)
data <- data[rowSums(is.na(data)) <= 11, ]

#retira virgulas para WEKA
data$IDADE <- as.numeric(sub(",",".",as.character(data$IDADE)))
data$IDADE <- as.numeric(sub(",",".",as.character(data$Peso)))

#retira NA para WEKA
data <- sapply(data, as.character)
data[is.na(data)] <- " "