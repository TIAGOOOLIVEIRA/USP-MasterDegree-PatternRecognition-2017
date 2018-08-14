#install.packages('readr')
library(readr)

data_bank <-
  read_delim(
    "bank-additional-full.csv",
    ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )

#RENOMEANDO AS VARIÁVEIS
colnames(data_bank) = c(
  'idade',
  'trabalho',
  'estado_civil',
  'educacao',
  'inadimplente',
  'financiamento_imobiliario',
  'emprestimo_pessoal',
  'tipo_contato',
  'mes_ult_contato',
  'dia_da_semana_ult_contato',
  'duracao_ultimo_contato',
  'num_contatos_campanha' , #menos 1
  'num_dias_contato_campanha_anterior',
  'num_contatos_antes_campanha',
  'resultado_campanha_anterior',
  'variacao_emprego_macro',
  'indice_precos_macro',
  'indice_confianca_macro',
  'taxa_juros_macro',
  'num_emprego_macro',
  'var_resp'
)

#VARIÁVEIS PARA RETIRAR:
#duracao_ultimo_contato: INFORMAÇÃO FUTURA
#num_emprego_macro: SEM VARIANCIA
data_bank = data_bank[, colnames(data_bank) != c('duracao_ultimo_contato')]
data_bank = data_bank[, colnames(data_bank) != c('num_emprego_macro')]
data_bank = data_bank[, colnames(data_bank) != c('mes_ult_contato')]
data_bank = data_bank[, colnames(data_bank) != c('dia_da_semana_ult_contato')]
data_bank = data_bank[, colnames(data_bank) != c('variacao_emprego_macro')]


#TRATANDO VARIÁVEIS
data_bank$var_resp = ifelse(data_bank$var_resp == 'no', 0, 1)


#PCA-------------------------------------------------------
require(FactoMineR)

teste_pca=data_bank[,colnames(data_bank) %in% c('idade','num_contatos_campanha','num_contatos_antes_campanha','num_dias_contato_campanha_anterior','indice_precos_macro','indice_confianca_macro','taxa_juros_macro')]

comp=PCA(teste_pca,ncp = 4)
barplot(comp$eig[, 2], names.arg=1:nrow(comp$eig), 
        main = "PCA",
        xlab = "Componente Principal",
        ylab = "Percentual de variância",
        col ="steelblue")

#install.packages('dummies')
require(dummies)

data_dummy=cbind(teste_pca
                 ,trabalho=dummy(data_bank$trabalho,sep='_')
                 ,estado_civil=dummy(data_bank$estado_civil,sep='_')
                 ,educacao=dummy(data_bank$educacao,sep='_')
                 ,inadimplente=dummy(data_bank$inadimplente,sep='_')
                 ,financiamento_imobiliario=dummy(data_bank$financiamento_imobiliario,sep='_')
                 ,emprestimo_pessoal=dummy(data_bank$emprestimo_pessoal,sep='_')
                 ,tipo_contato=dummy(data_bank$tipo_contato,sep='_')
                 ,resultado_campanha_anterior=dummy(data_bank$resultado_campanha_anterior,sep='_'))

comp=PCA(data_dummy,ncp = 2)
comp$eig

barplot(comp$eig[, 2], names.arg=1:nrow(comp$eig), 
        main = "PCA",
        xlab = "Componente Principal",
        ylab = "Percentual de variância",
        col ="steelblue")



teste_mca=data_bank[,colnames(data_bank) %in% c('idade','num_contatos_campanha','num_contatos_antes_campanha','num_dias_contato_campanha_anterior','indice_precos_macro','indice_confianca_macro','taxa_juros_macro','trabalho'
                                                ,'estado_civil'
                                                ,'educacao'
                                                ,'inadimplente'
                                                ,'financiamento_imobiliario'
                                                ,'emprestimo_pessoal'
                                                ,'tipo_contato'
                                                ,'resultado_campanha_anterior')]

teste_mca$trabalho=as.factor(teste_mca$trabalho)
teste_mca$estado_civil=as.factor(teste_mca$estado_civil)
teste_mca$educacao=as.factor(teste_mca$educacao)
teste_mca$inadimplente=as.factor(teste_mca$inadimplente)
teste_mca$financiamento_imobiliario=as.factor(teste_mca$financiamento_imobiliario)
teste_mca$emprestimo_pessoal=as.factor(teste_mca$emprestimo_pessoal)
teste_mca$tipo_contato=as.factor(teste_mca$tipo_contato)
teste_mca$resultado_campanha_anterior=as.factor(teste_mca$resultado_campanha_anterior)

comp=MCA(teste_mca,quanti.sup=c(1,9,10,11,13,14,15))
comp$eig

barplot(comp$eig[, 2], names.arg=1:nrow(comp$eig), 
        main = "MCA",
        xlab = "Componente Principal",
        ylab = "Percentual de variância",
        col ="steelblue")



#RELIEFT----------------------------------------
#install.packages('FSelector')
require(FSelector)

weights=relief(var_resp~., data_bank, neighbours.count = 5, sample.size = 20)
print(weights)
subset <- cutoff.k(weights, 12)
subset=cutoff.k.percent(weights, 0.7)
print(subset)

weights_=relief(var_resp~idade, data_bank, neighbours.count = 5, sample.size = 20)
print(weights)



#Função para CROSSVALIDATION------------------------------------
install.packages('FSelector')
require(FSelector)

View(data_bank)

weights=relief(var_resp~., data_bank, neighbours.count = 5, sample.size = 20)
print(weights)
subset <- cutoff.k(weights, 12)
subset=cutoff.k.percent(weights, 0.7)
print(subset)


data_bank$educacao=as.factor(data_bank$educacao)
require(evtree)
t=ctree(var_resp~educacao,data_bank,maxdepth=5)
plot(t,type='simple')

str(data_bank)

weights_=relief(var_resp~idade, data_bank, neighbours.count = 5, sample.size = 20)
print(weights)



#SVM--------------------------------------------------

#BASE TOTAL
teste_total=data_bank
teste_total=as.data.frame(cbind(var_resp=teste_total$var_resp,model.matrix(var_resp~.,teste_total)))
teste_total$idade=scale(teste_total$idade, center = TRUE, scale = TRUE) 
teste_total$num_dias_contato_campanha_anterior=scale(teste_total$num_dias_contato_campanha_anterior, center = TRUE, scale = TRUE) 
teste_total$num_contatos_antes_campanha=scale(teste_total$num_contatos_antes_campanha, center = TRUE, scale = TRUE) 
teste_total$indice_precos_macro=scale(teste_total$indice_precos_macro, center = TRUE, scale = TRUE) 
teste_total$taxa_juros_macro=scale(teste_total$taxa_juros_macro, center = TRUE, scale = TRUE) 
teste_total$indice_confianca_macro=scale(teste_total$indice_confianca_macro, center = TRUE, scale = TRUE) 
teste_total=teste_total[,colnames(teste_total)!="(Intercept)"];teste_total=teste_total[,colnames(teste_total)!='trabalhoblue-collar'];teste_total=teste_total[,colnames(teste_total)!='trabalhoself-employed']
x=cross_validation(data=teste_total,k=3,resposta=teste_total$var_resp)

#PCA
data_pca=as.data.frame(cbind(var_resp=data_bank$var_resp,comp$ind$coord))
x=cross_validation(data=data_pca,k=3,resposta=data_pca$var_resp)

#RELIEF
teste_relief=data_bank
teste_relief=as.data.frame(cbind(var_resp=teste_total$var_resp,model.matrix(var_resp~.,teste_total)))
teste_relief$idade=scale(teste_total$idade, center = TRUE, scale = TRUE) 
teste_relief$num_dias_contato_campanha_anterior=scale(teste_total$num_dias_contato_campanha_anterior, center = TRUE, scale = TRUE) 
teste_relief$num_contatos_antes_campanha=scale(teste_total$num_contatos_antes_campanha, center = TRUE, scale = TRUE) 
teste_relief$indice_precos_macro=scale(teste_total$indice_precos_macro, center = TRUE, scale = TRUE) 
teste_relief$taxa_juros_macro=scale(teste_total$taxa_juros_macro, center = TRUE, scale = TRUE) 
teste_relief$indice_confianca_macro=scale(teste_total$indice_confianca_macro, center = TRUE, scale = TRUE) 
teste_relief=teste_total[,colnames(teste_total)!="(Intercept)"];teste_total=teste_total[,colnames(teste_total)!='trabalhoblue-collar'];teste_total=teste_total[,colnames(teste_total)!='trabalhoself-employed']
teste_relief=data_bank[,colnames(data_bank) %in% c('educacao','var_resp','indice_precos_macro',
                                                   'indice_confianca_macro','emprestimo_pessoal','taxa_juros_macro','tipo_contato',
                                                   'idade','num_contatos_campanha','num_dias_contato_campanha_anterior','resultado_campanha_anterior',
                                                   'num_contatos_antes_campanha')]
x=cross_validation(data=teste_relief,k=3,resposta=teste_relief$var_resp)


precisao=array()
sensibilidade=array()
erro_total=array()
k=1:length(x)
for(i in k)
{
  svm_=NULL
  start.time <- Sys.time()
  j=k
  j=j[-i]
  fold_train=NULL
  fold_train=rbind(x[[j[1]]],x[[j[2]]])
  fold_train=as.data.frame(fold_train)
  print(start.time)
  print(dim(fold_train))
  #svm_=svm(var_resp~.,data=fold_train, type='C-classification', kernel='radial', cost=3, gamma=10)
  svm_=svm(var_resp~.,data=fold_train, type='C-classification', kernel='polynomial',cost=2, gamma=1, degree=3, coef0=0)
  #svm_=svm(var_resp~.,data=fold_train, type='C-classification', kernel='sigmoid',cost=cost_[u], gamma=gamma_[u], coef0=coef0_[u])
  
  fold=NULL
  fold=x[i]
  fold=as.data.frame(fold)
  fit=predict(svm_,fold)
  precisao[i]=table(fit,fold$var_resp)[1,1]/sum(table(fit,fold$var_resp)[1,])
  sensibilidade[i]=table(fit,fold$var_resp)[1,1]/sum(table(fit,fold$var_resp)[,1])
  erro_total[i]=(table(fit,fold$var_resp)[1,2]+table(fit,fold$var_resp)[2,1])/sum(table(fit,fold$var_resp))
  end.time <- Sys.time()
  print(i)
  print(end.time - start.time)
}
print(svm_)
print(mean(erro_total))
print(mean(sensibilidade))
print(mean(precisao))



#NAIVE BAYES----------------------------------------------
require('e1071')


#BASE TOTAL
teste_total=data_bank
teste_total=as.data.frame(cbind(var_resp=teste_total$var_resp,model.matrix(var_resp~.,teste_total)))
teste_total$idade=scale(teste_total$idade, center = TRUE, scale = TRUE) 
teste_total$num_dias_contato_campanha_anterior=scale(teste_total$num_dias_contato_campanha_anterior, center = TRUE, scale = TRUE) 
teste_total$num_contatos_antes_campanha=scale(teste_total$num_contatos_antes_campanha, center = TRUE, scale = TRUE) 
teste_total$indice_precos_macro=scale(teste_total$indice_precos_macro, center = TRUE, scale = TRUE) 
teste_total$taxa_juros_macro=scale(teste_total$taxa_juros_macro, center = TRUE, scale = TRUE) 
teste_total$indice_confianca_macro=scale(teste_total$indice_confianca_macro, center = TRUE, scale = TRUE) 
teste_total=teste_total[,colnames(teste_total)!="(Intercept)"];teste_total=teste_total[,colnames(teste_total)!='trabalhoblue-collar'];teste_total=teste_total[,colnames(teste_total)!='trabalhoself-employed']
x=cross_validation(data=teste_total,k=3,resposta=teste_total$var_resp)

#PCA
data_pca=as.data.frame(cbind(var_resp=data_bank$var_resp,comp$ind$coord))
x=cross_validation(data=data_pca,k=3,resposta=data_pca$var_resp)

#RELIEF
teste_relief=data_bank
teste_relief=as.data.frame(cbind(var_resp=teste_total$var_resp,model.matrix(var_resp~.,teste_total)))
teste_relief$idade=scale(teste_total$idade, center = TRUE, scale = TRUE) 
teste_relief$num_dias_contato_campanha_anterior=scale(teste_total$num_dias_contato_campanha_anterior, center = TRUE, scale = TRUE) 
teste_relief$num_contatos_antes_campanha=scale(teste_total$num_contatos_antes_campanha, center = TRUE, scale = TRUE) 
teste_relief$indice_precos_macro=scale(teste_total$indice_precos_macro, center = TRUE, scale = TRUE) 
teste_relief$taxa_juros_macro=scale(teste_total$taxa_juros_macro, center = TRUE, scale = TRUE) 
teste_relief$indice_confianca_macro=scale(teste_total$indice_confianca_macro, center = TRUE, scale = TRUE) 
teste_relief=teste_total[,colnames(teste_total)!="(Intercept)"];teste_total=teste_total[,colnames(teste_total)!='trabalhoblue-collar'];teste_total=teste_total[,colnames(teste_total)!='trabalhoself-employed']
teste_relief=data_bank[,colnames(data_bank) %in% c('educacao','var_resp','indice_precos_macro',
                                                   'indice_confianca_macro','emprestimo_pessoal','taxa_juros_macro','tipo_contato',
                                                   'idade','num_contatos_campanha','num_dias_contato_campanha_anterior','resultado_campanha_anterior',
                                                   'num_contatos_antes_campanha')]
x=cross_validation(data=teste_relief,k=3,resposta=teste_relief$var_resp)

precisao=array()
sensibilidade=array()
erro_total=array()
k=1:length(x)
for(i in k)
{
  svm_=NULL
  start.time <- Sys.time()
  j=k
  j=j[-i]
  fold_train=NULL
  fold_train=rbind(x[[j[1]]],x[[j[2]]])
  fold_train=as.data.frame(fold_train)
  fold_train$var_resp=as.factor(fold_train$var_resp)
  print(start.time)
  print(dim(fold_train))
  svm_=naiveBayes(var_resp ~ ., data = fold_train)
  
  fold=NULL
  fold=x[i]
  fold=as.data.frame(fold)
  fold$var_resp=as.factor(fold$var_resp)  
  fit=predict(svm_,fold)
  precisao[i]=table(fit,fold$var_resp)[1,1]/sum(table(fit,fold$var_resp)[1,])
  sensibilidade[i]=table(fit,fold$var_resp)[1,1]/sum(table(fit,fold$var_resp)[,1])
  erro_total[i]=(table(fit,fold$var_resp)[1,2]+table(fit,fold$var_resp)[2,1])/sum(table(fit,fold$var_resp))
  end.time <- Sys.time()
  print(i)
  print(end.time - start.time)
}
print(svm_)
print(mean(erro_total))
print(mean(sensibilidade))
print(mean(precisao))


#REDE NEURAL---------------------------------------------------------------

#BASE TOTAL
teste_total=data_bank
teste_total=as.data.frame(cbind(var_resp=teste_total$var_resp,model.matrix(var_resp~.,teste_total)))
teste_total$idade=scale(teste_total$idade, center = TRUE, scale = TRUE) 
teste_total$num_dias_contato_campanha_anterior=scale(teste_total$num_dias_contato_campanha_anterior, center = TRUE, scale = TRUE) 
teste_total$num_contatos_antes_campanha=scale(teste_total$num_contatos_antes_campanha, center = TRUE, scale = TRUE) 
teste_total$indice_precos_macro=scale(teste_total$indice_precos_macro, center = TRUE, scale = TRUE) 
teste_total$taxa_juros_macro=scale(teste_total$taxa_juros_macro, center = TRUE, scale = TRUE) 
teste_total$indice_confianca_macro=scale(teste_total$indice_confianca_macro, center = TRUE, scale = TRUE) 
teste_total=teste_total[,colnames(teste_total)!="(Intercept)"];teste_total=teste_total[,colnames(teste_total)!='trabalhoblue-collar'];teste_total=teste_total[,colnames(teste_total)!='trabalhoself-employed']
x=cross_validation(data=teste_total,k=3,resposta=teste_total$var_resp)

#PCA
data_pca=as.data.frame(cbind(var_resp=data_bank$var_resp,comp$ind$coord))
x=cross_validation(data=data_pca,k=3,resposta=data_pca$var_resp)

#RELIEF
teste_relief=data_bank
teste_relief=as.data.frame(cbind(var_resp=teste_total$var_resp,model.matrix(var_resp~.,teste_total)))
teste_relief$idade=scale(teste_total$idade, center = TRUE, scale = TRUE) 
teste_relief$num_dias_contato_campanha_anterior=scale(teste_total$num_dias_contato_campanha_anterior, center = TRUE, scale = TRUE) 
teste_relief$num_contatos_antes_campanha=scale(teste_total$num_contatos_antes_campanha, center = TRUE, scale = TRUE) 
teste_relief$indice_precos_macro=scale(teste_total$indice_precos_macro, center = TRUE, scale = TRUE) 
teste_relief$taxa_juros_macro=scale(teste_total$taxa_juros_macro, center = TRUE, scale = TRUE) 
teste_relief$indice_confianca_macro=scale(teste_total$indice_confianca_macro, center = TRUE, scale = TRUE) 
teste_relief=teste_total[,colnames(teste_total)!="(Intercept)"];teste_total=teste_total[,colnames(teste_total)!='trabalhoblue-collar'];teste_total=teste_total[,colnames(teste_total)!='trabalhoself-employed']
teste_relief=data_bank[,colnames(data_bank) %in% c('educacao','var_resp','indice_precos_macro',
                                                   'indice_confianca_macro','emprestimo_pessoal','taxa_juros_macro','tipo_contato',
                                                   'idade','num_contatos_campanha','num_dias_contato_campanha_anterior','resultado_campanha_anterior',
                                                   'num_contatos_antes_campanha')]
x=cross_validation(data=teste_relief,k=3,resposta=teste_relief$var_resp)


View(data_pca)

#RODANDO O MODELO
precisao=array();sensibilidade=array();erro_total=array()
neuronios=4 #ALTERAR
taxa_apren=0.1 #ALTERAR
k=1:length(x)
for(i in k)
{
  nn=NULL
  start.time <- Sys.time()
  j=k
  j=j[-i]
  fold_train=NULL
  fold_train=rbind(x[[j[1]]],x[[j[2]]])
  fold_train=as.data.frame(fold_train)
  print(start.time)
  print(dim(fold_train))
  nn=nnet(var_resp ~ ., data = fold_train, size = neuronios, rang = 0.1, decay = taxa_apren, maxit = 500)
  
  fold=NULL
  fold=x[i]
  fold=as.data.frame(fold)
  fit=ifelse(predict(nn,newdata=fold)>0.5,1,0)
  precisao[i]=table(fit,fold$var_resp)[1,1]/sum(table(fit,fold$var_resp)[1,])
  sensibilidade[i]=table(fit,fold$var_resp)[1,1]/sum(table(fit,fold$var_resp)[,1])
  erro_total[i]=(table(fit,fold$var_resp)[1,2]+table(fit,fold$var_resp)[2,1])/sum(table(fit,fold$var_resp))
  end.time <- Sys.time()
  print(i)
  print(end.time - start.time)
}
print(nn$n)
print(nn$decay)
print(mean(erro_total))
print(mean(sensibilidade))
print(mean(precisao))






