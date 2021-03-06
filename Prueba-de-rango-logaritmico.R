#Se requieren las siguientes bibliotecas para el an�lisis. 


library("survival")
library("survminer")
library("Rcpp")

#Los datos del c�ncer de pulm�n se utilizaron del paquete de supervivencia. 

data("lung")
head(lung)

#Convertimos los datos a un formato utilizado por la librer�a survival.

datos = Surv(lung$time, lung$status)

#Hacemos el an�lisis haciendo el ajuste en cuanto al sexo.

model <- survfit(datos ~ sex, data = lung)

#Esta funci�n calcula el tiempo medio de supervivencia y sus intervalos de 
#confianza al 95% para cada grupo por su sexo.

summary(model)$table


#Calculamos el valor de $\chi^2$ para determinar si existe diferencia entre los 
#dos grupos.


surv_diff <- survdiff(Surv(time, status) ~ sex, data = lung)
surv_diff

#Obtenemos una $\chi^2=10.3$.
#Calculamos el valor cr�tico $\chi_c^2$ para un grado de libertad.
```{r}
qchisq(0.05,1,lower.tail=FALSE)
```
#Dado que $\chi^2>\chi_c^2$, rechazamos la hip�tesis nula.

ggsurvplot(fit,
           pval = FALSE, conf.int = TRUE,
           risk.table = FALSE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))


#*Conclusi�n:* existe diferencia entre la tasa de supervivencia entre ambos grupos. Para ser m�s espec�ficos, la taza de supervivencia del sexo 2 (426 d�as) es mayor a la del sexo 1 (207 d�as).
#Visualizamos esto gr�ficamente.