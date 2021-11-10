# Este código permite obtener los valores para las encuestas de motivación 
# situacional y autoevaluación de los cuestionario realizados por Súa Jiménez Brenes.
#
# La base de datos está compuesta por una columna para cada pregunta y cada fila
# una persona. Las columnas deben de tener los nombres de cada ítem.
# 
#A continuación puede encontrar las preguntas del cuestionario de motivación 
# situacional con sus correspondientes ID.
#
#
# MI1	Las actividades de la clase sincrónica son interesantes.
# MI2	Realizo las actividades de la clase sincrónica porque son placenteras.
# MI3	Realizo las actividades de la clase sincrónica porque son actividades entretenidas.
# MI4	Realizo las actividades de la clase sincrónica porque me siento bien cuando las realizo.
# MEI1	Realizo las actividades de la clase sincrónica por mi propio bien.
# MEI2	Realizo las actividades de la clase sincrónica porque me hacen bien.
# MEI3	Realizo las actividades de la clase sincrónica por decisión personal.
# MEE1	Realizo las actividades de la clase sincrónica porque son obligatorias.
# MEE2	Realizo las actividades de la clase sincrónica porque tengo el deber de realizarlas.
# MEE3	Realizo las actividades de la clase sincrónica porque no me queda otra opción.
# AM1	Realizo las actividades de la clase sincrónica, aunque no estoy seguro de que sean la mejor opción.
# AM2	Quizás hay buenas razones para hacer las actividades de la clase sincrónica, pero yo no las veo.
# AM3	No lo sé, no veo como las actividades de la clase sincrónica pueden ayudarme.

# A continuación puede encontrar las preguntas del cuestionario 
# de autoevaluación con sus correspondientes ID.

# AU1	Establezco mis propios objetivos de aprendizaje para cada actividad.	
# AU2	Durante mi estudio individual, apliqué alguna estrategia de estudio.	
# AU3	Durante las actividades sincrónicas, soy capaz de resumir los puntos clave.	
# AU4	Tengo un manejo eficaz de mi estudio individual.	
# AU5	Logro evaluar mi aprendizaje basándome en los objetivos que me propuse.	
# AU6	Durante las actividades sincrónicas, fui capaz de generar preguntas relacionadas a la situación que se me presentó.	
# AU7	Durante las actividades sincrónicas, comuniqué mis ideas de forma clara.	
# AU8	Analicé las preguntas de las actividades sincrónicas basándome en la teoría que se me facilitó.	
# AU9	Logré generar asociaciones entre mis conocimientos previos y los adquiridos durante las actividades sincrónicas.	
# AU10	Durante las actividades sincrónicas, logré explicar con mis propias palabras conceptos de la teoría.	
# AU11	Durante las actividades sincrónicas, pude generar hipótesis que explicaran las situaciones o preguntas que se presentaron.	
	

#Valores de motivación situacional
modelo.motivacion  <- '  MI =~  MI3 + MI1 + MI2  + MI4 
		  	                     MEI=~ MEI1 + MEI2 + MEI3
                              MEE =~ MEE1 + MEE2 + 1*MEE3
                              AM =~  AM1 + AM2 + AM3'
m1 <- cfa(modelo.motivacion, data=data, ordered=TRUE, estimator="ULS") 
scores <- data.frame(lavPredict(m1, method = "EBM"))
factscores <- cbind(data$ID, scores)  
colnames(factscores) <- c("ID", "MI", "MEI", "MEE", "AM")
  
#Valores de autoevaluación
modelo.autoevaluacion  <- ' SA=~ AU10 + AU11 + AU9 + AU3  + AU8 + AU4
                            CO =~ AU6 + AU7
                            OE =~ AU5 + AU1 + AU2 ' 
cfa(modelo.autoevaluacion , data=data, ordered=TRUE, estimator="ULS") 
scores <- data.frame(lavPredict(m1, method = "EBM"))
factscores <- cbind(data$ID, scores)  
colnames(factscores) <- c("ID", "SA", "CO", "OE")
  