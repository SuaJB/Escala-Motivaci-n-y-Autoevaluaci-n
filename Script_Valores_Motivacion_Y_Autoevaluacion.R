# Este c�digo permite obtener los valores para las encuestas de motivaci�n 
# situacional y autoevaluaci�n de los cuestionario realizados por S�a Jim�nez Brenes.
#
# La base de datos est� compuesta por una columna para cada pregunta y cada fila
# una persona. Las columnas deben de tener los nombres de cada �tem.
# 
#A continuaci�n puede encontrar las preguntas del cuestionario de motivaci�n 
# situacional con sus correspondientes ID.
#
#
# MI1	Las actividades de la clase sincr�nica son interesantes.
# MI2	Realizo las actividades de la clase sincr�nica porque son placenteras.
# MI3	Realizo las actividades de la clase sincr�nica porque son actividades entretenidas.
# MI4	Realizo las actividades de la clase sincr�nica porque me siento bien cuando las realizo.
# MEI1	Realizo las actividades de la clase sincr�nica por mi propio bien.
# MEI2	Realizo las actividades de la clase sincr�nica porque me hacen bien.
# MEI3	Realizo las actividades de la clase sincr�nica por decisi�n personal.
# MEE1	Realizo las actividades de la clase sincr�nica porque son obligatorias.
# MEE2	Realizo las actividades de la clase sincr�nica porque tengo el deber de realizarlas.
# MEE3	Realizo las actividades de la clase sincr�nica porque no me queda otra opci�n.
# AM1	Realizo las actividades de la clase sincr�nica, aunque no estoy seguro de que sean la mejor opci�n.
# AM2	Quiz�s hay buenas razones para hacer las actividades de la clase sincr�nica, pero yo no las veo.
# AM3	No lo s�, no veo como las actividades de la clase sincr�nica pueden ayudarme.

# A continuaci�n puede encontrar las preguntas del cuestionario 
# de autoevaluaci�n con sus correspondientes ID.

# AU1	Establezco mis propios objetivos de aprendizaje para cada actividad.	
# AU2	Durante mi estudio individual, apliqu� alguna estrategia de estudio.	
# AU3	Durante las actividades sincr�nicas, soy capaz de resumir los puntos clave.	
# AU4	Tengo un manejo eficaz de mi estudio individual.	
# AU5	Logro evaluar mi aprendizaje bas�ndome en los objetivos que me propuse.	
# AU6	Durante las actividades sincr�nicas, fui capaz de generar preguntas relacionadas a la situaci�n que se me present�.	
# AU7	Durante las actividades sincr�nicas, comuniqu� mis ideas de forma clara.	
# AU8	Analic� las preguntas de las actividades sincr�nicas bas�ndome en la teor�a que se me facilit�.	
# AU9	Logr� generar asociaciones entre mis conocimientos previos y los adquiridos durante las actividades sincr�nicas.	
# AU10	Durante las actividades sincr�nicas, logr� explicar con mis propias palabras conceptos de la teor�a.	
# AU11	Durante las actividades sincr�nicas, pude generar hip�tesis que explicaran las situaciones o preguntas que se presentaron.	
	

#Valores de motivaci�n situacional
modelo.motivacion  <- '  MI =~  MI3 + MI1 + MI2  + MI4 
		  	                     MEI=~ MEI1 + MEI2 + MEI3
                              MEE =~ MEE1 + MEE2 + 1*MEE3
                              AM =~  AM1 + AM2 + AM3'
m1 <- cfa(modelo.motivacion, data=data, ordered=TRUE, estimator="ULS") 
scores <- data.frame(lavPredict(m1, method = "EBM"))
factscores <- cbind(data$ID, scores)  
colnames(factscores) <- c("ID", "MI", "MEI", "MEE", "AM")
  
#Valores de autoevaluaci�n
modelo.autoevaluacion  <- ' SA=~ AU10 + AU11 + AU9 + AU3  + AU8 + AU4
                            CO =~ AU6 + AU7
                            OE =~ AU5 + AU1 + AU2 ' 
cfa(modelo.autoevaluacion , data=data, ordered=TRUE, estimator="ULS") 
scores <- data.frame(lavPredict(m1, method = "EBM"))
factscores <- cbind(data$ID, scores)  
colnames(factscores) <- c("ID", "SA", "CO", "OE")
  