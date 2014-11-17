Murder-Rate-Map-Mexico
======================

This project is inspired in some great work from others (see [Diego Valle](https://www.diegovalle.net/) and [Jose Gonzalez](https://github.com/josecarlosgonz)), and by the commendable effort made by INEGI to made a murder data and other relevant statistics available.

This project is inspired in some great work from others (see Diego Valle (https://www.diegovalle.net/) and Jose Gonzalez (https://github.com/josecarlosgonz) work on this), and by the commendable effort made by INEGI to made a murder data and other relevant statistics available.


It consists on a couple of R scripts to prepare the data and plot murder rate maps for Mexico. The source information is (a) Homicide Murder Data from INEGI, (b) Population Data from INEGI and (c) Geostatistical Data (in the form of shapefiles/maps), also from INEGI.

You'll need to download the Census and Count data for Mexico from INEGI (TXT format), and place them in the [Primary_data] folder:
* Censo 2010. (http://www.inegi.org.mx/sistemas/consulta_resultados/iter2010.aspx)
* Conteo 2005. (http://www.inegi.org.mx/sistemas/consulta_resultados/iter2005.aspx)
* Censo 2000. (http://www.inegi.org.mx/sistemas/consulta_resultados/iter2000.aspx)
* Conteo 1995. (http://www.inegi.org.mx/sistemas/consulta_resultados/iter1995.aspx)
* Censo 1990. (http://www.inegi.org.mx/sistemas/consulta_resultados/iter1990.aspx)

On this folder you'll find a csv file called [Primary_data/Homicidios-INEGI.csv], that is a modified version of the raw information you can download from [INEGI](http://www.inegi.org.mx/sistemas/olap/Proyectos/bd/continuas/mortalidad/DefuncionesHom.asp?s=est&c=28820&proy=mortgral_dh); I've added Municipal and State codes to be able to match it to the population information and the shapefiles.

Additionally you'll need to download the necesary shapefiles from [INEGI](http://www.inegi.org.mx/geo/contenidos/geoestadistica/m_geoestadistico.aspx) to be able to plot the maps and place them in the [Shapefiles] folder:

On this folder you'll find a csv file called [Primary_data/Homicidios-INEGI.csv], that is a modified version of the raw information you can download from INEGI (http://www.inegi.org.mx/sistemas/olap/Proyectos/bd/continuas/mortalidad/DefuncionesHom.asp?s=est&c=28820&proy=mortgral_dh); I've added Municipal and State codes to be able to match it to the population information and the shapefiles.

Additionally you'll need to download the necesary shapefiles from INEGI (http://www.inegi.org.mx/geo/contenidos/geoestadistica/m_geoestadistico.aspx) to be able to plot the maps and place them in the [Shapefiles] folder:

* Marco geoestadístico municipal 1995 (Conteo de Población y Vivienda 1995)
* Marco geoestadístico municipal 2000 (Censo General de Población y Vivienda 2000)
* Marco geoestadístico 2010 versión 5.0.A a (Censo de Población y Vivienda 2010)