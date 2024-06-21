## dga_reports

El objetivo de este proyecto es generar códigos que permitan ordenar los reportes proporcionados por la Dirección General de Aguas de Chile (DGA).

Los códigos estan hehcos en el lenguaje R (enlace: https://www.r-project.org/). 

Los datos se obtienen desde la DGA a través de dos fuentes distintas: 
- Información Oficial Hidrometeorológica y de Calidad de Aguas en Línea (enlace: https://snia.mop.gob.cl/BNAConsultas/reportes)
- Sistema Hidrométrico en Línea (enlace: https://dga.mop.gob.cl/Paginas/hidrolineasatel.aspx) (Observación Importante: Todos los datos en línea son provisorios y están sujetos a revisiones y/o modificaciones)

Los errores / comentarios / preguntas / colaboración de cualquier tipo son muy bienvenidos.

## Información sobre el material: 

Para cada reporte hay dos archivos .R, main (código principal), y supplementary (material sumplementario).

Cada código main tiene un unico input (entrada): una ruta a una carpeta con reportes DGA, por ejemplo: "C:\\010_r\\dga_q_instant_reports_example".

## Material creado para los siguientes reportes DGA: 
* **Caudal Instantáneo**: dga_q_instant
* **Caudales Medios Diarios**: dga_q_daily
