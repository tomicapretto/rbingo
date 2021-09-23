# rbingo XXX

* Todas los premios estan seleccionados por defecto en el panel donde se
comienza el juego.
* Los botones del tablero ahora usan CSS Flex, por lo que su tamano se 
adapta segun la pantalla.
* Ya no es posible clickear botones luego de que se hayan sorteado todos los 
premios.
* El nombre del vendedor esta en negrita en el resumen en la solapa de ventas.
* Se reportan la cantidad de aciertos en el premio al menor acierto. Esta aparece
en el modal que informa el premio, en el panel lateral derecho, y en el informe
final.


# rbingo 0.1.4

* Focus on "input$from" when insert/delete buttons are pressed in Sales panel.
* stringsAsFactors=FALSE passed explicitly in all data.frames
* Added boilla para pozo acumulado

# rbingo 0.1.3

* Added buttons within the tables to delete rows. They are used in Sales and Vendors.

# rbingo 0.1.2

* Removed session$allowReconnect("force") in `server.r`.
* Removed session$onSessionEnded(stopApp) in `server.r`.

# rbingo 0.1.1

* Added session$allowReconnect("force") in `server.r`.

# rbingo 0.1.0

* Added a `NEWS.md` file to track changes to the package.
