%contructor de la socialNetwork
% se creara una lista con el nombre de la red, la fecha, una lista con los usuarios registrados y el usuario que este online
%socialNetwork = (string, [fecha],[usuarios], usuario)
socialNetwork(Nombre, Fecha, TDAUsuarios, UsuarioConectado,SOut):- 
SOut = [Nombre,Fecha,TDAUsuarios,UsuarioConectado].

%selectores para la socialNetwork
getRed([,Nombre,_,_,_],Nombre).
getFecha([,_,Fecha,_,_],Fecha).
getUsuarios([,_,_,TDAUsuarios,_],TDAUsuarios).
getOnline([,_,_,_,Online],Online).


/*TDA usuarios: 
   -Fecha
   -Nombre
   -Pass
   -Lista de publicaciones
   -Lista de follows

TDAUsuarios = (fecha, string, string, [], [])*/
TDAUsuarios = [Fecha, Nombre, Pass, TDApublicaciones , ListaSeguidores].

%selectores para el TDAUsuarios
getFechaU([,Fecha,_,_,_,_],Fecha).
getNombre([,_,Nombre,_,_,_],Nombre).
getPass([,_,_,Pass,_,_],Pass).
getPublicaciones([,_,_,_,TDApublicaciones,_],TDApublicaciones).
getSeguidores([,_,_,_,_,ListaSeguidores],ListaSeguidores).

/*TDA publicaciones: 
   -Fecha
   -Texto
   -Lista de Usuarios a los que se comparte el post
   -un por definir

TDApublicaciones = (fecha, string, [], un por definir)*/

%TDApublicaciones = (fecha, string, [], un por definir)
TDApublicaciones = [Fecha, Texto, ListaUsuarios, Share].

%Selectores para el TDApublicaciones
getFechaP([,Fecha,_,_,_],Fecha).
getPost([,_,Texto,_,_],Texto).
getCompartido([,_,_,ListaUsuarios,_],ListaUsuarios).
getShare([,_,_,_,Share],Share).