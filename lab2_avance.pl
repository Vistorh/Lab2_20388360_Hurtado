/*
TDA Fecha
lista con la forma
[DD,MM,YYYY] donde todos sus valores son numeros
*/
%primero revisa sus parametros
fecha(D,M,Y,_):- D>28,M=2,!,false.
fecha(D,M,Y,_):- D>30,(M=2;M=6;M=9;M=11),!,false.
%si no cumple  los anteriores entrara en esta 
fecha(D,M,Y,Salida):-
    number(D),number(M),number(Y),
    D>0,(D<31;D=31), M>0 ,( M<12 ; M=12),!,
    Salida = [D,M,Y].   
% SELECTORES
getFechaD([D,_,_],D).
getFechaM([_,M,_],M).
getFechaY([_,_,Y],Y).

/* 
TDA Usuarios
lista con la forma
[ID,nombre,pass,fecha,publicaciones]
integer x stringx x pass x fecha x lista
*/
% CONTRUCTOR

crearUsuario(ID,Username,Password,Fecha,Posts,Salida):-
    number(ID),not(ID<0),not(ID=0),string(Username),string(Password),is_list(Posts),
    Salida = [ID,Username,Password,Fecha,Posts].

% SELECTORES
getUsuarioID([ID,_,_,_,_],ID).    
getNombre([_,Username,_,_,_],Username).
getPass([_,_,Password,_,_],Password).
getUserFecha([_,_,_,Fecha,_],Fecha).
getUserPubli([_,_,_,_,Posts],Posts).

/*
TDA Paradigmadocs
lista de la forma
[nombre,fecha,usuarios,publicaciones,usuarioOnline]
string x fecha x lista x lista x integer
fecha(5,10,2021,F),crearRS("Googledocs",F,DSout).
*/
% CONSTRUCTOR
crearRS(Nombre,Fecha,SOut):-
    crearRedSocial(Nombre,Fecha,[],[],0,SOut).

crearRedSocial(Nombre,Fecha,Usuarios,Publicaciones,UsuarioOnline,SOut):-
    string(Nombre),number(UsuarioOnline),is_list(Fecha),is_list(Usuarios),is_list(Publicaciones),
    SOut = [Nombre,Fecha,Usuarios,Publicaciones,UsuarioOnline].

% SELECTORES
getNombreRedSocial([Nombre,_,_,_,_],Nombre).
getFechaRedSocial([_,Fecha,_,_,_],Fecha).
getUsuariosRedSocial([_,_,Usuarios,_,_],Usuarios).
getPublicacionesRedSocial([_,_,_,Publicaciones,_],Publicaciones).
getUsuarioOnline([_,_,_,_,UsuarioOnline],UsuarioOnline).

/*
TDA Publicaciones
lista de forma
[ID,formato texto, reacciones,comentarios,fecha,tags,shared]
integer x string x lista x lista x fecha x lista x lista
*/
% CONSTRUCTOR
crearPublicacion(ID,Texto,Reacts,Comments,Fecha,Tags,Shared,Salida):-
    number(ID),not(ID<0),not(ID=0),string(Texto),
    is_list(Reacts),is_list(Comments),is_list(Tags),is_list(Shared),
    Salida = [ID,Texto,Reacts,Comments,Fecha,Tags,Shared].

% SELECTORES
getPublicacionID([ID,_,_,_,_,_,_],ID).
getPublicacionTexto([_,Texto,_,_,_,_,_],Texto).
getPublicacionReacts([_,_,Reacts,_,_,_,_],Reacts).
getPublicacionComments([_,_,_,Comments,_,_,_],Comments).
getPublicacionFecha([_,_,_,_,Fecha,_,_],Fecha).
getPublicacionTags([_,_,_,_,_,Tags,_],Tags).
getPublicacionShared([_,_,_,_,_,_,Shared],Shared).


