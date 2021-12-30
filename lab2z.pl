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
[ID,fecha,titulo,contenido,shared]
integer x fecha x string x string x lista
*/
% CONSTRUCTOR
crearPublicacion(ID,Fecha,Titulo,Contenido,Shared,Salida):-
    number(ID),not(ID<0),not(ID=0),string(Titulo),string(Contenido),
    is_list(Shared),
    Salida = [ID,Fecha,Titulo,Contenido,Shared].

% SELECTORES
getPublicacionID([ID,_,_,_,_],ID).
getPublicacionFecha([_,Fecha,_,_,_],Fecha).
getPublicacionTitulo([_,_,Titulo,_,_],Titulo).
getPublicacionContenido([_,_,_,Contenido,_],Contenido).
getPublicacionShared([_,_,_,_,Shared],Shared).



% AUXILIARES


% Dominio: lista x String
% Descripcion: permite buscar en una lista de usuarios si este existe o no
buscarUsuario([H|_],Usuario):-
    getNombre(H,Username),
    (Usuario = Username).

buscarUsuario([_|T],Usuario):-
    buscarUsuario(T,Usuario).

% Dominio: lista x ID
% Descripcion: permite buscar en una lista de usuarios si este existe o no
buscar_user_por_ID([H|_],ID):-
    getUsuarioID(H,ID2),
    (ID = ID2).

buscar_user_por_ID([_|T],ID):-
    buscar_user_por_ID(T,ID).    

% Dominio: lista x number x var
% Descripcion: permite saber si existe una publicacion dada su id en una lista de publicaciones
id_to_publicacion([H|_],ID,Retorno):-
    getPublicacionID(H,ID2),
    (ID2 = ID),
    H = Retorno.

id_to_publicacion([_|T],ID,Retorno):-
    id_to_publicacion(T,ID,Retorno).

% Dominio: lista x number.
% Descripcion: permite saber si existe una ID en una lista
buscarID([H|_],ID):-
    (H = ID).
buscarID([_|T],ID):-
    buscarID(T,ID).          

% Dominio: list x string x Var
% Descripcion: retorna la id de un usuario si es que existe.
buscarIDUsuario([H|_],Usuario,Retorno):-
    getNombre(H,Username),
    (Usuario = Username),
    getUsuarioID(H,Retorno).
buscarIDUsuario([_|T],Usuario,Retorno):-
    buscarIDUsuario(T,Usuario,Retorno).     

% Dominio: string x list x Var
% Descripcion: a partir de un username, permite obtener el usuario completo.
username_to_usuario([H|_],Usuario,Retorno):-
    getNombre(H,Username),
    (Usuario = Username),
    Retorno = H.
username_to_usuario([_|T],Usuario,Retorno):-
    username_to_usuario(T,Usuario,Retorno). 

% Dominio: list x number x Var
% Descripcion: a partir de un ID, obtiene el usuario completo de una lista.
id_to_usuario([H|_],IDaBuscar,Retorno):-
    getUsuarioID(H,IDEncontrada),
    (IDaBuscar = IDEncontrada),
    H = Retorno.

id_to_usuario([_|T],IDaBuscar,Retorno):-
    id_to_usuario(T,IDaBuscar,Retorno).               


% Dominio: list x string x string
% Descripcion: permite buscar un usuario por su nombre de usuario y contrasena
buscarUsuarioPassword([H|_],Usuario,Password):-
    getNombre(H,Username),
    getPass(H,Contrasena),
    (Usuario = Username),
    (Password = Contrasena).
buscarUsuarioPassword([_|T],Usuario,Password):-
    buscarUsuarioPassword(T,Usuario,Password).

