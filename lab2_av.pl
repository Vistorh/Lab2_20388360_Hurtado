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
fecha(5,10,2021,F),crearPL("Googledocs",F,DSout).
*/
% CONSTRUCTOR
crearPL(Nombre,Fecha,SOut):-
    crearPlataforma(Nombre,Fecha,[],[],0,SOut).

crearPlataforma(Nombre,Fecha,Usuarios,Publicaciones,UsuarioOnline,SOut):-
    string(Nombre),number(UsuarioOnline),is_list(Fecha),is_list(Usuarios),is_list(Publicaciones),
    SOut = [Nombre,Fecha,Usuarios,Publicaciones,UsuarioOnline].

% SELECTORES
getNombrePlataforma([Nombre,_,_,_,_],Nombre).
getFechaPlataforma([_,Fecha,_,_,_],Fecha).
getUsuariosPlataforma([_,_,Usuarios,_,_],Usuarios).
getPublicacionesPlataforma([_,_,_,Publicaciones,_],Publicaciones).
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

% Dominio: number x list x Var
% Descripcion: permite eliminar un usuario o publicacion dada una ID de una lista
eliminar_por_ID(_,[],[]).

eliminar_por_ID(Y,[[Y|_]|Xs],Salida):-
    eliminar_por_ID(Y,Xs,Salida),!.

eliminar_por_ID(X,[Y|Xs],[Y|Salida]):-
    eliminar_por_ID(X,Xs,Salida).    

% Dominio: lista x list
% Descripcion: busca de la lista de usuarios si la lista de etiquetados existe
buscarEtiquetados([],L).
buscarEtiquetados([H|T],L):-
    buscarUsuario(L,H),
    buscarEtiquetados(T,L).

% Dominio: fecha x Var
% Descripcion: funcion que recibe una fecha como TDA y la retorna como string
fecha_to_string([D,M,Y],Retorno):-
    string_concat(D,"/",S1),
    string_concat(M,"/",S2),
    string_concat(S1,S2,S3),
    string_concat(S3,Y,Retorno).    

% Dominio: list x Var
% Descripcion: Permite obtener el largo de una lista para el ID de un usuario o publicacion
id_counter(Lista,ID) :- id_counter(Lista,1,ID).

id_counter( []     , ID , ID ).
id_counter( [_|Lista] , T , ID ) :-
  T1 is T+1 ,
  id_counter(Lista,T1,ID).

% Dominio: list x Var
% Descripcion: Funcion que permite obtener el largo de una lista
largo_lista(Lista,L) :- largo_lista(Lista,0,L).

largo_lista( []     , L , L ).
largo_lista( [_|Lista] , T , L ) :-
  T1 is T+1 ,
  largo_lista(Lista,T1,L).  

% Dominio: list x string, Var
% Descripcion: Funcion intermediaria que toma un Usuario de una lista de usuarios y lo convierte a string
iterar_usuarios([H|T],String,Salida):-
    largo_lista(T,LargoT),
    not(LargoT = 0),!,
    usuario_to_string(H,UserString),
    string_concat(String,UserString,NewString),
    iterar_usuarios(T,NewString,Salida).    

iterar_usuarios([H|T],String,Salida):-
    usuario_to_string(H,UserString),
    string_concat(String,UserString,NewString),
    Salida = NewString.    


% Dominio: list x Var
% Descripcion: Funcion que imprime los datos de un usuario
usuario_to_string(User,Retorno):-
    getNombre(User,Username),
    getUserFecha(User,Fecha),
    fecha_to_string(Fecha,FechaCreacion),
    getUserPubli(User,Posts),
    largo_lista(Posts,CPosts),
    atom_concat(CPosts,"",CantPosts),
    string_concat("\nNombre de usuario: ",Username,S1),
    string_concat(S1,"\nCuenta creada en la fecha: ",S3),
    string_concat(S3,FechaCreacion,S4),
    string_concat(S7,"\nTiene un total de: ",S8),
    string_concat(S8,CantPosts,S9),
    string_concat(S9," publicaciones\n\n",S10),
    S10 = Retorno.

% Dominio: list x string x Var
% Descripcion: Funcion intermediara que permite tomar una publicacion y hacer el llamado de publicacion_to_string, asi con todas las publciaciones
iterar_publicaciones([H|T],String,Salida):-
    largo_lista(T,LargoT),
    not(LargoT = 0),!,
    publicacion_to_string(H,PublicacionString),
    string_concat(String,PublicacionString,NewString),
    iterar_publicaciones(T,NewString,Salida).

iterar_publicaciones([H|T],String,Salida):-
    publicacion_to_string(H,PublicacionString),
    string_concat(String,PublicacionString,NewString),
    Salida = NewString.    

% Dominio: Publicacion x var
% Descripcion: Funcion que genera un string con todos los datos de una publicacion
publicacion_to_string(Publicacion,Retorno):-
    getPublicacionTexto(Publicacion,Texto),
    getPublicacionReacts(Publicacion,Reacts),
    largo_lista(Reacts,LargoReacts),
    /* Cantidad de reacciones como string
    atom_concat(LargoReacts,"",CantReacts),
    getPublicacionComments(Publicacion,Comments),
    largo_lista(Comments,LargoComments),
    */
    
    /* Cantidad de comentarios como string
    atom_concat(LargoComments,"",CantComments),
    getPublicacionFecha(Publicacion,PublicacionFecha),
    fecha_to_string(PublicacionFecha,FechaString),
    getPublicacionTags(Publicacion,Tags),
    largo_lista(Tags,LargoTags),
    */

    % Cantidad de etiquetados como string
    atom_concat(LargoTags,"",CantTags),
    atomic_list_concat(Tags,",",TagsAtom),
    % Etiquetados como string
    atom_concat(TagsAtom,"",TagsString),
    getPublicacionShared(Publicacion,Shared),
    largo_lista(Shared,LargoShared),


    % Cantidad de compartidas como string
    atom_concat(LargoShared,"",CantShared),
    string_concat("\nPublicacion creada en la fecha: ",FechaString,S1),
    string_concat(S1,"\nTiene como contenido: ",S2),
    string_concat(S2,Texto,S3),
    string_concat(S9,"Tiene un total de ",S10),
    string_concat(S10,CantTags,S11),
    string_concat(S11," etiquetados, los cuales son: ",S12),
    string_concat(S12,TagsString,S13),
    S16 = Retorno.

/*
% Descripcion: Funcion que permite saber si una ID es un comentario de una publicacion
% Dominio: list x integer
buscar_comentario([H|_],ID):-
    getPublicacionComments(H,Comments),
    buscarID(Comments,ID).
buscar_comentario([_|T],ID):-
    buscar_comentario(T,ID).    
*/


/*
FUNCIONES                                                
*/

% Dominio: paradigmadocs x fecha x string x string x PaDocs 
% Descripcion: Funcion que toma una plataforma y permite registrar un nuevo usuario
paradigmaDocsRegister(RSin,Fecha,Username,Password,RSout):-
    string(Username),
    string(Password),
    getNombrePlataforma(RSin,Nombre),
    getFechaPlataforma(RSin,FechaRS),
    getUsuariosPlataforma(RSin,Usuarios),
    getPublicacionesPlataforma(RSin,Publicaciones),
    getUsuarioOnline(RSin,UsuarioOnline),
    not(buscarUsuario(Usuarios,Username)),!,
    id_counter(Usuarios,ID),
    crearUsuario(ID,Username,Password,Fecha,[],[],NuevoUsuario),
    append(Usuarios,[NuevoUsuario],UsuariosNuevos),
    crearPlataforma(Nombre,FechaRS,UsuariosNuevos,Publicaciones,UsuarioOnline,RSout).

% Dominio: plataforma x string x string x paradigmadocs
% Descripcion: funcion que permite iniciar sesion a un usuario en una plataforma
paradigmaDocsLogin(RSin,Username,Password,RSout):-
    string(Username),
    string(Password),
    getNombrePlataforma(RSin,Nombre),
    getFechaPlataforma(RSin,FechaRS),
    getUsuariosPlataforma(RSin,Usuarios),
    buscarUsuarioPassword(Usuarios,Username,Password),!,
    getPublicacionesPlataforma(RSin,Publicaciones),
    buscarIDUsuario(Usuarios,Username,ID),!,
    crearPlataforma(Nombre,FechaRS,Usuarios,Publicaciones,ID,RSout).