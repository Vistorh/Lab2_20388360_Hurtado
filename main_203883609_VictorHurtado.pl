/*
    ////////   TDA FECHA    \\\\\\\\\

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
    ////////   TDA Usuarios    \\\\\\\\\


lista con la forma
[ID,nombre,pass,fecha,publicaciones]
entero x string x x pass x fecha x lista
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
    ////////   TDA Paradigmadocs    \\\\\\\\\

lista de la forma
[nombre,fecha,usuarios,publicaciones,usuarioOnline]
string x fecha x lista x lista x entero
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
    ////////    TDA Publicaciones    \\\\\\\\\


lista de forma
[ID,fecha,titulo,contenido,shared]
entero x fecha x string x string x lista
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



% //////// AUXILIARES \\\\\\\\


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

% Dominio: lista x numero x var
% Descripcion: permite saber si existe una publicacion dada su id en una lista de publicaciones
id_to_publicacion([H|_],ID,Retorno):-
    getPublicacionID(H,ID2),
    (ID2 = ID),
    H = Retorno.

id_to_publicacion([_|T],ID,Retorno):-
    id_to_publicacion(T,ID,Retorno).

% Dominio: lista x numero
% Descripcion: permite saber si existe una ID en una lista
buscarID([H|_],ID):-
    (H = ID).
buscarID([_|T],ID):-
    buscarID(T,ID).          

% Dominio: lista x string x Var
% Descripcion: retorna la id de un usuario si es que existe.
buscarIDUsuario([H|_],Usuario,Retorno):-
    getNombre(H,Username),
    (Usuario = Username),
    getUsuarioID(H,Retorno).
buscarIDUsuario([_|T],Usuario,Retorno):-
    buscarIDUsuario(T,Usuario,Retorno).     

% Dominio: string x lista x Var
% Descripcion: a partir de un username, permite obtener el usuario completo.
username_to_usuario([H|_],Usuario,Retorno):-
    getNombre(H,Username),
    (Usuario = Username),
    Retorno = H.
username_to_usuario([_|T],Usuario,Retorno):-
    username_to_usuario(T,Usuario,Retorno). 

% Dominio: lista x number x Var
% Descripcion: a partir de un ID, obtiene el usuario completo de una lista.
id_to_usuario([H|_],IDaBuscar,Retorno):-
    getUsuarioID(H,IDEncontrada),
    (IDaBuscar = IDEncontrada),
    H = Retorno.

id_to_usuario([_|T],IDaBuscar,Retorno):-
    id_to_usuario(T,IDaBuscar,Retorno).               


% Dominio: lista x string x string
% Descripcion: permite buscar un usuario por su nombre de usuario y contrasena
buscarUsuarioPassword([H|_],Usuario,Password):-
    getNombre(H,Username),
    getPass(H,Contrasena),
    (Usuario = Username),
    (Password = Contrasena).
buscarUsuarioPassword([_|T],Usuario,Password):-
    buscarUsuarioPassword(T,Usuario,Password).

% Dominio: number x lista x Var
% Descripcion: permite eliminar un usuario o publicacion dada una ID de una lista
eliminar_por_ID(_,[],[]).

eliminar_por_ID(Y,[[Y|_]|Xs],Salida):-
    eliminar_por_ID(Y,Xs,Salida),!.

eliminar_por_ID(X,[Y|Xs],[Y|Salida]):-
    eliminar_por_ID(X,Xs,Salida).    

% Dominio: lista x lista
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

% Dominio: lista x Var
% Descripcion: Permite obtener el largo de una lista para el ID de un usuario o publicacion
id_counter(Lista,ID) :- id_counter(Lista,1,ID).

id_counter( []     , ID , ID ).
id_counter( [_|Lista] , T , ID ) :-
  T1 is T+1 ,
  id_counter(Lista,T1,ID).

% Dominio: lista x Var
% Descripcion: Funcion que permite obtener el largo de una lista
largo_lista(Lista,L) :- largo_lista(Lista,0,L).

largo_lista( []     , L , L ).
largo_lista( [_|Lista] , T , L ) :-
  T1 is T+1 ,
  largo_lista(Lista,T1,L).  


% /////////// FUNCIONES \\\\\\\\\\\                                                


% Dominio: paradigmadocs x fecha x string x string x paradigmadocs 
% Descripcion: Funcion que toma una plataforma y permite registrar un nuevo usuario
%fecha(20,11,2021,F),crearPL("Google",F,Plataforma1),paradigmaDocsRegister(Plataforma1,F,"UsuarioNuevo","Contrasena",Plataforma2).
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
    crearUsuario(ID,Username,Password,Fecha,[],NuevoUsuario),
    append(Usuarios,[NuevoUsuario],UsuariosNuevos),
    crearPlataforma(Nombre,FechaRS,UsuariosNuevos,Publicaciones,UsuarioOnline,RSout).

% Dominio: paradigmadocs x string x string x paradigmadocs
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


% Dominio: plataforma x fecha x string x string x lista x paradigmadocs
% Descripcion: funcion que permite a un usuario loggeado en la red social realizar una publicacion
%fecha(20,11,2021,F),crearPL("Google",F,Plataforma1),paradigmaDocsRegister(Plataforma1,F,"User1","Pass1",Plataforma2),paradigmaDocsRegister(Plataforma2,F,"User2","Pass2",Plataforma3),paradigmaDocsLogin(Plataforma3,"User2","Pass2",Plataforma4),paradigmaDocsPublicacion(Plataforma4,F,"Titulo","Mi primer post",[],Plataforma5).
paradigmaDocsPublicacion(RSin,Fecha,Titulo,Texto,Permisos,RSout):-
    string(Texto),
    is_list(Permisos),
    getNombrePlataforma(RSin,NombreRS),
    getFechaPlataforma(RSin,FechaRS),
    getUsuariosPlataforma(RSin,Usuarios),
    getPublicacionesPlataforma(RSin,Publicaciones),
    getUsuarioOnline(RSin,UsuarioOnline),
    (UsuarioOnline>0),
    buscarEtiquetados(Permisos,Usuarios),
    id_counter(Publicaciones,IDpublicacion),
    % Creo la publicacion
    crearPublicacion(IDpublicacion,Fecha,Titulo,Texto,Permisos,PublicacionNueva),
    % busco al usuario
    id_to_usuario(Usuarios,UsuarioOnline,Autor),
    % obtengo sus datos
    getNombre(Autor,AutorUsername),
    getPass(Autor,AutorPassword),
    getUserFecha(Autor,AutorFecha),
    getUserPubli(Autor,AutorPosts),
    % Agrego la publicacion nueva a la lista de publicaciones de la red social y del autor
    append(Publicaciones,[PublicacionNueva],PublicacionesNuevas),
    append(AutorPosts,[PublicacionNueva],AutorPostsNuevo),
    eliminar_por_ID(UsuarioOnline,Usuarios,UsuariosNuevos),!,
    crearUsuario(UsuarioOnline,AutorUsername,AutorPassword,AutorFecha,AutorPostsNuevo,NuevoAutor),
    append(UsuariosNuevos,[NuevoAutor],UsuariosFinal),
    crearPlataforma(NombreRS,FechaRS,UsuariosFinal,PublicacionesNuevas,0,RSout).


% Dominio: Plataforma x lista x entero x string x Plataforma
% Descripcion: funcion que permite otrogar permisos a otros usarios
%fecha(20,11,2021,F),crearPL("GoogleDocs",F,Plataforma1),paradigmaDocsRegister(Plataforma1,F,"Usuario1","Pass1",Plataforma2),paradigmaDocsRegister(Plataforma2,F,"Usuario2","Pass2",Plataforma3),paradigmaDocsRegister(Plataforma3,F,"Usuario3","Pass3",Plataforma4),paradigmaDocsLogin(Plataforma4,"Usuario2","Pass2",Plataforma5),paradigmaDocsPublicacion(Plataforma5,F,"Titulo1","Mi primer contenido",[],Plataforma6),paradigmaDocsLogin(Plataforma6,"Usuario3","Pass3",Plataforma7),paradigmaDocsPublicacion(Plataforma7,F,"Titulo2","otro texto",[],Plataforma8),paradigmaDocsLogin(Plataforma8,"Usuario1","Pass1",Plataforma9),paradigmaDocsShare(Plataforma9,["w"],1,"Usuario2",Plataforma10).
paradigmaDocsShare(RSin,Permisos,ID,Usuario,RSout):-
    number(ID),
    is_list(Permisos),
    string(Usuario),
    getNombrePlataforma(RSin,NombreRS),
    getFechaPlataforma(RSin,FechaRS),
    getUsuarioOnline(RSin,UsuarioOnline),
    getUsuariosPlataforma(RSin,Usuarios),
    (UsuarioOnline>0),!,
    buscar_user_por_ID(Usuarios,UsuarioOnline),
    getPublicacionesPlataforma(RSin,Publicaciones),
    id_to_publicacion(Publicaciones,ID,Publicacion),!,
    getPublicacionFecha(Publicacion,FechaP),
    getPublicacionTitulo(Publicacion,Titulo),
    getPublicacionContenido(Publicacion,Texto),
    getPublicacionShared(Publicacion,Shared),
    append(Shared,[Permisos],NewShared1),
    append(NewShared1,[Usuario],NewShared2),
    crearPublicacion(ID,FechaP,Titulo,Texto,NewShared2,NewPost),
    eliminar_por_ID(ID,Publicaciones,NuevasPublicaciones),
    append(NuevasPublicaciones,[NewPost],PublicacionesFinal),
    crearPlataforma(NombreRS,FechaRS,Usuarios,PublicacionesFinal,0,RSout).


% Dominio: Plataforma x entero x fecha x string x Plataforma
% Descripcion: funcion que permite agregar texto al final de una publicacion
paradigmaDocsAdd(RSin,ID,Fecha,Texto,RSout):-
    number(ID),
    is_list(Fecha),
    string(Texto),
    getNombrePlataforma(RSin,NombreRS),
    getFechaPlataforma(RSin,FechaRS),
    getUsuarioOnline(RSin,UsuarioOnline),
    getUsuariosPlataforma(RSin,Usuarios),
    (UsuarioOnline>0),!,
    buscar_user_por_ID(Usuarios,UsuarioOnline),
    getPublicacionesPlataforma(RSin,Publicaciones),
    id_to_publicacion(Publicaciones,ID,Publicacion),!,
    getPublicacionFecha(Publicacion,FechaP),
    getPublicacionTitulo(Publicacion,Titulo),
    getPublicacionContenido(Publicacion,Contenido1),
    getPublicacionShared(Publicacion,Shared),
    append(Contenido1,[Texto],NewContenido),
    crearPublicacion(ID,FechaP,Titulo,NewContenido,Shared,NewPost),
    eliminar_por_ID(ID,Publicaciones,NuevasPublicaciones),
    append(NuevasPublicaciones,[NewPost],PublicacionesFinal),
    crearPlataforma(NombreRS,FechaRS,Usuarios,PublicacionesFinal,0,RSout).


/*             //// EJEMPLOS \\\\                

**Registrarse**
fecha(20,11,2021,F),crearPL("Googledocs",F,Plataforma1),paradigmaDocsRegister(Plataforma1,F,"UsuarioNuevo","Contrasena",Plataforma2).
fecha(20,11,2021,F),crearPL("Googledocs",F,Plataforma1),paradigmaDocsRegister(Plataforma1,F,"Usuario","Contra",Plataforma2).
fecha(20,11,2021,F),crearPL("Googledocs",F,Plataforma1),paradigmaDocsRegister(Plataforma1,F,"UsuarioNuevo","Contrasena",Plataforma2),paradigmaDocsRegister(Plataforma2,F,"OtroUsuario","pass",Plataforma3).

**realizar una publicacion**
fecha(20,11,2021,F),crearPL("Googledocs",F,Plataforma1),paradigmaDocsRegister(Plataforma1,F,"UsuarioNuevo","Contrasena",Plataforma2),paradigmaDocsRegister(Plataforma2,F,"OtroUsuario","Pass2",Plataforma3),paradigmaDocsLogin(Plataforma3,"OtroUsuario","Pass2",Plataforma4),paradigmaDocsPublicacion(Plataforma4,F,"Titulo","Mi primer cont",[],Plataforma5).
fecha(20,11,2021,F),crearPL("Googledocs",F,Plataforma1),paradigmaDocsRegister(Plataforma1,F,"UsuarioNuevo","Contrasena",Plataforma2),paradigmaDocsRegister(Plataforma2,F,"OtroUsuario","Pass2",Plataforma3),paradigmaDocsLogin(Plataforma3,"UsuarioNuevo","Contrasena",Plataforma4),paradigmaDocsPublicacion(Plataforma4,F,"Titulo","Mi primer cont",[],Plataforma5).
fecha(20,11,2021,F),crearPL("Googledocs",F,Plataforma1),paradigmaDocsRegister(Plataforma1,F,"UsuarioNuevo","Contrasena",Plataforma2),paradigmaDocsRegister(Plataforma2,F,"Usuario","Contra",Plataforma3),paradigmaDocsLogin(Plataforma3,"Usuario","Contra",Plataforma4),paradigmaDocsPublicacion(Plataforma4,F,"Titulo","otro contenido",[],Plataforma5).

**funcion share**
fecha(20,11,2021,F),crearPL("GoogleDocs",F,Plataforma1),paradigmaDocsRegister(Plataforma1,F,"Usuario1","Pass1",Plataforma2),paradigmaDocsRegister(Plataforma2,F,"Usuario2","Pass2",Plataforma3),paradigmaDocsRegister(Plataforma3,F,"Usuario3","Pass3",Plataforma4),paradigmaDocsLogin(Plataforma4,"Usuario2","Pass2",Plataforma5),paradigmaDocsPublicacion(Plataforma5,F,"Titulo1","Mi primer contenido",[],Plataforma6),paradigmaDocsLogin(Plataforma6,"Usuario3","Pass3",Plataforma7),paradigmaDocsPublicacion(Plataforma7,F,"Titulo2","otro texto",[],Plataforma8),paradigmaDocsLogin(Plataforma8,"Usuario1","Pass1",Plataforma9),paradigmaDocsShare(Plataforma9,["w"],1,"Usuario3",Plataforma10).
fecha(20,11,2021,F),crearPL("GoogleDocs",F,Plataforma1),paradigmaDocsRegister(Plataforma1,F,"Usuario1","Pass1",Plataforma2),paradigmaDocsRegister(Plataforma2,F,"Usuario2","Pass2",Plataforma3),paradigmaDocsRegister(Plataforma3,F,"Usuario3","Pass3",Plataforma4),paradigmaDocsLogin(Plataforma4,"Usuario2","Pass2",Plataforma5),paradigmaDocsPublicacion(Plataforma5,F,"Titulo1","Mi primer contenido",[],Plataforma6),paradigmaDocsLogin(Plataforma6,"Usuario3","Pass3",Plataforma7),paradigmaDocsPublicacion(Plataforma7,F,"Titulo2","otro texto",[],Plataforma8),paradigmaDocsLogin(Plataforma8,"Usuario2","Pass2",Plataforma9),paradigmaDocsShare(Plataforma9,["w"],1,"Usuario1",Plataforma10).
fecha(20,11,2021,F),crearPL("GoogleDocs",F,Plataforma1),paradigmaDocsRegister(Plataforma1,F,"Usuario1","Pass1",Plataforma2),paradigmaDocsRegister(Plataforma2,F,"Usuario2","Pass2",Plataforma3),paradigmaDocsRegister(Plataforma3,F,"Usuario3","Pass3",Plataforma4),paradigmaDocsLogin(Plataforma4,"Usuario2","Pass2",Plataforma5),paradigmaDocsPublicacion(Plataforma5,F,"Titulo1","Mi primer contenido",[],Plataforma6),paradigmaDocsLogin(Plataforma6,"Usuario3","Pass3",Plataforma7),paradigmaDocsPublicacion(Plataforma7,F,"Titulo2","otro texto",[],Plataforma8),paradigmaDocsLogin(Plataforma8,"Usuario3","Pass3",Plataforma9),paradigmaDocsShare(Plataforma9,["w"],1,"Usuario2",Plataforma10).


**Funcion Add**
fecha(20,11,2021,F),crearPL("GoogleDocs",F,Plataforma1),paradigmaDocsRegister(Plataforma1,F,"Usuario1","Pass1",Plataforma2),paradigmaDocsRegister(Plataforma2,F,"Usuario2","Pass2",Plataforma3),paradigmaDocsRegister(Plataforma3,F,"Usuario3","Pass3",Plataforma4),paradigmaDocsLogin(Plataforma4,"Usuario2","Pass2",Plataforma5),paradigmaDocsPublicacion(Plataforma5,F,"Titulo1","Mi primer",[],Plataforma6),paradigmaDocsLogin(Plataforma6,"Usuario3","Pass3",Plataforma7),paradigmaDocsPublicacion(Plataforma7,F,"Titulo2","No puedo",[],Plataforma8),paradigmaDocsLogin(Plataforma8,"Usuario1","Pass1",Plataforma9),paradigmaDocsAdd(Plataforma9,1,F,"Agregado",Plataforma10).


*/
