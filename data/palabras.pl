% -----------------------------------------------------------------------------
% palabras.pl
% Base de datos de palabras para el juego del Ahorcado
% -----------------------------------------------------------------------------

:- module(palabras, [
    palabra/1,
    obtener_palabra/1,
    agregar_palabra/1,
    listar_palabras/0,
    guardar_palabras/0,
    cargar_palabras_guardadas/0
]).

:- dynamic palabra/1.

% Palabras iniciales (solo se cargan la primera vez)
palabra([a,h,o,r,c,a,d,o]).
palabra([p,r,o,l,o,g]).
palabra([c,o,m,p,u,t,a,d,o,r]).
palabra([l,o,g,i,c,a]).
palabra([i,n,t,e,l,i,g,e,n,c,i,a]).
palabra([d,a,t,o,s]).
palabra([f,u,n,c,i,o,n]).
palabra([a,l,g,o,r,i,t,m,o]).
palabra([v,a,r,i,a,b,l,e]).
palabra([p,r,e,d,i,c,a,d,o]).

% -----------------------------------------------------------------------------
% cargar_palabras_guardadas_loop(+Stream)
% Lee términos del Stream y los agrega a la base de conocimiento.
% -----------------------------------------------------------------------------
cargar_palabras_guardadas_loop(Stream) :-
    read_term(Stream, Term, []), % Usar read_term para manejar correctamente el fin de archivo
    (   Term == end_of_file ->
        true
    ;   ( functor(Term, palabra, 1) -> % Asegurarse que es un término palabra/1
            assertz(Term)
        ;   write('Advertencia: Término inesperado en palabras_guardadas.pl: '), write(Term), nl
        ),
        cargar_palabras_guardadas_loop(Stream)
    ).

% -----------------------------------------------------------------------------
% obtener_palabra(-Palabra)
% Selecciona aleatoriamente una palabra de la base de conocimiento
% -----------------------------------------------------------------------------
obtener_palabra(Palabra) :-
    findall(W, palabra(W), Lista),
    Lista \= [],
    random_member(Palabra, Lista).

% -----------------------------------------------------------------------------
% agregar_palabra(+Palabra)
% Agrega una nueva palabra a la base, valida que sea lista de letras minúsculas
% y evita duplicados. Guarda automáticamente en disco.
% -----------------------------------------------------------------------------
agregar_palabra(Nueva) :-
    \+ es_palabra_valida(Nueva),
    write(' Palabra inválida. Usa solo letras minúsculas: [c,a,s,a]'), nl, !.

agregar_palabra(Nueva) :-
    palabra(Nueva),
    write(' La palabra ya existe.'), nl, !.

agregar_palabra(Nueva) :-
    assertz(palabra(Nueva)),
    guardar_palabras,
    write(' Palabra agregada exitosamente.'), nl.

% -----------------------------------------------------------------------------
% es_palabra_valida(+Lista)
% Verifica que sea una lista no vacía de letras minúsculas (átomos de 1 char)
% -----------------------------------------------------------------------------
es_palabra_valida([]) :- fail.
es_palabra_valida(Lista) :-
    is_list(Lista),
    forall(member(L, Lista), letra_valida(L)).

letra_valida(L) :-
    atom(L),
    atom_length(L, 1),
    char_type(L, lower).

% -----------------------------------------------------------------------------
% listar_palabras/0
% Imprime todas las palabras registradas en la base
% -----------------------------------------------------------------------------
listar_palabras :-
    findall(W, palabra(W), Lista),
    write(' Palabras registradas:'), nl,
    mostrar_lista(Lista), nl.

mostrar_lista([]).
mostrar_lista([X | R]) :-
    atomic_list_concat(X, '', PalabraAtom),
    write('- '), write(PalabraAtom), nl,
    mostrar_lista(R).

% -----------------------------------------------------------------------------
% guardar_palabras/0
% Guarda todas las palabras actuales en un archivo persistente
% -----------------------------------------------------------------------------
guardar_palabras :-
    open('data/palabras_guardadas.pl', write, Stream),
    % No escribir :- dynamic palabra/1. aquí, solo los hechos.
    % listing(palabra/1) ya hace esto correctamente.
    set_output(Stream),
    listing(palabra/1),
    close(Stream),
    set_output(user_output), % Restaurar salida estándar
    write(' Base de palabras guardada en data/palabras_guardadas.pl'), nl.

% -----------------------------------------------------------------------------
% cargar_palabras_guardadas/0
% Carga las palabras guardadas previamente (si el archivo existe)
% -----------------------------------------------------------------------------
cargar_palabras_guardadas :-
    exists_file('data/palabras_guardadas.pl'),
    !, % Si el archivo existe, nos comprometemos a esta cláusula.
    % Limpiar las palabras existentes si las guardadas deben reemplazarlas por completo.
    % Esto imita el comportamiento de "overwrite" de consult/1.
    retractall(palabra(_)),
    setup_call_cleanup(
        open('data/palabras_guardadas.pl', read, Stream),
        cargar_palabras_guardadas_loop(Stream), % Usar el nuevo loop de carga
        close(Stream)
    ),
    write(' Palabras guardadas cargadas correctamente.'), nl.
cargar_palabras_guardadas :-
    % Esta cláusula se ejecuta si palabras_guardadas.pl no existe.
    % Las palabras iniciales definidas en este archivo permanecerán activas.
    write('ℹ No se encontraron palabras guardadas, usando palabras iniciales del código.'), nl.
