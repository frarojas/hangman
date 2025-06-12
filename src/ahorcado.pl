:- module(ahorcado, [elegir_palabra/1]).

:- use_module('../data/palabras', [
    obtener_palabra/1,
    agregar_palabra/1,
    cargar_palabras_guardadas/0
]).
:- use_module(library(lists)). % para maplist/2

% -----------------------------------------------------------------------------
% elegir_palabra(-Palabra)
% Carga la base, pide opción al usuario y unifica Palabra con la lista de letras
% -----------------------------------------------------------------------------
elegir_palabra(Palabra) :-
    % Carga las palabras previamente guardadas
    cargar_palabras_guardadas,
    nl, write('=== SELECCIÓN DE PALABRA ==='), nl,
    write('1. Palabra aleatoria'), nl,
    write('2. Ingresar mi propia palabra'), nl,
    write('Elige una opción (1 o 2): '), flush_output,
    get_single_char(Code),
    char_code(Char, Code),
    nl,
    atom_number(Char, Opcion),
    seleccionar_opcion(Opcion, Palabra),
    !,
    write('Palabra seleccionada: '), atomic_list_concat(Palabra, '', Atom), write(Atom), nl.

% -----------------------------------------------------------------------------
% seleccionar_opcion(+Opcion, -Palabra)
% -----------------------------------------------------------------------------
seleccionar_opcion(1, Palabra) :-
    obtener_palabra(Palabra),
    write('=> Usando palabra aleatoria.'), nl.

seleccionar_opcion(2, Palabra) :-
    write('Ingresa la palabra (terminada con punto, ej: casa.): '),
    read(Input),
    consumir_resto_linea, % Consume el newline restante
    nl,
    (   atom(Input) ->
        downcase_atom(Input, LowerAtom),
        atom_chars(LowerAtom, LowerChars),
        (   valid_palabra(LowerChars) ->
            agregar_palabra(LowerChars),
            Palabra = LowerChars,
            write('=> Palabra válida ingresada y guardada.'), nl
        ;   write('❌ Palabra inválida. Usando aleatoria.'), nl,
            obtener_palabra(Palabra)
        )
    ;   write('❌ Entrada inválida. Usando aleatoria.'), nl,
        obtener_palabra(Palabra)
    ).

seleccionar_opcion(_, Palabra) :-
    write('Opción inválida. Usando aleatoria.'), nl,
    obtener_palabra(Palabra).

% -----------------------------------------------------------------------------
% valid_palabra(+Lista)
% Lista no vacía de átomos de una letra minúscula
% -----------------------------------------------------------------------------
valid_palabra(Lista) :-
    Lista \= [],
    forall(member(L, Lista), (atom(L), atom_length(L,1), char_type(L, lower))).

consumir_resto_linea :-
    get_char(Char),
    ( Char == '\n' -> true         
    ; Char == end_of_file -> true  
    ; consumir_resto_linea         
    ).
