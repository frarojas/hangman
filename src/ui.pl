% -----------------------------------------------------------------------------
% ui.pl
% Interfaz de usuario para el juego del ahorcado
% -----------------------------------------------------------------------------

:- module(ui, [
    bienvenida/0,
    mostrar_estado_juego/0,
    mostrar_palabra_oculta/0,
    mostrar_lista_letras_usadas/0,
    pedir_letra_juego/0,
    manejar_victoria/0,
    manejar_derrota/0,
    preguntar_jugar_de_nuevo/0
]).

:- use_module(ahorcado, []).
:- use_module(library(lists)).

% -----------------------------------------------------------------------------
% bienvenida/0
% Muestra un mensaje de bienvenida al jugador
% -----------------------------------------------------------------------------
bienvenida :-
    nl, write('============================='), nl,
    write('   Bienvenido al Ahorcado!'), nl,
    write('============================='), nl.

% -----------------------------------------------------------------------------
% mostrar_estado_juego/0
% Muestra el estado actual del juego: intentos restantes, letras usadas y palabra oculta
% -----------------------------------------------------------------------------
mostrar_estado_juego :-
    intentos_restantes(Intentos),
    nl, nl, write('------------------------------'), nl,
    write('Intentos restantes: '), write(Intentos), nl,
    write('Letras usadas: '), mostrar_lista_letras_usadas, nl,
    mostrar_palabra_oculta,
    nl, write('------------------------------'), nl.

% -----------------------------------------------------------------------------
% mostrar_palabra_oculta/0
% Muestra la palabra oculta con las letras adivinadas y guiones bajos para las no adivinadas
% -----------------------------------------------------------------------------
mostrar_palabra_oculta :-
    palabra_secreta(PalabraSecreta),
    letras_adivinadas(Adivinadas),
    write('Palabra: '),
    mostrar_letras_palabra(PalabraSecreta, Adivinadas),
    nl.

% -----------------------------------------------------------------------------
% mostrar_letras_palabra(+Palabra, +Adivinadas)
% Muestra las letras de la palabra, mostrando guiones bajos para las no adivinadas
% -----------------------------------------------------------------------------
mostrar_letras_palabra([], _).
mostrar_letras_palabra([Letra|Resto], Adivinadas) :-
    (   member(Letra, Adivinadas) ->
        write(Letra)
    ;   write('_')
    ),
    write(' '), % Espacio entre letras o guiones
    mostrar_letras_palabra(Resto, Adivinadas).

% -----------------------------------------------------------------------------
% mostrar_lista_letras_usadas/0
% Muestra las letras que el jugador ha adivinado
% -----------------------------------------------------------------------------
mostrar_lista_letras_usadas :-
    letras_adivinadas(Usadas),
    (   Usadas == [] ->
        write('Ninguna')
    ;   sort(Usadas, UsadasOrdenadas), % Opcional: mostrar letras usadas en orden
        atomic_list_concat(UsadasOrdenadas, ', ', Atom),
        write(Atom)
    ).

% -----------------------------------------------------------------------------
% pedir_letra_juego/0
% Pide al jugador que ingrese una letra y la procesa
% -----------------------------------------------------------------------------
pedir_letra_juego :-
    nl, write('Ingresa una letra: '), flush_output,
    get_single_char(Code),
    char_code(CharAtom, Code),
    downcase_atom(CharAtom, Letra), % Convertir a minúscula
    nl,
    procesar_letra(Letra). % procesar_letra ya maneja la validación y el feedback

% -----------------------------------------------------------------------------
% manejar_victoria/0
% Muestra mensaje de victoria y pregunta si quiere jugar de nuevo
% -----------------------------------------------------------------------------
manejar_victoria :-
    nl, write(' Felicidades, Has ganado!'), nl,
    palabra_secreta(Palabra),
    write('La palabra era: '), atomic_list_concat(Palabra, '', Atom), write(Atom), nl,
    preguntar_jugar_de_nuevo.

% -----------------------------------------------------------------------------
% manejar_derrota/0
% Muestra mensaje de derrota y pregunta si quiere jugar de nuevo
% -----------------------------------------------------------------------------
manejar_derrota :-
    nl, write('Te has quedado sin intentos. Has perdido!'), nl,
    palabra_secreta(Palabra),
    write('La palabra era: '), atomic_list_concat(Palabra, '', Atom), write(Atom), nl,
    preguntar_jugar_de_nuevo.

% -----------------------------------------------------------------------------
% preguntar_jugar_de_nuevo/0
% Pregunta al jugador si quiere jugar de nuevo o salir
% -----------------------------------------------------------------------------
preguntar_jugar_de_nuevo :-
    nl, write('Quieres jugar de nuevo? (s/n): '), flush_output,
    get_single_char(Code),
    char_code(Char, Code),
    nl,
    (   (Char == 's' ; Char == 'S') ->
        jugar % Reinicia el juego
    ;   (Char == 'n' ; Char == 'N') ->
        write('Gracias por jugar!.'), nl,
        halt % Termina la ejecución de Prolog
    ;   write('Opción no válida. Por favor, ingresa s o n.'), nl,
        preguntar_jugar_de_nuevo % Pregunta de nuevo
    ).
