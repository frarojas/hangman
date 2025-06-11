% -----------------------------------------------------------------------------
% logica.pl
% Gestión del estado y lógica del juego del ahorcado
% -----------------------------------------------------------------------------

:- module(logica, [
    iniciar_juego/0,
    procesar_letra/1,
    verificar_victoria/0,
    verificar_derrota/0
]).

:- dynamic letras_adivinadas/1.
:- dynamic intentos_restantes/1.
:- dynamic palabra_secreta/1.

:- use_module(ahorcado, [elegir_palabra/1]).

% -----------------------------------------------------------------------------
% iniciar_juego/0
% Inicializa el estado del juego: selecciona palabra, reinicia intentos y letras
% -----------------------------------------------------------------------------
iniciar_juego :-
    retractall(letras_adivinadas(_)),
    retractall(intentos_restantes(_)),
    retractall(palabra_secreta(_)),
    elegir_palabra(Palabra),
    assertz(palabra_secreta(Palabra)),
    assertz(letras_adivinadas([])),
    assertz(intentos_restantes(7)).

% -----------------------------------------------------------------------------
% procesar_letra(+Letra)
% Procesa una letra ingresada por el jugador
% -----------------------------------------------------------------------------
procesar_letra(Letra) :-
    letra_valida(Letra),
    letras_adivinadas(Usadas),
    (   member(Letra, Usadas)
    ->  write(' Ya has usado esa letra.'), nl
    ;   actualizar_estado(Letra)
    ).

procesar_letra(Letra) :-
    \+ letra_valida(Letra),
    write(' Entrada inválida. Usa una letra minúscula.'), nl.

% -----------------------------------------------------------------------------
% actualizar_estado(+Letra)
% Actualiza el juego según si la letra está o no en la palabra
% -----------------------------------------------------------------------------
actualizar_estado(Letra) :-
    letras_adivinadas(Usadas),
    retract(letras_adivinadas(Usadas)),
    palabra_secreta(Palabra),
    NewUsadas = [Letra | Usadas],
    assertz(letras_adivinadas(NewUsadas)),
    (   member(Letra, Palabra)
    ->  write(' Letra correcta!'), nl
    ;   intentos_restantes(N),
        retract(intentos_restantes(N)),
        N1 is N - 1,
        assertz(intentos_restantes(N1)),
        write(' Letra incorrecta. Intentos restantes: '), write(N1), nl
    ).


% -----------------------------------------------------------------------------
% verificar_victoria/0
% True si todas las letras de la palabra ya fueron adivinadas
% -----------------------------------------------------------------------------
verificar_victoria :-
    palabra_secreta(Palabra),
    letras_adivinadas(Usadas),
    forall(member(L, Palabra), member(L, Usadas)).

% -----------------------------------------------------------------------------
% verificar_derrota/0
% True si los intentos se agotaron
% -----------------------------------------------------------------------------
verificar_derrota :-
    intentos_restantes(N),
    N =< 0.

% -----------------------------------------------------------------------------
% letra_valida(+Letra)
% Verifica si el valor es una letra minúscula válida
% -----------------------------------------------------------------------------
letra_valida(L) :-
    atom(L),
    atom_length(L, 1),
    char_type(L, lower).
