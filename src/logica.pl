% -----------------------------------------------------------------------------
% logica.pl
% Gestión del estado y lógica del juego del ahorcado
% -----------------------------------------------------------------------------

:- set_prolog_flag(encoding, utf8).
:- encoding(utf8).

:- dynamic letras_adivinadas/1.
:- dynamic intentos_restantes/1.
:- dynamic palabra_secreta/1.

:- use_module(ahorcado, [elegir_palabra/1]).
:- use_module(ui, [
    bienvenida/0,
    mostrar_estado_juego/0,
    mostrar_palabra_oculta/0,
    mostrar_lista_letras_usadas/0,
    pedir_letra_juego/0,
    manejar_victoria/0,
    manejar_derrota/0,
    preguntar_jugar_de_nuevo/0
]).

% -----------------------------------------------------------------------------
% iniciar_juego/0 y iniciar_juego/1
% Inicializa el estado del juego: selecciona palabra, reinicia intentos y letras
% -----------------------------------------------------------------------------
iniciar_juego :-
    iniciar_juego(7).

iniciar_juego(Intentos) :-
    retractall(letras_adivinadas(_)),
    retractall(intentos_restantes(_)),
    retractall(palabra_secreta(_)),
    elegir_palabra(Palabra),
    assertz(palabra_secreta(Palabra)),
    assertz(letras_adivinadas([])),
    assertz(intentos_restantes(Intentos)).

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

% --- Ciclo Principal del Juego ---

% -----------------------------------------------------------------------------
% jugar/0
% Inicia el juego, muestra bienvenida y pregunta intentos
% -----------------------------------------------------------------------------
jugar :-
    bienvenida,
    preguntar_intentos(Intentos),
    iniciar_juego(Intentos),
    ciclo_del_juego.

% -----------------------------------------------------------------------------
% ciclo_del_juego/0
% Ciclo principal del juego, maneja turnos hasta victoria o derrota
% -----------------------------------------------------------------------------
ciclo_del_juego :-
    (   verificar_victoria ->
        manejar_victoria
    ;   verificar_derrota ->
        manejar_derrota
    ;   % Juego en curso
        mostrar_estado_juego,
        pedir_letra_juego,
        ciclo_del_juego % Llamada recursiva para el siguiente turno
    ).

% -----------------------------------------------------------------------------
% preguntar_intentos(-Intentos)
% Pregunta al usuario cuántos intentos quiere para el juego
% -----------------------------------------------------------------------------
preguntar_intentos(Intentos) :-
    nl, write('Con qué cantidad de intentos se va jugar? (Enter para usar 7): '), flush_output,
    read_line_to_string(user_input, Str),
    (   Str = "" ->
            Intentos = 7
    ;   number_string(N, Str), N > 0 ->
            Intentos = N
    ;   write('Entrada inválida. Se usará el valor por defecto (7).'), nl,
            Intentos = 7
    ).

% --- Inicio Automático del Juego ---
:- jugar.
