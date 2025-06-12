% -----------------------------------------------------------------------------
% logica.pl
% Gestión del estado y lógica del juego del ahorcado
% -----------------------------------------------------------------------------

:- set_prolog_flag(encoding, utf8).

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

% --- Ciclo Principal del Juego ---

jugar :-
    bienvenida,
    iniciar_juego, % Sets up palabra_secreta, intentos_restantes, letras_adivinadas
    ciclo_del_juego.

bienvenida :-
    nl, write('============================='), nl,
    write('    Bienvenido al Ahorcado!'), nl,
    write('============================='), nl.

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

% --- Mostrar Estado del Juego ---

mostrar_estado_juego :-
    intentos_restantes(Intentos),
    nl, nl, write('------------------------------'), nl,
    write('Intentos restantes: '), write(Intentos), nl,
    write('Letras usadas: '), mostrar_lista_letras_usadas, nl,
    mostrar_palabra_oculta,
    nl, write('------------------------------'), nl.

mostrar_palabra_oculta :-
    palabra_secreta(PalabraSecreta),
    letras_adivinadas(Adivinadas),
    write('Palabra: '),
    mostrar_letras_palabra(PalabraSecreta, Adivinadas),
    nl.

mostrar_letras_palabra([], _).
mostrar_letras_palabra([Letra|Resto], Adivinadas) :-
    (   member(Letra, Adivinadas) ->
        write(Letra)
    ;   write('_')
    ),
    write(' '), % Espacio entre letras o guiones
    mostrar_letras_palabra(Resto, Adivinadas).

mostrar_lista_letras_usadas :-
    letras_adivinadas(Usadas),
    (   Usadas == [] ->
        write('Ninguna')
    ;   sort(Usadas, UsadasOrdenadas), % Opcional: mostrar letras usadas en orden
        atomic_list_concat(UsadasOrdenadas, ', ', Atom),
        write(Atom)
    ).

% --- Pedir Letra al Jugador ---

pedir_letra_juego :-
    nl, write('Ingresa una letra: '), flush_output,
    get_single_char(Code),
    char_code(CharAtom, Code),
    downcase_atom(CharAtom, Letra), % Convertir a minúscula
    nl,
    procesar_letra(Letra). % procesar_letra ya maneja la validación y el feedback

% --- Manejo de Fin de Juego ---

manejar_victoria :-
    nl, write(' Felicidades, Has ganado!'), nl,
    palabra_secreta(Palabra),
    write('La palabra era: '), atomic_list_concat(Palabra, '', Atom), write(Atom), nl,
    preguntar_jugar_de_nuevo.

manejar_derrota :-
    nl, write('Te has quedado sin intentos. Has perdido!'), nl,
    palabra_secreta(Palabra),
    write('La palabra era: '), atomic_list_concat(Palabra, '', Atom), write(Atom), nl,
    preguntar_jugar_de_nuevo.

% --- Jugar de Nuevo ---

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

% --- Inicio Automático del Juego ---
:- jugar.
