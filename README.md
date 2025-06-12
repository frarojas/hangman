# Juego del Ahorcado (Prolog)

## Requisitos
- Tener instalado [SWI-Prolog](https://www.swi-prolog.org/) en Windows.

## Estructura del Proyecto
- `src/logica.pl`         → Archivo principal, inicia el juego.
- `src/ui.pl`             → Interfaz de usuario (mensajes, interacción).
- `src/ahorcado.pl`       → Lógica de selección y validación de palabras.
- `data/palabras.pl`      → Base de datos de palabras iniciales.
- `data/palabras_guardadas.pl` → Palabras agregadas por el usuario.

## Cómo iniciar el juego
1. Abre una terminal (PowerShell) en la carpeta raíz del proyecto.
2. Inicia SWI-Prolog escribiendo:
   ```powershell
   swipl
   ```
3. Carga el archivo principal del juego:
   ```prolog
   ['src/logica.pl'].
   ```
   El juego comenzará automáticamente.

## Cómo jugar
- El juego te dará la bienvenida y te preguntará cuántos intentos deseas (puedes presionar Enter para usar el valor por defecto de 7).
- Luego, podrás elegir si quieres una palabra aleatoria o ingresar tu propia palabra.
- El objetivo es adivinar la palabra letra por letra antes de que se acaben los intentos.
- Ingresa una letra cuando se te pida. Si ya la usaste, el juego te avisará.
- El juego mostrará el estado actual: intentos restantes, letras usadas y la palabra oculta.
- Si adivinas todas las letras, ganas. Si se acaban los intentos, pierdes.
- Al final, puedes elegir si quieres jugar de nuevo.

### Agregar nuevas palabras
- Si eliges la opción de ingresar tu propia palabra, esta se agregará a la base de datos para futuras partidas.

### Salir del juego
- Cuando se te pregunte "¿Quieres jugar de nuevo? (s/n):", responde "n" para salir.
- También puedes salir en cualquier momento escribiendo `halt.` en la consola de Prolog.

---
¡Disfruta jugando y practicando Prolog!