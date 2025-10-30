EMCC=emcc
EMAR=emar

set -e
echo "Checking if raylib is built..."
if [ ! -f "raylib/src/libraylib.so" ]; then
# if [ ! -f "raylib/src/libraylib.a" ]; then
  echo "raylib archive does not exist"
  echo $'Building raylib...\n'

  cd raylib/src

  # $EMCC -c rcore.c -pthread -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2
  # $EMCC -c rshapes.c -pthread -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2
  # $EMCC -c rtextures.c -pthread -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2
  # $EMCC -c rtext.c -pthread -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2
  # $EMCC -c rmodels.c -pthread -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2
  # $EMCC -c utils.c -pthread -Os -Wall -DPLATFORM_WEB
  # $EMCC -c raudio.c -pthread -Os -Wall -DPLATFORM_WEB
  
  $EMCC -c rcore.c -pthread -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2 -fPIC -DBUILD_LIBTYPE_SHARED $OPTIONS
  $EMCC -c rshapes.c -pthread -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2 -fPIC -DBUILD_LIBTYPE_SHARED $OPTIONS
  $EMCC -c rtextures.c -pthread -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2 -fPIC -DBUILD_LIBTYPE_SHARED $OPTIONS
  $EMCC -c rtext.c -pthread -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2 -fPIC -DBUILD_LIBTYPE_SHARED $OPTIONS
  $EMCC -c rmodels.c -pthread -Os -Wall -DPLATFORM_WEB -DGRAPHICS_API_OPENGL_ES2 -fPIC -DBUILD_LIBTYPE_SHARED $OPTIONS
  $EMCC -c utils.c -pthread -Os -Wall -DPLATFORM_WEB -fPIC -DBUILD_LIBTYPE_SHARED $OPTIONS
  $EMCC -c raudio.c -pthread -Os -Wall -DPLATFORM_WEB -fPIC -DBUILD_LIBTYPE_SHARED $OPTIONS

  # $EMAR rcs libraylib.a rcore.o rshapes.o rtextures.o rtext.o rmodels.o utils.o raudio.o
  $EMCC -shared -o libraylib.so rcore.o rshapes.o rtextures.o rtext.o rmodels.o utils.o raudio.o

  cd ../..

  echo $'\nraylib archive built successfully, continuing\n'
else
  echo $'raylib archive already exists, continuing\n'
fi

echo $'Compiling bindings...'

# $EMCC cbits/rl_bindings.c raylib/src/libraylib.a \
$EMCC cbits/rl_bindings.c raylib/src/libraylib.so \
  -Iraylib/src \
  -DPLATFORM_WEB \
  -DNO_MAIN_LOOP=1 \
  -DGRAPHICS_API_OPENGL_ES2 \
  \
  -sUSE_GLFW=3 \
  -sIMPORTED_MEMORY \
  -sSHARED_MEMORY \
  -sUSE_PTHREADS \
  -sALLOW_MEMORY_GROWTH \
  -INITIAL_MEMORY=33554432 \
  -sMAXIMUM_MEMORY=67108864 \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/birdFlapUp.png \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/backdrop.png \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/pipeBottom.png \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/pipeTop.png \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/ok.png \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/tap.png \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/birdFlapDown.png \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/bird.png \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/ground.png \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/gameOver.png \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/scoreboard.png \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/font.png \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/shadeblue.ttf \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/flap.ogg \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/score.ogg \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/death.ogg \
  --embed-file /home/k/Documents/webraylib/h-raylib-web-template/swoosh.ogg \
  -sSTACK_SIZE=3MB \
  \
  -sMODULARIZE \
  -sEXPORT_NAME=Raylib \
  -sEXPORTED_RUNTIME_METHODS=ccall,cwrap,setMainLoop \
  \
  -Wno-pthreads-mem-growth \
  \
  -o raylib.js

echo $'Bindings compiled successfully!\n'

echo "Moving output files..."

rm -f ./public/raylib.wasm
mv raylib.wasm ./public

rm -f ./src/raylib.js
mv raylib.js ./src

rm -f raylib.worker.js

echo "Output files moved successfully!"
