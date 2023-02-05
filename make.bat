@echo off

if NOT "%COB_MAIN_DIR%" == "" goto cont

set COB_MAIN_DIR=C:\gnuCOBOL\
set COB_CONFIG_DIR=%COB_MAIN_DIR%config
set COB_COPY_DIR=%COB_MAIN_DIR%copy
set COB_CFLAGS=-I"%COB_MAIN_DIR%include" %COB_CFLAGS%
set COB_LDFLAGS=-L"%COB_MAIN_DIR%lib" %COB_LDFLAGS%
set COB_LIBRARY_PATH=%COB_MAIN_DIR%extras
set PATH=%COB_MAIN_DIR%bin;%PATH%

:cont

gcc -g -O -c src/audio_out.c -o src/audio_out.o -Isrc/include -Lsrc/lib -lmingw32 -lSDL2main -lSDL2
cobc -c -static -x src/COBOLsound.cbl -o src/COBOLsound.o
cobc -x -o COBOLsound src/COBOLsound.o src/audio_out.o -Isrc/include -Lsrc/lib -lmingw32 -lSDL2main -lSDL2