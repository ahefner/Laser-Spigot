
;;;; System definition

(pushnew :with-gui *features*)

(defun cflags () (list #-win32 "-I/usr/include/freetype2"
		       #+win32 "-Ic:/MinGW/include/freetype2"
                       ;; On OS X, use the MacPorts stuff installed in /opt
                       #+darwin "-I/opt/local/include" 
                       #+darwin "-I/opt/local/include/SDL"
                       #+darwin "-I/opt/local/include/freetype2"
                       #+darwin "-D_THREAD_SAFE"
		       "-DGLEW_STATIC=1" "-Isrc/glew" "-std=c99"))

(defun ld-flags ()
  "Non-library linker flags"
  (list 
   ;;#+win32 "-Wl,-subsystem,windows"
   #+darwin "-framework" #+darwin "OpenGL"
   #+darwin "-L/opt/local/lib"))

(defun c-sources ()
  "C source code modules."
  (list "src/sys.c"
	"src/glew/glew.c"
        "src/audio.c"
        "src/text-render.c"))

(defun lisp-compile-sources ()
  "Lisp source needed at compile time (macros, package definitions, etc.) for all source files."
  (list "src/package.lisp"
        "src/macrology.lisp"
        "src/globals.lisp"))

(defun lisp-sources ()
  "Lisp sources to be linked into the final executable."
  (list
   "sysdef.lisp"               ; For runtime recompile feature..
   "src/package.lisp"
   "src/globals.lisp"
   "src/util.lisp"
   "src/math.lisp"
   '("src/vid-sdl-opengl.lisp" #|depends on:|# "src/math.lisp")
   "src/text-layout.lisp"
   "src/uim-defs.lisp"
   '("src/uim.lisp"         #|depends on:|# "src/uim-defs.lisp" "src/math.lisp")
   '("src/event-loop.lisp"  #|depends on:|# "src/uim-defs.lisp")
   '("src/game.lisp" "src/uim-defs.lisp" "src/math.lisp")
   "src/main.lisp"))

(defun shared-libraries ()
  (list "SDL" "SDL_image"
        #- (or darwin win32) "GL"
	#+win32 "opengl32"
        "vorbisfile"
        "freetype"
	#-win32 "z"
	#+win32 "zlib1"))

