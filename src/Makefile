GHC          = ghc-9.0.2
FLAGS        = -Wall -Wno-unused-do-bind
FRONTEND_DIR = Frontend/
BACKEND_DIR  = Backend/
GRAMMAR_DIR  = Grammar/
MAIN         = Main.hs
TARGET_LLVM  = ../latc_llvm


all:
	${GHC} ${FLAGS} ${MAIN} -o ${TARGET_LLVM}


clean:
	rm -f *.hi *.o
	rm -f ${FRONTEND_DIR}/*.hi ${FRONTEND_DIR}/*.o
	rm -f ${BACKEND_DIR}/*.hi ${BACKEND_DIR}/*.o
	rm -f ${GRAMMAR_DIR}/*.hi ${GRAMMAR_DIR}/*.o
	rm -f ${TARGET_LLVM}
