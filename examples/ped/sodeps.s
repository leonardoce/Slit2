@Chapter
@Title { Funzioni dipendenti dal sistema operativo }
@Begin @LP

Nonostante l'interfaccia di ped sia volutamente scarna ci sono delle funzioni
che sono dipendenti dal sistema operativo sul quale viene eseguito. Prima
fra tutte è la funzione che calcola la lunghezza della console sulla quale
Ped viene eseguito. @PP

Su Linux c'è un comando, @F {stty size}, che può essere utilizzato per
lo scopo. Il comando restituisce, separati da uno spazio, il numero di righe
e il numero di colonne del terminale corrente. @PP

Su Windows, almeno per quello che ho trovato io, non c'è niente di simile.
L'unico modo è implementare un piccolo programmino C che chiama la API
di Windows @F GetConsoleScreenBufferInfo che produce una struttura nella quale
uno dei membri enuncia anche la larghezza e l'altezza della console corrente. @PP

Il programma ha bisogno che lo stream di output (hStdout) sia connesso
direttamente alla console, altrimenti non funziona. Per questo motivo
l'eseguibile non può essere utilizzato con la popen di Python, che
per funzionare redirige l'input e l'output del processo figlio. @PP

Per comunicare le dimenzioni della console viene quindi scritto un file,
il cui nome è passato come primo argomenti, che contiene le coordinate in formato
simile a quello del comando "stty size". @PP

Quindi:
@PP

@o consolesize.c
@{
#include <windows.h>
#include <stdio.h>

int main(int argc, char **argv) {
  CONSOLE_SCREEN_BUFFER_INFO conInfo;
  HANDLE hStdout;
  FILE* outFile;

  if(argc!=2) {
    printf("Sintassi: %s <nomeFileOutput>\n", argv[0], argv[1]);
    return 0;
  }

  outFile = fopen(argv[1], "w");
  if(outFile == NULL) {
    return 0;
  }

  hStdout = GetStdHandle( STD_ERROR_HANDLE );
  if(! GetConsoleScreenBufferInfo( hStdout, &conInfo ) ) {
    fprintf(outFile, "25 80\n");
  } else {
    int rows = conInfo.srWindow.Bottom - conInfo.srWindow.Top;
    int cols = conInfo.srWindow.Right - conInfo.srWindow.Left;

    fprintf(outFile, "%i %i\n", rows, cols);
  }

  fclose( outFile );
  return 0;
}
@}

Questo programma può anche essere compilato con Tcc (Tiny C Compiler)
cha ha già a disposizione le dichiarazioni delle API di Windows. @PP

@d getScreenHeight
@{
def getScreenHeight():
  if os.name == "posix":
    fDimensioni = os.popen("stty size")
    sDimensioni = fDimensioni.read()
    fDimensioni.close()
  elif os.name == "nt":
    sTempName = os.tempnam()
    os.system("consolesize \"" + sTempName +"\"")
    f = open(sTempName, "r")
    sDimensioni = f.read()
    f.close()
    os.remove( sTempName )
  else:
    return 25

  return int(sDimensioni.split(" ")[0])
@}


Un'altra funzione che è strettamente dipendente dal sistema operativo
è quella per la pulizia dello schermo. Su linux c'è il comando "clear"
oppure le sequenze ANSI per la pulizia dello schermo. Su windows l'unico
modo è sfruttare il comando "cls". Quindi:

@d clearScreen
@{
def clearScreen():
  if os.name == "posix":
    sys.stdout.write("\x1B[2J")
  elif os.name == "nt":
    os.system("cls")
@}

Per adesso non ci sono altre cose dipendenti dal sistema
operativo e spero di trovarne sempre di meno!

Quindi:

@d dipendenze da so
@{
@<clearScreen@>

@<getScreenHeight@>
@}

@End @Chapter
