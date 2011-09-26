@Chapter
@Title { Comandi descritti con macro }
@Begin @LP

Ped ammette di essere esteso con delle macro che vengono scritte
in Python. Le macro devono risiedere in files con estensione @F {.py} nella
stessa directory dove c'Š il file @F {ped.py}. @PP

Le macro possono accedere all'oggetto @F {command} che Š l'unica interfaccia
fra l'editor e i comandi e che viene inserito nel dizionario globale
della macro. @PP

Il comando per lanciare una macro Š quindi il seguente:

@d comandoMacro
@{
def comandoMacro( comando ):
  sNomeMacro = comando.txtComando.split( " " )[0]
  sNomeFileMacro = os.path.join( os.path.realpath( sys.path[0] ), sNomeMacro )
  sNomeFileMacro = sNomeFileMacro + ".py"

  if os.path.exists( sNomeFileMacro ):
    try:
      execfile( sNomeFileMacro, { 'comando':comando } )
    except BaseException, e:
      comando.stampaSchermo = False
      print str(e)
  else:
    print "Comando non esistente (" + sNomeFileMacro + ")"
@}

@End @Chapter
