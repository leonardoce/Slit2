@x no_line_markers
@SysInclude { tbl }
@Include { book }

@Book
  @Title { Piccolo editor orientato alle righe }
  @Author { Leonardo Cecchi }
//

# Questo documento è scritto secondo l'encoding CP1252

@i introduzione.s
@i universo.s
@i arealavoro.s
@i comandi.s
@i comandi_areelavoro.s
@i comandi_programmazione.s
@i comandi_macro.s
@i macro_programmazione.s
@i macro_generiche.s
@i sodeps.s

@Chapter
@Title { Codice sorgente }
@Begin @LP

Riassumendo il codice sorgente di Ped è il seguente:

@o ped.py
@{
#!/usr/bin/env python
import sys
import re
import os
import os.path

@<dipendenze da so@>

try:
  import readline
except Exception, e:
  pass

class ErrorePed(Exception):
  def __init__(self, msg):
    self.msg = msg

  def __str__(self):
    return self.msg

@<class AreaLavoro@>
@<class Comando@>
@<class Ped@>

def leggiRigheDaUtente():
  linee = []
  while True:
    # Legge togliendo il ritorno carrello
    l = raw_input()

    if l == ".":
      break
    else:
      linee.append( l )

  return linee

def main():
  oPed = Ped()
  oAreaLavoro = oPed.getAreaLavoroCorrente()

  if len(sys.argv) > 1:
    if os.path.exists( sys.argv[1] ):
      try:
        oAreaLavoro.leggiFile( sys.argv[1] )
      except IOError, e:
        print str(e)
        raw_input()
    else:
      oAreaLavoro.setNomeBuffer( sys.argv[1] )

  oPed.setDimensioneFinestra(  getScreenHeight() - 8 )

  bContinua = True
  bStampaSchermo = True

  while bContinua:
    oAreaLavoro = oPed.getAreaLavoroCorrente()
    if bStampaSchermo:
      oAreaLavoro.stampaSchermo()

    try:
      sComando = raw_input( "ped> " )
      cmd = oPed.eseguiComando( sComando )
    except ErrorePed, e:
      print "?", str(e)
      bStampaSchermo = False
      bContinua = True
    else:
      bContinua = cmd.continua
      bStampaSchermo = cmd.stampaSchermo

if __name__=="__main__":
  main()
@}

@End @Chapter
