# :folding=explicit:mode=slitpascal:
@SysInclude { tbl }
@Include { book }

@Book 
  @Title { Slit - Simple Literate Tool }
  @Author { Leonardo Cecchi }
//

# Introduzione {{{
@Introduction
@Title { Introduzione }
@Begin @PP

Slit è un programma che permette di scrivere codice e documentazione all'interno dello stesso
file. Il rapporto fra il codice e la documentazione è però ribaltato: se normalmente si
scrive il codice e all'interno di questo si inserisce la documentazione usando Slit si
scrive la documentazione e all'interno di questa il codice. @PP

Inoltre a questo il codice può essere diviso in frammenti che si richiamano per comporre
l'intero programma. Per avere maggiori informazioni su questa modalità di lavoro
si può consultare la pagina: @PP

@F { @Verbatim {http://en.wikipedia.org/wiki/Literate_programming}}

@End @Introduction
# }}}

# Il comando Slit {{{
@Chapter
@Title { Il comando Slit }
@Begin @LP

I programmi vengono normalmente scritti in file di testo che possono avere qualunque esensione.
Per separare il codice dalla documentazione di utilizza il comando @F {slit}: @PP

@Display @F @Verbatim {
slit <nomefile>
}

Questo comando processa il file con il nome passato e ne interpreta le direttive. Dal file
passato vengono quindi generati: @PP

@BulletList
@ListItem { i file della documentazione; }
@ListItem { i vari file che sono il codice sorgente. }
@EndList

La procedura principale infatti recita:

@d slit procedura principale
@{
begin
  if ParamCount = 1 then
  begin
    @<slit preparazione dell'ambiente@>
    @<slit riempimento del magazzino delle macro@>
    @<slit calcola riferimenti@>
    @<slit controlla macro non utilizzate@>
    @<slit generazione della documentazione@>
    @<slit generazione del codice sorgente@>
    @<slit pulizia@>
  end
  else
  begin
    writeln('Uso: ', ParamStr(0), ' <nomefile>');
  end;
end.
@}

Il file di ingresso è rappresentato da un oggetto della classe @F TSlitStream
mentre il file di uscita della documentazione è rappresentato da una classe
della famiglia @F { TSlitOutputTxt }.

Le macro vengono memorizzate all'interno di un "magazzino" dal quale vengono
poi riprese per scrivere i files sorgenti.

@d slit preparazione dell'ambiente
@{
store := TMacroStore.Create;
stream := TSlitStream.CreateForFile(ParamStr(1));
streamHtml := TSlitOutputLout.CreateForFileAndStore(ParamStr(1), store);
driverMagazzinoMacro := TSlitStreamDriverMagazzino.CreateWithMacroStore( store );
driverScriviDocumentazione := 
  TSlitStreamDriverGenerazioneDoc.CreateWithOutputStream( streamHtml );
@}

La documentazione viene elaborata attraverso un metodo dello stream in ingresso:

@d slit riempimento del magazzino delle macro
@{
stream.Driver := driverMagazzinoMacro;
stream.Process();
stream.ResetStream();
@}

Dopo aver popolato il magazino delle macro vengono calcolati i riferimenti:

@d slit calcola riferimenti
@{
store.CalcolaRiferimenti();
@}

Una volta calcolati i riferimenti è possibile controllare la presenza di macro
mai utilizzate:

@d slit controlla macro non utilizzate
@{
ControllaMacroNonUtilizzate();
@}

Il controllo viene fatto scorrendo tutto il magazzino della macro.

@d slit ControllaMacroNonUtilizzate
@{
procedure ControllaMacroNonUtilizzate;
var
  tempMacro : TMacroRecord;
  i : integer;
begin
  for i := 0 to store.MacroCount-1 do
  begin
    tempMacro := store.GetRecord( i );
    if (tempMacro.macroUsersCount = 0) and (tempMacro.macroType <> FileMacro) then
    begin
      writeln(StdErr, 'La macro ', tempMacro.macroName, ' non è mai stata utilizzata.');
    end;
  end;
end;
@}

Viene adesso avviata la generazione della documentazione:

@d slit generazione della documentazione
@{
stream.Driver := driverScriviDocumentazione;
stream.Process();
stream.ResetStream();
writeln(store.MacroCount, ' macro processate');
@}

Viene poi avviata l'elaborazione dei file sorgenti:

@d slit generazione del codice sorgente
@{
ProcessaFiles();
@}

Poi vengono deallocati gli oggetti creati:

@d slit pulizia
@{
  FreeAndNil(driverMagazzinoMacro);
  FreeAndNil(driverScriviDocumentazione);
  FreeAndNil(streamHtml);
  FreeAndNil(stream);
  FreeAndNil(store);
@}

@End @Chapter
# }}}

@i ingresso.s
@i magazzino.s
@i drivermagazzino.s
@i drivergenerazionedoc.s
@i outputdocumentazione.s
@i outputsorgenti.s
@i utilita.s
