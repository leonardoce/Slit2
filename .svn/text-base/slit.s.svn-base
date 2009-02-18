Slit
Simple Literate Tool

% :folding=explicit:mode=slitpascal:
%!target:html
%!options: --toc

%%TOC

== Introduzione ==

Slit e' un programma che permette di scrivere codice e documentazione all'interno dello stesso
file. Il rapporto fra il codice e la documentazione e' pero' ribaltato: se normalmente si
scrive il codice e all'interno di questo si inserisce la documentazione usando Slit si
scrive la documentazione e all'interno di questa il codice.

Inoltre a questo il codice puo' essere diviso in frammenti che si richiamano per comporre
l'intero programma. Per avere maggiori informazioni su questa modalita' di lavoro
si puo' consultare la pagina:

http://en.wikipedia.org/wiki/Literate_programming

== Il comando Slit ==

I programmi vengono normalmente scritti in file di testo che possono avere qualunque esensione.
Per separare il codice dalla documentazione di utilizza il comando "slit":

```
slit <nomefile>
```

Questo comando processa il file con il nome passato e ne interpreta le direttive. Dal file
passato vengono quindi generati:

+ i file della documentazione;
+ i vari file che sono il codice sorgente.


La procedura principale infatti recita:

@d slit procedura principale
@{
begin
  if ParamCount = 1 then
  begin
    @<slit preparazione dell'ambiente@>
    @<slit riempimento del magazzino delle macro@>
    @<slit calcola riferimenti@>
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

Il file di ingresso e' rappresentato da un oggetto della classe ""TSlitStream""
mentre il file di uscita della documentazione e' rappresentato da una classe
della famiglia ""TSlitOutputTxt"".

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

== Il formato dei files di ingresso ==

@i ingresso.s

== Il magazzino delle macro ==

@i macro.s

== Driver per il riempimento del magazzino delle macro ==

@i drivermagazzino.s

== Driver per la generazione della documentazione ==

@i drivergenerazionedoc.s

== Backend di generazione della documentazione ==

@i outputdocumentazione.s

== Creazione dei files sorgenti ==

@i outputsorgenti.s

== Utilita' per la gestione dei files html ==

@i utilita.s
