# :folding=explicit:mode=slitpascal:
@x output_lout
@SysInclude { tbl }
@Include { book }

@Book 
  @Title { Slit - Simple Literate Tool }
  @Author { Leonardo Cecchi }
//

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

@F { @Verbatim {http://en.wikipedia.org/wiki/Literate_programming} }

@End @Introduction

@Chapter
@Title { Sintassi dei files Slit }
@Begin
@LP

I files in formato Slit sono composti da linee di testo di documentazione e da
direttive. Le direttive sono linee che iniziano per un carattere chiocciola
("@") e sono seguite da una lettera che identifica la direttiva. @PP

In questo documento le direttive supportate sono le seguenti: @PP

@BulletList
@ListItem { "@d" @Char egrave la direttiva che definisce una macro, }
@ListItem { "@o" @Char egrave la direttiva che serve per scrivere un file
sorgente }
@ListItem { "@i" @Char egrave la direttiva che serve per includere 
un file slit da un'altro. }
@EndList

C'@Char egrave una cosa molto importante da ricordare: le direttive sono
valide solamente se sono scritte al primo carattere della linea. Questo
per fare in modo che slit non interpreti una stringa come una direttiva. @PP

@BeginSections
@Section @Title { La direttiva "@d" }
@Begin @PP

La direttiva "@d" @Char egrave la direttiva principale di Slit. @PP

Questa direttiva serve per memorizzare una macro di testo all'interno del
sistema. La macro pu{@Char ograve} essere utilizzata all'interno di altre
macro oppure semplicimente per scrivere un certo file sorgente. @PP

Ecco un esempio di una direttiva "@d":

@IndentedDisplay @F 
@Verbatim @Begin
 @d funzione somma
@{
function somma(a,b:Integer) return Integer
begin
  return a+b;
end;
@}
@End @Verbatim

Come si vede la direttiva "@d" @Char egrave seguita da uno @I scrap. Uno
@I scrap, secondo la terminologia Slit, @Char egrave il contenuto della
macro definita. @PP

Uno scrap inizia per "@{" e finisce con "@}". Tutto quello che 
@Char egrave fra queste due righe @Char egrave considerato come
contenuto della macro in corso di definizione. @PP

In base al tipo di processore scelto per l'output Slit enuncia il
nome della macro e il suo contenuto all'interno della documentazione.
Ad ogni macro viene associato un numero che la identifica e alla fine
della macro vengono enunciati i numeri di macro che utilizzano la macro
appena definita. @PP

Una macro pu{@Char ograve} anche essere non utilizzata, in questo 
caso Slit avverte
l'utente alla fine del processo di produzione dei files sorgenti. @PP

@End @Section

@Section @Title { La direttiva "@o" e sintassi degli scrap }
@Begin @PP

La direttiva "@o" @Char egrave molto simile, dal punto di vista sintattico,
a quella "@d". Infatti viene utilizzata per definire un tipo particolare di
macro, il cui nome viene utilizzato per scrivere un file. @PP

Ad esempio: 

@IndentedDisplay @F 
@Verbatim @Begin
 @o prova.c
@{
#include <stdio.h>

int main(int argc, char **argv)
{
    printf("Hello world!\n");
    return 0;
}
@}
@End @Verbatim

All'interno di uno scrap si pu{@Char ograve} includere un riferimento ad
un'altro:

@IndentedDisplay @F 
@Verbatim @Begin
 @d saluta
@{
printf("Hello world!\n");
@}
@End @Verbatim

@IndentedDisplay @F 
@Verbatim @Begin
 @o provadue.c
@{
#include <stdio.h>

int main(int argc, char **argv)
{
    @<saluta@>
    return 0;
}
@}
@End @Verbatim

Nello scrivere il codice sorgente slit ricorda il livello di indentazione dei
riferimenti alle macro e li utilizza per scrivere il codice.

@End @Section

@Section @Title { La direttiva "@i" }
@Begin @PP

La direttiva "@i" invece serve per includere un file slit da un'altro. @PP

Molto spesso i programmi scritti con Slit sono composti da pi{@Char ugrave}
files Slit. La direttiva "@i" dice a Slit di andare a leggere un'altro file
e ritornare al file chiamante quando la lettura viene completata. @PP

Ad esempio:

@IndentedDisplay @F @Verbatim { @i provatre_funzioni.s }
@IndentedDisplay @F @Verbatim { @i provatre_dichiarazioni.s }

@IndentedDisplay @F
@Verbatim @Begin
 @o provatre.c
@{
@include <stdio.h>

@<dichiarazioni@>
@<funzioni@>

int main(int argc, char **argv)
{
    funzionePrincipale();
    return 0;
}
@}
@End @Verbatim

@End @Section

@Section @Title { La direttiva "@+" }
@Begin @PP

La direttiva @F "@+" aggiunge uno scrap in coda a una macro gi{@Char agrave}
esistente. Questo serve per ridurre la quantit{@Char agrave} dei nomi di
macro utilizzati e per non essere costretti ad inventarsi nomi non utili.
@PP

Ad esempio:

@IndentedDisplay @F
@Verbatim @Begin
 @o provatre.c
@{
@include <stdio.h>

@<dichiarazioni@>

int main(int argc, char **argv)
{
    funzionePrincipale();
    return 0;
}
@}
@End @Verbatim

@IndentedDisplay @F
@Verbatim @Begin
 @d dichiarazioni
@{
void funzioneUno();
@}
@End @Verbatim

@IndentedDisplay @F
@Verbatim @Begin
 @+ dichiarazioni
@{
void funzionePrincipale();
@}
@End @Verbatim

@End @Section

@Section @Title { Opzioni (la direttiva "@x") }
@Begin @PP

Slit pu{@Char ograve} essere configurato attraverso delle opzioni. Le opzioni
si comunicano utilizzando la direttiva @F @Verbatim { @x }. @PP
Questa direttiva @Char egrave seguita da una stringa che @Char egrave
il nome dell'opzione stessa. Ad esempio:

@IndentedDisplay @F @Verbatim { @x output_html }

Questa @Char egrave la lista delle opzioni supportate:

@BulletList
@ListItem { @F output_html : seleziona il formato html per l'output }
@ListItem { @F output_txt : seleziona il formato testo per l'output }
@ListItem { @F output_lout : seleziona il formato lout per l'output }
@EndList

@End @Section

@Section @Title { Documentazione }
@Begin @PP

Tutto quello che non @Char egrave una direttiva viene direttamente passato 
nel file di documentazione generato senza alcuna trasformazione
intermedia. @PP

Slit deve essere utilizzato insieme a un formato di interscambio di testi.
In questo momento si pu{@Char ograve} utilizzare:

@BulletList
@ListItem { il formato HTML, }
@ListItem { il formato testo, }
@ListItem { il sistema di elaborazione testi Lout. }
@EndList

La documentazione deve essere inserita in modo coerente con il formato
scelto. @PP
Il formato Lout permette anche la generazione di una cross-reference
fra le macro inserite: ogni macro viene numerata e al termine della 
macro viene inserita la lista di tutte le macro che la utilizzano. @PP

@End @Section

@EndSections

@End @Chapter


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
driverMagazzinoMacro := TSlitStreamDriverMagazzino.CreateWithMacroStore( store );
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
streamOutputDocumentazione := CreaStreamOutputDaOpzioni(ParamStr(1), store);
driverScriviDocumentazione := 
  TSlitStreamDriverGenerazioneDoc.CreateWithOutputStream( streamOutputDocumentazione );

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
FreeAndNil(streamOutputDocumentazione);
FreeAndNil(stream);
FreeAndNil(store);
@}

@End @Chapter

@i magazzino.s
@i statocorrente.s
@i ingresso.s
@i drivermagazzino.s
@i drivergenerazionedoc.s
@i outputdocumentazione.s
@i outputsorgenti.s
@i utilita.s
