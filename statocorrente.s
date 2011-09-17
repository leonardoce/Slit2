@Chapter @Title { Lo stato della traduzione }
@Begin @LP

All'interno di slit esiste un modulo il cui scopo @Char egrave contenere
lo stato corrente del processo di traduzione. @PP

Le informazioni presenti in questo modulo sono trattate in questo 
capitolo. @PP

@BeginSections
@Section @Title { Gestione delle opzioni }
@Begin @PP

Slit ha un insieme di parametri di configurazione che permette all'utente
di adattare il motore di traduzione in base alle sue preferenze. @PP

Questo insieme di parametri, chiamati @I opzioni, viene gestito da una
unit apposita, che gestisce tutti i dati e fornisce delle operazioni
di alto livello che operano su questi. @PP

@BeginSubSections
@SubSection @Title { Formato di output della documentazione }
@Begin @PP

Slit deve scrivere, all'interno della documentazione del codice, il
contenuto delle varie macro presenti all'interno del documento. Per
questa ragione deve sapere qual @Char egrave il formato della documentazione.
@PP

Il valore di questa opzione memorizza proprio questo formato, in modo che
possa essere creata la corrispondente implementazione del driver per la
generazione della documentazione. @PP

@d slitstatus, gestore del processore di documentazione
@{
function GetNomeProcessoreInformazioni():String;
begin
  Result := NomeProcessoreInformazioni;
end;

procedure SetNomeProcessoreInformazioni(value:String);
begin
  if value='html' then
  begin
    NomeProcessoreInformazioni := 'html';
  end
  else if value='lout' then
  begin
    NomeProcessoreInformazioni := 'lout';
  end
  else if value='txt' then
  begin
    NomeProcessoreInformazioni := 'txt';
  end
  else
  begin
    raise Exception.Create('Nome processore informazioni non conosciuto: ' +
      value);
  end;
end;
@}

Visto che adesso sappiamo qual @Char egrave il processore delle informazioni
da utilizzare @Char egrave possibile anche creare una funzione che crea
il file di output per slit.

@d slitstatus, crea lo stream di output
@{
function CreaStreamOutputDaOpzioni(NomeFile:String; store:TMacroStore):TSlitOutput;
begin
  if NomeProcessoreInformazioni='lout' then
  begin
    Result := TSlitOutputLout.CreateForFileAndStore (NomeFile, store);
  end
  else if NomeProcessoreInformazioni='txt' then
  begin
    Result := TSlitOutputTxt.CreateForFile (NomeFile);
  end
  else if NomeProcessoreInformazioni='html' then
  begin
    Result := TSlitOutputHtml.CreateForFile (NomeFile);
  end
  else
  begin
    Result := Nil;
  end;
end;
@}

@End @SubSection

@SubSection @Title { Marcatore di inizio e fine dei commenti }
@Begin @PP

Nei file sorgenti generati da Slit @Char egrave possibile inserire
o meno un riferimento al file originale, che contiene la documentazione
e il codice. @PP

In questo caso il commento deve essere racchiuso fra delimitatori che
lo caratterizzino come commento valido per il linguaggio generato. @PP

@Char Egrave possibile che un singolo documento Slit generi 
pi{@Char ugrave} sorgenti in linguaggi diversi. In questo caso occorre
distinguere fra linguaggio e linguaggio. @PP

Slit fa questa distinzione utilizzando l'estensione del file. @PP

Ad ogni estensione viengono associati un marcatore di inizio e un marcatore
di fine commento. All'interno del programma questa tabella di associazione
@Char egrave gi{@Char agrave} popolata con dei valori utili per i
linguaggi di programmazione pi{@Char ugrave} popolari. @PP

Slit inserisce solamente commenti alla fine della riga dei files sorgenti
e quindi questo meccanismo funziona anche per i linguaggi che non
prevedono aree di commento all'interno della linea di codice ma solamente
alla fine (ad esempio Ada). In questo caso basta lasciare il marcatore
di fine commento vuoto. @PP

Slit @Char egrave pronto per supportare i linguaggi:

@BulletList
@ListItem { Pascal (.pas, .pp), }
@ListItem { C, C++ (.c, .cpp, .h), }
@ListItem { D (.d), }
@ListItem { Java (.java), }
@ListItem { @Verbatim { C# (.cs), } }
@ListItem { Ada (.ada, .adb, .ads), }
@ListItem { Haskell (.hs), }
@ListItem { Python (.py), }
@ListItem { Lout (.lout), }
@ListItem { Ruby (.rb). }
@EndList

I linguaggi elencati vengono direttamente configurati nel codice:

@d slitstatus, configurazione delle estensioni predefinite
@{
AggiungiLinguaggio( '.pas', '{', '}' );
AggiungiLinguaggio( '.pp', '{', '}' );
AggiungiLinguaggio( '.c', '/*', '*/' );
AggiungiLinguaggio( '.d', '/*', '*/' );
AggiungiLinguaggio( '.cpp', '/*', '*/' );
AggiungiLinguaggio( '.java', '/*', '*/' );
AggiungiLinguaggio( '.cs', '/*', '*/' );
AggiungiLinguaggio( '.ada', '--', '' );
AggiungiLinguaggio( '.adb', '--', '' );
AggiungiLinguaggio( '.ads', '--', '' );
AggiungiLinguaggio( '.hs', '--', '' );
AggiungiLinguaggio( '.py', '#', '' );
AggiungiLinguaggio( '.rb', '#', '' );
AggiungiLinguaggio( '.lout', '#', '' );
@}

Le informazioni vengono conservate all'interno di un record:

@d slitstatus, RInformazioniLinguaggi
@{
RInformazioniLinguaggi = record
  Estensione:String;
  Inizio:String;
  Fine:String;
end;
@}

Queste informazioni vengono aggiunte ad una tabella che viene
tenuta nello stato globale della configurazione di Slit:

@d slitstatus, AggiungiLinguaggio
@{
procedure AggiungiLinguaggio (Estensione, Inizio, Fine:String);
begin
  if Length(TabellaLinguaggi)>=TabellaLinguaggi_Count then
  begin
    SetLength (TabellaLinguaggi, TabellaLinguaggi_Count+50);
  end;

  TabellaLinguaggi[TabellaLinguaggi_Count].Estensione := Estensione;
  TabellaLinguaggi[TabellaLinguaggi_Count].Inizio := Inizio;
  TabellaLinguaggi[TabellaLinguaggi_Count].Fine := Fine;
  TabellaLinguaggi_Count := TabellaLinguaggi_Count + 1;
end;
@}

Per prelevare queste informazioni il programma utilizza questa
funzione:

@d slitstatus, PrendiMarcatori
@{
procedure PrendiMarcatori (NomeFile:String; var Inizio:String; var Fine:String);
var 
  i:Integer;
  fatto:Boolean;

begin
  fatto := false;

  for i:=TabellaLinguaggi_Count-1 downto 0 do
  begin
    if AnsiEndsText (TabellaLinguaggi[i].Estensione, NomeFile) then
    begin
      Inizio := TabellaLinguaggi[i].Inizio;
      Fine := TabellaLinguaggi[i].Fine;
      Fatto := True;
    end;
  end;

  if not fatto then
  begin
    Inizio := '';
    Fine := '';
  end;
end;
@}

All'interno della tabella le istruzioni vengono cercate all'incontrario
perch{@Char egrave} in questo modo le indicazioni dell'utente vengono
prese in considerazione prima di quelle fornite come predefinite.

@End @SubSection

@SubSection @Title { Generazione dei marcatori di inizio e file sezione }
@Begin @PP

Nei file sorgenti generati @Char egrave possibile conservare il nome
della macro Slit che @Char egrave stata letta per generarli. @PP

@d slitstatus, Get/Set GenerazioneMarcatoriAbilitata
@{
function GetGenerazioneMarcatoriAbilitata:Boolean;
begin
  Result := GenerazioneMarcatoriAbilitata;
end;

procedure SetGenerazioneMarcatoriAbilitata(value:Boolean);
begin
  GenerazioneMarcatoriAbilitata := value;
end;
@}

@End @SubSection

@SubSection @Title { Generazione dei marcatori di riga }
@Begin @PP

Spesso @Char egrave importante conservare, all'interno del
file sorgente generato, il nome del file di documentazione
e il numero di riga corrispondente. @PP 

Queste informazioni permettono di rintracciare facilmente un errere
emesso dal compilatore. @PP

@d slitstatus, Get/Set GenerazioneNumeriRigaAbilitata
@{
function GetGenerazioneNumeriRigaAbilitata:Boolean;
begin
  Result := GenerazioneNumeriRigaAbilitata;
end;

procedure SetGenerazioneNumeriRigaAbilitata(value:Boolean);
begin
  GenerazioneNumeriRigaAbilitata := value;
end;
@}

I marcatori, per non dare noia, vengono messi ad una precisa
colonna se disponibile oppure alla fine della riga di codice
sorgente. Questa colonna @Char egrave configurabile:

@d slitstatus, Get/Set ColonnaNumeriRiga
@{
function GetColonnaNumeriRiga:Integer;
begin
  Result := ColonnaNumeriRiga;
end;

procedure SetColonnaNumeriRiga(value:Integer);
begin
  ColonnaNumeriRiga := value;
end;
@}


@End @SubSection

@EndSubSections

@End @Section

@Section @Title { Tracciatura del file correntemente elaborato }
@Begin @PP

Slit tiene traccia del file correntemente elaborato per emettere
dei messaggi di errore il pi{@Char ugrave} accurati possibile. Il
parser, subito dopo l'apertura di un file e subito prima della sua
chiusura, segnala a questo modulo l'evento. Cos{@Char igrave} viene
tenuto traccia del file correntemente elaborato. @PP

Gli streams vengono tenuti in uno stack dinamico:

@d slitstatus, SegnalaInizioElaborazioneStream
@{
procedure SegnalaInizioElaborazioneStream (stream:TSlitStream);
begin
  if Length(StreamStack)>=StreamStackCount then
  begin
    SetLength(StreamStack, Length(StreamStack)+10);
  end;

  StreamStack[StreamStackCount] := stream;
  StreamStackCount := StreamStackCount+1;
end;
@}

@d slitstatus, SegnalaFineElaborazioneStream
@{
procedure SegnalaFineElaborazioneStream;
begin
  if StreamStackCount>0 then
  begin
    StreamStack[StreamStackCount] := Nil;
    StreamStackCount := StreamStackCount-1;
  end;
end;
@}

In questo modo possiamo creare delle chiamate per ottenere il file e
la riga corrente:

@d slitstatus, GetCurrentParsingFile
@{
function GetCurrentParsingFile:String;
begin
  if StreamStackCount>0 then
  begin
    Result := StreamStack[StreamStackCount-1].CurrentFile;
  end
  else
  begin
    Result := '';
  end;
end;
@}

Se non c'{@Char egrave} la linea corrente allora la funzione 
ritorna -1:

@d slitstatus, GetCurrentParsingLine
@{
function GetCurrentParsingLine:Integer;
begin
  if StreamStackCount>0 then
  begin
    Result := StreamStack[StreamStackCount-1].CurrentLine;
  end
  else
  begin
    Result := -1;
  end;
end;
@}

Grazie a queste chiamate possiamo creare una procedura per l'emissione
di messaggi di errori che hanno un riferimento al file correntemente
processato:

@d slitstatus, LogErrorMessage
@{
procedure LogErrorMessage(message:String);
begin
  if StreamStackCount>0 then
  begin
    write (StdErr, StreamStack[StreamStackCount-1].CurrentFile);
    write (StdErr, ':');
    write (StdErr, StreamStack[StreamStackCount-1].CurrentLine);
    write (StdErr, ' ');
  end;

  writeln (StdErr, message);
end;
@}

@End @Section

@Section @Title { Definizione della unit slitstatus }
@Begin @PP

@o slitstatus.pas
@{
{$MODE OBJFPC}
{$H+}
unit slitstatus;

interface

uses slitoutput, macrostore, slitstream;
                                         
function GetNomeProcessoreInformazioni():String;
procedure SetNomeProcessoreInformazioni(value:String);
function CreaStreamOutputDaOpzioni(NomeFile:String; store:TMacroStore):TSlitOutput;
procedure SegnalaInizioElaborazioneStream (stream:TSlitStream);
procedure SegnalaFineElaborazioneStream;
procedure LogErrorMessage(message:String);
function GetCurrentParsingFile:String;
function GetCurrentParsingLine:Integer;
function GetGenerazioneMarcatoriAbilitata:Boolean;
procedure SetGenerazioneMarcatoriAbilitata(value:Boolean);
function GetGenerazioneNumeriRigaAbilitata:Boolean;
procedure SetGenerazioneNumeriRigaAbilitata(value:Boolean);
function GetColonnaNumeriRiga:Integer;
procedure SetColonnaNumeriRiga(value:Integer);
procedure AggiungiLinguaggio (Estensione, Inizio, Fine:String);
procedure PrendiMarcatori (NomeFile:String; var Inizio:String; var Fine:String);

implementation

uses sysutils, slithtml, slitlout, slittxt, strutils;

type
  @<slitstatus, RInformazioniLinguaggi@>

var
  NomeProcessoreInformazioni:String;
  GenerazioneMarcatoriAbilitata:Boolean;
  GenerazioneNumeriRigaAbilitata:Boolean;
  StreamStack: array of TSlitStream;
  StreamStackCount: Integer;
  ColonnaNumeriRiga:Integer;
  TabellaLinguaggi:array of RInformazioniLinguaggi;
  TabellaLinguaggi_Count:Integer;

@<slitstatus, gestore del processore di documentazione@>
@<slitstatus, crea lo stream di output@>
@<slitstatus, SegnalaInizioElaborazioneStream@>
@<slitstatus, SegnalaFineElaborazioneStream@>
@<slitstatus, LogErrorMessage@>
@<slitstatus, GetCurrentParsingFile@>
@<slitstatus, GetCurrentParsingLine@>
@<slitstatus, Get/Set GenerazioneMarcatoriAbilitata@>
@<slitstatus, Get/Set GenerazioneNumeriRigaAbilitata@>
@<slitstatus, Get/Set ColonnaNumeriRiga@>
@<slitstatus, AggiungiLinguaggio@>
@<slitstatus, PrendiMarcatori@>

initialization

  NomeProcessoreInformazioni := 'lout';
  StreamStackCount := 0;
  GenerazioneMarcatoriAbilitata := True;
  GenerazioneNumeriRigaAbilitata := True;
  ColonnaNumeriRiga := 100;
  TabellaLinguaggi_Count := 0;
  SetLength (TabellaLinguaggi, 0);

  @<slitstatus, configurazione delle estensioni predefinite@>

end.
@}

@End @Section

@EndSections

@End @Chapter
