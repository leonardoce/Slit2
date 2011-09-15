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

@End @SubSection

@SubSection @Title { Generazione dei marcatori di inizio e file sezione }
@Begin @PP

Nei file sorgenti generati @Char egrave possibile conservare il nome
della macro Slit che @Char egrave stata letta per generarli. @PP

@d Get/Set GenerazioneMarcatoriAbilitata
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

@d Get/Set GenerazioneNumeriRigaAbilitata
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

implementation

uses sysutils, slithtml, slitlout, slittxt;

var
  NomeProcessoreInformazioni:String;
  GenerazioneMarcatoriAbilitata:Boolean;
  GenerazioneNumeriRigaAbilitata:Boolean;
  StreamStack: array of TSlitStream;
  StreamStackCount: Integer;

@<slitstatus, gestore del processore di documentazione@>
@<slitstatus, crea lo stream di output@>
@<slitstatus, SegnalaInizioElaborazioneStream@>
@<slitstatus, SegnalaFineElaborazioneStream@>
@<slitstatus, LogErrorMessage@>
@<slitstatus, GetCurrentParsingFile@>
@<slitstatus, GetCurrentParsingLine@>
@<Get/Set GenerazioneMarcatoriAbilitata@>
@<Get/Set GenerazioneNumeriRigaAbilitata@>

initialization

NomeProcessoreInformazioni := 'lout';
StreamStackCount := 0;
GenerazioneMarcatoriAbilitata := True;
GenerazioneNumeriRigaAbilitata := True;
end.
@}

@End @Section

@EndSections

@End @Chapter
