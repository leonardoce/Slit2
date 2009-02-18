% :folding=explicit:mode=slitpascal:

Slit legge dei file di testo che sono composti da righe di testo normale e da direttive.
Le direttive sono delle righe che iniziano con il prefisso "@" e che possono essere
seguite da uno "scrap".

Uno "scrap" e' una parte del corpo della macro (oppure l'intera macro) 
e che inizia con la riga "@{" e termina con la riga "@}".

Uno scrap viene quindi letto con il codice seguente:

@d TSlitStream.ReadScrap
@{
function TSlitStream.ReadScrap():String;
var
  buffer:String;
  bufferLine:String;
begin
  bufferLine := NextLine;
  if Trim(bufferLine)<>'@{' then
  begin
    LogError('Mi aspettavo l''inizio di una macro');
  end;

  buffer := '';
  while (not EOF) do
  begin
    bufferLine := NextLine();
    if Trim(bufferLine)='@}' then
    begin
      break;
    end;
    buffer := buffer + bufferLine + Chr(13) + Chr(10);
  end;
  Result := buffer;
end;
@}

=== Il driver del parser ===

Il parser dei files di Slit interpreta direttamente i file e chiama un driver
per processare le direttive. In questo modo è possibile sfruttare il parser
per effettuare più operazioni in fasi diverse del processo.

Il driver del parser è una classe che ha la seguente struttura:

@d slitstream definizione TSlitStreamDriver
@{
TSlitStreamDriver = class
public
  procedure ProcessaDefinizioneMacro(nomeMacro:String; scrap:String); 
    virtual; abstract;
  procedure ProcessaDefinizioneFile(nomeMacro:String; scrap:String);  
    virtual; abstract;
  procedure ProcessaRigaDocumentazione(riga:String);
    virtual; abstract;
end;
@}

=== Direttive ===

La direttiva di definizione ""@d"" permette di definire una nuova macro, il cui
nome segue la direttiva, con il contenuto dello scrap che segue.

Per questo viene processata cosi':

@d processa direttiva d
@{
scrapBuffer := ReadScrap();
macroName := Trim(MidStr(lineBuffer, 3, Length(lineBuffer)-2));

if FDriver <> Nil then
begin
  FDriver.ProcessaDefinizioneMacro(macroName, scrapBuffer);
end;
@}

All'interno di ogni definizione si puo' richiamare un'altra macro con la sintassi
"@<nomemacro@>". Per ulteriori informazioni consultare il capitolo relativo all'output
dei file sorgenti.

La direttiva ""@o"" e' equivalente ma la macro viene utilizzata per scrivere un file
il cui nome e' quello della macro.

L'unica cosa da osservare e' che il nome del file potrebeb essere racchiuso fra
virtolette ("\""). In questo caso le virgolette devono essere rimosse
dalla stringa del nome della macro.

@d processa direttiva o
@{
scrapBuffer := ReadScrap();
macroName := Trim(MidStr(lineBuffer, 3, Length(lineBuffer)-2));

if AnsiStartsStr('"', macroName) and AnsiEndsStr('"', macroName) then
begin
  macroName := MidStr(macroName, 2, Length(macroName)-2);
end;

if FDriver <> Nil then
begin
  FDriver.ProcessaDefinizioneFile(macroName, scrapBuffer);
end;  
@}

La direttiva ""@i"" permette invece di includere un file in un'altro e quindi
richiama la lettura di un altro file sorgente utilizzando lo stesso magazzino
di macro e lo stesso output:

@d processa direttiva i
@{
  macroName := Trim(MidStr(lineBuffer, 3, Length(lineBuffer)-2));
  temporaryStream := TSlitStream.CreateForFile(macroName);
  temporaryStream.Driver := FDriver;
  temporaryStream.Process();
  FreeAndNil(temporaryStream);
@}

Se la riga letta non e' una direttiva allora questa viene direttamente scritta sull'output.

@d TSlitStream.Process
@{
procedure TSlitStream.Process();
var
  lineBuffer:String;
  scrapBuffer:String;
  macroName:String;
  temporaryStream:TSlitStream;
begin
  writeln('Leggo ', FNomeFile);

  while (not Eof) do
  begin
    lineBuffer := NextLine();
    lineBuffer := Trim(lineBuffer);

    if AnsiStartsStr('@d ',lineBuffer) then
    begin
      @<processa direttiva d@>
    end
    else if AnsiStartsStr('@o ', lineBuffer) then
    begin
      @<processa direttiva o@>
    end
    else if AnsiStartsStr('@i ', lineBuffer) then
    begin
      @<processa direttiva i@>
    end
    else
    begin
      if FDriver <> Nil then
      begin
        FDriver.ProcessaRigaDocumentazione(lineBuffer);
      end;
    end;
  end;
end;
@}

=== Gestione del file di input ===

Il file viene aperto quando il flusso viene costruito:

@d TSlitStream.CreateForFile
@{
constructor TSlitStream.CreateForFile(fileName:String);
begin
  if not FileExists(fileName) then
  begin
    writeln('Il file ', fileName, ' non esiste');
    Abort;
  end;

  currentLine := 0;
  FNomeFile := fileName;
  FDriver := Nil;
  Assign(handle, fileName);
  Reset(handle);
end;
@}

e chiuso quando il flusso viene dismesso:

@d TSlitStream.Destroy
@{
destructor TSlitStream.Destroy;
begin
  Close(Handle);
  inherited Destroy;
end;
@}

@d slitstream definizione TSlitStream
@{
TSlitStream = class
private
  currentLine:integer;
  FNomeFile:String;
  FDriver:TSlitStreamDriver;
  handle:Text;

  function IsEof:Boolean;
public
  constructor CreateForFile(fileName:String);
  destructor Destroy; override;
  function NextLine:String;
  procedure LogError(msg:String);
  function ReadScrap():String;
  procedure Process();
  procedure ResetStream();

  property EOF:Boolean read IsEof;
  property Driver:TSlitStreamDriver read FDriver write FDriver;
end;
@}

Ci sono poi altre operazioni che richiamano semplicemente quelle primitive:

@d TSlitStream altre
@{
function TSlitStream.NextLine:String;
var
  bufLine:String;
begin
  readln(Handle, bufLine);
  currentLine := currentLine + 1;
  Result := bufLine;
end;

function TSlitStream.IsEof:Boolean;
begin
  Result := system.EOF(handle);
end;

procedure TSlitStream.ResetStream();
begin
  Reset(handle);
end;
@}

=== Segnalazione degli errori ===

Gli errori vengono segnalati in riferimento alla riga correntemente
processata il cui progressivo viene tenuto in "currentLine":

@d TSlitStream.LogError
@{
procedure TSlitStream.LogError(msg:String);
begin
  writeln(FNomeFile, ' errore: ', msg, ' alla riga ', currentLine);
end;
@}

=== Definizione dei flussi di ingresso ===

@o slitstream.pas
@{
{$MODE OBJFPC}
{$H+}
unit slitstream;

interface
  uses macrostore, slitoutput;

type
  @<slitstream definizione TSlitStreamDriver@>
  @<slitstream definizione TSlitStream@>

implementation
  uses sysutils, strutils;

  @<TSlitStream.CreateForFile@>
  @<TSlitStream.Destroy@>
  @<TSlitStream.LogError@>
  @<TSlitStream.ReadScrap@>
  @<TSlitStream.Process@>
  @<TSlitStream altre@>
end.
@}
