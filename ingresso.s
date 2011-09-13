# :folding=explicit:mode=slitpascal:

@Chapter
@Title { Il formato dei files di ingresso }
@Begin @LP

Slit legge dei file di testo che sono composti da righe di testo normale e da direttive.
Le direttive sono delle righe che iniziano con il prefisso @Verbatim {@} e che possono essere
seguite da uno @I {scrap}. @PP

Uno @I {scrap} è una parte del corpo della macro (oppure l'intera macro) 
e che inizia con la riga @F @Verbatim @Begin @{ @End @Verbatim e termina con la riga @F @Verbatim @Begin @} @End @Verbatim .
@PP

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

@BeginSections

# Il driver del parser {{{
@Section
@Title { Il driver del parser }
@Begin @PP

Il parser dei files di Slit interpreta direttamente i file e chiama un driver
per processare le direttive. In questo modo è possibile sfruttare il parser
per effettuare più operazioni in fasi diverse del processo. @PP

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

@End @Section
# }}}

# Direttive {{{
@Section 
@Title { Direttive }
@Begin @PP

La direttiva di definizione @F @Verbatim {@d} permette di definire una nuova macro, il cui
nome segue la direttiva, con il contenuto dello scrap che segue. @PP

Per questo viene processata così:

@d processa direttiva d
@{
scrapBuffer := ReadScrap();
macroName := Trim(MidStr(lineBuffer, 3, Length(lineBuffer)-2));

if FDriver <> Nil then
begin
  FDriver.ProcessaDefinizioneMacro(macroName, scrapBuffer);
end;
@}

All'interno di ogni definizione si può richiamare un'altra macro con la sintassi
@F @Verbatim { @<nomemacro@> }. Per ulteriori informazioni consultare il capitolo relativo all'output
dei file sorgenti. @PP

La direttiva @F @Verbatim { @o } è equivalente ma la macro viene utilizzata per scrivere un file
il cui nome è quello della macro. @PP

L'unica cosa da osservare è che il nome del file potrebbe essere racchiuso fra
virtolette @F @Verbatim { "" }. In questo caso le virgolette devono essere rimosse
dalla stringa del nome della macro. @PP

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

La direttiva @F @Verbatim { @i } permette invece di includere un file in un'altro e quindi
richiama la lettura di un altro file sorgente utilizzando lo stesso magazzino
di macro e lo stesso output: @PP

@d processa direttiva i
@{
  macroName := Trim(MidStr(lineBuffer, 3, Length(lineBuffer)-2));
  temporaryStream := TSlitStream.CreateForFile(
    ExtractFilePath(FNomeFile) + macroName);
  temporaryStream.Driver := FDriver;
  temporaryStream.Process();
  FreeAndNil(temporaryStream);
@}

Il nome del file viene interpretato in modo relativo al file corrente. @PP

La direttiva @F @Verbatim { @# } ignora tutto quello che segue e può venire utilizzata
come commento. @PP

Se la riga letta non è una direttiva allora questa viene direttamente scritta sull'output. @PP

@d TSlitStream.Process
@{
procedure TSlitStream.Process();
var
  lineBuffer:String;
  scrapBuffer:String;
  macroName:String;
  temporaryStream:TSlitStream;
begin
  while (not Eof) do
  begin
    lineBuffer := NextLine();

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
    else if AnsiStartsStr('@# ', lineBuffer) then
    begin
      { no-op }
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

@End @Section
# }}}

# Gestione del file di input {{{
@Section
@Title { Gestione del file di input }
@Begin @PP

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

@End @Section
# }}}

# Segnalazione degli errori {{{
@Section
@Title { Segnalazione degli errori }
@Begin @PP

Gli errori vengono segnalati in riferimento alla riga correntemente
processata il cui progressivo viene tenuto in @F { currentLine }:

@d TSlitStream.LogError
@{
procedure TSlitStream.LogError(msg:String);
begin
  writeln(FNomeFile, ' errore: ', msg, ' alla riga ', currentLine);
end;
@}
@End @Section
# }}}

# Definizione dei flussi di ingresso {{{
@Section
@Title { Definizione dei flussi di ingresso }
@Begin @PP

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
@End @Section
# }}}

@EndSections
@End @Chapter
