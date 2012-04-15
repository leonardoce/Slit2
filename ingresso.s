# -*- mode: lout -*-

@Chapter
@Title { Parser }
@Begin @LP

Slit read text files made be text rows and directives.
Directives are rows starting with the prefix @Verbatim {@} and
optionally followed by @I scraps. @PP

@BeginSections

@Section
@Title { Drivers }
@Begin @PP

The Slit file parter reads the source files and uses a driver to
process directives. In this war, we can use the parser to drive
different phases of the translation process. @PP

Drivers share this structure:

@d slitstream definizione TSlitStreamDriver
@{
TSlitStream = class;

TSlitStreamDriver = class
private
  FParser:TSlitStream;

public
  procedure ProcessaDefinizioneMacro(nomeMacro:String; scrap:String; 
    scrapStartLine:Integer); virtual; abstract;
  procedure ProcessaAggiungiNellaMacro(nomeMacro:String; scrap:String; 
    scrapStartLine:Integer); virtual; abstract;
  procedure ProcessaDefinizioneFile(nomeMacro:String; scrap:String; 
    scrapStartLine:Integer); virtual; abstract;
  procedure ProcessaRigaDocumentazione(riga:String);
    virtual; abstract;
  procedure ProcessaOpzione(opzione:String);
    virtual; abstract;

  property Parser:TSlitStream read FParser write FParser;
end;
@}

@End @Section

@Section @Title { Scrap reading }
@Begin @PP

A scrap represent a part of the body of a macro and starts with the
row @F @Verbatim @Begin @{ @End @Verbatim and ends with the row
@F @Verbatim @Begin @} @End @Verbatim .
@PP

A scrap can be read with this code:

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
    LogErrorMessage('Mi aspettavo l''inizio di una macro');
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

@End @Section

@Section 
@Title { Directives }
@Begin @PP

The macro definition directive @F @Verbatim {@d} permit to create a
new macro. The directive is followed by the name of the macro and the
content of the macro is the content of the following scrap. @PP

The @F @Verbatim {@d} is readden with this code:

@d processa direttiva d
@{
scrapStartLine := CurrentLine+2;
scrapBuffer := ReadScrap();
macroName := Trim(MidStr(lineBuffer, 3, Length(lineBuffer)-2));

if FDriver <> Nil then
begin
  FDriver.ProcessaDefinizioneMacro(macroName, scrapBuffer, scrapStartLine);
end;
@}

All'interno di ogni definizione si pu{@Char ograve} richiamare un'altra macro con la sintassi
@F @Verbatim { @<nomemacro@> }. Per ulteriori informazioni consultare il capitolo relativo all'output
dei file sorgenti. @PP

La direttiva @F @Verbatim { @o } {@Char egrave} equivalente ma la macro viene utilizzata per scrivere un file
il cui nome {@Char egrave} quello della macro. @PP

L'unica cosa da osservare {@Char egrave} che il nome del file potrebbe essere racchiuso fra
virtolette @F @Verbatim { "" }. In questo caso le virgolette devono essere rimosse
dalla stringa del nome della macro. @PP

@d processa direttiva o
@{
scrapStartLine := CurrentLine+2;
scrapBuffer := ReadScrap();
macroName := Trim(MidStr(lineBuffer, 3, Length(lineBuffer)-2));

if AnsiStartsStr('"', macroName) and AnsiEndsStr('"', macroName) then
begin
  macroName := MidStr(macroName, 2, Length(macroName)-2);
end;

if FDriver <> Nil then
begin
  FDriver.ProcessaDefinizioneFile(macroName, scrapBuffer, scrapStartLine);
end;  
@}

Anche la direttiva @F @Verbatim { @+ } @Char egrave piuttosto simile: infatti serve per
aggiungere contenuto ad una macro gi{@Char agrave} definita. 
Processare una direttiva @F @Verbatim {@+} @Char egrave molto simile a 
processare una direttiva @F @Verbatim { @d }: dal punto di vista del parser
basta solamente invocare una diversa funzione del driver:

@d processa direttiva +
@{
scrapStartLine := CurrentLine+1;
scrapBuffer := ReadScrap();
macroName := Trim(MidStr(lineBuffer, 3, Length(lineBuffer)-2));

if FDriver <> Nil then
begin
  FDriver.ProcessaAggiungiNellaMacro(macroName, scrapBuffer, scrapStartLine);
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

La direttiva @F @Verbatim { @# } ignora tutto quello che segue e pu{@Char ograve} venire utilizzata
come commento. @PP

La direttiva @F @Verbatim { @x } invece passa a Slit una opzione, che viene gestita
dal driver che @Char egrave agganciato al parser. @PP

@d processa direttiva x
@{
FDriver.ProcessaOpzione (MidStr(lineBuffer,3,Length(lineBuffer)-2));
@}

Se la riga letta non {@Char egrave} una direttiva allora questa viene direttamente scritta sull'output. @PP

@d TSlitStream.Process
@{
procedure TSlitStream.Process();
var
  lineBuffer:String;
  scrapBuffer:String;
  scrapStartLine:Integer;
  macroName:String;
  temporaryStream:TSlitStream;
begin
  SegnalaInizioElaborazioneStream(Self);
  FDriver.Parser := Self;

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
      { no-op, si tratta di un commento }
    end
    else if AnsiStartsStr('@x ', lineBuffer) then
    begin
      @<processa direttiva x@>
    end
    else if AnsiStartsStr('@+ ', lineBuffer) then
    begin
      @<processa direttiva +@>
    end
    else
    begin
      if FDriver <> Nil then
      begin
        FDriver.ProcessaRigaDocumentazione(lineBuffer);
      end;
    end;
  end;

  SegnalaFineElaborazioneStream;
end;
@}

@End @Section

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

  FCurrentLine := 0;
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
  FCurrentLine:integer;
  FNomeFile:String;
  FDriver:TSlitStreamDriver;
  handle:Text;

  function IsEof:Boolean;
public
  constructor CreateForFile(fileName:String);
  destructor Destroy; override;
  function NextLine:String;
  function ReadScrap():String;
  procedure Process();
  procedure ResetStream();

  property EOF:Boolean read IsEof;
  property Driver:TSlitStreamDriver read FDriver write FDriver;
  property CurrentFile:String read FNomeFile;
  property CurrentLine:Integer read FCurrentLine;
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
  FCurrentLine := currentLine + 1;
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
  uses sysutils, strutils, slitstatus;

  @<TSlitStream.CreateForFile@>
  @<TSlitStream.Destroy@>
  @<TSlitStream.ReadScrap@>
  @<TSlitStream.Process@>
  @<TSlitStream altre@>
end.
@}
@End @Section

@EndSections
@End @Chapter
