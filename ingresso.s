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
process directives. In this way, we can use the parser to drive
different phases of the translation process. @PP

Drivers share this structure:

@d TSlitStreamDriver definition
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
  procedure ProcessEmitMacro(macroName:String); virtual; abstract;
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

Inside every macro definition you can call another macro with the
sintax @F @Verbatim { @<nomemacro@> }. @PP

The directive @F @Verbatim { @o } {@Char egrave} is like the
definition one but is used to write a file whose name is the name of
the macro. @PP 

The filename can be enclosed by quotation marks @F @Verbatim { ""
}. When this is true the quotation marks must be removed from the
filename. @PP

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

The directive @F @Verbatim { @+ } @Char egrave is somewhat similiar:
is't used to add content to an existing macro. 
To process an @I add directive we can use a process similiar to the
processing of the definition directive: from the parser point of view
the only difference is that he must call a different function of the
driver.

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

The directive @F @Verbatim { @i } is used to include a file in the
main file. This code will make the parser read another file using the
same macro store and the same output: @PP

@d processa direttiva i
@{
macroName := Trim(MidStr(lineBuffer, 3, Length(lineBuffer)-2));
temporaryStream := TSlitStream.CreateForFile(
  ExtractFilePath(FNomeFile) + macroName);
temporaryStream.Driver := FDriver;
temporaryStream.Process();
FreeAndNil(temporaryStream);
@}

The filename is interpresed as relative to the current file. @PP

The directive @F @Verbatim { @# } is the comment directive. All the
content following this directive is ignored. @PP

The directive @F @Verbatim { @x } is used to configure a parameter and
is managed by the current driver: @PP

@d processa direttiva x
@{
FDriver.ProcessaOpzione (MidStr(lineBuffer,3,Length(lineBuffer)-2));
@}

The directive @F "@e" can be used to emit, multiple times, a macro:

@d manage directive e
@{
macroName := Trim(MidStr(lineBuffer, 3, Length(lineBuffer)-2));

if AnsiStartsStr('"', macroName) and AnsiEndsStr('"', macroName) then
begin
  macroName := MidStr(macroName, 2, Length(macroName)-2);
end;

if FDriver <> Nil then
begin
  FDriver.ProcessEmitMacro(macroName);
end;  
@}

If the read row is not a directive then is interpreted as a
documentation line: @PP

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
    else if AnsiStartsStr('@e ', lineBuffer) then
    begin
      @<manage directive e@>
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
@Title { Streams }
@Begin @PP

Every file is opened then the stream get created:

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

and closed when the stream is destroyed:

@d TSlitStream.Destroy
@{
destructor TSlitStream.Destroy;
begin
  Close(Handle);
  inherited Destroy;
end;
@}

@d TSlitStream definition
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

The other operations call only the primitives of the stream:

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
@Title { slitstream definition }
@Begin @PP

@o slitstream.pas
@{
{$MODE OBJFPC}
{$H+}
unit slitstream;

interface
  uses macrostore, slitoutput;

type
  @<TSlitStreamDriver definition@>
  @<TSlitStream definition@>

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
