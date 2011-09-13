{$MODE OBJFPC}
{$H+}
unit slitstream;

interface
  uses macrostore, slitoutput;

type
  TSlitStreamDriver = class
  public
    procedure ProcessaDefinizioneMacro(nomeMacro:String; scrap:String); 
      virtual; abstract;
    procedure ProcessaDefinizioneFile(nomeMacro:String; scrap:String);  
      virtual; abstract;
    procedure ProcessaRigaDocumentazione(riga:String);
      virtual; abstract;
  end;
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

implementation
  uses sysutils, strutils;

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
  destructor TSlitStream.Destroy;
  begin
    Close(Handle);
    inherited Destroy;
  end;
  procedure TSlitStream.LogError(msg:String);
  begin
    writeln(FNomeFile, ' errore: ', msg, ' alla riga ', currentLine);
  end;
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
  procedure TSlitStream.Process();
  var
    lineBuffer, lineBufferOrig:String;
    scrapBuffer:String;
    macroName:String;
    temporaryStream:TSlitStream;
  begin
    while (not Eof) do
    begin
      lineBufferOrig := NextLine();
      lineBuffer := Trim(lineBufferOrig);
  
      if AnsiStartsStr('@d ',lineBuffer) then
      begin
        scrapBuffer := ReadScrap();
        macroName := Trim(MidStr(lineBuffer, 3, Length(lineBuffer)-2));
        
        if FDriver <> Nil then
        begin
          FDriver.ProcessaDefinizioneMacro(macroName, scrapBuffer);
        end;
      end
      else if AnsiStartsStr('@o ', lineBuffer) then
      begin
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
      end
      else if AnsiStartsStr('@i ', lineBuffer) then
      begin
          macroName := Trim(MidStr(lineBuffer, 3, Length(lineBuffer)-2));
          temporaryStream := TSlitStream.CreateForFile(
            ExtractFilePath(FNomeFile) + macroName);
          temporaryStream.Driver := FDriver;
          temporaryStream.Process();
          FreeAndNil(temporaryStream);
      end
      else if AnsiStartsStr('@# ', lineBuffer) then
      begin
        { no-op }
      end
      else
      begin
        if FDriver <> Nil then
        begin
          FDriver.ProcessaRigaDocumentazione(lineBufferOrig);
        end;
      end;
    end;
  end;
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
end.
