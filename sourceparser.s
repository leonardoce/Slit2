# -*- mode:lout -*-
@Chapter
@Title { Source code file parser }
@Begin @LP

Slit can also do reverse literate programming. It reads source code
file with specifics markers and populate the macro store using their
content. @PP

Row markers are already used for @I {code folding} in source code
editors supporting it: in effect code folding and literate programming
are the same idea from a different point of view. @PP

This is an example of source code who can be read by Slit: @PP

@F @Verbatim @Begin
// [open] includes
#include <stdio.h>
// [end] includes

// [open] main
int main(int argc, char **argv) {
  printf("Hello world!");
}
// [close] main
@End @Verbatim @PP @PP

The previous source code will generate, when read, two macros: @F
includes and @F main. @PP

With reverse literate programming you can write your source code using
the development tool you like and then document it with @I Slit. @PP

@BeginSections

@Section @Title { Folding markers }
@Begin @PP

Folding markers use this syntax: @PP

@F @Verbatim @Begin
<comment start> [open]|[close] <macro name> <comment end>
@End @Verbatim @PP @PP

The string that starts and ends a comment is already managed by the
slit parameters used to generate source code. @PP

The following function detect if a line is a comment start or end: 

@d TSourceStream.RowIsMarker
@{
function TSourceStream.RowIsMarker(line:String):boolean;
begin
  Result := False;
  line := Trim(line);
  
  if AnsiStartsStr(FCommentStart, line) and AnsiEndsStr(FCommentEnd, line) then
  begin
    line := MidStr (line, Length(FCommentStart)+1, Length(Line));
    line := LeftStr (line, Length(line)-Length(FCommentEnd));
    line := Trim(Line);
    if AnsiStartsStr('[open]', line) or AnsiStartsStr('[close]', line) then
    begin
      Result := True;
    end;
  end
  else
  begin
    Result := False;
  end;
end;
@}

Markers can be opening or closing a macro:

@d EMarkerType
@{
EMarkerType = (OpeningMarker, ClosingMarker);
@}

Now we need a function to extract the macro name and the marker type:

@d TSourceStream.InfoFromMarker
@{
procedure TSourceStream.InfoFromMarker
  (Line:String; var MacroName:String; var MarkerType:EMarkerType);
begin
  line := Trim(line);
  
  if AnsiStartsStr(FCommentStart, line) and AnsiEndsStr(FCommentEnd, line) then
  begin
    line := MidStr (line, Length(FCommentStart)+1, Length(Line));
    line := LeftStr (line, Length(line)-Length(FCommentEnd));
    line := Trim(Line);
    if AnsiStartsStr('[open]', line) then
    begin
      line := MidStr (line, 7, Length(line));
      MacroName := Trim(line);
    end
    else if AnsiStartsStr('[close]', line) then
    begin
      line := MidStr (line, 8, Length(line));
      MacroName := Trim(line);
    end;
  end;
end;
@}
@End @Section

@Section @Title { Reading macros } 
@Begin @PP



@End @Section

@Section @Title { File management }
@Begin @PP

A sourcestream will be constructed with a file name:

@d TSourceStream.CreateForFile
@{
constructor TSourceStream.CreateForFile (fileName:String; MacroStore:TMacroStore);
begin
  FFileName := fileName;
  FCurrentLine := 0;
  FMacroStore := MacroStore;

  PrendiMarcatori (FFileName, FCommentStart, FCommentEnd);

  Assign (handle, fileName);
  Reset (handle);
end;
@}

When the stream is destroyed the file is closed:

@d TSourceStream.Destroy
@{
destructor TSourceStream.Destroy;
begin
  Close (handle);
  inherited Destroy;
end;
@}

Source files are read line by line:

@d TSourceStream.NextLine
@{
function TSourceStream.NextLine:String;
var 
  bufLine : String;
begin
  readln(handle, bufLine);
  FCurrentLine := FCurrentLine+1;
  Result := bufLine;
end;
@}

The eof condition is checked using the system functions:

@d TSourceStream.Eof
@{
function TSourceStream.Eof:Boolean;
begin
  Result := System.Eof(Handle);
end;
@}

@End @Section

@Section @Title { TSourceStream class }
@Begin @PP

This is the definition of the class

@d TSourceStream
@{
TSourceStream = class
private
  FCommentStart, FCommentEnd:String;
  FFileName:String;
  FCurrentLine:Integer;
  FMacroStore:TMacroStore;
  Handle:Text;

  function NextLine:String;
  function Eof:Boolean;

  function RowIsMarker(line:String):boolean;
  procedure InfoFromMarker(Line:String; var MacroName:String; 
    var MarkerType:EMarkerType);

public
  constructor CreateForFile (fileName:String; MacroStore:TMacroStore);
  destructor Destroy; override;
end;
@}

@End @Section

@Section @Title { slitsource unit definition }
@Begin @PP

@o slitsource.pas
@{
{$MODE OBJFPC}
{$H+}
unit slitsource;

interface
  uses MacroStore;

type

  @<EMarkerType@>
  @<TSourceStream@>

implementation
  uses SysUtils, Classes, StrUtils, slitstatus;

  @<TSourceStream.CreateForFile@>
  @<TSourceStream.RowIsMarker@>
  @<TSourceStream.InfoFromMarker@>
  @<TSourceStream.Destroy@>
  @<TSourceStream.NextLine@>
  @<TSourceStream.Eof@>
end.
@}

@End @Section

@EndSections

@End @Chapter
