# -*- mode:lout -*-
@Section
@Title { Text output }
@Begin @PP

Slit can also write text files. The format used is designed to use the
@F txt2tags software. @PP

@d TSlitOutputTxt.CreateForFile
@{
constructor TSlitOutputTxt.CreateForFile(fileName:String);
begin
  Assign(handle, ExtractFileName(fileName)+'.txt');
  Rewrite(handle);
end;
@}

When the backend is destroyed the stream get closed:

@d TSlitOutputTxt.Destroy
@{
destructor TSlitOutputTxt.Destroy;
begin
  Close(handle);
end;
@}

The documentation lines are added to the output file exactly as they are:

@d TSlitOutputTxt.PutLine
@{
procedure TSlitOutputTxt.PutLine(str:String);
begin
  writeln(handle, str);
end;
@}

A scrap is written in @F txt2tags format. The title of the scrap is
written in bold and the content of the scrap is added to a text
block. @PP

@d TSlitOutputTxt.ScriviScrap
@{
procedure TSlitOutputTxt.ScriviScrap(tipo:EScrapType; nome, contenuto:String);
var
  titolo: String;
begin
  if tipo = FileScrap then
  begin
    titolo := 'File';
  end
  else if tipo = AppendScrap then
  begin
    titolo := 'Aggiunta alla definizione';
  end
  else
  begin
    titolo := 'Definizione';
  end;

  writeln(handle, '------------------------------');
  writeln(handle, '| **', titolo, ' ', nome, '**');
  writeln(handle, '------------------------------');

  writeln(handle, '```');
  writeln(handle, contenuto);
  writeln(handle, '```');
  writeln(handle, '------------------------------');
end;
@}

This is the text front-end:

@d TSlitOutputTxt
@{
TSlitOutputTxt = class(TSlitOutput)
private
  handle:Text;
public
  constructor CreateForFile(fileName:String);
  destructor Destroy; override;

  procedure ScriviScrap(tipo:EScrapType; nome, contenuto:String); override;
  procedure PutLine(str:String); override;
end;
@}

This is the definition of the text backend:

@o slittxt.pas
@{
{$MODE OBJFPC}
{$H+}
unit slittxt;

interface
  uses slitstream, slitoutput, macrostore;

type

  @<TSlitOutputTxt@>

implementation
  uses sysutils, strutils, classes;

  @<TSlitOutputTxt.CreateForFile@>
  @<TSlitOutputTxt.Destroy@>
  @<TSlitOutputTxt.PutLine@>
  @<TSlitOutputTxt.ScriviScrap@>

end.
@}
@End @Section
