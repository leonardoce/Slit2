# -*- mode:lout -*-
@Section
@Title { HTML Output }
@Begin @PP

This backend write the output documentation in HTML format. @PP

The created file has @F {.html} extension added to the documentation filename:

@d TSlitOutputHtml.CreateForFile
@{
  constructor TSlitOutputHtml.CreateForFile(fileName:String);
  begin
    Assign(handle, ExtractFileName(fileName)+'.html');
    Rewrite(handle);
  end;
@}

The file is closed when the output stream get destroyed:

@d TSlitOutputHtml.Destroy
@{
  destructor TSlitOutputHtml.Destroy;
  begin
    Close(handle);
  end;
@}

Documentation lines are added exactly as they are in the documentation
file so you can add your tag to documentation:

@d TSlitOutputHtml.PutLine
@{
  procedure TSlitOutputHtml.PutLine(str:String);
  begin
    writeln(handle, str);
  end;
@}

Scraps are wrote as @F div elements and are inserted in preformatted blocks:

@d TSlitOutputHtml.ScriviScrap
@{
procedure TSlitOutputHtml.ScriviScrap(tipo:EScrapType; nome, contenuto:String);
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

  writeln(handle,
    '<div class=''testata''>', text2html(titolo), ' ', text2html(nome), '</div>');

  writeln(handle, '<pre>');
  writeln(handle, text2html(contenuto));
  writeln(handle, '</pre>');
end;
@}

This is the definition of the HTML backend:

@o slithtml.pas
@{
{$MODE OBJFPC}
{$H+}
unit slithtml;

interface
  uses slitoutput, macrostore;

type
  TSlitOutputHtml = class(TSlitOutput)
  private
    handle:Text;
  public
    constructor CreateForFile(fileName:String);
    destructor Destroy; override;

    procedure ScriviScrap(tipo:EScrapType; nome, contenuto:String); override;
    procedure PutLine(str:String); override;
  end;

implementation
  uses sysutils, strutils, classes, htmlutils;

  @<TSlitOutputHtml.CreateForFile@>
  @<TSlitOutputHtml.Destroy@>
  @<TSlitOutputHtml.PutLine@>
  @<TSlitOutputHtml.ScriviScrap@>

end.
@}
@End @Section
