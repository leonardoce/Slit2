# -*- mode:lout -*-
@Section
@Title { Output in formato testo }
@Begin @PP

Slit può anche lavorare con file in formato testo. In questo caso i file sono
pensati per essere utilizzati con txt2tags. @PP

Al file di documentazione viene aggiunta l'estensione ".txt": 

@d TSlitOutputTxt.CreateForFile
@{
constructor TSlitOutputTxt.CreateForFile(fileName:String);
begin
  Assign(handle, ExtractFileName(fileName)+'.txt');
  Rewrite(handle);
end;
@}

E al rilascio del flusso il file viene chiuso:

@d TSlitOutputTxt.Destroy
@{
destructor TSlitOutputTxt.Destroy;
begin
  Close(handle);
end;
@}

Le linee di documentazione vengono inserite nel file direttamente come sono:

@d TSlitOutputTxt.PutLine
@{
procedure TSlitOutputTxt.PutLine(str:String);
begin
  writeln(handle, str);
end;
@}

La testata viene scritta in modo che prima e dopo di essa ci sia una
linea orizzontale. Il titolo della testata viene scritto in grassetto. @PP

Gli scrap vengono scritti fra blocchi di testo in modo che non vengono
interpretati da txt2tags:

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

Riassumendo:

@o slittxt.pas
@{
{$MODE OBJFPC}
{$H+}
unit slittxt;

interface
  uses slitstream, slitoutput, macrostore;

type

  TSlitOutputTxt = class(TSlitOutput)
  private
    handle:Text;
  public
    constructor CreateForFile(fileName:String);
    destructor Destroy; override;

    procedure ScriviScrap(tipo:EScrapType; nome, contenuto:String); override;
    procedure PutLine(str:String); override;
  end;

implementation
  uses sysutils, strutils, classes;

  @<TSlitOutputTxt.CreateForFile@>
  @<TSlitOutputTxt.Destroy@>
  @<TSlitOutputTxt.PutLine@>
  @<TSlitOutputTxt.ScriviScrap@>

end.
@}
@End @Section
