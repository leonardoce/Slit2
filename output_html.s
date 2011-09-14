@Section
@Title { Output in formato HTML }
@Begin @PP

L'output in formato HTML permette di creare un file HTML per la documentazione.
I tag devono essere immessi manualmente (eccetto quegli che gestiscono gli scrap).

Il file creato ha l'estensione @F {.html} aggiunta al nome del file di documentazione:

@d TSlitOutputHtml.CreateForFile
@{
  constructor TSlitOutputHtml.CreateForFile(fileName:String);
  begin
    Assign(handle, ExtractFileName(fileName)+'.html');
    Rewrite(handle);
  end;
@}

Il file viene chiuso quando l'output viene liberato:

@d TSlitOutputHtml.Destroy
@{
  destructor TSlitOutputHtml.Destroy;
  begin
    Close(handle);
  end;
@}

Le righe di documentazione vengono inserite esattamente come
sono all'interno del file HTML. Questo permette di
aggiungere dei tag alla documentazione:

@d TSlitOutputHtml.PutLine
@{
  procedure TSlitOutputHtml.PutLine(str:String);
  begin
    writeln(handle, str);
  end;
@}

La testata viene scritta in un ""div"" e gli scrap vengono inseriti in 
blocchi preformattati:

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

Riassumendo, il codice di gestione del formato HTML @Char egrave il seguente:

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
