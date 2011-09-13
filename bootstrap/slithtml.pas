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

    procedure ScriviScrap(tipo:EMacroType; nome, contenuto:String); override;
    procedure PutLine(str:String); override;
  end;

implementation
  uses sysutils, strutils, classes, htmlutils;

    constructor TSlitOutputHtml.CreateForFile(fileName:String);
    begin
      Assign(handle, ExtractFileName(fileName)+'.html');
      Rewrite(handle);
    end;
    destructor TSlitOutputHtml.Destroy;
    begin
      Close(handle);
    end;
    procedure TSlitOutputHtml.PutLine(str:String);
    begin
      writeln(handle, str);
    end;
  procedure TSlitOutputHtml.ScriviScrap(tipo:EMacroType; nome, contenuto:String);
  var
    titolo: String;
  begin
    if tipo = FileMacro then
    begin
      titolo := 'File';
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

end.
