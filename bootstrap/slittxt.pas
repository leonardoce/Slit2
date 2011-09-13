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

    procedure ScriviScrap(tipo:EMacroType; nome, contenuto:String); override;
    procedure PutLine(str:String); override;
  end;

implementation
  uses sysutils, strutils, classes;

  constructor TSlitOutputTxt.CreateForFile(fileName:String);
  begin
    Assign(handle, ExtractFileName(fileName)+'.txt');
    Rewrite(handle);
  end;
  destructor TSlitOutputTxt.Destroy;
  begin
    Close(handle);
  end;
  procedure TSlitOutputTxt.PutLine(str:String);
  begin
    writeln(handle, str);
  end;
  procedure TSlitOutputTxt.ScriviScrap(tipo:EMacroType; nome, contenuto:String);
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
  
    writeln(handle, '------------------------------');
    writeln(handle, '| **', titolo, ' ', nome, '**');
    writeln(handle, '------------------------------');
  
    writeln(handle, '```');
    writeln(handle, contenuto);
    writeln(handle, '```');
    writeln(handle, '------------------------------');
  end;

end.
