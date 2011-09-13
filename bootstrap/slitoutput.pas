                                                      
{$MODE OBJFPC}                                  
{$H+}
unit slitoutput;

interface
  uses macrostore;

type
  TSlitOutput = class
  public                      
    procedure ScriviScrap(tipo:EMacroType; nome, contenuto:String); virtual; abstract;
    procedure PutLine(str:String); virtual; abstract;
  end;

implementation

end.
