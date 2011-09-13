{$MODE OBJFPC}
{$H+}
unit slitlout;

interface
  uses slitoutput, macrostore;

type
  TSlitOutputLout = class(TSlitOutput)
  private
    handle:Text;
    FStore: TMacroStore;
  public
    constructor CreateForFileAndStore(fileName:String; store:TMacroStore);
    destructor Destroy; override;
    procedure PutLine(str:String); override;
    procedure ScriviScrap(tipo:EMacroType; nome, contenuto:String); override;
  end;

implementation
  uses sysutils, strutils, classes;

  constructor TSlitOutputLout.CreateForFileAndStore(fileName:String; store:TMacroStore);
  begin
    Assign(handle, ExtractFileName(fileName)+'.lout');
    Rewrite(handle);
    FStore := store;
  end;
  destructor TSlitOutputLout.Destroy;
  begin
    Close(handle);
  end;
  procedure TSlitOutputLout.PutLine(str:String);
  begin
    writeln(handle, str);
  end;
  procedure TSlitOutputLout.ScriviScrap(tipo:EMacroType; nome, contenuto:String);
  var
    stringhe:TStringList;
    spazi:integer;
    stringaPulita, nomeDefinizione: String;
    i, j:integer;
    currentMacro, macroTemp: TMacroRecord;  
  begin
    currentMacro := FStore.GetMacro(nome);
  
    writeln(handle, '@PageMark { ', currentMacro.macroProgr, ' } ');
    
    writeln(handle, '@LeftDisplay lines @Break {');  
    write(handle, '@Sym angleleft { BoldSlope } @Font @','Verbatim @Begin ');
    write(handle, nome, ' @','End @','Verbatim ');
    
    write(handle, '@I {', currentMacro.macroProgr, ' } ');
    
    writeln(handle, ' @Sym angleright @Sym equivalence');
    stringhe := TStringList.Create;
    stringhe.Text := contenuto;
    
    for i := 0 to stringhe.Count-1 do
    begin
      spazi := Length( stringhe.Strings[i] );
      spazi := spazi - Length(TrimLeft(stringhe.Strings[i]));
      for j := 1 to spazi do
      begin
        write(handle, ' ');
      end;
      write(handle, '   ');
      
      stringaPulita := Trim(stringhe.Strings[i]);
      if AnsiStartsStr('@<', stringaPulita) and
         AnsiEndsStr('@>', stringaPulita) then
      begin
        nomeDefinizione := MidStr(stringaPulita, 3, Length(stringaPulita)-4);
        macroTemp := FStore.GetMacro( nomeDefinizione );
        write(handle, '@I { ', macroTemp.macroProgr, ' } @CrossLink ');
        stringaPulita := '<' + nomeDefinizione + ' ' + IntToStr(macroTemp.macroProgr) + '>';
      end;
      
      write(handle, '@','Verbatim @','Begin ');
      write(handle, stringaPulita);
      writeln(handle, '@','End @','Verbatim'); 
    end;
    
    FreeAndNil(stringhe);
    if currentMacro.macroUsersCount <> 0 then
    begin
      write(handle, '{ -1p setsmallcaps 0.9 } @Font { ');
      write(handle, 'Usata da: ');
      for i:=0 to currentMacro.macroUsersCount-1 do
      begin
        write(handle, ' { ', currentMacro.macroUsers[i], ' } @CrossLink { ');
        write(handle, currentMacro.macroUsers[i], ' } ');
      end;
      write(handle, ' } ');
    end;
    writeln(handle, '}');
  end;

end.
