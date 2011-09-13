{$MODE OBJFPC}
{$H+}
unit macrostore;

interface

type
  EMacroType = ( FileMacro, ScrapMacro );
  
  TMacroRecord = record
    macroName:String;
    macroProgr:Integer;
    macroContent:String;
    macroType:EMacroType;
    macroUsers: array of Integer;
    macroUsersCount: Integer;
  end;
  TMacroStore = class
  private
    count:integer;
    store:array of TMacroRecord;
  public
    constructor Create;
    function MacroCount:Integer;
    procedure StoreMacro(macroName:String; macroContent:String; macroType:EMacroType);
    function GetMacro(macroName:String):TMacroRecord;
    function GetRecord(i:integer):TMacroRecord;
    procedure CalcolaRiferimenti;
  end;

implementation
  uses SysUtils, Classes, StrUtils;

  constructor TMacroStore.Create;
  begin
    count:=0;
    SetLength(store, 50);
  end;
  procedure TMacroStore.StoreMacro(macroName:String; macroContent:String; macroType:EMacroType);
  begin
    if GetMacro(macroName).macroName <> '' then
    begin
      writeln('Attenzione: macro ', macroName, ' duplicata.');
    end
    else
    begin
      if count>=Length(store) then
      begin
        SetLength(store, Length(Store)+50);
      end;
      store[count].macroName := macroName;
      store[count].macroContent := macroContent;
      store[count].macroType := macroType;
      store[count].macroProgr := count + 1;
      count := count + 1;
    end;
  end;
  function TMacroStore.GetMacro(macroName:String):TMacroRecord;
  var
    i:integer;
  begin
    Result.macroName := '';
    for i:=0 to length(Store)-1 do
    begin
      if store[i].macroName=macroName then
      begin
        Result:=store[i];
        exit;
      end;
    end;
  end;
  function TMacroStore.MacroCount:Integer;
  begin
    Result := count;
  end;
  function TMacroStore.GetRecord(i:integer):TMacroRecord;
  begin
    Result := store[i];
  end;
  procedure TMacroStore.CalcolaRiferimenti;
  var
    i, j, k: Integer;
    listaStringhe: TStringList;
    stringaPulita: String;
    tempRecord: TMacroRecord;
  begin
    listaStringhe := TStringList.Create;
    
    for i:=0 to count-1 do
    begin
      store[i].macroUsersCount:=0;
      SetLength(store[i].macroUsers, 10); 
    end;
    for i:=0 to count-1 do
    begin
      listaStringhe.Text := store[i].macroContent;
    
      for j := 0 to listaStringhe.Count-1 do
      begin
        stringaPulita := Trim( listaStringhe.Strings[j] );
        if AnsiStartsStr('@<', stringaPulita) and
           AnsiEndsStr('@>', stringaPulita) then
        begin
          stringaPulita := MidStr(stringaPulita, 3, Length(stringaPulita)-4);
          tempRecord := GetMacro( stringaPulita );
          if tempRecord.macroProgr<>0 then
          begin
            k := tempRecord.macroProgr-1;
            
            if store[k].macroUsersCount = Length(store[k].macroUsers) then
            begin
              SetLength(store[k].macroUsers, Length(store[k].macroUsers)+10);
            end;
            store[k].macroUsers[store[k].macroUsersCount] := i + 1;
            store[k].macroUsersCount := store[k].macroUsersCount + 1;
          end;
        end;
      end;
    end;
    
    FreeAndNil( listaStringhe );
  end;
end.
