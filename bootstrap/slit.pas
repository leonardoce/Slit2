{$MODE objfpc}
{$H+}
program slit;

uses Classes, macrostore, sysutils, 
    strutils, slitstream, slithtml,
    slittxt, slitoutput, slitlout,
    drivermagazzino, driverdoc;

var
  store:TMacroStore;
  stream:TSlitStream;
  streamHtml: TSlitOutput;
  streamOutput:Text;
  driverMagazzinoMacro: TSlitStreamDriver;
  driverScriviDocumentazione: TSlitStreamDriver;

  procedure ScriviScrapEspanso(nome:String; indent:Integer);
  var
    stringhe:TStringList;
    rec:TMacroRecord;
    i:integer;
    tempStringa:String;
    tempIndentazione:String;
  begin
    tempIndentazione := '';
    for i := 1 to indent do
    begin
      tempIndentazione := tempIndentazione + ' ';
    end;
  
    rec := store.GetMacro(nome);
    if rec.macroName = '' then
    begin
      writeln(streamOutput, '<', nome, '>');
      writeln(StdErr, 'Attenzione: macro ', nome, ' sconosciuta');
    end
    else
    begin
      stringhe := TStringList.Create;
      stringhe.Text := rec.macroContent;
      for i := 0 to stringhe.Count-1 do
      begin
        tempStringa := Trim(stringhe.Strings[i]);
        if AnsiStartsStr('@<', tempStringa) and AnsiEndsStr('@>', tempStringa) then
        begin
          ScriviScrapEspanso(MidStr(tempStringa, 3, Length(tempStringa)-4),
            indent + 
            Length(stringhe.Strings[i]) - Length(TrimLeft(stringhe.Strings[i])));
        end
        else
        begin
          writeln(streamOutput, tempIndentazione, stringhe.Strings[i]);
        end;
      end;
  
      FreeAndNil(stringhe);
    end;
  end;
  
  procedure ProcessaFiles();
  var
    i:Integer;
  begin
    for i:=0 to store.MacroCount-1 do
    begin
      if store.GetRecord(i).macroType = FileMacro then
      begin
        Assign(streamOutput, Trim(store.GetRecord(i).macroName));
        Rewrite(streamOutput);
        ScriviScrapEspanso(store.GetRecord(i).macroName, 0);
        Close(streamOutput);
      end;
    end;
  end;
  
  procedure ControllaMacroNonUtilizzate;
  var
    tempMacro : TMacroRecord;
    i : integer;
  begin
    for i := 0 to store.MacroCount-1 do
    begin
      tempMacro := store.GetRecord( i );
      if (tempMacro.macroUsersCount = 0) and (tempMacro.macroType <> FileMacro) then
      begin
        writeln(StdErr, 'La macro ', tempMacro.macroName, ' non è mai stata utilizzata.');
      end;
    end;
  end;
  
  begin
    if ParamCount = 1 then
    begin
      store := TMacroStore.Create;
      stream := TSlitStream.CreateForFile(ParamStr(1));
      streamHtml := TSlitOutputLout.CreateForFileAndStore(ParamStr(1), store);
      driverMagazzinoMacro := TSlitStreamDriverMagazzino.CreateWithMacroStore( store );
      driverScriviDocumentazione := 
        TSlitStreamDriverGenerazioneDoc.CreateWithOutputStream( streamHtml );
      stream.Driver := driverMagazzinoMacro;
      stream.Process();
      stream.ResetStream();
      store.CalcolaRiferimenti();
      ControllaMacroNonUtilizzate();
      stream.Driver := driverScriviDocumentazione;
      stream.Process();
      stream.ResetStream();
      writeln(store.MacroCount, ' macro processate');
      ProcessaFiles();
        FreeAndNil(driverMagazzinoMacro);
        FreeAndNil(driverScriviDocumentazione);
        FreeAndNil(streamHtml);
        FreeAndNil(stream);
        FreeAndNil(store);
    end
    else
    begin
      writeln('Uso: ', ParamStr(0), ' <nomefile>');
    end;
  end.
