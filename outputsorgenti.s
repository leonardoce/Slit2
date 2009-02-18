% :folding=explicit:mode=slitpascal:

La lettura dei files non fa altro che creare

@o slit.pas
@{
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

  procedure ScriviScrapEspanso(nome:String);
  var
    stringhe:TStringList;
    rec:TMacroRecord;
    i:integer;
    tempStringa:String;
  begin
    rec := store.GetMacro(nome);
    if rec.macroName = '' then
    begin
      writeln(streamOutput, '<', nome, '>');
      writeln('Attenzione: macro ', nome, ' sconosciuta');
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
          ScriviScrapEspanso(MidStr(tempStringa, 3, Length(tempStringa)-4));
        end
        else
        begin
          writeln(streamOutput, stringhe.Strings[i]);
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
        writeln('Scrivo ', store.GetRecord(i).macroName);

        Assign(streamOutput, Trim(store.GetRecord(i).macroName));
        Rewrite(streamOutput);
        ScriviScrapEspanso(store.GetRecord(i).macroName);
        Close(streamOutput);
      end;
    end;
  end;

  @<slit procedura principale@>
@}

