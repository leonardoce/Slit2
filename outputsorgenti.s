# :folding=explicit:mode=slitpascal:
@Chapter
@Title { Creazione dei files sorgenti }
@Begin @PP

La lettura dei files non fa altro che creare i files sorgenti a partire dalle
macro memorizzate nel magazzino. @PP

La procedura @F {ProcessaFiles}, infatti, legge tutto il magazzino delle
macro alla ricerca delle macro che devono generare dei files:

@d procedure ProcessaFiles
@{
procedure ProcessaFiles();
var
  i:Integer;
begin
  for i:=0 to store.MacroCount-1 do
  begin
    if store.GetRecord(i).macroType = FileMacro then
    begin
      Assign(streamOutputSorgenti, Trim(store.GetRecord(i).macroName));
      Rewrite(streamOutputSorgenti);
      ScriviScrapEspanso(store.GetRecord(i).macroName, 0);
      Close(streamOutputSorgenti);
    end;
  end;
end;
@}

Ogni file viene generato con lo scrap che corrisponde al suo nome. La
procedura che scrive lo scrap divide in righe il codice dello scrap
alla ricerca del riferimento ad una macro. @PP

Sei il riferimento è quello di una macro si richiama ricorsivamente
per generare il file, altrimenti stampa la riga al livello di indentazione
desiderato, che inizialmente è 0. 

@d procedure ScriviScrapEspanso
@{
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
    writeln(streamOutputSorgenti, '<', nome, '>');
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
        writeln(streamOutputSorgenti, tempIndentazione, stringhe.Strings[i]);
      end;
    end;

    FreeAndNil(stringhe);
  end;
end;
@}

Riassumendo questo e' il programma principale:

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
  streamOutputDocumentazione: TSlitOutput;
  streamOutputSorgenti:Text;
  driverMagazzinoMacro: TSlitStreamDriver;
  driverScriviDocumentazione: TSlitStreamDriver;

  @<procedure ScriviScrapEspanso@>
  
  @<procedure ProcessaFiles@>  
  
  @<slit ControllaMacroNonUtilizzate@>
  
  @<slit procedura principale@>
@}
@End @Chapter
