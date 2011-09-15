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
      {TODO gestire marcatori in base all'estensione}
      ScriviScrapEspanso(True, store.GetRecord(i).macroName, 0, '{', '}');
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
procedure ScriviScrapEspanso(isMain:Boolean; nome:String; indent:Integer; inizioCommento:String; fineCommento:String);
var
  rec:TMacroRecord;
  i:integer;
  tempStringa:String;
  tempIndentazione:String;
  linea:String;

begin
  tempIndentazione := '';
  for i := 1 to indent do
  begin
    tempIndentazione := tempIndentazione + ' ';
  end;

  if (not isMain) and GetGenerazioneMarcatoriAbilitata() then
  begin
    writeln (streamOutputSorgenti, tempIndentazione, inizioCommento, '[open] ', nome, fineCommento);
  end;

  rec := store.GetMacro(nome);
  if rec=Nil then
  begin
    writeln(streamOutputSorgenti, '<', nome, '>');
    LogErrorMessage('Attenzione: macro ' + nome + ' sconosciuta');
  end
  else
  begin
    for i := 0 to rec.MacroLinesCount-1 do
    begin
      linea := rec.MacroLine[i].Content;
      tempStringa := Trim(linea);

      if AnsiStartsStr('@<', tempStringa) and AnsiEndsStr('@>', tempStringa) then
      begin
        ScriviScrapEspanso(False, 
          MidStr(tempStringa, 3, Length(tempStringa)-4),
          indent + 
          Length(linea) - Length(TrimLeft(linea)),
          inizioCommento,
          fineCommento);
      end
      else
      begin
        writeln(streamOutputSorgenti, tempIndentazione, linea);
      end;
    end;
  end;

  if (not isMain) and GetGenerazioneMarcatoriAbilitata() then
  begin
    writeln (streamOutputSorgenti, tempIndentazione, inizioCommento, '[close] ', nome, fineCommento);
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
    drivermagazzino, driverdoc,
    slitstatus;

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
