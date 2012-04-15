# -*- mode:lout -*-
@Chapter
@Title { Creazione dei files sorgenti }
@Begin @PP

This unit will create source code files using the content already
added to the macro store. @PP

The procedure @F {ProcessaFiles} read all the macro store and find the
macro that define output files:

@d procedure ProcessaFiles
@{
procedure ProcessaFiles();
var
  i:Integer;
  Marcatore_Inizio:String;
  Marcatore_Fine:String;
begin
  for i:=0 to store.MacroCount-1 do
  begin
    if store.GetRecord(i).macroType = FileMacro then
    begin
      Assign(streamOutputSorgenti, Trim(store.GetRecord(i).macroName));
      Rewrite(streamOutputSorgenti);

      PrendiMarcatori (store.GetRecord(i).macroName, 
        Marcatore_Inizio, Marcatore_Fine);
      ScriviScrapEspanso(True, store.GetRecord(i).macroName, 0, 
        Marcatore_Inizio, Marcatore_Fine);
      Close(streamOutputSorgenti);
    end;
  end;
end;
@}

Every file is generated using the macro name. This procedure will
divide the scrap in lines and will process directives including other
macros. @PP

If it finds a macro reference it will recursively call the same
procedure processing the indentation level which, initially, is
zero. @PP

@d procedure ScriviScrapEspanso
@{
procedure ScriviScrapEspanso(isMain:Boolean; nome:String; 
  indent:Integer; inizioCommento:String; fineCommento:String);
var
  rec:TMacroRecord;
  i, j:integer;
  tempStringa:String;
  tempIndentazione:String;
  linea:String;
  indicazioneRiga:String;

begin
  tempIndentazione := '';
  for i := 1 to indent do
  begin
    tempIndentazione := tempIndentazione + ' ';
  end;

  if (not isMain) and GetGenerazioneMarcatoriAbilitata() then
  begin
    writeln (streamOutputSorgenti, tempIndentazione, inizioCommento, 
      '[open] ', nome, fineCommento);
  end;

  @<ScriviScrapEspanso, scrittura righe del file sorgente@>

  if (not isMain) and GetGenerazioneMarcatoriAbilitata() then
  begin
    writeln (streamOutputSorgenti, tempIndentazione, inizioCommento, 
      '[close] ', nome, fineCommento);
  end;
end;
@}

The line inside a file are written from the macro store:

@d ScriviScrapEspanso, scrittura righe del file sorgente
@{
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

    @<ScriviScrapEspanso, calcolo indicazione del numero di riga@>

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
      writeln(streamOutputSorgenti, tempIndentazione, linea, indicazioneRiga);
    end;
  end;
end;
@}

The row number is calculated, if the parameters are saying so:

@d ScriviScrapEspanso, calcolo indicazione del numero di riga
@{
if GetGenerazioneNumeriRigaAbilitata() then
begin
  indicazioneRiga := '';
 
  for j := 1 to GetColonnaNumeriRiga()-(length(linea)+length(tempIndentazione)) do
  begin
    indicazioneRiga := indicazioneRiga + ' ';
  end;

  indicazioneRiga := indicazioneRiga + 
    inizioCommento+
    rec.MacroLine[i].FileName +
    ':' +
    IntToStr(rec.MacroLine[i].LineNumber) +
    fineCommento ;
end
else
begin
  indicazioneRiga := '';
end;
@}

We are at the point to enounce the main file:

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

Goodbye and happy literate programming!

@End @Chapter
