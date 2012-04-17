# -*- mode:lout -*-

@Chapter
@Title { The macro store }
@Begin @PP

Macro are memorized in the macro store.

@d TMacroRecord
@{
EMacroType = ( FileMacro, ScrapMacro, ExternalMacro );

RScrapLine = record
  Content:String;
  FileName:String;
  LineNumber:Integer;
end;

TMacroRecord = class
private
  FMacroName:String;
  FMacroContent:array of RScrapLine;
  FMacroLinesCount:Integer;
  FMacroProgr:Integer;
  FMacroType:EMacroType;
  FEnounced:Boolean;

  procedure AddLine (Content:String; FileCorrente:String; LineaCorrente:Integer);
  function ReadMacroContent:String;
  function GetMacroLine(Idx:Integer):RScrapLine;
public
  macroUsers: array of Integer;
  macroUsersCount: Integer;

  constructor CreateWithData(Name:String; Progressivo:Integer; 
    Tipo:EMacroType);

  property MacroName:String read FMacroName;
  property MacroContent:String read ReadMacroContent;
  property MacroProgr:Integer read FMacroProgr;
  property MacroType:EMacroType read FMacroType;
  procedure AddContent (Content:String; FileCorrente:String; LineaCorrente:Integer);
  property MacroLinesCount:integer read FMacroLinesCount;
  property MacroLine[idx:Integer]:RScrapLine read GetMacroLine;
  property IsEnounced:Boolean read FEnounced write FEnounced;
end;
@}

For every macro the following attribute are memorized:

@BulletList
@ListItem { the name; }
@ListItem { a progressive number; }
@ListItem { the content; }
@ListItem { the type (this permits to remember if it's a file
generating macro or not); }
@ListItem { the macros where this macro is used (@F "macroUsers" vector
and @F "macroUsersCount") }
@EndList

Every macro is populated with scraps and every scrap is created by a
set of lines. Slit must memorize, for every row in a scrap, the name
of the input file and the number of the row. In this way we can build,
afterwords, a map between the row in the documentation file and the
row in the source code. @PP

This map is really useful for developer to demangle the error messages
given by compilers. @PP

Macro are always created with a name:

@d TMacroRecord.CreateWithData
@{
constructor TMacroRecord.CreateWithData(Name:String; 
  Progressivo:Integer; Tipo:EMacroType);
begin
  FMacroName := Name;
  FMacroProgr := Progressivo;
  FMacroType := Tipo;
  FMacroLinesCount := 0;
  SetLength(FMacroContent, 50);
end;
@}

You can add rows to a macro, and this is managed by hold the current
filename and the row number. The row number passed to this procedure
is that of the first row of the content passed.

@d TMacroRecord.AddContent
@{
procedure TMacroRecord.AddContent (Content:String; FileCorrente:String; 
  LineaCorrente:Integer);
var
  divisioneRighe:TStringList;
  i:Integer;
begin
  divisioneRighe := TStringList.Create;
  divisioneRighe.Text := Content;
  
  for i:=0 to divisioneRighe.Count-1 do
  begin
    AddLine (divisioneRighe.Strings[i], FileCorrente, LineaCorrente+i);
  end;

  FreeAndNil (divisioneRighe);
end;
@}

The following function instead add the content line by line:

@d TMacroRecord.AddLine
@{
procedure TMacroRecord.AddLine (Content:String; FileCorrente:String; 
  LineaCorrente:Integer);
begin
  if Length(FMacroContent)>=FMacroLinesCount then
  begin
    SetLength(FMacroContent, Length(FMacroContent)+50);
  end;

  FMacroContent[FMacroLinesCount].Content := Content;
  FMacroContent[FMacroLinesCount].FileName := FileCorrente;
  FMacroContent[FMacroLinesCount].LineNumber := LineaCorrente;
  FMacroLinesCount := FMacroLinesCount+1;
end;
@}

This function gives the content of the macro:

@d TMacroRecord.ReadMacroContent
@{
function TMacroRecord.ReadMacroContent:String;
var
  i:Integer;

begin
  Result:='';
  for i:=0 to FMacroLinesCount-1 do
  begin
    Result:=Result+FMacroContent[i].Content+LineEnding;
  end;
end;
@}

@d TMacroRecord.GetMacroLine
@{
function TMacroRecord.GetMacroLine(idx:Integer):RScrapLine;
begin
  Result := FMacroContent[idx];
end;
@}

Macros are memorized in a dynamic vector whose size is initially 50.

@d TMacroStore.Create
@{
constructor TMacroStore.Create;
var
  i:Integer;
begin
  count:=0;
  SetLength(store, 50);

  for i:=0 to Length(Store)-1 do
  begin
    Store[i] := Nil;
  end;
end;
@}

Per memorizzare una macro viene controllato lo spazio disponibile nel vettore
(che {@Char egrave} memorizzato nella variabile "count"). Se c'{@Char egrave} spazio a sufficienza
allora la macro viene memorizzata altrimenti prima di essere memorizzata
il vettore viene ampliato per far posto ad altre 50 macro. @PP

Una macro non pu{@Char ograve} essere ripetuta all'interno dello stesso file.
Per questo motivo, prima di memorizzare la macro, viene controllata l'esistenza
di una macro con lo stesso nome e, caso mai, viene segnalato un errore
all'utente. 

@d TMacroStore.StoreMacro
@{
procedure TMacroStore.StoreMacro(macroName:String; macroContent:String; 
  macroType:EMacroType; FileName:String; CurrentLine:Integer);
var
  i:Integer;
begin
  if GetMacro(macroName) <> Nil then
  begin
    writeln('Attenzione: macro ', macroName, ' duplicata.');
  end
  else
  begin
    if count>=Length(store) then
    begin
      SetLength(store, Length(Store)+50);
      for i:=count to Length(Store)-1 do
      begin
        Store[i] := Nil;
      end;
    end;
    store[count] := TMacroRecord.CreateWithData (macroName, count+1, macroType);
    store[count].AddContent (macroContent, FileName, CurrentLine);
    count := count + 1;
  end;
end;
@}

To find a macro by name every element in the vector is read:

@d TMacroStore.GetMacro
@{
function TMacroStore.GetMacro(macroName:String):TMacroRecord;
var
  i:integer;
begin
  Result := Nil;
  for i:=0 to length(Store)-1 do
  begin
    if (store[i]<>Nil) and (store[i].MacroName=macroName) then
    begin
      Result:=store[i];
      exit;
    end;
  end;
end;
@}

This procedures gives the count of the macro in the store:

@d TMacroStore.MacroCount
@{
function TMacroStore.MacroCount:Integer;
begin
  Result := count;
end;
@}

This procedure instead find a macro given it's progressive number:

@d TMacroStore.GetRecord
@{
function TMacroStore.GetRecord(i:integer):TMacroRecord;
begin
  Result := store[i];
end;
@}

@BeginSections

@Section
@Title { Cross-reference }
@Begin @PP 

The store computes also the cross-reference database.

For example if the macro @I one includes the macro @I two, in the
record of the macro @I two is inserted the ID of the macro @I one.

The cross-reference database if computed by the procedure 
@I {CalcolaRiferimenti}.

@d TMacroStore.CalcolaRiferimenti
@{
procedure TMacroStore.CalcolaRiferimenti;
var
  i, j, k: Integer;
  listaStringhe: TStringList;
  stringaPulita: String;
  tempRecord: TMacroRecord;
begin
  listaStringhe := TStringList.Create;
  
  @<TMacroStore.CalcolaRiferimenti pulizia record@>
  @<TMacroStore.CalcolaRiferimenti calcolo@>
  
  FreeAndNil( listaStringhe );
end;
@}

The cross-reference database is initially cleared:

@d TMacroStore.CalcolaRiferimenti pulizia record
@{
for i:=0 to count-1 do
begin
  store[i].macroUsersCount:=0;
  SetLength(store[i].macroUsers, 10); 
end;
@}

Now all the rows are read:

@d TMacroStore.CalcolaRiferimenti calcolo
@{
for i:=0 to count-1 do
begin
  listaStringhe.Text := store[i].macroContent;

  for j := 0 to listaStringhe.Count-1 do
  begin
    @<TMacroStore.CalcolaRiferimenti processa riga@>
  end;
end;
@}

If the row is a reference the ID of the corresponding macro is
retrieved and inserted in the reference database.

@d TMacroStore.CalcolaRiferimenti processa riga
@{
stringaPulita := Trim( listaStringhe.Strings[j] );
if AnsiStartsStr('@<', stringaPulita) and
   AnsiEndsStr('@>', stringaPulita) then
begin
  stringaPulita := MidStr(stringaPulita, 3, Length(stringaPulita)-4);
  tempRecord := GetMacro( stringaPulita );
  if (tempRecord<>Nil) and (tempRecord.macroProgr<>0) then
  begin
    @<TMacroStore.CalcolaRiferimenti inserisci riferimento@> 
  end;
end;
@}

To insert a now reference the dynamic vector must be checked and
resized if necessary:

@d TMacroStore.CalcolaRiferimenti inserisci riferimento
@{
k := tempRecord.macroProgr-1;

if store[k].macroUsersCount = Length(store[k].macroUsers) then
begin
  SetLength(store[k].macroUsers, Length(store[k].macroUsers)+10);
end;
store[k].macroUsers[store[k].macroUsersCount] := i + 1;
store[k].macroUsersCount := store[k].macroUsersCount + 1;
@}

@End @Section

@Section
@Title { Definition of the macrostore unit }
@Begin @PP

To sum up, the definition of the @F TMacroStore class is the following:

@d TMacroStore
@{
TMacroStore = class
private
  count:integer;
  store:array of TMacroRecord;
public
  constructor Create;
  function MacroCount:Integer;
  procedure StoreMacro(macroName:String; macroContent:String; 
    macroType:EMacroType; FileName:String; CurrentLine:Integer);
  function GetMacro(macroName:String):TMacroRecord;
  function GetRecord(i:integer):TMacroRecord;
  procedure CalcolaRiferimenti;
end;
@}

And this is the definition of the @F macrostore unit:

@o macrostore.pas
@{
{$MODE OBJFPC}
{$H+}
unit macrostore;

interface

type
  @<TMacroRecord@>
  @<TMacroStore@>

implementation
  uses SysUtils, Classes, StrUtils, slitstatus, slitsource;

  @<TMacroStore.Create@>
  @<TMacroStore.StoreMacro@>
  @<TMacroStore.GetMacro@>
  @<TMacroStore.MacroCount@>
  @<TMacroStore.GetRecord@>
  @<TMacroStore.CalcolaRiferimenti@>

  @<TMacroRecord.ReadMacroContent@>
  @<TMacroRecord.CreateWithData@>
  @<TMacroRecord.AddContent@>
  @<TMacroRecord.AddLine@>
  @<TMacroRecord.GetMacroLine@>
end.
@}

@End @Section

@EndSections

@End @Chapter
