% :folding=explicit:mode=slitpascal:

=== Formato delle macro in memoria ===

Snit conserva le macro all'interno della memoria. 

@d TMacroRecord
@{
EMacroType = ( FileMacro, ScrapMacro );

TMacroRecord = record
  macroName:String;
  macroProgr:Integer;
  macroContent:String;
  macroType:EMacroType;
  macroUsers: array of Integer;
  macroUsersCount: Integer;
end;
@}

Per ogni macro viene memorizzato:

+ il nome;
+ il numero progressivo;
+ il contenuto;
+ il tipo (permette di distinguere 
  le macro che generano dei file dalle macro pure)
+ le macro dove questa macro viene utilizzata (vettore ""macroUsers"")
+ il numero delle macro dove questa macro viene utilizzata (vettore 
  ""macroUsersCount"")

Le macro vengono conservate in un vettore dinamico la cui dimensione viene
fissata, inizialmente, a 50.

@d TMacroStore.Create
@{
constructor TMacroStore.Create;
begin
  count:=0;
  SetLength(store, 50);
end;
@}

Per memorizzare una macro viene controllato lo spazio disponibile nel vettore
(che e' memorizzato nella variabile ""count""). Se c'e' spazio a sufficienza
allora la macro viene memorizzata altrimenti prima di essere memorizzata
il vettore viene ampliato per far posto ad altre 50 macro.

Una macro non puo' essere ripetuta all'interno dello stesso file.
Per questo motivo, prima di memorizzare la macro, viene controllata l'esistenza
di una macro con lo stesso nome e, caso mai, viene segnalato un errore
all'utente.

@d TMacroStore.StoreMacro
@{
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
@}

Per localizzare una macro per nome e' necessario scorrere tutto il vettore
delle macro presenti:

@d TMacroStore.GetMacro
@{
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
@}

Vengono anche previste delle chiamate per ottenere il numero di
macro presenti nel magazzino:

@d TMacroStore.MacroCount
@{
function TMacroStore.MacroCount:Integer;
begin
  Result := count;
end;
@}

C'e' anche una chiamata per ottenere una macro dal numero progressivo.
(TODO: quando viene usata?):

@d TMacroStore.GetRecord
@{
function TMacroStore.GetRecord(i:integer):TMacroRecord;
begin
  Result := store[i];
end;
@}

=== Calcolo dei riferimenti ===

Il magazzino delle macro si occupa anche di calcolare i vari riferimenti
fra le macro.

Ad esempio, se la macro "uno" include la macro "due", all'interno del record
che corrisponde alla macro "due" viene inserito il progressivo della macro
"uno".

Il calcolo dei riferimenti viene effettuato dalla procedura
"CalcolaRiferimenti".

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

Inizialmente vengono cancellati tutti i riferimenti all'interno del
magazzino e fatto spazio per 10 riferimenti:

@d TMacroStore.CalcolaRiferimenti pulizia record
@{
for i:=0 to count-1 do
begin
  store[i].macroUsersCount:=0;
  SetLength(store[i].macroUsers, 10); 
end;
@}

Poi vengono lette le righe di ogni macro:

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

Se la riga corrisponde a un riferimento viene rintracciato il progressivo
della macro corrispondente e inserito fra i riferimenti:

@d TMacroStore.CalcolaRiferimenti processa riga
@{
stringaPulita := Trim( listaStringhe.Strings[j] );
if AnsiStartsStr('@<', stringaPulita) and
   AnsiEndsStr('@>', stringaPulita) then
begin
  stringaPulita := MidStr(stringaPulita, 3, Length(stringaPulita)-4);
  tempRecord := GetMacro( stringaPulita );
  if tempRecord.macroProgr<>0 then
  begin
    @<TMacroStore.CalcolaRiferimenti inserisci riferimento@> 
  end;
end;
@}

Per inserire il riferimento viene controllato se c'è posto per un nuovo
riferimento e se non c'è viene creato:

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

=== Definizione della unit macrostore ===

Riassumendo, la definizione del ""TMacroStore"" e' la seguente:

@d TMacroStore
@{
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
@}

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
  uses SysUtils, Classes, StrUtils;

  @<TMacroStore.Create@>
  @<TMacroStore.StoreMacro@>
  @<TMacroStore.GetMacro@>
  @<TMacroStore.MacroCount@>
  @<TMacroStore.GetRecord@>
  @<TMacroStore.CalcolaRiferimenti@>
end.
@}
