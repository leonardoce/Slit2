# -*- mode: lout -*-
@Section
@Title { TeX output }
@Begin @PP

TeX is the mitical typesetting system created by Donald Knuth. @PP

Slit can also write TeX documentation. @PP

@d TSlitOutputTeX.CreateForFileAndStore
@{
constructor TSlitOutputTeX.CreateForFileAndStore(fileName:String; store:TMacroStore);
begin
  Assign(handle, ExtractFileName(fileName)+'.tex');
  Rewrite(handle);
  FStore := store;
end;
@}

Stream get closed when the backend is destroyed:

@d TSlitOutputTeX.Destroy
@{
destructor TSlitOutputTeX.Destroy;
begin
  Close(handle);
end;
@}

The documentation lines are added exactly as they are. @PP

As what happens with the Lout backend Slit will write, as a comment, a
reference to the source file so you can easily debug TeX error
messages. @PP 

To write comment lines Slit would identify if we are inside a @B
verbatim block or not. To exactly identify this condition we should
laxically analyse the output file. This approach is too heavyweight
for a simple software like Slit so I have choosen to identify @B
verbatim blocks by adding this comments only if the line starts with a
TeX comment, which is created by a percent sign. @PP

@d TSlitOutputTeX.PutLine
@{
procedure TSlitOutputTeX.PutLine(str:String);
var
  tempStr : String;

begin
  tempStr := Trim(str);
  if (AnsiStartsStr('% ', tempStr)) then
  begin
    writeln (handle, '% ', GetCurrentParsingFile(), ':',
      GetCurrentParsingLine() );
  end;

  writeln(handle, str);
end;
@}

The scrap must be formatted in a particular way to make Lout handle them correctly.
The macro name is written in italic inside angular brackets. @PP

The header is written like this:

@d TSlitOutputTeX scrivi testata
@{
write(handle, '\ll ');
if tipo=FileScrap then
begin
  write(handle, 'FILE ');
end;
write(handle, nome, ' @','End @','Verbatim ');

write(handle, '@I {', currentMacro.macroProgr, ' } ');

write(handle, ' @Sym angleright'); 
if tipo=AppendScrap then
begin
  write(handle, ' @Sym plus');
end;
writeln(handle, ' @Sym equivalence');

@}

For every macro name a tag is generated so Lout can generate links for
every macro definition. @PP

Every created tag has a name generated using an auto-incremented integer. @PP

@d TSlitOutputTeX scrivi tag
@{
writeln(handle, '@PageMark { ', currentMacro.macroProgr, ' } ');
@}

Il codice scritto dall'utente viene inserito fra blocchi
verbatim ovvero fra @Verbatim { "@Verbatim @Begin" } e @Verbatim { "@End @Verbatim" }. @PP 

Tutto il codice {@Char egrave} racchiuso fra @Verbatim { "@LeftDisplay lines @Break" }, che permette
di rendere l'indentazione significativa. @PP

Il codice viene prima diviso in linee e gli spazi che vengono prima del
primo scritto vengono isolati dal codice perch{@Char eacute} sono significativi per
la versione: @PP

@d TSlitOutputTeX scrivi codice
@{
stringhe := TStringList.Create;
stringhe.Text := contenuto;

for i := 0 to stringhe.Count-1 do
begin
  @<TSlitOutputTeX.ScriviScrap processa linea@>
end;

FreeAndNil(stringhe);
@}

If a line in a scraps represent a reference to a macro the link is written in bold face. @PP

@d TSlitOutputTeX.ScriviScrap processa linea
@{
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
  if macroTemp<>Nil then
  begin
    write(handle, '@I { ', macroTemp.macroProgr, ' } @CrossLink ');
    stringaPulita := '<' + nomeDefinizione + ' ' + IntToStr(macroTemp.macroProgr) + '>';
  end;
end;

write(handle, '@','Verbatim @','Begin ');
write(handle, stringaPulita);
writeln(handle, '@','End @','Verbatim'); 
@}

At the end of a scrap we write also the cross-reference:

@d TSlitOutputTeX scrivi riferimenti
@{
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
@}

In summary this is the code to write a scrap:

@d TSlitOutputTeX.ScriviScrap
@{
procedure TSlitOutputTeX.ScriviScrap(tipo:EScrapType; nome, contenuto:String);
var
  stringhe:TStringList;
  spazi:integer;
  stringaPulita, nomeDefinizione: String;
  i, j:integer;
  currentMacro, macroTemp: TMacroRecord;  
begin
  currentMacro := FStore.GetMacro(nome);

  if currentMacro<>Nil then
  begin
    @<TSlitOutputTeX scrivi tag@>
    
    writeln(handle, '@LeftDisplay lines @Break {');  
    @<TSlitOutputTeX scrivi testata@>
    @<TSlitOutputTeX scrivi codice@>
    @<TSlitOutputTeX scrivi riferimenti@>
    writeln(handle, '}');
  end;
end;
@}

This is the Lout frontend:

@d TSlitOutputTeX
@{
TSlitOutputTeX = class(TSlitOutput)
private
  handle:Text;
  FStore: TMacroStore;
public
  constructor CreateForFileAndStore(fileName:String; store:TMacroStore);
  destructor Destroy; override;
  procedure PutLine(str:String); override;
  procedure ScriviScrap(tipo:EScrapType; nome, contenuto:String); override;
end;
@}

This is the lout backend definition:

@o slittex.pas
@{
{$MODE OBJFPC}
{$H+}
unit slittex;

interface
  uses slitoutput, macrostore, htmlutils;

type
  @<TSlitOutputTeX@>

implementation
  uses sysutils, strutils, classes, slitstatus;

  @<TSlitOutputTeX.CreateForFileAndStore@>
  @<TSlitOutputTeX.Destroy@>
  @<TSlitOutputTeX.PutLine@>
  @<TSlitOutputTeX.ScriviScrap@>

end.
@}
@End @Section
