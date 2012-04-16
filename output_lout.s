# -*- mode: lout -*-
@Section
@Title { Lout output }
@Begin @PP

Lout is a typesetting system as Tex but has a more simple sintax and
is easy to program. @PP

Lout is also really lightweight.
Slit can create Lout documentation. @PP

Lout files haven't a predefined extension: slit assumes that this extension is @F "".lout"".

@d TSlitOutputLout.CreateForFileAndStore
@{
constructor TSlitOutputLout.CreateForFileAndStore(fileName:String; store:TMacroStore);
begin
  Assign(handle, ExtractFileName(fileName)+'.lout');
  Rewrite(handle);
  FStore := store;
end;
@}

Stream get closed when the backend is destroyed:

@d TSlitOutputLout.Destroy
@{
destructor TSlitOutputLout.Destroy;
begin
  Close(handle);
end;
@}

The documentation lines are added exactly as they are so you can add
your Lout directives inside Slit files. @PP

When slit writed the Lout file it write, as a comment, a reference to
the source file so you can easily fix the errors given by Lout
compiler. @PP

To write comment lines Slit would identify if we are inside a @B
verbatim block or not. To exactly identify this condition we should
laxically analyse the output file. This approach is too heavyweight
for a simple software like Slit so I have choosen to identify @B
verbatim blocks by adding this comments only if the line starts with a
section starting directive like @F "@Section". The same check is done
when you are starting a chapter or the introduction. @PP

@d TSlitOutputLout.PutLine
@{
procedure TSlitOutputLout.PutLine(str:String);
var
  tempStr : String;

begin
  tempStr := Trim(str);
  if (AnsiStartsStr('@Section', tempStr)) or
     (AnsiStartsStr('@Chapter', tempStr)) or
     (AnsiStartsStr('@Introduction', tempStr)) then
  begin
    writeln (handle, '# ', GetCurrentParsingFile(), ':',
      GetCurrentParsingLine() );
  end;

  writeln(handle, str);
end;
@}

The scrapt must be formatted in a particular way to make Lout handle them correctly. @PP

This is a simple example of a code scrap formatted by Slit: @PP

@F @Verbatim {
@LeftDisplay lines @Break {
@Sym angleleft @I @Verbatim @Begin definizione di ciao 
  @End @Verbatim 
  @Sym angleright @Sym equivalence
    @Verbatim @Begin while(true) { @End @Verbatim 
      @Verbatim @Begin putstrln("ciao!"); @End @Verbatim
    @Verbatim @Begin } @End @Verbatim
}
}

@PP

The macro name is written in italic inside angular brackets. @PP

The header get written like this:

@d TSlitOutputLout scrivi testata
@{
{ Index support }
if (tipo<>FileScrap) and AnsiContainsStr(currentMacro.macroName, '.') then
begin
  write(handle, 'idx.tag.', cleanText(currentMacro.macroName), ' @SubIndex {');
  write(handle, MidStr(nome, NPos('.',nome,1), Length(nome)), ' } ');
end
else
begin
  if tipo=FileScrap then
  begin
    write(handle, 'file.idx.tag.', cleanText(currentMacro.macroName), ' @Index {');
    write(handle, 'FILE ');
  end
  else
  begin
    write(handle, 'idx.tag.', cleanText(currentMacro.macroName), ' @Index {');
  end;
  write(handle, nome, ' } ');
end;

write(handle, '@Sym angleleft { BoldSlope } @Font @','Verbatim @Begin ');
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

@d TSlitOutputLout scrivi tag
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

@d TSlitOutputLout scrivi codice
@{
stringhe := TStringList.Create;
stringhe.Text := contenuto;

for i := 0 to stringhe.Count-1 do
begin
  @<TSlitOutputLout.ScriviScrap processa linea@>
end;

FreeAndNil(stringhe);
@}

If a line in a scraps represent a reference to a macro the link is written in bold face. @PP

@d TSlitOutputLout.ScriviScrap processa linea
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

@d TSlitOutputLout scrivi riferimenti
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

@d TSlitOutputLout.ScriviScrap
@{
procedure TSlitOutputLout.ScriviScrap(tipo:EScrapType; nome, contenuto:String);
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
    @<TSlitOutputLout scrivi tag@>
    
    writeln(handle, '@LeftDisplay lines @Break {');  
    @<TSlitOutputLout scrivi testata@>
    @<TSlitOutputLout scrivi codice@>
    @<TSlitOutputLout scrivi riferimenti@>
    writeln(handle, '}');
  end;
end;
@}

This is the Lout frontend:

@d TSlitOutputLout
@{
TSlitOutputLout = class(TSlitOutput)
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

@o slitlout.pas
@{
{$MODE OBJFPC}
{$H+}
unit slitlout;

interface
  uses slitoutput, macrostore, htmlutils;

type
  @<TSlitOutputLout@>

implementation
  uses sysutils, strutils, classes, slitstatus;

  @<TSlitOutputLout.CreateForFileAndStore@>
  @<TSlitOutputLout.Destroy@>
  @<TSlitOutputLout.PutLine@>
  @<TSlitOutputLout.ScriviScrap@>

end.
@}
@End @Section
