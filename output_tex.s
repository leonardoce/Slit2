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

@d TSlitOutputTeX.header
@{
writeln(handle, '\vbox {');
writeln(handle, '\vskip 20 pt');
writeln(handle, '\parindent = 0 pt');
writeln(handle, '\parskip = 0 pt');
writeln(handle, '$ \ll $ { \textbf ', 
	EscapeSpecialCharacters(currentMacro.macroName), ' } { \itshape ', 
  currentMacro.macroProgr ,' } $ \gg \equiv $');
writeln(handle, '\begin{quote}');
writeln(handle, '\begin{verbatim}');
@}

For every macro name a tag is generated for cross-reference sake. @PP

@d TSlitOutputTeX.tag
@{
writeln(handle, '\label{tag:', currentMacro.macroProgr, '} ');
@}

This section will write source code. LaTex won't insert tab characters
in verbatim section. So tabs must be expanded to spaces. Here we suppose
that tab characters are represented by 4 spaces.

@d TSlitOutputTeX.code
@{
stringhe := TStringList.Create;
stringhe.Text := contenuto;

for i := 0 to stringhe.Count-1 do
begin
  writeln(handle, StringReplace(stringhe.Strings[i],#9,'    ',[rfReplaceAll]));
end;

FreeAndNil(stringhe);
@}

At the end of a scrap we write also write the cross-reference:

@d TSlitOutputTeX.refs
@{
writeln(handle, '\end{verbatim}');
if currentMacro.macroUsersCount <> 0 then
begin
  writeln(handle, 'Used by: ');
  for i:=0 to currentMacro.macroUsersCount-1 do
  begin
    writeln(handle, '\hyperref[tag:', currentMacro.macroUsers[i], '] {', currentMacro.macroUsers[i], '}');
  end;
end;
@}

When we write strings in the documentation we must escape TeX special 
characters with this function:

@d TSlitOutputTeX.EscapeSpecialCharacters
@{
function TSlitOutputTeX.EscapeSpecialCharacters(orig:String):String;
  @<TSlitOutputTeX.IsCharSpecial@>  

var    
  i:Integer;
  buffer:String;
  
begin
  buffer := '';
  
  for i:=1 to Length(orig) do
  begin
    if IsCharSpecial(orig[i]) then
    begin
      buffer := buffer + '\';
    end;
    buffer := buffer + orig[i];
  end;
  
  Result := buffer;
end;
@}

Special characters are identified here:

@d TSlitOutputTeX.IsCharSpecial
@{
function IsCharSpecial (v:Char):Boolean;
begin
  if v='#' then      Result:=True
  else if v='$' then Result:=True
  else if v='%' then Result:=True
  else if v='&' then Result:=True
  else if v='~' then Result:=True
  else if v='_' then Result:=True
  else if v='^' then Result:=True
  else if v='\' then Result:=True
  else if v='{' then Result:=True
  else if v='}' then Result:=True
  else               Result:=False;
end;
@}

The footer is plain simple:

@d TSlitOutputTeX.footer
@{
writeln(handle, '\end{quote}');
writeln(handle, '\vskip 20 pt');
writeln(handle, '}');
@}

In summary this is the code to write a scrap:

@d TSlitOutputTeX.ScriviScrap
@{
procedure TSlitOutputTeX.ScriviScrap(tipo:EScrapType; nome, contenuto:String);
var
  stringhe:TStringList;
  i:integer;
  currentMacro: TMacroRecord;  
begin
  currentMacro := FStore.GetMacro(nome);

  if currentMacro<>Nil then
  begin
    @<TSlitOutputTeX.tag@>
    
    @<TSlitOutputTeX.header@>
    @<TSlitOutputTeX.code@>
    @<TSlitOutputTeX.refs@>
    @<TSlitOutputTeX.footer@>
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
  function EscapeSpecialCharacters(orig:String):String;
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
  @<TSlitOutputTeX.EscapeSpecialCharacters@>

end.
@}
@End @Section
