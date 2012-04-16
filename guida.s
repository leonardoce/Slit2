# -*- mode:lout -*-
@Chapter
@Title { User guide}
@Begin
@LP
Slit's files are composed of text's lines of documentation and instructions. Instructions are lines
that starts by a "@" character and are followed by a letter which determine the directive. @PP

There is a very important thing to remember: the directive is valid only if it is written at the first
character of the line. If you want you can make Slit ignore a directive inserting a space before
the @I at character. @PP

@BeginSections
@Section @Title { The definition directive: "@d" }
@Begin @PP

The directive "@d" is the primary instruction of Slit:
this instruction is used to memorize a text's macro into the sistem.
the macro can be used into others macro or only to write a sourced file. @PP
this is an example of a directive "@d":

@IndentedDisplay @F
@Verbatim @Begin
 @d function sum

function sum (a,b:Integer) return Integer
begin
  return a+b;
end;

@End @Verbatim

How we can see the directive "@d" is followed to an @I scrap. An @I scrap, in the Slit terminology, is
the content of the defined macro. @PP
A scrap starts with "@{" and is finished by "@}". The text between this two lines
is the content of the macro that will be defined. @PP
According to the type of processor selected for the output Slit enounce the name of the macro and its content into the
documentation. @PP
To each macro is associate a number that identify it and at the and of the macro are enounce
the number of macro that use the just define macro. @PP
A macro can be not used too, in this case Slit advise the user at the end of the process of production
of source files. @PP
@End @Section
@Section @Title { The directive "@o" and the syntax of scraps }
@Begin @PP
The directive "@o" is very similar, according to the syntactic point of view, to that "@d".
Infact is used to define a particular type of macro, which name is used to write a file. @PP
For example:
@IndentedDisplay @F
@Verbatim @Begin
 @o prova.c
@{
#include <stdio.h>

int main(int argc, char **argv)
{
    printf("Hello world!\n");
    return 0;
}
@}
@End @Verbatim
Into a scrap we can include a reference to an other:

@IndentedDisplay @F
@Verbatim @Begin
 @d saluta
@{
printf("Hello world!\n");
@}
@End @Verbatim

@IndentedDisplay @F
@Verbatim @Begin
 @o provadue.c
@{
#include <stdio.h>

int main(int argc, char **argv)
{
    @<saluta@>
    return 0;
}
@}
@End @Verbatim

Slit remembers the source code indentation level and uses that information
to write the target source code.

@End @Section

@Section @Title { "@i" directive }
@Begin @PP

The "@i" directive can be used to include a slit file in another slit file. @PP

Often, the software written using Slit, are composed by more Slit files. The
"@i" directive make Slit read another source file and return to the caller 
file when the included file is readden. @PP

For example:

@IndentedDisplay @F @Verbatim { @i provatre_funzioni.s }
@IndentedDisplay @F @Verbatim { @i provatre_dichiarazioni.s }

@IndentedDisplay @F
@Verbatim @Begin
 @o provatre.c
@{
@include <stdio.h>

@<dichiarazioni@>
@<funzioni@>

int main(int argc, char **argv)
{
    funzionePrincipale();
    return 0;
}
@}
@End @Verbatim

@End @Section

@Section @Title { "@+" directive }
@Begin @PP

The @F "@+" directive append a scrap to the end of an already existent one.
This behaviour is useful when you need to reduce the used macro names.
@PP

For example:

@IndentedDisplay @F
@Verbatim @Begin
 @o provatre.c
@{
@include <stdio.h>

@<dichiarazioni@>

int main(int argc, char **argv)
{
    funzionePrincipale();
    return 0;
}
@}
@End @Verbatim

@IndentedDisplay @F
@Verbatim @Begin
 @d dichiarazioni
@{
void funzioneUno();
@}
@End @Verbatim

@IndentedDisplay @F
@Verbatim @Begin
 @+ dichiarazioni
@{
void funzionePrincipale();
@}
@End @Verbatim

@End @Section

@Section @Title { Options ("@x" directive) }
@Begin @PP

Slit can be configured via options. Options are configurated 
using the @F @Verbatim { @x } directive. @PP

This directive must be followed by a string whi is the name of the
option being configured. For example:

@IndentedDisplay @F @Verbatim { @x output_html }

This is the list of the supported options:

@BulletList
@ListItem { @F output_html : select HTML as output format }
@ListItem { @F output_txt : select TXT as output format }
@ListItem { @F output_lout : select LOUT as output format }
@ListItem { @F section_markers : enable start or end section markers in 
source code }
@ListItem { @F no_section_markers : disable start or end section markers
in source code }
@ListItem { @F line_markers : enable line markers in source code }
@ListItem { @F no_line_markers : disable line markers in source code }
@ListItem { @F comment_markers : this options customize the comment markers
for programming languages not already supported by Slit or can be used
to personalize existing languages support. }
@EndList

The option @F comment_markers is followed by a parameter saying the
programming language and the comment start and end marker. This
parameter has the syntax @F @Verbatim {
<separator><extension><separator><comment-start><separator><comment-end>
}, where the separator can be any character. @PP

For example, the predefined configuration for @I Pascal can be written
like this:

@F @Verbatim @Begin
@x comment_markers /.pas/{/}/
@End @Verbatim. @PP

@End @Section

@Section @Title { "@e" directive }
@Begin @PP

The @F "@e" directive insert in the documentation an already
defined macro. This directive is useful to repeat macros more time in
the documentation, without rewriting the code, and to enable reverse
literate programming, reading macros directly from the source
code. @PP

It can be used in this way: @PP

@F @Verbatim @Begin
 @d my macro
@{
this is a little macro
@}

 @e my_macro

and I will insert it another time:

 @e my_macro
@End @Verbatim. @PP

@End @Section

@Section @Title { "@r" directive }
@Begin @PP

The @F "@r" directive read macros directly from source
files. This enable reverse literate programming: you can write your
source code adding special markers that Slit will parse to directly
create macros. @PP

The macros created by the @F "@r" can be used in the
documentation with the @F "@e" directive.

@End @Section

@Section @Title { Documentation }
@Begin @PP

All that is not a directory goes in the generated documentation
without any transformation. @PP

Slit must be used with a documentation syntax. For now the following
languages can be used: @PP

@BulletList
@ListItem { HTML language, }
@ListItem { plain old text, }
@ListItem { the Lout typesetting syntax. }
@EndList

The documentation can must be written using the choosen format. @PP

The Lout format will also generate a cross-reference between the
scraps: every scrap is numbered and, at the end, the list
of all the using scraps is generated automatically. @PP

@End @Section

@EndSections

@End @Chapter


@Chapter
@Title { The Slit command }
@Begin @LP

Slit programs can be written in text files which can have every
extension. To separate the code from the documentation you can use the
@F {slit} command: @PP

@Display @F @Verbatim {
slit <nomefile>
}

This command will process the file passed and will process all the
directives. The @F slit command generates:

@BulletList
@ListItem { the documentation files; }
@ListItem { the source code. }
@EndList

The main procedure is like this:

@d slit procedura principale
@{
begin
  if ParamCount = 1 then
  begin
    @<slit preparazione dell'ambiente@>
    @<slit riempimento del magazzino delle macro@>
    @<slit calcola riferimenti@>
    @<slit controlla macro non utilizzate@>
    @<slit generazione della documentazione@>
    @<slit generazione del codice sorgente@>
    @<slit pulizia@>
  end
  else
  begin
    writeln('Use: ', ParamStr(0), ' <nomefile>');
  end;
end.
@}

The input file is represented by an object of the class @F TSlitStream
ant the documentation file is represented by an object of the class 
@F { TSlitOutputTxt }.

Macros are memorized in a @I store and are retrieved when the source
code is generated.

@d slit preparazione dell'ambiente
@{
store := TMacroStore.Create;
stream := TSlitStream.CreateForFile(ParamStr(1));
driverMagazzinoMacro := TSlitStreamDriverMagazzino.CreateWithMacroStore( store );
@}

Documentation is read by a method in the input stream:

@d slit riempimento del magazzino delle macro
@{
stream.Driver := driverMagazzinoMacro;
stream.Process();
stream.ResetStream();
@}

When the macro store is populated whe cross-reference is calculated:

@d slit calcola riferimenti
@{
store.CalcolaRiferimenti();
@}

When the cross-reference is calculated is possible to check
unreferenced macros:

@d slit controlla macro non utilizzate
@{
ControllaMacroNonUtilizzate();
@}

This check is done reading all the macro store:

@d slit ControllaMacroNonUtilizzate
@{
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
      LogErrorMessage('The macro ' + tempMacro.macroName + ' is never used.');
    end;
  end;
end;
@}

Now the documentation can be generated:

@d slit generazione della documentazione
@{
streamOutputDocumentazione := CreaStreamOutputDaOpzioni(ParamStr(1), store);
driverScriviDocumentazione :=
  TSlitStreamDriverGenerazioneDoc.CreateWithOutputStream( streamOutputDocumentazione );

stream.Driver := driverScriviDocumentazione;
stream.Process();
stream.ResetStream();
writeln(store.MacroCount, ' macro processate');
@}

Then the source files are generated:

@d slit generazione del codice sorgente
@{
ProcessaFiles();
@}

And now a bit of cleaning:

@d slit pulizia
@{
FreeAndNil(driverMagazzinoMacro);
FreeAndNil(driverScriviDocumentazione);
FreeAndNil(streamOutputDocumentazione);
FreeAndNil(stream);
FreeAndNil(store);
@}

@End @Chapter
