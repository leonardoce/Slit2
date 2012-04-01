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
@Section @Title { La direttiva "@o" e sintassi degli scrap }
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

Slit pu{@Char ograve} essere configurato attraverso delle opzioni. Le opzioni
si comunicano utilizzando la direttiva @F @Verbatim { @x }. @PP
Questa direttiva @Char egrave seguita da una stringa che @Char egrave
il nome dell'opzione stessa. Ad esempio:

@IndentedDisplay @F @Verbatim { @x output_html }

Questa @Char egrave la lista delle opzioni supportate:

@BulletList
@ListItem { @F output_html : seleziona il formato html per l'output }
@ListItem { @F output_txt : seleziona il formato testo per l'output }
@ListItem { @F output_lout : seleziona il formato lout per l'output }
@ListItem { @F section_markers : abilita la produzione dei marcatori di
inizio e fine sezione }
@ListItem { @F no_section_markers : disabilita la produzione dei marcatori
di inizio e fine sezione }
@ListItem { @F line_markers : abilita la produzione dei marcatori di
riga nei file sorgenti generati }
@ListItem { @F no_line_markers : disabilita la produzione dei marcatori
di riga nei file sorgenti generati }
@ListItem { @F comment_markers : questa opzione serve per inserire
i delimitatori di commento per i linguaggi non ancora supportati da Slit
oppure per personalizzare quelli gi{@Char agrave} esistenti. }
@EndList

L'opzione @F comment_markers {@Char egrave} seguita da un parametro
che indica il linguaggio e i marcatori di inizio e di fine commento.
Questo parametro ha la forma @F @Verbatim {
<separatore><estensione><separatore><inizio><separatore><fine>
}, dove il separatore pu{@Char ograve} essere qualsiasi carattere @PP

Ad esempio la configurazione predefinita per il linguaggio Pascal
potrebbe essere espressa nella seguente forma:
@F @Verbatim @Begin
 @x comment_markers /.pas/{/}/
@End @Verbatim. @PP

@End @Section

@Section @Title { Documentazione }
@Begin @PP

Tutto quello che non @Char egrave una direttiva viene direttamente passato
nel file di documentazione generato senza alcuna trasformazione
intermedia. @PP

Slit deve essere utilizzato insieme a un formato di interscambio di testi.
In questo momento si pu{@Char ograve} utilizzare:

@BulletList
@ListItem { il formato HTML, }
@ListItem { il formato testo, }
@ListItem { il sistema di elaborazione testi Lout. }
@EndList

La documentazione deve essere inserita in modo coerente con il formato
scelto. @PP
Il formato Lout permette anche la generazione di una cross-reference
fra le macro inserite: ogni macro viene numerata e al termine della
macro viene inserita la lista di tutte le macro che la utilizzano. @PP

@End @Section

@EndSections

@End @Chapter


@Chapter
@Title { Il comando Slit }
@Begin @LP

I programmi vengono normalmente scritti in file di testo che possono avere qualunque esensione.
Per separare il codice dalla documentazione di utilizza il comando @F {slit}: @PP

@Display @F @Verbatim {
slit <nomefile>
}

Questo comando processa il file con il nome passato e ne interpreta le direttive. Dal file
passato vengono quindi generati: @PP

@BulletList
@ListItem { i file della documentazione; }
@ListItem { i vari file che sono il codice sorgente. }
@EndList

La procedura principale infatti recita:

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
    writeln('Uso: ', ParamStr(0), ' <nomefile>');
  end;
end.
@}

Il file di ingresso {@Char egrave} rappresentato da un oggetto della classe @F TSlitStream
mentre il file di uscita della documentazione {@Char egrave} rappresentato da una classe
della famiglia @F { TSlitOutputTxt }.

Le macro vengono memorizzate all'interno di un "magazzino" dal quale vengono
poi riprese per scrivere i files sorgenti.

@d slit preparazione dell'ambiente
@{
store := TMacroStore.Create;
stream := TSlitStream.CreateForFile(ParamStr(1));
driverMagazzinoMacro := TSlitStreamDriverMagazzino.CreateWithMacroStore( store );
@}

La documentazione viene elaborata attraverso un metodo dello stream in ingresso:

@d slit riempimento del magazzino delle macro
@{
stream.Driver := driverMagazzinoMacro;
stream.Process();
stream.ResetStream();
@}

Dopo aver popolato il magazino delle macro vengono calcolati i riferimenti:

@d slit calcola riferimenti
@{
store.CalcolaRiferimenti();
@}

Una volta calcolati i riferimenti {@Char egrave} possibile controllare la presenza di macro
mai utilizzate:

@d slit controlla macro non utilizzate
@{
ControllaMacroNonUtilizzate();
@}

Il controllo viene fatto scorrendo tutto il magazzino della macro.

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
      LogErrorMessage('La macro ' + tempMacro.macroName + ' non e'' mai stata utilizzata.');
    end;
  end;
end;
@}

Viene adesso avviata la generazione della documentazione:

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

Viene poi avviata l'elaborazione dei file sorgenti:

@d slit generazione del codice sorgente
@{
ProcessaFiles();
@}

Poi vengono deallocati gli oggetti creati:

@d slit pulizia
@{
FreeAndNil(driverMagazzinoMacro);
FreeAndNil(driverScriviDocumentazione);
FreeAndNil(streamOutputDocumentazione);
FreeAndNil(stream);
FreeAndNil(store);
@}

@End @Chapter
