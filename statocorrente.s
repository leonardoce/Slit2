# -*- mode:lout -*-

@Chapter @Title { The translation status }
@Begin @LP

Slit has a unit to manage the translation process and the translation
status. @PP

The informations in this unit and the data structured are documented
in this chapter. @PP

@BeginSections
@Section @Title { Options management }
@Begin @PP

The Slit parameters system makes the translation process configurable
and adaptable to the user preferences. @PP

This parameters set, called @I options, is managed by a unit, who
provides high-level operation managing that data. @PP

@BeginSubSections
@SubSection @Title { The output format }
@Begin @PP

Slit must write, in the documentation, the content of the macros. So
he need to know the documentation format. @PP

This parameters rembember the format used and this information is used
to create the right documentation driver. @PP

@d slitstatus, gestore del processore di documentazione
@{
function GetNomeProcessoreInformazioni():String;
begin
  Result := NomeProcessoreInformazioni;
end;

procedure SetNomeProcessoreInformazioni(value:String);
begin
  if value='html' then
  begin
    NomeProcessoreInformazioni := 'html';
  end
  else if value='lout' then
  begin
    NomeProcessoreInformazioni := 'lout';
  end
  else if value='txt' then
  begin
    NomeProcessoreInformazioni := 'txt';
  end
  else if value='tex' then
  begin
    NomeProcessoreInformazioni := 'tex';
  end
  else  
  begin
    raise Exception.Create('Nome processore informazioni non conosciuto: ' +
      value);
  end;
end;
@}

Now that we know the documentation format we can write a function to
generated the right instance of the documentation driver. 

@d slitstatus, crea lo stream di output
@{
function CreaStreamOutputDaOpzioni(NomeFile:String; store:TMacroStore):TSlitOutput;
begin
  if NomeProcessoreInformazioni='lout' then
  begin
    Result := TSlitOutputLout.CreateForFileAndStore (NomeFile, store);
  end
  else if NomeProcessoreInformazioni='txt' then
  begin
    Result := TSlitOutputTxt.CreateForFile (NomeFile);
  end
  else if NomeProcessoreInformazioni='html' then
  begin
    Result := TSlitOutputHtml.CreateForFile (NomeFile);
  end
  else if NomeProcessoreInformazioni='tex' then
  begin
    Result := TSlitOutputTex.CreateForFileAndStore (NomeFile, store);
  end
  else
  begin
    Result := Nil;
  end;
end;
@}

@End @SubSection

@SubSection @Title { Comment start and end markers }
@Begin @PP

In the source code generated Slit can insert a reference to the
original documentation file. @PP

This information is enclosed in a comment block that must be valid for
the generated language. A software can be composed of source code
written in different languages so we must distuish between the source
code languages to choose the right comment marker. We do this using
the extension of the generated source code file. @PP

At every extension corresponds the comment start and end markers. Slit
has a database with predefines associations for the popular
programming languages. @PP

Slit insert comments only at the end of line so works also for
languages that doesn't admit comment between the source code lines. @PP

Slit is ready for the following programming languages:

@BulletList
@ListItem { Pascal (.pas, .pp), }
@ListItem { C, C++ (.c, .cpp, .h), }
@ListItem { D (.d), }
@ListItem { Java (.java), }
@ListItem { @Verbatim { C# (.cs), } }
@ListItem { Ada (.ada, .adb, .ads), }
@ListItem { Haskell (.hs), }
@ListItem { Python (.py), }
@ListItem { Lout (.lout), }
@ListItem { Ruby (.rb), }
@ListItem { Assembler (.s, .asm), }
@ListItem { Basic (.bas), }
@ListItem { NSIS (.NSI), }
@ListItem { Zinc (.zc). }
@EndList

These languages are configured directly in the source code of slit:

@d slitstatus, configurazione delle estensioni predefinite
@{
AggiungiLinguaggio( '.pas', '{', '}' );
AggiungiLinguaggio( '.pp', '{', '}' );
AggiungiLinguaggio( '.c', '/*', '*/' );
AggiungiLinguaggio( '.d', '/*', '*/' );
AggiungiLinguaggio( '.cpp', '/*', '*/' );
AggiungiLinguaggio( '.java', '/*', '*/' );
AggiungiLinguaggio( '.cs', '/*', '*/' );
AggiungiLinguaggio( '.ada', '--', '' );
AggiungiLinguaggio( '.adb', '--', '' );
AggiungiLinguaggio( '.ads', '--', '' );
AggiungiLinguaggio( '.hs', '--', '' );
AggiungiLinguaggio( '.py', '#', '' );
AggiungiLinguaggio( '.rb', '#', '' );
AggiungiLinguaggio( '.lout', '#', '' );
AggiungiLinguaggio( '.s', '; ', '' );
AggiungiLinguaggio( '.asm', '; ', '' );
AggiungiLinguaggio( '.bas', '''''', '' );
AggiungiLinguaggio( '.nsi', '; ', '' );
AggiungiLinguaggio( '.zc', '// ', '' );
@}

This informations are hold in records:

@d slitstatus, RInformazioniLinguaggi
@{
RInformazioniLinguaggi = record
  Estensione:String;
  Inizio:String;
  Fine:String;
end;
@}

The records are held in a table inside the Slit global configuration status:

@d slitstatus, AggiungiLinguaggio
@{
procedure AggiungiLinguaggio (Estensione, Inizio, Fine:String);
begin
  if Length(TabellaLinguaggi)>=TabellaLinguaggi_Count then
  begin
    SetLength (TabellaLinguaggi, TabellaLinguaggi_Count+50);
  end;

  TabellaLinguaggi[TabellaLinguaggi_Count].Estensione := Estensione;
  TabellaLinguaggi[TabellaLinguaggi_Count].Inizio := Inizio;
  TabellaLinguaggi[TabellaLinguaggi_Count].Fine := Fine;
  TabellaLinguaggi_Count := TabellaLinguaggi_Count + 1;
end;
@}

To use these informations slit use the following procedure:

@d slitstatus, PrendiMarcatori
@{
procedure PrendiMarcatori (NomeFile:String; var Inizio:String; var Fine:String);
var 
  i:Integer;
  fatto:Boolean;

begin
  fatto := false;

  for i:=TabellaLinguaggi_Count-1 downto 0 do
  begin
    if AnsiEndsText (TabellaLinguaggi[i].Estensione, NomeFile) then
    begin
      Inizio := TabellaLinguaggi[i].Inizio;
      Fine := TabellaLinguaggi[i].Fine;
      Fatto := True;
    end;
  end;

  if not fatto then
  begin
    Inizio := '';
    Fine := '';
  end;
end;
@}

Inside the table the instructions are searched in backward manner
because the configuration made by the user must take precedence over
the default configurations.

@End @SubSection

@SubSection @Title { Generation of the section start and end marker }
@Begin @PP

The generated source code can also contain the name of the Slit macro
that has been read. This parameter enable or disable this
functionality:@PP

@d slitstatus, Get/Set GenerazioneMarcatoriAbilitata
@{
function GetGenerazioneMarcatoriAbilitata:Boolean;
begin
  Result := GenerazioneMarcatoriAbilitata;
end;

procedure SetGenerazioneMarcatoriAbilitata(value:Boolean);
begin
  GenerazioneMarcatoriAbilitata := value;
end;
@}

@End @SubSection

@SubSection @Title { Generation of line markers }
@Begin @PP

Sometimes it's important to know, in the generated source code, the
name of the documentation file and the row number. This is useful to
decode the error messages given by the compiler. @PP

@d slitstatus, Get/Set GenerazioneNumeriRigaAbilitata
@{
function GetGenerazioneNumeriRigaAbilitata:Boolean;
begin
  Result := GenerazioneNumeriRigaAbilitata;
end;

procedure SetGenerazioneNumeriRigaAbilitata(value:Boolean);
begin
  GenerazioneNumeriRigaAbilitata := value;
end;
@}

The markers, for aestetics reasons, are put at a precise column or at
the end of the source code line. This column number is configurable:

@d slitstatus, Get/Set ColonnaNumeriRiga
@{
function GetColonnaNumeriRiga:Integer;
begin
  Result := ColonnaNumeriRiga;
end;

procedure SetColonnaNumeriRiga(value:Integer);
begin
  ColonnaNumeriRiga := value;
end;
@}


@End @SubSection

@EndSubSections

@End @Section

@Section @Title { Current file name management }
@Begin @PP

The current filename must be hold to make error messages as precise as
possible. The parser, after opening a file and before closing it,
signal an event to this module. These events are received and used to
held the current filename. @PP

Streams are held in a dynamic stack:

@d slitstatus, SegnalaInizioElaborazioneStream
@{
procedure SegnalaInizioElaborazioneStream (stream:TSlitStream);
begin
  if Length(StreamStack)>=StreamStackCount then
  begin
    SetLength(StreamStack, Length(StreamStack)+10);
  end;

  StreamStack[StreamStackCount] := stream;
  StreamStackCount := StreamStackCount+1;
end;
@}

@d slitstatus, SegnalaFineElaborazioneStream
@{
procedure SegnalaFineElaborazioneStream;
begin
  if StreamStackCount>0 then
  begin
    StreamStack[StreamStackCount] := Nil;
    StreamStackCount := StreamStackCount-1;
  end;
end;
@}

In this way we can write functions to get the current filename and the
current row:

@d slitstatus, GetCurrentParsingFile
@{
function GetCurrentParsingFile:String;
begin
  if StreamStackCount>0 then
  begin
    Result := StreamStack[StreamStackCount-1].CurrentFile;
  end
  else
  begin
    Result := '';
  end;
end;
@}

Se non c'{@Char egrave} la linea corrente allora la funzione 
ritorna -1:

@d slitstatus, GetCurrentParsingLine
@{
function GetCurrentParsingLine:Integer;
begin
  if StreamStackCount>0 then
  begin
    Result := StreamStack[StreamStackCount-1].CurrentLine;
  end
  else
  begin
    Result := -1;
  end;
end;
@}

Thanks to these procedures we can create a procedure to emit error
messages referring to che current processed file:

@d slitstatus, LogErrorMessage
@{
procedure LogErrorMessage(message:String);
begin
  if StreamStackCount>0 then
  begin
    write (StdErr, StreamStack[StreamStackCount-1].CurrentFile);
    write (StdErr, ':');
    write (StdErr, StreamStack[StreamStackCount-1].CurrentLine);
    write (StdErr, ' ');
  end;

  writeln (StdErr, message);
end;
@}

@End @Section

@Section @Title { slitstatus unit definition }
@Begin @PP

@o slitstatus.pas
@{
{$MODE OBJFPC}
{$H+}
unit slitstatus;

interface

uses slitoutput, macrostore, slitstream;
                                         
function GetNomeProcessoreInformazioni():String;
procedure SetNomeProcessoreInformazioni(value:String);
function CreaStreamOutputDaOpzioni(NomeFile:String; store:TMacroStore):TSlitOutput;
procedure SegnalaInizioElaborazioneStream (stream:TSlitStream);
procedure SegnalaFineElaborazioneStream;
procedure LogErrorMessage(message:String);
function GetCurrentParsingFile:String;
function GetCurrentParsingLine:Integer;
function GetGenerazioneMarcatoriAbilitata:Boolean;
procedure SetGenerazioneMarcatoriAbilitata(value:Boolean);
function GetGenerazioneNumeriRigaAbilitata:Boolean;
procedure SetGenerazioneNumeriRigaAbilitata(value:Boolean);
function GetColonnaNumeriRiga:Integer;
procedure SetColonnaNumeriRiga(value:Integer);
procedure AggiungiLinguaggio (Estensione, Inizio, Fine:String);
procedure PrendiMarcatori (NomeFile:String; var Inizio:String; var Fine:String);

implementation

uses sysutils, slithtml, slitlout, slittxt, slittex, strutils;

type
  @<slitstatus, RInformazioniLinguaggi@>

var
  NomeProcessoreInformazioni:String;
  GenerazioneMarcatoriAbilitata:Boolean;
  GenerazioneNumeriRigaAbilitata:Boolean;
  StreamStack: array of TSlitStream;
  StreamStackCount: Integer;
  ColonnaNumeriRiga:Integer;
  TabellaLinguaggi:array of RInformazioniLinguaggi;
  TabellaLinguaggi_Count:Integer;

@<slitstatus, gestore del processore di documentazione@>
@<slitstatus, crea lo stream di output@>
@<slitstatus, SegnalaInizioElaborazioneStream@>
@<slitstatus, SegnalaFineElaborazioneStream@>
@<slitstatus, LogErrorMessage@>
@<slitstatus, GetCurrentParsingFile@>
@<slitstatus, GetCurrentParsingLine@>
@<slitstatus, Get/Set GenerazioneMarcatoriAbilitata@>
@<slitstatus, Get/Set GenerazioneNumeriRigaAbilitata@>
@<slitstatus, Get/Set ColonnaNumeriRiga@>
@<slitstatus, AggiungiLinguaggio@>
@<slitstatus, PrendiMarcatori@>

initialization

  NomeProcessoreInformazioni := 'lout';
  StreamStackCount := 0;
  GenerazioneMarcatoriAbilitata := True;
  GenerazioneNumeriRigaAbilitata := True;
  ColonnaNumeriRiga := 100;
  TabellaLinguaggi_Count := 0;
  SetLength (TabellaLinguaggi, 0);

  @<slitstatus, configurazione delle estensioni predefinite@>

end.
@}

@End @Section

@EndSections

@End @Chapter
