# -*- mode:lout -*-
@Chapter
@Title { Macro store and parameters }
@Begin @LP

With this driver the macro store get populated. @PP

This drives is called with a macro store. @PP

@d TSlitStreamDriverMagazzino.CreateWithMacroStore
@{
constructor TSlitStreamDriverMagazzino.CreateWithMacroStore(ms:TMacroStore);
begin
  FMacroStore := ms;
  FTipoOutput := '';
end;
@}

@BeginSections
@Section @Title { Adding a new macro }
@Begin @PP

When a macro get read it's checked and, if a macro with the same name
already exists, a warning is emitted and the new macro is not
considerated. @PP

If it's all ok the macro is added to the macro store:

@d TSlitStreamDriverMagazzino.ProcessaDefinizioneMacro
@{
procedure TSlitStreamDriverMagazzino.ProcessaDefinizioneMacro(
  nomeMacro:String; scrap:String; scrapStartLine:Integer);
var
  tempMacro : TMacroRecord;
begin
  tempMacro := FMacroStore.GetMacro(nomeMacro);
  if tempMacro<>Nil then
  begin
    LogErrorMessage('Macro ' + nomeMacro + ' definita piu'' volte');
  end
  else
  begin
    FMacroStore.StoreMacro(nomeMacro, scrap, ScrapMacro, Parser.CurrentFile,
      scrapStartLine);
  end;
end;
@}

The same happens for file definition macros:

@d TSlitStreamDriverMagazzino.ProcessaDefinizioneFile
@{
procedure TSlitStreamDriverMagazzino.ProcessaDefinizioneFile(
  nomeMacro:String; scrap:String; scrapStartLine:Integer);
var
  tempMacro : TMacroRecord;
begin
  tempMacro := FMacroStore.GetMacro(nomeMacro);
  if tempMacro<>Nil then
  begin
    LogErrorMessage('Macro ' + nomeMacro + ' definita piu'' volte');
  end
  else
  begin
    FMacroStore.StoreMacro(nomeMacro , scrap, FileMacro,
      Parser.CurrentFile, scrapStartLine);
  end;
end;
@}

When a scrap is added to an already existing macro we must do a
different sequence of checks: in this case the macro must already
exists. If the macro is already existing, the content of the scrap get
added. @PP

@d TSlitStreamDriverMagazzino.ProcessaAggiungiNellaMacro
@{
procedure TSlitStreamDriverMagazzino.ProcessaAggiungiNellaMacro(
  nomeMacro:String; scrap:String; scrapStartLine:Integer);
var
  tempMacro : TMacroRecord;
begin
  tempMacro := FMacroStore.GetMacro(nomeMacro);
  if tempMacro<>Nil then
  begin
    tempMacro.AddContent (scrap, Parser.CurrentFile, scrapStartLine);
  end
  else
  begin
    LogErrorMessage('Richiesta aggiunta di uno scrap alla macro ' + nomeMacro +
      ' che non e'' stata ancora definita');
  end;
end;
@}

The rows of documentation, in this implementation, are discarded:

@d TSlitStreamDriverMagazzino.ProcessaRigaDocumentazione
@{
procedure TSlitStreamDriverMagazzino.ProcessaRigaDocumentazione(riga:String);
begin
  { no op }
end;
@}

@End @Section

@Section @Title { Parameters }
@Begin @PP

The parameters are treated here and managed calling the functions of
the Slit global state: @PP

@d TSlitStreamDriverMagazzino.ProcessaOpzione
@{
procedure TSlitStreamDriverMagazzino.ProcessaOpzione(opzione:String);
begin
  opzione := Trim(Opzione);
  if opzione='output_lout' then
  begin
    SetNomeProcessoreInformazioni ('lout');
  end
  else if opzione='output_html' then
  begin
    SetNomeProcessoreInformazioni ('html');
  end
  else if opzione='output_txt' then
  begin
    SetNomeProcessoreInformazioni ('txt');
  end
  else if opzione='section_markers' then
  begin
    SetGenerazioneMarcatoriAbilitata(true);
  end
  else if opzione='no_section_markers' then
  begin
    SetGenerazioneMarcatoriAbilitata(false);
  end
  else if opzione='line_markers' then
  begin
    SetGenerazioneNumeriRigaAbilitata(true);
  end
  else if opzione='no_line_markers' then
  begin
    SetGenerazioneNumeriRigaAbilitata(false);
  end
  else if AnsiStartsStr('comment_markers', opzione) then
  begin
    GestioneOpzioneCommenti (opzione);
  end
  else
  begin
    LogErrorMessage('Opzione non conosciuta: ' + opzione);
  end;
end;
@}

The parameters needed to add comment to the generated source code is a
little different because the markers are to be extracted from the
option parameter. @PP

@d TSlitStreamDriverMagazzino.GestioneOpzioneCommenti
@{
procedure TSlitStreamDriverMagazzino.GestioneOpzioneCommenti (opzione:String);
var
  estensione, inizio, fine:String;
  delimitatori: TSysCharSet;

begin
  opzione := MidStr (opzione, Length('comment_markers')+1,
    Length(opzione)-Length('comment_markers')-1);
  opzione := Trim (opzione);

  if Length(opzione)<5 then
  begin
    LogErrorMessage ('opzione comment_markers con valore non valido');
  end
  else
  begin
    delimitatori := [opzione[1]];

    estensione := ExtractDelimited (2, opzione, delimitatori);
    inizio := ExtractDelimited (3, opzione, delimitatori);
    fine := ExtractDelimited (4, opzione, delimitatori);

    AggiungiLinguaggio (estensione, inizio, fine);
  end;
end;
@}

@End @Section

@Section @Title { Reverse literate programming support }
@Begin @PP

The @I emit directive is simply ignored: in fact it's only considered
while generating the documentation. @PP

@d TSlitStreamDriverMagazzino.ProcessEmitMacro
@{
procedure TSlitStreamDriverMagazzino.ProcessEmitMacro(macroName:String); 
begin
end;
@}

@End @Section

@Section @Title { Driver definition }
@Begin @PP

The definition of the class @F @Verbatim { TSlitStreamDriverMagazzino }
and of the relative unit is following:

@o drivermagazzino.pas
@{
{$MODE OBJFPC}
{$H+}
unit drivermagazzino;

interface
  uses macrostore, slitstream;

type
  TSlitStreamDriverMagazzino = class(TSlitStreamDriver)
    FMacroStore: TMacroStore;
    FTipoOutput: String;
  public
    constructor CreateWithMacroStore(ms:TMacroStore);
    procedure ProcessaDefinizioneMacro(nomeMacro:String; scrap:String;
      scrapStartLine:Integer); override;
    procedure ProcessaAggiungiNellaMacro(nomeMacro:String; scrap:String;
      scrapStartLine:Integer); override;
    procedure ProcessaDefinizioneFile(nomeMacro:String; scrap:String;
      scrapStartLine:Integer); override;
    procedure ProcessEmitMacro(macroName:String); 
      override;
    procedure ProcessaRigaDocumentazione(riga:String);
      override;
    procedure ProcessaOpzione(opzione:String);
      override;
    procedure GestioneOpzioneCommenti (opzione:String);
  end;

implementation
  uses strutils, sysutils, slitstatus;

  @<TSlitStreamDriverMagazzino.CreateWithMacroStore@>
  @<TSlitStreamDriverMagazzino.ProcessaDefinizioneMacro@>
  @<TSlitStreamDriverMagazzino.ProcessaDefinizioneFile@>
  @<TSlitStreamDriverMagazzino.ProcessaRigaDocumentazione@>
  @<TSlitStreamDriverMagazzino.ProcessaOpzione@>
  @<TSlitStreamDriverMagazzino.ProcessaAggiungiNellaMacro@>
  @<TSlitStreamDriverMagazzino.GestioneOpzioneCommenti@>
  @<TSlitStreamDriverMagazzino.ProcessEmitMacro@>
end.
@}

@End @Section

@EndSections

@End @Chapter
