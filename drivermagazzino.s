# -*- mode:lout -*-
@Chapter
@Title { Driver per il magazzino delle macro e le opzioni }
@Begin @LP

Questo driver viene agganciato al parser di Slit per riempire un magazzino
con le macro. @PP

Il driver viene creato collegandolo ad un magazzino di macro e le
opzioni vengono inizializzate alla stringa vuota.

@d TSlitStreamDriverMagazzino.CreateWithMacroStore
@{
constructor TSlitStreamDriverMagazzino.CreateWithMacroStore(ms:TMacroStore);
begin
  FMacroStore := ms;
  FTipoOutput := '';
end;
@}

@BeginSections
@Section @Title { Immagazzinamento delle macro }
@Begin @PP

Alla ricezione di una definizione di macro viene controllato se esiste
gi{@Char agrave} una macro con questo nome, e nel caso viene dato uno warning, altrimenti
questa viene direttamente inserita nel magazzino:

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

Anche quando viene ricevuta una definizione di file questa viene
inserita nel magazzino:

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

Il caso nel quale alla macro ne viene aggiunta un'altra @Char egrave un
po' diverso: bisogna prima controllare l'esistenza della macro prima
di aggiungerci altre cose. @PP


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


Invece le righe di documentazione vengono scartate:

@d TSlitStreamDriverMagazzino.ProcessaRigaDocumentazione
@{
procedure TSlitStreamDriverMagazzino.ProcessaRigaDocumentazione(riga:String);
begin
  { no op }
end;
@}

@End @Section

@Section @Title { Trattamento delle opzioni }
@Begin @PP

Le opzioni vengono trattate qui e trasformate in chiamate alle
opportune funzioni dello stato globale del programma. @PP

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

L'opzione dei commenti merita un discorso in pi{@Char ugrave} rispetto
alle altre perch{@Char egrave} il codice per la gestione del parametro
{@Char egrave} quantomeno particolare. @PP

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

@Section @Title { Definizione del driver }
@Begin @PP

La definizione della classe ""TSlitStreamDriverMagazzino"" e del file
dove {@Char egrave} contenuta {@Char egrave} quindi la seguente:

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
end.
@}

@End @Section

@EndSections

@End @Chapter
