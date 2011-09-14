# :mode=slitpascal:folding=explicit:
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
procedure TSlitStreamDriverMagazzino.ProcessaDefinizioneMacro(nomeMacro:String; scrap:String); 
var
  tempMacro : TMacroRecord;
begin
  tempMacro := FMacroStore.GetMacro(nomeMacro);
  if tempMacro<>Nil then
  begin
    writeln(StdErr, 'Macro ', nomeMacro, ' definita piu'' volte');
  end
  else
  begin
    FMacroStore.StoreMacro(nomeMacro, scrap, ScrapMacro);
  end;
end;
@}

Anche quando viene ricevuta una definizione di file questa viene 
inserita nel magazzino:

@d TSlitStreamDriverMagazzino.ProcessaDefinizioneFile
@{
procedure TSlitStreamDriverMagazzino.ProcessaDefinizioneFile(nomeMacro:String; scrap:String); 
var
  tempMacro : TMacroRecord;
begin
  tempMacro := FMacroStore.GetMacro(nomeMacro);
  if tempMacro<>Nil then
  begin
    writeln(StdErr, 'Macro ', nomeMacro, ' definita piu'' volte');
  end
  else
  begin
    FMacroStore.StoreMacro(nomeMacro , scrap, FileMacro);
  end;
end;
@}

Il caso nel quale alla macro ne viene aggiunta un'altra @Char egrave un
po' diverso: bisogna prima controllare l'esistenza della macro prima
di aggiungerci altre cose. @PP

@d TSlitStreamDriverMagazzino.ProcessaAggiungiNellaMacro
@{
procedure TSlitStreamDriverMagazzino.ProcessaAggiungiNellaMacro(nomeMacro:String; scrap:String); 
var
  tempMacro : TMacroRecord;
begin
  tempMacro := FMacroStore.GetMacro(nomeMacro);
  if tempMacro<>Nil then
  begin
    tempMacro.AddContent (scrap);
  end
  else
  begin
    writeln(StdErr, 'Richiesta aggiunta di uno scrap alla macro ', nomeMacro, 
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
  else
  begin
    raise Exception.Create('Opzione non conosciuta: ' + opzione);
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
    procedure ProcessaDefinizioneMacro(nomeMacro:String; scrap:String); 
      override;
    procedure ProcessaAggiungiNellaMacro(nomeMacro:String; scrap:String);
      override;
    procedure ProcessaDefinizioneFile(nomeMacro:String; scrap:String);  
      override;
    procedure ProcessaRigaDocumentazione(riga:String);
      override;
    procedure ProcessaOpzione(opzione:String);
      override;
  end;

implementation
  uses strutils, sysutils, slitstatus;

  @<TSlitStreamDriverMagazzino.CreateWithMacroStore@>
  @<TSlitStreamDriverMagazzino.ProcessaDefinizioneMacro@>
  @<TSlitStreamDriverMagazzino.ProcessaDefinizioneFile@>
  @<TSlitStreamDriverMagazzino.ProcessaRigaDocumentazione@>
  @<TSlitStreamDriverMagazzino.ProcessaOpzione@>
  @<TSlitStreamDriverMagazzino.ProcessaAggiungiNellaMacro@>
end.
@}

@End @Section

@EndSections

@End @Chapter
