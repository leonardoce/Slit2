# :mode=slitpascal:folding=explicit:
@Chapter 
@Title { Driver per il magazzino delle macro e le opzioni }
@Begin @LP

Questo driver viene agganciato al parser di Slit per riempire un magazzino
con le macro. @PP

Il driver viene creato collegandolo ad un magazzino di macro:

@d TSlitStreamDriverMagazzino.CreateWithMacroStore
@{
constructor TSlitStreamDriverMagazzino.CreateWithMacroStore(ms:TMacroStore);
begin
  FMacroStore := ms;
end;
@}

@BeginSections
@Section @Title { Immagazzinamento delle macro }
@Begin @PP

Alla ricezione di una definizione di macro viene controllato se esiste
già una macro con questo nome, e nel caso viene dato uno warning, altrimenti
questa viene direttamente inserita nel magazzino:

@d TSlitStreamDriverMagazzino.ProcessaDefinizioneMacro
@{
procedure TSlitStreamDriverMagazzino.ProcessaDefinizioneMacro(nomeMacro:String; scrap:String); 
var
  tempMacro : TMacroRecord;
begin
  tempMacro := FMacroStore.GetMacro(nomeMacro);
  if tempMacro.macroName <> '' then
  begin
    writeln(StdErr, 'Macro ', nomeMacro, ' definita più volte');
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
  if tempMacro.macroName <> '' then
  begin
    writeln(StdErr, 'Macro ', nomeMacro, ' definita più volte');
  end
  else
  begin
    FMacroStore.StoreMacro(nomeMacro , scrap, FileMacro);
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
  { no op }
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
  public
    constructor CreateWithMacroStore(ms:TMacroStore);
    procedure ProcessaDefinizioneMacro(nomeMacro:String; scrap:String); 
      override;
    procedure ProcessaDefinizioneFile(nomeMacro:String; scrap:String);  
      override;
    procedure ProcessaRigaDocumentazione(riga:String);
      override;
    procedure ProcessaOpzione(opzione:String);
      override;
  end;

implementation
  @<TSlitStreamDriverMagazzino.CreateWithMacroStore@>
  @<TSlitStreamDriverMagazzino.ProcessaDefinizioneMacro@>
  @<TSlitStreamDriverMagazzino.ProcessaDefinizioneFile@>
  @<TSlitStreamDriverMagazzino.ProcessaRigaDocumentazione@>
  @<TSlitStreamDriverMagazzino.ProcessaOpzione@>
end.
@}

@End @Section

@EndSections

@End @Chapter
