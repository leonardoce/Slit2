% :mode=slitpascal:folding=explicit:

Questo driver viene agganciato al parser di Slit per riempire un magazzino
con le macro.

Il driver viene creato collegandolo ad un magazzino di macro:

@d TSlitStreamDriverMagazzino.CreateWithMacroStore
@{
constructor TSlitStreamDriverMagazzino.CreateWithMacroStore(ms:TMacroStore);
begin
  FMacroStore := ms;
end;
@}

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

La definizione della classe ""TSlitStreamDriverMagazzino"" e del file
dove è contenuta è quindi la seguente:

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
  end;

implementation
  @<TSlitStreamDriverMagazzino.CreateWithMacroStore@>
  @<TSlitStreamDriverMagazzino.ProcessaDefinizioneMacro@>
  @<TSlitStreamDriverMagazzino.ProcessaDefinizioneFile@>
  @<TSlitStreamDriverMagazzino.ProcessaRigaDocumentazione@>
  
end.

@}

