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
  constructor TSlitStreamDriverMagazzino.CreateWithMacroStore(ms:TMacroStore);
  begin
    FMacroStore := ms;
  end;
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
  procedure TSlitStreamDriverMagazzino.ProcessaRigaDocumentazione(riga:String);
  begin
    { no op }
  end;
  
end.
