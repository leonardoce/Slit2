{$MODE OBJFPC}
{$H+}
unit driverdoc;

interface
  uses slitoutput, slitstream, macrostore;

type
  TSlitStreamDriverGenerazioneDoc = class(TSlitStreamDriver)
    FOutputStream: TSlitOutput;
  public
    constructor CreateWithOutputStream(output:TSlitOutput);
    procedure ProcessaDefinizioneMacro(nomeMacro:String; scrap:String); 
      override;
    procedure ProcessaDefinizioneFile(nomeMacro:String; scrap:String);  
      override;
    procedure ProcessaRigaDocumentazione(riga:String);
      override;
  end;

implementation
  constructor TSlitStreamDriverGenerazioneDoc.CreateWithOutputStream(output:TSlitOutput);
  begin
    FOutputStream := output;
  end;
  procedure TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneMacro(nomeMacro:String; scrap:String); 
  begin
    FOutputStream.ScriviScrap(ScrapMacro, nomeMacro, scrap);
  end;
  procedure TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneFile(nomeMacro:String; scrap:String); 
  begin
    FOutputStream.ScriviScrap(FileMacro, nomeMacro, scrap);
  end;
  procedure TSlitStreamDriverGenerazioneDoc.ProcessaRigaDocumentazione(riga:String);
  begin
    FOutputStream.PutLine(riga);
  end;
  
end.

