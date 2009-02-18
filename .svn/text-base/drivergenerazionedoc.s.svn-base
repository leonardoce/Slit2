% :mode=slitpascal:folding=explicit:

Questo driver viene agganciato al parser di Slit per generare la documentazione
utilizzando uno fra i vari back-end disponibili.

Il driver viene creato collegandolo ad un magazzino di macro:

@d TSlitStreamDriverGenerazioneDoc.CreateWithOutputStream
@{
constructor TSlitStreamDriverGenerazioneDoc.CreateWithOutputStream(output:TSlitOutput);
begin
  FOutputStream := output;
end;
@}

Alla ricezione di una definizione di macro questa viene scritta attraverso
il backend:

@d TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneMacro
@{
procedure TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneMacro(nomeMacro:String; scrap:String); 
begin
  FOutputStream.ScriviScrap(ScrapMacro, nomeMacro, scrap);
end;
@}

Anche quando viene ricevuta una definizione di file questa viene 
scritta attraverso il backend:

@d TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneFile
@{
procedure TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneFile(nomeMacro:String; scrap:String); 
begin
  FOutputStream.ScriviScrap(FileMacro, nomeMacro, scrap);
end;
@}

Le righe di documentazione vengono passate direttamente:

@d TSlitStreamDriverGenerazioneDoc.ProcessaRigaDocumentazione
@{
procedure TSlitStreamDriverGenerazioneDoc.ProcessaRigaDocumentazione(riga:String);
begin
  FOutputStream.PutLine(riga);
end;
@}

La definizione della classe ""TSlitStreamDriverGenerazioneDoc"" e del file
dove è contenuta è quindi la seguente:

@o driverdoc.pas
@{
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
  @<TSlitStreamDriverGenerazioneDoc.CreateWithOutputStream@>
  @<TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneMacro@>
  @<TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneFile@>
  @<TSlitStreamDriverGenerazioneDoc.ProcessaRigaDocumentazione@>
  
end.

@}

