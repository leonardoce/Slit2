# :mode=slitpascal:folding=explicit:
@Chapter
@Title { Driver per la generazione della documentazione }
@Begin @LP

Questo driver viene agganciato al parser di Slit per generare la documentazione
utilizzando uno fra i vari back-end disponibili. @PP

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
procedure TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneMacro(nomeMacro:String; scrap:String; scrapStartLine:Integer); 
begin
  FOutputStream.ScriviScrap(DefinitionScrap, nomeMacro, scrap);
end;
@}

Anche quando viene ricevuta una definizione di file questa viene 
scritta attraverso il backend:

@d TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneFile
@{
procedure TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneFile(nomeMacro:String; scrap:String; scrapStartLine:Integer); 
begin
  FOutputStream.ScriviScrap(FileScrap, nomeMacro, scrap);
end;
@}


Quando invece si tratta di una aggiunta in coda ad un'altra macro:

@d TSlitStreamDriverGenerazioneDoc.ProcessaAggiungiNellaMacro
@{
procedure TSlitStreamDriverGenerazioneDoc.ProcessaAggiungiNellaMacro(nomeMacro:String; scrap:String; scrapStartLine:Integer); 
begin
  FOutputStream.ScriviScrap(AppendScrap, nomeMacro, scrap);
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

Le opzioni, per la generazione della documentazione, vengono semplicemente
ignorate. @PP

@d TSlitStreamDriverGenerazioneDoc.ProcessaOpzione
@{
procedure TSlitStreamDriverGenerazioneDoc.ProcessaOpzione(opzione:String);
begin
  {* nop() *}
end;
@}

La definizione della classe @F "TSlitStreamDriverGenerazioneDoc" e del file
dove {@Char egrave} contenuta {@Char egrave} quindi la seguente:

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
    procedure ProcessaDefinizioneMacro(nomeMacro:String; scrap:String; scrapStartLine:Integer); 
      override;
    procedure ProcessaDefinizioneFile(nomeMacro:String; scrap:String; scrapStartLine:Integer);  
      override;
    procedure ProcessaAggiungiNellaMacro(nomeMacro:String; scrap:String; scrapStartLine:Integer);
      override;
    procedure ProcessaRigaDocumentazione(riga:String);
      override;
    procedure ProcessaOpzione(opzione:String);
      override;
  end;

implementation
  @<TSlitStreamDriverGenerazioneDoc.CreateWithOutputStream@>
  @<TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneMacro@>
  @<TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneFile@>
  @<TSlitStreamDriverGenerazioneDoc.ProcessaRigaDocumentazione@>
  @<TSlitStreamDriverGenerazioneDoc.ProcessaOpzione@>
  @<TSlitStreamDriverGenerazioneDoc.ProcessaAggiungiNellaMacro@>
  
end.

@}

@End @Chapter
