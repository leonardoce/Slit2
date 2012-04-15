# -*- mode:lout -*-
@Chapter
@Title { Documentation generator }
@Begin @LP

This driver get bound to the parser to generate documentation using
one of the predefined backend. @PP

The driver is created with a macro store:

@d TSlitStreamDriverGenerazioneDoc.CreateWithOutputStream
@{
constructor TSlitStreamDriverGenerazioneDoc.CreateWithOutputStream(output:TSlitOutput);
begin
  FOutputStream := output;
end;
@}

When a macro definition is received the macro definition must be
written in the documentation using the backend:

@d TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneMacro
@{
procedure TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneMacro(
  nomeMacro:String; scrap:String; scrapStartLine:Integer); 
begin
  FOutputStream.ScriviScrap(DefinitionScrap, nomeMacro, scrap);
end;
@}

The same happens whene a file definition is received:

@d TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneFile
@{
procedure TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneFile(
  nomeMacro:String; scrap:String; scrapStartLine:Integer); 
begin
  FOutputStream.ScriviScrap(FileScrap, nomeMacro, scrap);
end;
@}

When a macro get added to the end of another:

@d TSlitStreamDriverGenerazioneDoc.ProcessaAggiungiNellaMacro
@{
procedure TSlitStreamDriverGenerazioneDoc.ProcessaAggiungiNellaMacro(
  nomeMacro:String; scrap:String; scrapStartLine:Integer); 
begin
  FOutputStream.ScriviScrap(AppendScrap, nomeMacro, scrap);
end;
@}

The documentation lines are directly wrote:

@d TSlitStreamDriverGenerazioneDoc.ProcessaRigaDocumentazione
@{
procedure TSlitStreamDriverGenerazioneDoc.ProcessaRigaDocumentazione(
  riga:String);
begin
  FOutputStream.PutLine(riga);
end;
@}

Parameters are simply ignored because are handled by the macro store
driver: @PP

@d TSlitStreamDriverGenerazioneDoc.ProcessaOpzione
@{
procedure TSlitStreamDriverGenerazioneDoc.ProcessaOpzione(opzione:String);
begin
  {* nop() *}
end;
@}

The definition of the class @F "TSlitStreamDriverGenerazioneDoc" and of the containing file
is the following:

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
    procedure ProcessaDefinizioneMacro(nomeMacro:String; scrap:String; 
      scrapStartLine:Integer); override;
    procedure ProcessaDefinizioneFile(nomeMacro:String; scrap:String; 
      scrapStartLine:Integer); override;
    procedure ProcessaAggiungiNellaMacro(nomeMacro:String; scrap:String; 
      scrapStartLine:Integer); override;
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
