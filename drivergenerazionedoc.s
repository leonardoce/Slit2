# -*- mode:lout -*-
@Chapter
@Title { Documentation generator }
@Begin @LP

This driver get bound to the parser to generate documentation using
one of the predefined backend. @PP

The driver is created with a macro store:

@d TSlitStreamDriverGenerazioneDoc.Create
@{
constructor TSlitStreamDriverGenerazioneDoc.Create(output:TSlitOutput; store:TMacroStore);
begin
  FOutputStream := output;
  FMacroStore := store;
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

When a macro must be recalled from the store with the @I emit
directive we must interact with the macro store and read the macro:

@d TSlitStreamDriverGenerazioneDoc.ProcessEmitMacro
@{
procedure TSlitStreamDriverGenerazioneDoc.ProcessEmitMacro(macroName:String); 
var
  MRecord : TMacroRecord;
begin
  MRecord := FMacroStore.GetMacro (macroName);
  if MRecord=Nil then
  begin
    LogErrorMessage ('This macro is unknown: <' + macroName + '>');
  end
  else
  begin
    FOutputStream.ScriviScrap(AppendScrap, macroName, MRecord.MacroContent);    
  end;
end;
@}

The directive to read another source file has no effect in this driver:

@d TSlitStreamDriverGenerazioneDoc.ProcessReadSourceFile
@{
procedure TSlitStreamDriverGenerazioneDoc.ProcessReadSourceFile(fileName:String); 
begin
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
  uses slitoutput, slitstream, macrostore, slitstatus;

type
  TSlitStreamDriverGenerazioneDoc = class(TSlitStreamDriver)
    FOutputStream: TSlitOutput;
    FMacroStore: TMacroStore;
  public
    constructor Create(output:TSlitOutput; store:TMacroStore);
    procedure ProcessaDefinizioneMacro(nomeMacro:String; scrap:String; 
      scrapStartLine:Integer); override;
    procedure ProcessaDefinizioneFile(nomeMacro:String; scrap:String; 
      scrapStartLine:Integer); override;
    procedure ProcessaAggiungiNellaMacro(nomeMacro:String; scrap:String; 
      scrapStartLine:Integer); override;
    procedure ProcessaRigaDocumentazione(riga:String);
      override;
    procedure ProcessEmitMacro(macroName:String); 
      override;
    procedure ProcessReadSourceFile(fileName:String); 
      override;
    procedure ProcessaOpzione(opzione:String);
      override;
  end;

implementation
  @<TSlitStreamDriverGenerazioneDoc.Create@>
  @<TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneMacro@>
  @<TSlitStreamDriverGenerazioneDoc.ProcessaDefinizioneFile@>
  @<TSlitStreamDriverGenerazioneDoc.ProcessaRigaDocumentazione@>
  @<TSlitStreamDriverGenerazioneDoc.ProcessaOpzione@>
  @<TSlitStreamDriverGenerazioneDoc.ProcessaAggiungiNellaMacro@>
  @<TSlitStreamDriverGenerazioneDoc.ProcessEmitMacro@>
  @<TSlitStreamDriverGenerazioneDoc.ProcessReadSourceFile@>
  
end.

@}

@End @Chapter
