# -*- mode:lout -*-
@Chapter
@Title { Backend di generazione della documentazione }
@Begin @LP

Slit {@Char egrave} pensato per avere pi{@Char ugrave} formati per la documentazione. Per questo
l'oggetto che gestisce l'output della documentazione viene modellato
nel seguente modo:

@d TSlitOutput
@{                                                  
EScrapType = (DefinitionScrap, AppendScrap, FileScrap);

TSlitOutput = class
public                      
  procedure ScriviScrap(tipo:EScrapType; nome, contenuto:String); virtual; abstract;
  procedure PutLine(str:String); virtual; abstract;
end;
@}

Con il metodo "ScriviScrap" {@Char egrave} possibile scrivere sulla documentazione uno
"scrap" di codice.

@o slitoutput.pas
@{
                                                      
{$MODE OBJFPC}                                  
{$H+}
unit slitoutput;

interface
  uses macrostore;

type
  @<TSlitOutput@>

implementation

end.
@}

@BeginSections

@i output_html.s
@i output_txt.s
@i output_lout.s

@EndSections
@End @Chapter
