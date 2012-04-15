# -*- mode:lout -*-
@Chapter
@Title { Documentation generating backend }
@Begin @LP

Slit can managed different documentation formats. The backend that
manage the documentation output are designed in this way:

@d TSlitOutput
@{                                                  
EScrapType = (DefinitionScrap, AppendScrap, FileScrap);

TSlitOutput = class
public                      
  procedure ScriviScrap(tipo:EScrapType; nome, contenuto:String); virtual; abstract;
  procedure PutLine(str:String); virtual; abstract;
end;
@}

With the method @F ScriviScrap you can write to the documentation a scrap of code.

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
