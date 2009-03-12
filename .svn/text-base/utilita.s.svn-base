# :folding=explicit:mode=slitpascal:
@Chapter
@Title { Utilità per la gestione dei files html }
@Begin @LP

Slit produce output anche in formato HTML e il formato HTML prevede, per il testo
in esso contenuto, una sintassi particolare. @PP

Il testo, in HTML, è composto da una serie di carattere oppure di entità. I
caratteri che devono essere trattati in modo articolare sono il maggiore, il minore,
e la "e commerciale". @PP

Per questo motivo esiste una funzione il cui scopo è di implementare
questa politica: 

@d text2html dichiarazione
@{
  function text2html(str:String):String;
@}

La funzione scorre tutta la stringa passata codificando ogni carattere in formato
HTML:

@d text2html
@{
  function text2html(str:String):String;
  var
    buffer:String;
    i:Integer;
  begin
    buffer := '';
    for i:=1 to Length(str) do
    begin
      @<text2html controllo carattere@>
    end;

    Result := buffer;
  end;
@}

Ogni carattere viene controllato:

@d text2html controllo carattere
@{
  if str[i]='<' then
  begin
    buffer := buffer + '&lt;';
  end
  else if str[i]='>' then
  begin
    buffer := buffer + '&gt;';
  end
  else if str[i]='&' then
  begin
    buffer := buffer + '&amp;';
  end
  else
  begin
    buffer := buffer + str[i];
  end;
@}

Riassumendo esiste una unit definita in modo seguente:


@d unit htmlutils
@{
{$MODE OBJFPC}
{$M+}
unit htmlutils;

interface

  uses sysutils, strutils;

  @<text2html dichiarazione@>

implementation

  @<text2html@>
end.
@}

@o htmlutils.pas
@{
  @<unit htmlutils@>
@}

@End @Chapter
