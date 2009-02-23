# :folding=explicit:mode=slitpascal: 

# {{{
Slit e' pensato per avere piu' formati per la documentazione. Per questo
l'oggetto che gestisce l'output della documentazione viene modellato
nel seguente modo:

@d TSlitOutput
@{                                                  
TSlitOutput = class
public                      
  procedure ScriviScrap(tipo:EMacroType; nome, contenuto:String); virtual; abstract;
  procedure PutLine(str:String); virtual; abstract;
end;
@}

Con il metodo ""ScriviScrap"" e' possibile scrivere sulla documentazione uno
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
# }}}

=== Output in formato HTML ===
# {{{
L'output in formato HTML permette di creare un file HTML per la documentazione.
I tag devono essere immessi manualmente (eccetto quegli che gestiscono gli scrap).

Il file creato ha l'estensione "".html"" aggiunta al nome del file di documentazione:

@d TSlitOutputHtml.CreateForFile
@{
  constructor TSlitOutputHtml.CreateForFile(fileName:String);
  begin
    Assign(handle, ExtractFileName(fileName)+'.html');
    Rewrite(handle);
  end;
@}

Il file viene chiuso quando l'output viene liberato:

@d TSlitOutputHtml.Destroy
@{
  destructor TSlitOutputHtml.Destroy;
  begin
    Close(handle);
  end;
@}

Le righe di documentazione vengono inserite esattamente come
sono all'interno del file HTML. Questo permette di
aggiungere dei tag alla documentazione:

@d TSlitOutputHtml.PutLine
@{
  procedure TSlitOutputHtml.PutLine(str:String);
  begin
    writeln(handle, str);
  end;
@}

La testata viene scritta in un ""div"" e gli scrap vengono inseriti in 
blocchi preformattati:

@d TSlitOutputHtml.ScriviScrap
@{
procedure TSlitOutputHtml.ScriviScrap(tipo:EMacroType; nome, contenuto:String);
var
  titolo: String;
begin
  if tipo = FileMacro then
  begin
    titolo := 'File';
  end
  else
  begin
    titolo := 'Definizione';
  end;

  writeln(handle,
    '<div class=''testata''>', text2html(titolo), ' ', text2html(nome), '</div>');

  writeln(handle, '<pre>');
  writeln(handle, text2html(contenuto));
  writeln(handle, '</pre>');
end;
@}

Riassumendo, il codice di gestione del formato HTML e' il seguente:

@o slithtml.pas
@{
{$MODE OBJFPC}
{$H+}
unit slithtml;

interface
  uses slitoutput, macrostore;

type
  TSlitOutputHtml = class(TSlitOutput)
  private
    handle:Text;
  public
    constructor CreateForFile(fileName:String);
    destructor Destroy; override;

    procedure ScriviScrap(tipo:EMacroType; nome, contenuto:String); override;
    procedure PutLine(str:String); override;
  end;

implementation
  uses sysutils, strutils, classes, htmlutils;

  @<TSlitOutputHtml.CreateForFile@>
  @<TSlitOutputHtml.Destroy@>
  @<TSlitOutputHtml.PutLine@>
  @<TSlitOutputHtml.ScriviScrap@>

end.
@}

= }}}

=== Output in formato testo ===
# {{{
Slit puo' anche lavorare con file in formato testo. In questo caso i file sono
pensati per essere utilizzati con txt2tags.

Al file di documentazione viene aggiunta l'estensione ".txt":

@d TSlitOutputTxt.CreateForFile
@{
constructor TSlitOutputTxt.CreateForFile(fileName:String);
begin
  Assign(handle, ExtractFileName(fileName)+'.txt');
  Rewrite(handle);
end;
@}

E al rilascio del flusso il file viene chiuso:

@d TSlitOutputTxt.Destroy
@{
destructor TSlitOutputTxt.Destroy;
begin
  Close(handle);
end;
@}

Le linee di documentazione vengono inserite nel file direttamente come sono:

@d TSlitOutputTxt.PutLine
@{
procedure TSlitOutputTxt.PutLine(str:String);
begin
  writeln(handle, str);
end;
@}

La testata viene scritta in modo che prima e dopo di essa ci sia una
linea orizzontale. Il titolo della testata viene scritto in grassetto.

Gli scrap vengono scritti fra blocchi di testo in modo che non vengono
interpretati da txt2tags:

@d TSlitOutputTxt.ScriviScrap
@{
procedure TSlitOutputTxt.ScriviScrap(tipo:EMacroType; nome, contenuto:String);
var
  titolo: String;
begin
  if tipo = FileMacro then
  begin
    titolo := 'File';
  end
  else
  begin
    titolo := 'Definizione';
  end;

  writeln(handle, '------------------------------');
  writeln(handle, '| **', titolo, ' ', nome, '**');
  writeln(handle, '------------------------------');

  writeln(handle, '```');
  writeln(handle, contenuto);
  writeln(handle, '```');
  writeln(handle, '------------------------------');
end;
@}

Riassumendo:

@o slittxt.pas
@{
{$MODE OBJFPC}
{$H+}
unit slittxt;

interface
  uses slitstream, slitoutput, macrostore;

type

  TSlitOutputTxt = class(TSlitOutput)
  private
    handle:Text;
  public
    constructor CreateForFile(fileName:String);
    destructor Destroy; override;

    procedure ScriviScrap(tipo:EMacroType; nome, contenuto:String); override;
    procedure PutLine(str:String); override;
  end;

implementation
  uses sysutils, strutils, classes;

  @<TSlitOutputTxt.CreateForFile@>
  @<TSlitOutputTxt.Destroy@>
  @<TSlitOutputTxt.PutLine@>
  @<TSlitOutputTxt.ScriviScrap@>

end.
@}
# }}}

=== Output in formato Lout ===
# {{{
LOut e' un sistema di typesetting di spirito simile a TeX ma di sintassi piu' semplice e piu'
facilmente programmabile.

Uno dei vantaggi di LOut e' la programmabilita' e la leggerezza.

Slit puo' creare documentazione in formato LOut.

I file di lout non hanno una estensione determinata. Per quanto riguarda slit l'estensione
che viene aggiunta alla documentazione e' "".lout"".

@d TSlitOutputLout.CreateForFileAndStore
@{
constructor TSlitOutputLout.CreateForFileAndStore(fileName:String; store:TMacroStore);
begin
  Assign(handle, ExtractFileName(fileName)+'.lout');
  Rewrite(handle);
  FStore := store;
end;
@}

La chiusura avviene alla dismissione del flusso:

@d TSlitOutputLout.Destroy
@{
destructor TSlitOutputLout.Destroy;
begin
  Close(handle);
end;
@}

Le righe di documentazione vengono inserite esattamente come
sono all'interno del file Lout. Questo permette di
aggiungere istruzioni lout alla documentazione:

@d TSlitOutputLout.PutLine
@{
procedure TSlitOutputLout.PutLine(str:String);
begin
  writeln(handle, str);
end;
@}

Per quanto riguarda i settori di codice, questi devono essere
scritti in un formato particolare, che permette a Lout di formattarli
in modo corretto.

Questo e' un estratto di un esempio di codice formattato correttamente
per usarlo con Lout:

```
@LeftDisplay lines @Break {
@Sym angleleft @I @Verbatim @Begin definizione di ciao @End @Verbatim @Sym angleright @Sym equivalence
    @Verbatim @Begin while(true) { @End @Verbatim 
      @Verbatim @Begin putstrln("ciao!"); @End @Verbatim
    @Verbatim @Begin } @End @Verbatim
}
```

Il nome della definizione viene scritto fra parentesi angolate in italico
(@I). 

La testata viene scritta così:

@d TSlitOutputLout scrivi testata
@{
write(handle, '@Sym angleleft { BoldSlope } @Font @Verbatim @Begin ');
write(handle, nome, ' @End @Verbatim ');

write(handle, '@I {', currentMacro.macroProgr, ' } ');

writeln(handle, ' @Sym angleright @Sym equivalence');
@}

Per ogni testata viene scritto un tag che serve per generare i link che portano
alla definizione della macro.

Ad ogni tag corrisponde un nome che non deve avere caratteri che non siano
alfanumerici oppure underscore. 

Per questo motivo viene usato, come tag, il progressivo della macro.

@d TSlitOutputLout scrivi tag
@{
writeln(handle, '@PageMark { ', currentMacro.macroProgr, ' } ');
@}

Il codice scritto dall'utente viene inserito fra blocchi
verbatim ovvero fra "@Verbatim @Begin" e "@End @Verbatim".

Tutto il codice e' racchiuso fra "@LeftDisplay lines @Break", che permette
di rendere l'indentazione significativa.

Il codice viene prima diviso in linee e gli spazi che vengono prima del
primo scritto vengono isolati dal codice perche' sono significativi per
la versione:

@d TSlitOutputLout scrivi codice
@{
stringhe := TStringList.Create;
stringhe.Text := contenuto;

for i := 0 to stringhe.Count-1 do
begin
  @<TSlitOutputLout.ScriviScrap processa linea@>
end;

FreeAndNil(stringhe);
@}

Per ogni linea vengono separati gli spazi iniziali dal resto della stringa
e gli spazi iniziali vengono inseriti prima del blocchi verbatim.

Se la riga che si va a scrivere e' un riferimento ad una macro
allora viene evidenziata in grassetto e viene inserito un link alla
definizione della macro.

@d TSlitOutputLout.ScriviScrap processa linea
@{
spazi := Length( stringhe.Strings[i] );
spazi := spazi - Length(TrimLeft(stringhe.Strings[i]));
for j := 1 to spazi do
begin
  write(handle, ' ');
end;
write(handle, '   ');

stringaPulita := Trim(stringhe.Strings[i]);
if AnsiStartsStr('@<', stringaPulita) and
   AnsiEndsStr('@>', stringaPulita) then
begin
  nomeDefinizione := MidStr(stringaPulita, 3, Length(stringaPulita)-4);
  macroTemp := FStore.GetMacro( nomeDefinizione );
  write(handle, '@I { ', macroTemp.macroProgr, ' } @CrossLink ');
  stringaPulita := '<' + nomeDefinizione + ' ' + IntToStr(macroTemp.macroProgr) + '>';
end;

write(handle, '@Verbatim @Begin ');
write(handle, stringaPulita);
writeln(handle, '@End @Verbatim'); 
@}

Dopo aver scritto il codice viene scritto anche i riferimenti alle macro
che usano questa.

@d TSlitOutputLout scrivi riferimenti
@{
if currentMacro.macroUsersCount <> 0 then
begin
  write(handle, '{ -1p setsmallcaps 0.9 } @Font { ');
  write(handle, 'Usata da: ');
  for i:=0 to currentMacro.macroUsersCount-1 do
  begin
    write(handle, ' { ', currentMacro.macroUsers[i], ' } @CrossLink { ');
    write(handle, currentMacro.macroUsers[i], ' } ');
  end;
  write(handle, ' } ');
end;
@}

La scrittura di una macro è quindi riassunta in questo codice:

@d TSlitOutputLout.ScriviScrap
@{
procedure TSlitOutputLout.ScriviScrap(tipo:EMacroType; nome, contenuto:String);
var
  stringhe:TStringList;
  spazi:integer;
  stringaPulita, nomeDefinizione: String;
  i, j:integer;
  currentMacro, macroTemp: TMacroRecord;  
begin
  currentMacro := FStore.GetMacro(nome);

  @<TSlitOutputLout scrivi tag@>
  
  writeln(handle, '@LeftDisplay lines @Break {');  
  @<TSlitOutputLout scrivi testata@>
  @<TSlitOutputLout scrivi codice@>
  @<TSlitOutputLout scrivi riferimenti@>
  writeln(handle, '}');
end;
@}

Riassumendo, il codice di gestione del formato Lout e' il seguente:

@o slitlout.pas
@{
{$MODE OBJFPC}
{$H+}
unit slitlout;

interface
  uses slitoutput, macrostore;

type
  TSlitOutputLout = class(TSlitOutput)
  private
    handle:Text;
    FStore: TMacroStore;
  public
    constructor CreateForFileAndStore(fileName:String; store:TMacroStore);
    destructor Destroy; override;
    procedure PutLine(str:String); override;
    procedure ScriviScrap(tipo:EMacroType; nome, contenuto:String); override;
  end;

implementation
  uses sysutils, strutils, classes;

  @<TSlitOutputLout.CreateForFileAndStore@>
  @<TSlitOutputLout.Destroy@>
  @<TSlitOutputLout.PutLine@>
  @<TSlitOutputLout.ScriviScrap@>

end.
@}
# }}}
