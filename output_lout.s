@Section
@Title { Output in formato Lout }
@Begin @PP

LOut {@Char egrave} un sistema di typesetting di spirito simile a TeX ma di sintassi pi{@Char ugrave} semplice e pi{@Char ugrave}
facilmente programmabile. @PP

Uno dei vantaggi di LOut {@Char egrave} la programmabilit{@Char agrave} e la leggerezza.

Slit pu{@Char ograve} creare documentazione in formato LOut. @PP

I file di lout non hanno una estensione determinata. Per quanto riguarda slit l'estensione
che viene aggiunta alla documentazione @Char egrave "".lout"".

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
aggiungere istruzioni lout alla documentazione. @PP

Oltre a scrivere la linea di documentazione viene anche
scritta, in un commento, la linea di sorgente dalla quale
quella linea di documentazione {@Char egrave} derivata. @PP

L'obiettivo {@Char egrave} di semplificare da ricerca di
errori dati dal compilatore. Questo per{@Char ograve} 
deve essere fatto solamente se
non siamo dentro ad un blocco @F "@Verbatim", altrimenti il commento
riguardante la riga corrente verrebbe inserito all'interno del documento
proprio come se fosse testo. 

Identificare se siamo dentro ad un blocco @B verbatim {@Char egrave} un
problema che potrebbe sembrare di semplice soluzione ma in realt{@Char agrave},
per farlo in modo esatto, bisognerebbe procedere almeno ad una analisi
lessicale della stringa dal punto di vista Lout. 

Per Slit {@Char egrave} stato scelto un approccio intermedio
che permette di avere contemporaneamente la comodit{@Char agrave} dei
commenti che indicano il file sorgente di provenienza e che non
modificano l'output finale. @PP

Il commento che enuncia lo stato corrente di lettura viene inserito
solamente se la linea di documentazione inizia per @F "@Section": in
questo modo siamo ragionevolmente sicuri che la riga non apparterr{@Char agrave}
a un blocco @B verbatim. La stessa cosa viene fatta per l'inizio di un 
capitolo e dell'introduzione:

@d TSlitOutputLout.PutLine
@{
procedure TSlitOutputLout.PutLine(str:String);
var
  tempStr : String;

begin
  tempStr := Trim(str);
  if (AnsiStartsStr('@Section', tempStr)) or
     (AnsiStartsStr('@Chapter', tempStr)) or
     (AnsiStartsStr('@Introduction', tempStr)) then
  begin
    writeln (handle, '# ', GetCurrentParsingFile(), ':',
      GetCurrentParsingLine() );
  end;

  writeln(handle, str);
end;
@}

Per quanto riguarda i settori di codice, questi devono essere
scritti in un formato particolare, che permette a Lout di formattarli
in modo corretto. @PP

Questo {@Char egrave} un estratto di un esempio di codice formattato correttamente
per usarlo con Lout: @PP

@F @Verbatim {
@LeftDisplay lines @Break {
@Sym angleleft @I @Verbatim @Begin definizione di ciao @End @Verbatim @Sym angleright @Sym equivalence
    @Verbatim @Begin while(true) { @End @Verbatim 
      @Verbatim @Begin putstrln("ciao!"); @End @Verbatim
    @Verbatim @Begin } @End @Verbatim
}
}

Il nome della definizione viene scritto fra parentesi angolate in italico
@Verbatim {(@I)}. @PP 

La testata viene scritta cos{@Char igrave}:

@d TSlitOutputLout scrivi testata
@{
write(handle, '@Sym angleleft { BoldSlope } @Font @','Verbatim @Begin ');
if tipo=FileScrap then
begin
  write(handle, 'FILE ');
end;
write(handle, nome, ' @','End @','Verbatim ');

write(handle, '@I {', currentMacro.macroProgr, ' } ');

write(handle, ' @Sym angleright'); 
if tipo=AppendScrap then
begin
  write(handle, ' @Sym plus');
end;
writeln(handle, ' @Sym equivalence');

@}

Per ogni testata viene scritto un tag che serve per generare i link che portano
alla definizione della macro. @PP

Ad ogni tag corrisponde un nome che non deve avere caratteri che non siano
alfanumerici oppure underscore. @PP

Per questo motivo viene usato, come tag, il progressivo della macro. @PP

@d TSlitOutputLout scrivi tag
@{
writeln(handle, '@PageMark { ', currentMacro.macroProgr, ' } ');
@}

Il codice scritto dall'utente viene inserito fra blocchi
verbatim ovvero fra @Verbatim { "@Verbatim @Begin" } e @Verbatim { "@End @Verbatim" }. @PP 

Tutto il codice {@Char egrave} racchiuso fra @Verbatim { "@LeftDisplay lines @Break" }, che permette
di rendere l'indentazione significativa. @PP

Il codice viene prima diviso in linee e gli spazi che vengono prima del
primo scritto vengono isolati dal codice perch{@Char eacute} sono significativi per
la versione: @PP

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
e gli spazi iniziali vengono inseriti prima del blocchi verbatim. @PP

Se la riga che si va a scrivere {@Char egrave} un riferimento ad una macro
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
  if macroTemp<>Nil then
  begin
    write(handle, '@I { ', macroTemp.macroProgr, ' } @CrossLink ');
    stringaPulita := '<' + nomeDefinizione + ' ' + IntToStr(macroTemp.macroProgr) + '>';
  end;
end;

write(handle, '@','Verbatim @','Begin ');
write(handle, stringaPulita);
writeln(handle, '@','End @','Verbatim'); 
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

La scrittura di una macro {@Char egrave} quindi riassunta in questo codice:

@d TSlitOutputLout.ScriviScrap
@{
procedure TSlitOutputLout.ScriviScrap(tipo:EScrapType; nome, contenuto:String);
var
  stringhe:TStringList;
  spazi:integer;
  stringaPulita, nomeDefinizione: String;
  i, j:integer;
  currentMacro, macroTemp: TMacroRecord;  
begin
  currentMacro := FStore.GetMacro(nome);

  if currentMacro<>Nil then
  begin
    @<TSlitOutputLout scrivi tag@>
    
    writeln(handle, '@LeftDisplay lines @Break {');  
    @<TSlitOutputLout scrivi testata@>
    @<TSlitOutputLout scrivi codice@>
    @<TSlitOutputLout scrivi riferimenti@>
    writeln(handle, '}');
  end;
end;
@}

Riassumendo, il codice di gestione del formato Lout {@Char egrave} il seguente:

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
    procedure ScriviScrap(tipo:EScrapType; nome, contenuto:String); override;
  end;

implementation
  uses sysutils, strutils, classes, slitstatus;

  @<TSlitOutputLout.CreateForFileAndStore@>
  @<TSlitOutputLout.Destroy@>
  @<TSlitOutputLout.PutLine@>
  @<TSlitOutputLout.ScriviScrap@>

end.
@}
@End @Section
