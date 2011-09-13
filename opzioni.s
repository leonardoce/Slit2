@Chapter @Title { Gestione delle opzioni }
@Begin @LP

Slit ha un insieme di parametri di configurazione che permette all'utente
di adattare il motore di traduzione in base alle sue preferenze. @PP

Questo insieme di parametri, chiamati @I opzioni, viene gestito da una
unit apposita, che gestisce tutti i dati e fornisce delle operazioni
di alto livello che operano su questi. @PP

@BeginSections

@Section @Title { Motore di output della documentazione }
@Begin @PP

Slit deve scrivere, all'interno della documentazione del codice, il
contenuto delle varie macro presenti all'interno del documento. Per
questa ragione deve sapere qual @Char egrave il formato della documentazione.
@PP

Il valore di questa opzione memorizza proprio questo formato, in modo che
possa essere creata la corrispondente implementazione del driver per la
generazione della documentazione. @PP

@d slitopzioni, gestore del processore di documentazione
@{
function GetNomeProcessoreInformazioni():String;
begin
  Result := NomeProcessoreInformazioni;
end;

procedure SetNomeProcessoreInformazioni(value:String);
begin
  if value='html' then
  begin
    NomeProcessoreInformazioni := 'html';
  end
  else if value='lout' then
  begin
    NomeProcessoreInformazioni := 'lout';
  end
  else if value='txt' then
  begin
    NomeProcessoreInformazioni := 'txt';
  end
  else
  begin
    raise Exception.Create('Nome processore informazioni non conosciuto: ' +
      value);
  end;
end;
@}

Visto che adesso sappiamo qual @Char egrave il processore delle informazioni
da utilizzare @Char egrave possibile anche creare una funzione che crea
il file di output per slit.

@d slitopzioni, crea lo stream di output
@{
function CreaStreamOutputDaOpzioni(NomeFile:String; store:TMacroStore):TSlitOutput;
begin
  if NomeProcessoreInformazioni='lout' then
  begin
    Result := TSlitOutputLout.CreateForFileAndStore (NomeFile, store);
  end
  else if NomeProcessoreInformazioni='txt' then
  begin
    Result := TSlitOutputTxt.CreateForFile (NomeFile);
  end
  else if NomeProcessoreInformazioni='html' then
  begin
    Result := TSlitOutputHtml.CreateForFile (NomeFile);
  end
  else
  begin
    Result := Nil;
  end;
end;
@}

@End @Section

@Section @Title { Definizione della unit slitopzioni }
@Begin @PP

@o slitopzioni.pas
@{
{$MODE OBJFPC}
{$H+}
unit slitopzioni;

interface

uses slitoutput, macrostore;
                                         
function GetNomeProcessoreInformazioni():String;
procedure SetNomeProcessoreInformazioni(value:String);
function CreaStreamOutputDaOpzioni(NomeFile:String; store:TMacroStore):TSlitOutput;

implementation

uses sysutils, slithtml, slitlout, slittxt;

var
  NomeProcessoreInformazioni:String;

@<slitopzioni, gestore del processore di documentazione@>
@<slitopzioni, crea lo stream di output@>

initialization

NomeProcessoreInformazioni := 'lout';
end.
@}
@End @Section

@EndSections

@End @Chapter
