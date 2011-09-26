# :mode=slitasm:folding=explicit:
@SysInclude { tbl }
@Include { report }
@Report 
  @Title { Gestione delle attività }
  @Author { Leonardo Cecchi }
//

# Motivazioni e analisi {{{
@Section
@Title { Motivazioni e analisi }
@Begin @PP

# 130 é
# 138 è
# 149 ò

Questo programma è stato scritto per tenere traccia dei tempi di lavoro
nei vari progetti. Non ha nessun requisito preciso né è stato pensato per
essere usato da alcuno che non sia io. @PP

Per provare un nuovo assemblatore la cui sintassi è compatibile a quella
del MASM della Microsoft ma che può essere utilizzato anche sotto
Linux ho deciso di scrivere questo programma direttamente in Assembly. @PP

I dati riguardanti i progetti vengono memorizzati in un database PostgreSQL
in due tabelle che hanno il seguente schema: @PP

@Display @F @Verbatim {
CREATE TABLE progetti
(
  codice serial NOT NULL,
  nome character varying(50) NOT NULL,
  CONSTRAINT progetti_pkey PRIMARY KEY (codice)
)

CREATE TABLE tempo
(
  id serial NOT NULL,
  codice_commessa integer NOT NULL,
  inizio timestamp without time zone NOT NULL,
  fine timestamp without time zone NOT NULL,
  CONSTRAINT tempo_pkey PRIMARY KEY (id),
  CONSTRAINT fk_tempo_commessa FOREIGN KEY (codice_commessa)
      REFERENCES progetti (codice) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE
)
}

La tabella dei progetti memorizza le varie commesse attive, e la 
tabella dei tempi memorizza i tempi lavorati per ogni commessa. L'inserimento
della chiave primaria all'interno di questa tabella è automatico, infatti il
tipo del campo è un @I {serial}. @PP 

L'interfaccia grafica viene creata con le librerie GTK data la loro presenza
sia su Linux che su Windows e la loro interfaccia C che le rende semplici
da essere utilizzate direttamente in Assembly. @PP

@End @Section
# }}}

# Prototipi {{{
@Section
@Title { Prototipi }
@Begin

Per chiamare correttamente le funzioni delle varie librerie sono stati 
scritti dei prototipi che permettono all'assemblatore di verificare il
numero dei parametri passati ad ogni funzione. @PP

Questi prototipi, per le funzioni GTK, sono elencati qua di seguito:

@d prototipi GTK
@{
GTK_BUTTONS_NONE equ 0
GTK_BUTTONS_OK equ 1
GTK_BUTTONS_CLOSE equ 2
GTK_BUTTONS_CANCEL equ 3
GTK_BUTTONS_YES_NO equ 4
GTK_BUTTONS_OK_CANCEL equ 5

GTK_DIALOG_MODAL equ 1
GTK_DIALOG_DESTROY_WITH_PARENT equ 2
GTK_DIALOG_NO_SEPARATOR equ 4

GTK_MESSAGE_INFO equ 0
GTK_MESSAGE_WARNING equ 1
GTK_MESSAGE_QUESTION equ 2
GTK_MESSAGE_ERROR equ 3
GTK_MESSAGE_OTHER equ 4

GTK_TEXT_ITER_SIZE equ 56

GTK_WINDOW_TOPLEVEL equ 0
GTK_WINDOW_POPUP equ 1

G_TIMEVAL_SIZE equ 8

g_date_free proto c date:dword
g_date_new proto c
g_date_set_time_val proto c date:dword, timeval:dword
g_date_strftime proto c s:dword, slen:dword, format:dword, date:dword
g_free proto c mem:dword
g_get_current_time proto c result:dword
g_signal_connect_data proto c :dword, :dword, :dword, :dword, :dword, :dword
g_snprintf proto c string:dword, count:dword, format:dword, altri:vararg 
g_sprintf proto c string:dword, format:dword, altri:vararg
g_time_val_from_iso8601 proto c iso_date:dword, time:dword
g_time_val_to_iso8601 proto c time:dword
gtk_box_pack_start proto c box:dword, child:dword, expand:dword, fill:dword, padding:dword
gtk_button_new_with_label proto c :dword
gtk_combo_box_append_text proto c combo_box:dword, text:dword
gtk_combo_box_new_text proto c
gtk_combo_box_get_active_text proto c combo_box:dword
gtk_container_add proto c :dword, :dword
gtk_container_set_border_width proto c :dword, :dword
gtk_dialog_run proto c dialog:dword
gtk_entry_get_text proto c entry:dword
gtk_entry_new proto c
gtk_entry_set_text proto c entry:dword, text:dword
gtk_hbox_new proto c :dword, :dword
gtk_init proto c :dword, :dword
gtk_label_new proto c :dword
gtk_main proto c
gtk_main_quit proto c
gtk_message_dialog_new proto c parent:dword, flags:dword, iType:dword, \
  buttons:dword, message_format:dword, others:vararg
gtk_scrolled_window_new proto c hadjustment:dword, vadjustment:dword
gtk_text_buffer_get_end_iter proto c buffer:dword, iter:dword
gtk_text_buffer_insert proto c buffer:dword, iter:dword, text:dword, len:dword
gtk_text_view_get_buffer proto c text_view:dword
gtk_text_view_new proto c
gtk_vbox_new proto c :dword, :dword
gtk_widget_destroy proto c handle:dword
gtk_widget_show_all proto c :dword
gtk_window_new proto c :dword
gtk_window_set_title proto c :dword, :dword
@}

Invece questi sono quelli utilizzati per le librerie PQ che servono per
interfacciarsi a PostgreSQL.

@d prototipi PQ
@{
CONNECTION_OK equ 0
CONNECTION_BAD equ 1

PGRES_EMPTY_QUERY equ 0
PGRES_COMMAND_OK equ 1
PGRES_TUPLES_OK equ 2

PQclear proto c conn:dword
PQconnectdb proto c conninfo:dword
PQerrorMessage proto c conn:dword
PQexec proto c conn:dword, sql:dword
PQfinish proto c conn:dword
PQgetvalue proto c res:dword, row_number:dword, col_number:dword 
PQntuples proto c res:dword
PQnfields proto c res:dword
PQresultErrorMessage proto c res:dword
PQresultStatus proto c res:dword
PQstatus proto c conn:dword       
@}

Inoltre a questo ci sono alcune funzioni delle librerie runtime del linguaggio
C che sono molto utili. I prototipi delle funzioni utilizzate sono inclusi
qua:

@d prototipi C
@{
puts proto c string:dword
@}

@End @Section
# }}}

# Funzioni di utilità {{{
@Section
@Title { Funzioni di utilità }
@Begin @PP

Le librerie GTK hanno alcune funzionalità che sono implementate come delle
macro ma sono comunque molto utili. La prima è la @F {g_signal_connect} che 
permette di connettere una procedura ad un evento di un componente grafico. @PP

La @F {g_signal_connect} è utile principalmente perché la relativa chiamata
"vera", ovvero la @F {g_signal_connect_data}, ha un mare di parametri che
sono difficili da ricordare. @PP

@d g_signal_connect
@{
g_signal_connect proc c instance:dword, signal:dword, handler:dword, data:dword
  invoke g_signal_connect_data, instance, signal, handler, data, 0, 0
  ret
g_signal_connect endp
@}

Questa funzione invece crea un semplice messaggio di informazioni
modale:

@d msgbox
@{
msgbox proc c sCaption:dword
  local dlgId:dword
  
  invoke gtk_message_dialog_new, 0, GTK_DIALOG_MODAL, GTK_MESSAGE_INFO, \
    GTK_BUTTONS_OK, sCaption
  mov dlgId, eax
  invoke gtk_dialog_run, eax
  invoke gtk_widget_destroy, dlgId
  
  ret
msgbox endp
@}

Le seguenti funzioni invece hanno a che fare con le librerie PQ e con la 
chiusura delle connessioni. Sono essenzialmente delle versioni null-save
delle relative funzioni delle librerie PQ. Quando il parametro è nullo non
fanno niente. 

@d safePQfinish
@{
safePQfinish proc c connDb:dword
  .if connDb != 0
    invoke PQfinish, connDb
  .endif
  ret
safePQfinish endp
@}

@d safePQclear
@{
safePQclear proc c res:dword
  .if res != 0
    invoke PQclear, res
  .endif
  ret
safePQclear endp
@}

#gtk_text_iter_new proc
#  invoke malloc, GTK_TEXT_ITER_SIZE
#  ret
#gtk_text_iter_new endp
 
@End @Section
# }}}

# La maschera di inserimento dei dati {{{
@Section
@Title { Maschera di inserimento dei dati }
@Begin @PP

Per creare la maschera di inserimento dei dati vengono utilizzate le API
delle GTK. La prima riga della finestra presenta una casella di scelta
che permette di selezionare fra i vari progetti esistenti. La seconda e la
terza riga invece permettono di inserire la data di inizio e la data di fine
delle ore lavorate. Le righe relative alle date devono avere 
accanto un bottone per l'inserimento
della data e dell'ora corrente. @PP

Dopo aver inizializzato la maschera viene connesso il database e riempita
la combobox relativa alla lista dei progetti. @PP

@d creaMascheraInserimento, dati
@{
sigDestroyLbl db "destroy", 0
sigClickedLbl db "clicked", 0

wndTitleLbl db "Gestione dei tempi", 0
lbProgetto db "Progetto:", 0
lbDataInizio db "Data Inizio:", 0
lbDataFine db "Data Fine:", 0
btSalvaLbl db "Salva e Esci", 0
btAnnullaLbl db "Annulla", 0
btInizioLbl db "Adesso", 0
btFineLbl db "Adesso", 0
comboProgetto dd 0
txtInizio dd 0
txtFine dd 0
@}

@d creaMascheraInserimento
@{
creaMascheraInserimento proc c
  local wnd:dword, boxProgetto:dword, boxInizio:dword, boxFine:dword, \
    boxBottoni:dword, boxGlobale:dword, btSalva:dword, btAnnulla:dword, \
    btInizio:dword, btFine:dword
  
  @<creaMascheraInserimento, creazione componenti@>
  
  @<creaMascheraInserimento, inizializzazione gerarchia@>
  
  @<creaMascheraInserimento, eventi@>
  
  invoke inizializzaComboBox
  ret
creaMascheraInserimento endp
@}

Prima di tutto vengono creati i componenti della maschera:

@d creaMascheraInserimento, creazione componenti
@{
invoke gtk_window_new, GTK_WINDOW_TOPLEVEL 
mov wnd, eax
invoke gtk_hbox_new, 0, 5
mov boxProgetto, eax
invoke gtk_combo_box_new_text
mov comboProgetto, eax
invoke gtk_hbox_new, 0, 5
mov boxInizio, eax
invoke gtk_hbox_new, 0, 5
mov boxFine, eax
invoke gtk_hbox_new, 0, 5
mov boxBottoni, eax
invoke gtk_vbox_new, 0, 5
mov boxGlobale, eax
invoke gtk_entry_new
mov txtInizio, eax
invoke gtk_entry_new
mov txtFine, eax
invoke gtk_button_new_with_label, offset btSalvaLbl
mov btSalva, eax
invoke gtk_button_new_with_label, offset btAnnullaLbl
mov btAnnulla, eax
invoke gtk_button_new_with_label, offset btInizioLbl
mov btInizio, eax
invoke gtk_button_new_with_label, offset btFineLbl
mov btFine, eax
@}

Poi i componenti vengono inizializzati in modo da assumere la gerarchia
finale:

@d creaMascheraInserimento, inizializzazione gerarchia
@{
invoke gtk_label_new, offset lbProgetto
invoke gtk_box_pack_start, boxProgetto, eax, 0, 0, 0
invoke gtk_box_pack_start, boxProgetto, comboProgetto, 1, 1, 0

invoke gtk_label_new, offset lbDataInizio
invoke gtk_box_pack_start, boxInizio, eax, 0, 0, 0
invoke gtk_box_pack_start, boxInizio, txtInizio, 1, 1, 0
invoke gtk_box_pack_start, boxInizio, btInizio, 0, 0, 0

invoke gtk_label_new, offset lbDataFine
invoke gtk_box_pack_start, boxFine, eax, 0, 0, 0
invoke gtk_box_pack_start, boxFine, txtFine, 1, 1, 0
invoke gtk_box_pack_start, boxFine, btFine, 0, 0, 0

invoke gtk_box_pack_start, boxBottoni, btSalva, 0, 0, 0
invoke gtk_box_pack_start, boxBottoni, btAnnulla, 0, 0, 0

invoke gtk_box_pack_start, boxGlobale, boxProgetto, 0, 0, 0
invoke gtk_box_pack_start, boxGlobale, boxInizio, 0, 0, 0
invoke gtk_box_pack_start, boxGlobale, boxFine, 0, 0, 5
invoke gtk_box_pack_start, boxGlobale, boxBottoni, 0, 0, 5

invoke gtk_window_set_title, wnd, offset wndTitleLbl
invoke gtk_container_set_border_width, wnd, 5
invoke gtk_container_add, wnd, boxGlobale
invoke gtk_widget_show_all, wnd
@}

Questo pezzo di codice si preoccupa di agganciare gli eventi secondo
questa tabella: @PP

@Display @Tbl 
  rule { yes }
  aformat { @Cell @B A | @Cell @B B }
  bformat { @Cell A | @Cell B }
{
@Rowa  
  A { Evento }
  B { Procedura da chiamare }
@Rowb
  A { Chisura della finestra e pulsante annulla }
  B { Chiamata @F {onWndDestroy}}
@Rowb
  A { Pulsante data di inizio e data di fine }
  B { Chiamata @F {onBtDataClick}}
@Rowb
  A { Pulsante ok }
  B { Chiamata @F {onBtOkClick}}
  
}

Il seguente codice aggancia gli eventi:

@d creaMascheraInserimento, eventi
@{
invoke g_signal_connect, wnd, offset sigDestroyLbl, onWndDestroy, 0
invoke g_signal_connect, btAnnulla, offset sigClickedLbl, onWndDestroy, 0
invoke g_signal_connect, btInizio, offset sigClickedLbl, onBtDataClick, txtInizio
invoke g_signal_connect, btFine, offset sigClickedLbl, onBtDataClick, txtFine
invoke g_signal_connect, btSalva, offset sigClickedLbl, onBtOkClick, 0
@}

@BeginSubSections

@SubSection
@Title { Chiusura della finestra e pulsante annulla }
@Begin @PP

Per chiudere la finestra viene semplicemente terminato il ciclo degli eventi:

@d onWndDestroy
@{
onWndDestroy proc c
  invoke gtk_main_quit

  ret
onWndDestroy endp
@}

@End @SubSection

@SubSection
@Title { Date di default }
@Begin @PP

Queste funzioni inseriscono nella casella di testo che viene passata
come argomento la data di default in un formato buono per PostgreSQL. @PP

@d onBtDataClick, dati
@{
strFormatoData db "%Y-%m-%d %H:%M:%S", 0
@}

@d onBtDataClick
@{
onBtDataClick proc c wid:dword, txt:dword
  local newString:dword, timeval[G_TIMEVAL_SIZE]:byte
  
  invoke g_get_current_time, addr timeval
  invoke g_time_val_to_iso8601, addr timeval
  mov newString, eax   

  invoke gtk_entry_set_text, txt, newString
  invoke g_free, newString
  
  ret
onBtDataClick endp
@}

@End @SubSection

@SubSection
@Title { Inserimento }
@Begin @PP

Questa funzionalità viene invocata con il pulsante "Ok" e è composta di due 
fasi: il controllo dell'input dell'utente e il richiamo della query di
inserimento. @PP

@d onBtOkClick, dati
@{
sMessaggioErroreInizio db "Data e ora di inizio errate.", 0 
sMessaggioErroreFine db "Data e ora di fine errate.", 0
sMessaggioErroreProgetto db "Scegliere un progetto.", 0
@}

@d onBtOkClick
@{
onBtOkClick proc c
  local timeval[G_TIMEVAL_SIZE]:byte, sDataInizio:dword, sDataFine:dword, \
    sNewProgetto:dword
    
  mov sNewProgetto, 0
  
  @<onBtOkClick, controllo@>
  
  @<onBtOkClick, estrai id progetto@>
  
  @<onBtOkClick, inserimento@>
  
  invoke gtk_main_quit
  
onBtOkClick_ret:
  .if sNewProgetto != 0
    invoke g_free, sNewProgetto
  .endif
  
  ret
onBtOkClick endp
@}

Il controllo sfrutta la chiamata @F {g_time_val_from_iso8601}, che ritorna
TRUE se la stringa passata è nel formato ISO8601. @PP

@d onBtOkClick, controllo
@{
invoke gtk_entry_get_text, txtInizio
mov sDataInizio, eax
invoke g_time_val_from_iso8601, sDataInizio, addr timeval
.if !eax
  invoke msgbox, offset sMessaggioErroreInizio
  jmp onBtOkClick_ret
.endif

invoke gtk_entry_get_text, txtFine
mov sDataFine, eax
invoke g_time_val_from_iso8601, sDataFine, addr timeval
.if !eax
  invoke msgbox, offset sMessaggioErroreFine
  jmp onBtOkClick_ret
.endif

invoke gtk_combo_box_get_active_text, comboProgetto
mov sNewProgetto, eax

.if eax == 0
  invoke msgbox, offset sMessaggioErroreProgetto
  jmp onBtOkClick_ret
.endif
@}

Per estrarre il nome del progetto il primo spazio trovato viene
sostituito da un carattere di fine stringa @F {00}.

@d onBtOkClick, estrai id progetto
@{
.while byte [eax] != 32
  inc eax
.endw
mov byte [eax], 0
@}

A questo punto può essere invocata la procedura di inserimento del nuovo
record:

@d onBtOkClick, inserimento
@{
invoke inserisciNuovaRiga, sDataInizio, sDataFine, sNewProgetto
@}

@End @SubSection

@EndSubSections

@End @Section
# }}}

# Lista dei progetti {{{
@Section
@Title { Lista dei progetti }
@Begin @PP

In questa funzione è implementato il riempimenti della combobox dei progetti
con i contenuti derivanti dal database. @PP

@d inizializzaComboBox, dati
@{
sqlSelectProgetti db "SELECT codice, nome FROM progetti ORDER BY nome", 0
strFormatoCombo db "%s - %s", 0
@}

Gli errori vengono gestiti rinviando alla @F {inizializzaComboBox_ret}, che
libera le risorse se sono state allocate.

@d inizializzaComboBox
@{
inizializzaComboBox proc c
  local connDb:dword, res:dword, recNo:dword, sCodiceProgetto:dword, \ 
    sDescrizioneProgetto:dword, buffer[100]:byte
  
  mov connDb, 0
  mov res, 0
  
  @<inizializzaComboBox, query@>
  
inizializzaComboBox_ret:
  invoke safePQclear, res
  invoke safePQfinish, connDb
  ret
inizializzaComboBox endp
@}

Nella combobox vengono inserite stringhe della forma @F {<n> - <desc>}. Fra
il codice del progetto e la relativa descrizione viene inserita la stringa 
@F @Verbatim {" - "} in modo da poterli facilmente separare quando
sarà necessario inserire il record.

@d inizializzaComboBox, query
@{
invoke PQconnectdb, offset connInfoLbl
mov connDb, eax
invoke PQstatus, connDb
.if eax != CONNECTION_OK
  invoke PQerrorMessage, connDb
  invoke puts, eax
  jmp inizializzaComboBox_ret
.endif

invoke PQexec, connDb, offset sqlSelectProgetti
mov res, eax
invoke PQresultStatus, res
.if eax != PGRES_TUPLES_OK
  invoke PQresultErrorMessage, res
  invoke puts, eax
  jmp inizializzaComboBox_ret
.endif

@<inizializzaComboBox, inserimento in combo@> 
@}

Per ogni record letto viene inserita la relativa stringa
all'interno della combobox. La stringa viene prodotta con la chiamata
@F {g_snprintf}:

@d inizializzaComboBox, inserimento in combo
@{
mov recNo, 0
.while 1
  invoke PQntuples, res
  .break .if recNo >= eax
  
  invoke PQgetvalue, res, recNo, 0
  mov sCodiceProgetto, eax
  invoke PQgetvalue, res, recNo, 1
  mov sDescrizioneProgetto, eax
  
  invoke g_snprintf, addr buffer, 100, offset strFormatoCombo, sCodiceProgetto, sDescrizioneProgetto
  invoke gtk_combo_box_append_text, comboProgetto, addr buffer
  
  inc recNo
.endw
@}

@End @Section
# }}}

# Inserimento della nuova riga {{{
@Section
@Title { Inserimento della nuova riga }
@Begin @PP

La nuova riga viene inserita con una query sul database:

@d inserisciNuovaRiga, dati
@{
sSqlNuovaRiga db "INSERT INTO tempo (codice_commessa, inizio, fine) "
  db "VALUES ('%s', '%s', '%s')",0
@}

Nel costruire la query si ignorano i problemi di sicurezza derivanti
dall'avere degli accentini @F {'} all'interno dei dati. Intanto è roba
che devo usare io.

@d inserisciNuovaRiga
@{
inserisciNuovaRiga proc c sDataInizio:dword, sDataFine:dword, sIdProgetto:dword
  local connDb:dword, res:dword, buffer[1024]:byte
  
  mov connDb, 0
  mov res, 0
  
  invoke PQconnectdb, offset connInfoLbl
  mov connDb, eax
  invoke PQstatus, connDb
  .if eax != CONNECTION_OK
    invoke PQerrorMessage, connDb
    invoke puts, eax
    jmp inserisciNuovaRiga_ret
  .endif

  invoke g_snprintf, addr buffer, 1024, offset sSqlNuovaRiga, sIdProgetto, \
    sDataInizio, sDataFine
  invoke PQexec, connDb, addr buffer
  mov res, eax
  
  invoke PQresultStatus, res
  .if eax != PGRES_COMMAND_OK
    invoke PQresultErrorMessage, res
    invoke msgbox, eax
    jmp inserisciNuovaRiga_ret
  .endif
  
inserisciNuovaRiga_ret:
  invoke safePQclear, res
  invoke safePQfinish, connDb
  ret
  
  ret
inserisciNuovaRiga endp
@}

@End @Section
# }}}

# Codice {{{
@Section
@Title { Inizializzazione }
@Begin @PP

Le librerie GTK, prima di essere utilizzate, devono essere inizializzate
con la chiamata @F {gtk_init}. Dopo l'inizializzazione viene avviata
la procedura che crea la finestra di inserimento e il controllo viene
passato direttamente alle librerie GTK. @PP

@d start
@{
invoke gtk_init, 0, 0
invoke creaMascheraInserimento
invoke gtk_main
@}

@End @Section

@Section 
@Title { Codice }
@Begin @PP

@o tempi.asm
@{
  .386
  .model flat, stdcall
  
  @<prototipi GTK@>
  @<prototipi PQ@>  
  @<prototipi C@>
 
  creaMascheraInserimento proto c
  inizializzaComboBox proto c  
  inserisciNuovaRiga proto c sDataInizio:dword, sDataFine:dword, sIdProgetto:dword
  onWndDestroy proto c
  onBtOkClick proto c
  
  g_signal_connect proto c instance:dword, signal:dword, handler:dword, data:dword
  msgbox proto c sCaption:dword
  safePQfinish proto c connDb:dword
  safePQclear proto c res:dword
  
  public start

  .const
  
  connInfoLbl db "dbname=commesse", 0
  sNomeApplicazione db "Gestione Tempi", 0
  
  .data
  
  @<creaMascheraInserimento, dati@>
  
  @<inizializzaComboBox, dati@>
  
  @<inserisciNuovaRiga, dati@>
  
  @<onBtDataClick, dati@>

  @<onBtOkClick, dati@>
  
  .code

start:  
  @<start@>
  ret

  @<creaMascheraInserimento@>
  
  @<g_signal_connect@>
  
  @<msgbox@>
  
  @<onWndDestroy@>
  
  @<onBtDataClick@>
  
  @<onBtOkClick@>
  
  @<safePQfinish@>
  
  @<safePQclear@>
  
  @<inizializzaComboBox@>
  
  @<inserisciNuovaRiga@>

  end
@}

@End @Section
# }}}
