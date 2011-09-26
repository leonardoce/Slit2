@Introduction
@Title { Introduzione }
@Begin @LP

Questo vuole essere un piccolo editor orientato alle righe ispirato a due
editor dalla lunga storia: @F ed di unix e @F xedit di VMS, che ho potuto
conoscere grazie alla implementazione di Hessling (THE - The Hessling
Editor). @PP

L'editor è implementato in Python per motivi di velocità di scrittura e
per supportare il più ampio numero di piattaforme possibili. Stò scrivendo
direttamente con questo editor e ho incominciato a scrivere l'editor stesso
stamani mattina :-) @PP

Inoltre a questo utilizzare direttamente un linguaggio di scripting
rende l'editor più espandibile che utilizzare un linguaggio
compilato. E' più semplice scrivere un programma in Python e
agganciarlo di qua che fornire una interfaccia via delle chiamate
DLL.... eccetera eccetera.... @PP

Le funzionalità sono volutamente ridotte ma nonostante questo io trovo
questo editor utile o perlomeno molto interessante. Infatti il grande uso
delle regexp, dei marks e dei comandi da tastiera lo rendono comodo da
utilizzare anche con le sue limitazioni. @PP

Questo editor non supporta in alcun modo il carattere di tabulazione e
questo è un comportamento voluto: infatti ho pensato l'editor per i
miei codici sorgenti e io non uso spesso i caratteri di tabulazione. Questo
mi permette di avere codici che vengono visualizzati allo stesso modo in
tutti i sistemi. @PP

Tab o non tab? E' una domanda interessante. Rimando il lettore a una
delle tante discussioni in rete sull'argomento: ne sanno sicuramente più
di me. @PP

@End @Introduction
