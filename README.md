uLI-daemon
==========

uLI-daemon je klientská aplikace hJOPserveru, umožňuje ruční řízení 
vozidel pomocí Roco multiMAUS a uLI-master.

Vytvořil Jan Malina (ex Horáček).

Licencováno pod Apache License v2.0.

## Funkce

 * Řízení jízdního stupně, směru.
 * Možnost nouzového zastavení vozidla.
 * Multitrakce.
 * Rozlišení mezi ručním řízením a ovládáním pouze funkcí.
 * Spolupráce s ovladači Roco multiMAUS připojenými k uLI-master.
 * Až 6 samostatných ovladačů = slotů adres 1–6.
 * Zobrazení seznamu slotů, rychlosti v km/h, vozidel ve slotech v GUI.
 * Předání do slotu přímo z hJOPpanelu.
 * Uvolnění loko ze slotu pomocí tlačítka nebo Shift+STOP na multiMAUS.

## Idea programu

uLI-daemon běží na obslužném PC výpravčího (PC s hJOPpanelem) jako daemon. Je
spuštěn se spuštěním prvního panelu, k hJOPserveru se připojuje po připojení panelu,
zůstává spuštěný do vypnutí počítače.

uLI-daemon disponuje rozhraními:
 1. klient k hJOPserveru,
 2. BridgeServer,
 3. připojení k uLI-master pomocí virtuálního sériového portu.

Princip funkce aplikace:
 * Po spuštění vyhledá uLI-master připojená k PC. Pokud najde právě jedno zařízení,
   připojí se k němu, jinak nabídne uživateli možnost vybrat zařízení.
 * Po spuštění dojde ke spuštění BridgeServeru – serveru, na kterém uLI-daemon
   naslouchá kontrolní příkazy z hJOPpanelu.
 * hJOPpanel se připojí k BridgeServeru, pošle příkaz "připoj se" s adresou a portem hJOPserveru a přihlašovacími údaji,
   uLI-daemon se připojí k hJOPserveru, autorizuje.
 * Po úspěšné autorizaci uLI-daemon zapne napájení ovladačů a je připraven přijímat
   vozidla pro řízení.
 * Vozidlo lze do slotu uLI-daemona přiřadit pomocí příkazu poslaného z hJOPpanelu do BridgeServeru.

## Argumenty

 * `-u` username
 * `-p` password
 * `-s` server (ip/dns)
 * `-pt` port
 * `-l` zobrazit logovací okno

Příklad:
```
uLI-daemon.exe -u root -p heslo -s server-tt -p 1234
```

Pokud je předáno `-u`, `-p` a `-s`, uLI-daemon se pokusí připojit k zadanému serveru.
Předávání argumentů aplikaci je zamýšleno především pro vývoj,
v produkčním nasazení uLI-daemon získává data z hJOPpanelu.

## Specialni komponenty

- [JEDI Code Library](http://wiki.delphi-jedi.org/index.php?title=JEDI_Code_Library)