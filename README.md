# uLI-daemon

Tento program slouží jako klientská aplikace k hJOP serveru k řízení hnacích
vozidel pomocí Roco multiMaus a uLI-master.

(c) Jan Horacek 2016
Licencováno pod Apache License v2.0.

## Funkce
 - řízení jízdního stupně, směru
 - možnost nouzového zastavení
 - multitrakce
 - rozlišení mezi totálnim ručním řízením (vhodné např. pro posun) a
   poloručním řízením (vhodné např. pro řízení funkcí HV v trati)
 - kontrola odpovědi serveru, resp. cetrály, na příkaz
 - spolupráce s ovladači pčipojenými k XpressNETu přímo do centrály
 - kompatibilita s vnějšími programy akceptováním argumentů

## Idea programu

uLI-daemon běží na obslužném pracovišti (pracovišti s panelem) jako daemon. Je
spuštěn se spuštěním prvního panelu, k hJOPserveru se připojuje po připojení panelu
a zůstává spuštěný do vypnutí počítače.

uLI-daemon disponuje těmito rozhraními:
 * klient socket pro připojení k hJOPserveru
 * server socket pro BridgeServer
 * virutální COM port pri připojení k uLI-master

uLI-daemon funguje takto:
 * Po spuštění vyhledá uLI-master připojená k PC. Pokud najde právě jedno zařízení,
   připojí se k němu, jinak nabídne uživateli možnost vybrat zařízení.
 * Po spuštění dojde ke spuštění BridgeServeru -- serveru, na kterém uLI-daemon
   naslouchá kontrolní příkazy.
 * hJOPpanel se připjí k BridgeServeru, pošle příkaz "připoj se", uLI-daemon
   se pokusí připojit k hJOPserveru a autorizovat se.
 * Po úspěšné autorizaci uLI-daemon zapne napájení Rocomaus a je připraven přijímat
   lokomotivy pro řízení.
 * Lokomotivu lze do příslušného slotu uLI-daemona přiřadit pomocí příkazu
   poslaného do BridgeServeru.

## Limity

uLI-daemon podporuje až 6 slotů pro lokomotivy. Sloty se číslují 1..6.

## Argumenty
 * "-u" username
 * "-p" password
 * "-s" server (ip/dns)
 * "-pt" port

 např.
   uLI-daemon.exe -u root -p heslo -s server-tt -p 1234

 Port je nepovinný argument, ostatní argumenty jsou povinné. Pokud jsou předány
 povinné argumenty, uLI-daemon se pokusí připojit k serveru. Předávání argument§
 aplikaci je zamýšleno především pro DEBUG, v reálném nasazení uLI-daemon získává
 autorizaci od hJOPpanelu.
