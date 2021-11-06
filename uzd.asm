.MODEL small         ; atminties modelis
.STACK 100h          ; steko dydis
.DATA                ; duomenu segmentas


message db 'Enter text:',13,10,'$'

buffer db 17, ?, 17 dup(?)	
	
enter db 13, 10, '$'

sum dw 0
		

.CODE                ; kodo segmentas

strt:
mov ax,@data         ; ds registro inicializavimas
mov ds, ax           ;

mov dx, offset message  ;
mov ah, 9				; zinutes isspausdinimas
int 21h					;

mov dx, offset buffer
mov ah, 0ah			 ; nuskaitome bufferi
int 21h


mov bx, 0
mov cx, 0               ;pakeiciame cx registro verte i nuli
mov cl, buffer [1]      ;issaugo ivestu simboliu skaiciu
mov si, 1               ;indeksas bus naudojamas masyvo reiksmems rasti 


ciklas:
inc si                       ; pastumiame masyvo reiksme per viena   
shl sum, 1                   ;dauginame sumatoriu
mov bl, buffer[si]
sub bl, '0'                  ;keiciame is simbolio i desimtaini
add sum, bx
      
dec cl     ; sumazina likusiu patikrinti simboliu skaiciu
cmp cl, 0  ; patikrina ar dar liko simboliu
jne ciklas ; grizta i cikla
jmp spausdinti; jei ne, spausdiname reiksme

    
        
spausdinti:
mov dx, offset enter   ;
mov ah, 9              ; spausdiname enter
int 21h                ;

mov ax, sum            ;
mov cx, 0              ;  nustatome reikiamus rodiklius i registrus
mov dx, 0              ;
jmp paruosti

paruosti:
        
        cmp ax, 0      ;tikriname ar kintamasis dar turi reiksme
        je pradeti     ; jei ne, spausdiname
        
        mov bx, 10     
        
        div bx         ; daliname kintamaji(ax) is 10
        
        push dx        ; issaugome dx reiksme
        
        inc cx         ; skaiciuojame kiek kartu ivyko dalyba (skaitmenu skaicius)
        
        xor dx, dx     ; liekana = 0 
        jmp paruosti
        
pradeti:
   
        cmp cx,0       ; tikriname ar skaitmenu skaicius != 0
        je pabaiga     ; jei taip, baigiame programa
         
        pop dx         ; graziname dx reiksme
         
        add dx,48      ; konvertuojame reiksme pagal ascii lentele
         

        mov ah,02h     ; spausdina skaiciu
        int 21h        ;
         
        dec cx         ; sumazinamas skaitmenu skaicius
        jmp pradeti


pabaiga:
mov ax,4C00h        ; programos darbo pabaiga
int 21h
end strt
