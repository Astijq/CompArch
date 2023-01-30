.model small
.stack 256
.DATA

help db "To execute this program you need to enter two data files and result file name.$"
message db "Bad input$"
duom1 db 100 dup(0) 
duom2 db 100 dup(0)
result db 100 dup(0)
dollar db "$"
buffer db 100 dup (0)  
buffer1 db 100 dup (0)
BufPrint db 100 dup (0)                    
handle dw 0
length1 dw 0
length2 dw 0
hex dw 0
counter dw 0

.code
start:
    mov ax, @data
    mov ds, ax

    mov bx, 82h                ;priskiriame bx pirmajai skaitymo eil. poz.
    mov si, offset duom1       ;priskiriame si, duom1 adresa
       
    cmp byte ptr es:[80h], 0   ;
    je error                   ; 
    cmp es:[82h], '?/'         ;jeigu randa ?/ spausdina pagalbos eilute
    jne cycle1                 ;
    cmp byte ptr es:[84h], 13  ;
    je PrintHelp
    jmp cycle1   
       
    cycle1:                    ; nuskaitome pirmo failo pavadinima
	cmp byte ptr es:[bx], ' ' 
    je read                    ; kai randame tarpa, skaitome sekanti
        
    mov dl, byte ptr es:[bx]      ;
	mov [si], dl                  ; priskyrimas
	
	inc bx
    inc si       
    jmp cycle1
    
    read:                         
    mov si, offset duom2      ; imame kito duomenu failo adresa
    
    inc bx
    
    cycle2:     
	cmp byte ptr es:[bx], ' '    ;jei nuskaito tarpa, skaito rezutlatu failo pav 
    je rez 
        
    mov dl, byte ptr es:[bx]    ;
	mov [si], dl                ; priskyrimas
	
	inc bx
    inc si
    jmp cycle2
    
    rez:                      
    mov si, offset result        ; imame rez failo adresa
     
    inc bx        
        
    cycle3:        
	cmp byte ptr es:[bx], 13    ;jeigu randa enter, pradeda programa
    je program     
    
	mov dl, byte ptr es:[bx]    ; priskyrimas
	mov [si], dl                ;
	
	inc bx
    inc si 
    jmp cycle3
    
    error:
    mov dx, offset message     ;bloga ivestis
    mov ah, 09h
    int 21h
    jmp exit

    PrintHelp:
    mov ah, 09h
    mov dx, offset help        ; pagalbos eilute
    int 21h
    jmp exit
               
    program:
    
;    mov ah, 09h
;    mov dx, offset duom1
;    int 21h
    
         
    mov ax, 3D00h
    mov dx,offset duom1     ;atidaryti
    int 21h
    jc error                                    
    mov [handle],ax
    mov bx, ax

    mov ah,3fh              ;nuskaityti i buf
    mov bx,handle
    mov cx,100
    lea dx,buffer
    int 21h
    jc error1 
    mov si, ax
    mov length1, si 
    
    mov ah,3eh              ;uzdaryti
    mov bx,handle
    int 21h
    jc error1
        
    call conversion         ;kvieciame funkcijas
    call copy     
    
    mov dx,offset duom2     ;atidaryti
    mov ax,3d00h
    int 21h
    jc error1                                                               
    mov handle,ax

    mov ah,3fh              ;nuskaityti i buf
    mov bx,handle
    mov cx,100
    lea dx,buffer
    int 21h      
    jc error1
    mov si, ax
    mov length2, si
         
    mov ah,3eh              ;uzdaryti
    mov bx,handle
    int 21h
    jc error1
    
    call conversion            
                       
    mov ah,3ch              ;sukurti
    mov cx,0
    lea dx,result
    int 21h
    jc error1
    mov handle,ax
        
    mov dx, offset result
    mov ax,3d02h              ; atidaryti
    int 21h
    jc error1
    mov handle,ax    
        
    call sum
    jmp exit
     
    error1:
    jmp error               ; tarpinis suolis
    
    exit:  
    mov bx,handle
    or bx, bx
    jz noclose
    
    mov ah, 3eh                ; uzdaryti
    int 21h
    
    noclose: 
    mov ax, 04c00h            ; iseiti is programos
    int 21h
    
    conversion proc
     
     cicle:
     dec si     
     cmp buffer[si], 30h     ;
     jb error1               ;
     cmp buffer[si], 3Ah     ;
     jb number               ; tikrina ar skaicius desimtainis
     cmp buffer[si], 41h     ; tikrina ar tai skaicius ar raide
     jb error1               ;
     cmp buffer[si], 47h     ;
     jb char
     jmp error
          
        number:
        sub buffer[si], 30h
        cmp si, 0            ;pakeicia reikme is ascii i skaiciu
        je CicleEnd
        jmp cicle
                 
        char:
        sub buffer[si], 37h
        cmp si, 0
        je CicleEnd          ;pakeicia reikme is ascii i skaiciu
        jmp cicle
                
      CicleEnd:        
      ret  
     conversion endp     
     
    sum proc
    dec length1        
    dec length2                                   
    mov si, length1    ; priskiriame buferiu ilgius si ir di 
    mov di, length2
    xor ax, ax
 
    calc:    
    cmp si, 65535    ;-1 ;tikrian ar nepasibaige pirmasis buferis
    je if1           ; tikrina kita buferi
    jmp true2        
                
        if1:
        cmp di, 65535    ; tikrina ar nepasibaige buferis
        je if2           ; jei baigesi, soka prideti tik kita buferi
        jmp true1        ; jei ne, soka tikrinti ax
                
        if2:
        cmp ax, 0
        je prepare      ; jei ax 0, baigia skaiciavimus ir spausdina
        jmp printAX   ; soka jei abu buferiai pasibaiage
        
            true1:
            add al, buffer[di]     ;prideda antrojo buferio dydi
            jmp ad                 ; nes pirmas pasibaige
            
            true2:
            cmp di, 65535    ; tikrina ar buferis nepasibaige
            jne trueBoth          ; jei ne sudeda abu
            add al, buffer1[si]   ; jei ne, prideda tik kito buferio reiksme
            jmp ad      
            
    trueBoth:                                    
    add al, buffer1[si]          ;prideda vieno buferio reiksme
    add al, buffer[di]           ; su kitu buferiu
    
    ad:    
        cmp si, 65535     ; tikrina ar buferis netuscias
        je skip1          ; jei tuscia nebekeicia si reiksmes
        dec si        
        
        skip1:            
        cmp di, 65535     ; tikrina ar buferis netuscias
        je skip2          ; jei tuscias nebekeicia di reiksmes
        dec di
        skip2:
             
        xor dx, dx   ;dalyba
        mov bx, 10h  ;
        div bx       ;
        push dx      ; ideda liekana i steka
        inc counter
        
        jmp calc

    prepare:
    
       mov di, counter
                                   
    print:
        dec di
        pop dx                  
        
        mov BufPrint[di], dl 
        cmp dx, 10     ; tikrina ar tai turi buti skaicius ar raide
        jb low
        jmp high
                
                low:
                add BufPrint[di], 30h    ;padaryti skaiciu
                jmp continue                
                high:           ;padaryti raide
                add BufPrint[di], 37h
                
        continue:
        
        cmp di, 0
        jne  print
                         
        next:        
        mov ah,40h              ;rasyti i faila
        mov bx,handle
        mov cx,50                 ;rasyti i buferi
        lea dx, BufPrint
        int 21h
        
        jmp finish
             
     printAX:
        
        mov hex, ax
        add hex, 30h             
        mov ah,40h              ;rasyti i faila
        mov bx,handle           
        mov cx,1       
        lea dx, hex             ; ax(keliamojo skaiciaus)
        int 21h
        xor ax, ax
        jmp calc
                
        finish:
        ret
        sum endp
    
    copy proc
        
    mov di, offset buffer1     ; pasiima tuscio buferio adresa
    mov si, 0
    run:                      ; isistato buferio
    mov al,[buffer + si]      ; atitinkama adresa i al
    mov [di], al              ; kopijuoja elemntus is vieno bufferio i kita
    inc di
    inc si
    cmp si, length1           ; tikrina su pirmojo buferio ilgiu
    jne run
     
    ret
    copy endp

end start