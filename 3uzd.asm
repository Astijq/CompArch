;---------------------------------------------------------------
; This program recognizes MOV r/m <= const
;---------------------------------------------------------------

.model small

.stack 100h

.data
    ;Is lenteles
	reg_bxsi db "BX + SI$"
	reg_bxdi db "BX + DI$"
	reg_bpsi db "BP + SI$"
	reg_bpdi db "BP + DI$"
	reg_si db "SI$" 
	reg_di db "DI$"
	reg_bp db "BP$"
	;------------------
	reg_ax db "AX$"
	reg_al db "AL$"
	reg_ah db "AH$"
	reg_bx db "BX$"
	reg_bl db "BL$"
	reg_bh db "BH$"
	reg_cx db "CX$"
	reg_cl db "CL$"
	reg_ch db "CH$"
	reg_dx db "DX$"
	reg_dl db "DL$"
	reg_dh db "DH$"
	reg_sp db "SP$"
	;------------------
    byte1 db ?
    byte2 db ?
    byte3 db ?
    byte4 db ?
    byte5 db ?
    byte6 db ?
    ;------------------
    regAX dw ?
	regBX dw ?
	regCX dw ?
	regDX dw ?
	regSP dw ?
	regBP dw ?
	regSI dw ?
	regDI dw ?
	;------------------
	code_w db ?
	code_rm db ?
	code_mod db ?
	
	check_mov db 0
	;------------------
	printMOV db "MOV $"
	printTEST db "TEST $"
	Message	db "Zingsninio rezimo pertraukimas!", 13, 10, "$"
	False	db "Komanda ne MOV r/m <= bet. operand.", 13, 10, "$"
	;------------------
	byte_ptr db "byte_ptr $"
	word_ptr db "word_ptr $"
	Enter db 13, 10, "$"
	Equal db " = $"
    ;------------------
.code
;------------------
SeeRM MACRO rm, name       
	mov al, rm
	mov dx, offset name
	call printRM
ENDM
;------------------
SeeRM_MOD11 MACRO rm, name, reg 
	mov al, rm
	mov dx, offset name
	mov bx, reg
	call printRM
ENDM
;------------------
SeeRM_Reg MACRO rm, name, reg
	mov al, rm
	mov dx, offset name
	mov bx, reg
	call IfAddress
ENDM
;------------------
SeeRM_Reg_2 MACRO rm, name, reg, name_nd, reg_nd
	push bx
	mov al, rm
	mov dx, offset name
	mov bx, reg
	call IfAddress
	mov al, rm
	mov dx, offset name_nd
	mov bx, reg_nd
	call IfAddress
	pop bx
ENDM
;------------------
  Pradzia:
  
	MOV	ax, @data	 
	MOV	ds, ax			  

	MOV	ax, 0
	MOV	es, ax		;i es isirasome 0, nes ten pertraukimu vektoriu lentele 
	
    ; Issisaugome kad programos gale galetume ji grazinti
	PUSH	es:[4]
	PUSH	es:[6]

	MOV	word ptr es:[4], offset ReadInterupt	;i pertraukimu vektoriu lentele idedame savo proc
	MOV	es:[6], cs				;i pertraukimu vektoriu lentele irasome pertraukimo apdorojimo proceduros segmenta

    ; Iskvieciame INT1
	PUSHF			; Flagai i stack
	PUSHF			;Issisaugome SF kad galetume ja isimti
	POP ax			;Isimame SF reiksme
	OR ax, 0100h		;Nustatome TF=1
	PUSH ax			;Idedame pakoreguota reiksme
	POPF			;is stack i SF; Nuo cia TF=1
	NOP			    ;Pirmas pertraukimas kyla ne pries sia komanda, o po jos
		
	;komandos testavimui		 
	mov [bp+si], 16h
	dec bx
	mov [si + 1234h], 15h
	mov byte ptr [si], 12h
	test [si], 13h
				
	POPF			; is stack i SF reiksme, kuri buvo pradzioje (tf buvo 0)	

    ; Atstatome pertraukimo apdorojimo programos adresa
	POP	es:[6]
	POP	es:[4]
	
    ;exit
	MOV	ah, 4Ch		
	MOV	al, 0		
	INT	21h		


    PROC ReadInterupt    ;adresas;koda,mnemonika, operandus bx ir [bx]
    ; issisaugome registrus
    mov regAX, ax				
	mov regBX, bx
	mov regCX, cx
	mov regDX, dx
	mov regSP, sp
	mov regBP, bp
	mov regSI, si
	mov regDI, di
	 
	;[cs:ip] 
	pop si  ;ip
	pop di  ;cs
	push di   
	push si 
    ;pasiimame pirmus 6 baitus		
	mov ax, cs:[si]
	mov bx, cs:[si+2]
	mov cx, cs:[si+4]
	;------------------
	mov byte1, al
    mov byte2, ah
    mov byte3, bl
    mov byte4, bh
    mov byte5, cl
    mov byte6, ch
        	
	;Tikriname, ar INT buvo iskviestas pries komanda MOV
	AND al, 0FEh		;Tikriname pagal pirmus 7 OPK bitus
	CMP al, 0C6h		;Ar tai MOV registras / atmintis <- betarpiskas operandas - 1100 011w mod 000 r/m [poslinkis] bet. o.
	JNE look_test
	
	AND ah, 38h             
	CMP ah, 0
	JNE look_test
	mov al, 1
	mov check_mov, al
	
	jmp corr
	
	look_test:
	mov al, byte1
	
	AND al, 0FEh
	CMP al, 0F6h
	JNE int_end
	
	mov ah, byte2
	
	AND ah, 38h             ;TEST;1111 011w mod 000 r/m [poslinkis] bet.op
	CMP ah, 0
	JNE int_end
	mov al, 0
	mov check_mov, al
	 
	corr:
	;Issisaugom w, mod, rm reiksmes
	
	mov al, byte1
	mov ah, byte2
	
	push ax
	and al, 01h
	mov code_w, al      ; eilutes w reiksme
	pop ax
	
	push ax
	and ah, 0C0h
	mov code_mod, ah    ; eilutes mod reiksme
	pop ax
	
	push ax
	and ah, 07h
	mov code_rm, ah     ; eilutes r/m reiksme
	pop ax
	
	jmp mov2
	;------------------
	
	;Jei INT buvo iskviestas ne pries komanda MOV
int_end:	
	MOV ah, 9
	MOV dx, offset False
	INT 21h
	JMP Finish
    ;------------------
    
mov2:
    mov ah, 9
    mov dx, offset Message
    int 21h
    
    ;Spausdiname adresa            
    mov ax, di
    call printAX
    
    mov ah, 2
    mov dl, ":"
    int 21h
    
    mov ax, si
    call printAX
    ;------------------
    
    call printSpace
    
    ;Spausdiname masinini koda
    mov ah, byte1    ;opk
    mov al, byte2    ;ads
    call printAX
                            
    
    ;Tikrinam mod
    cmp code_mod, 0C0h ; ar mod = 11
    je MOD11           ; atskiras atvejis
    
    cmp code_mod, 0    ; ar mod = 00
    jne check_offset   ; jei ne tikrinam kitus vriantus
    ;------------------
    cmp code_rm, 06h   ;  jei r/m = 110 ir mod = 00, tai tiesioginis adresas
    je offset_word     ;  kuris yra 2 baitu
    jmp offset_zero    ; jei r/m kitoks ir mod = 00, nera poslinkio
    ;------------------
    check_offset:
    cmp code_mod, 040h ; ar mod = 01
    je offset_byte     ; jei taip poslinkis 1baitas
    
    ;spausdiname 2 baitu poslinki
    offset_word:
    mov al, byte4
    call printAL
    mov al, byte3
    call printAL
    jmp analizeMOV
    ;spausdiname 1 baito poslinki
    offset_byte:
    mov al, byte3
    call printAL
    jmp analizeMOV
    ; poslinkio nera
    offset_zero:
    mov al, byte3
    call printAL
    
    ;------------------
    jmp analizeMOV
    ;kai mod = 11, atskiras atvejis
    MOD11:
    
    mov al, byte3        ;neturi poslinkio, spausdinam bet. o
    call printAL
    ;------------------
    call printSpace
    
    cmp check_mov, 1
    je true_mov
    mov dx, offset printTEST
    jmp print_com
    true_mov:
    mov dx, offset printMOV   
    print_com:
    mov ah, 9
    int 21h
    ;------------------
    cmp code_w, 0   ; kai mod 11 tikriname w reiksme 
    jne w_1
    call printSpace
    jmp when_w_0
    ;------------------
    w_1:
    ;printinam tipa    
    mov ah, 9
    mov dx, offset word_ptr
    int 21h
    ;------------------
    SeeRM_MOD11 00h, reg_ax, regAX        ; is lenteles
	SeeRM_MOD11 01h, reg_cx, regCX        ; r/m * mod 11, w = 1
	SeeRM_MOD11 02h, reg_dx, regDX
	SeeRM_MOD11 03h, reg_bx, regBX
	SeeRM_MOD11 04h, reg_sp, regSP
	SeeRM_MOD11 05h, reg_bp, regBP
	SeeRM_MOD11 06h, reg_si, regSI
	SeeRM_MOD11 07h, reg_di, regDI            ; reg = hex
    ;------------------
    call printEnter
    jmp finish
    ;------------------
    when_w_0:
    ;------------------
    SeeRM_MOD11 00h, reg_al, regAX
	SeeRM_MOD11 01h, reg_cl, regCX     ; is lenteles
	SeeRM_MOD11 02h, reg_dl, regDX     ; r/m * mod 11, w = 0
	SeeRM_MOD11 03h, reg_bl, regBX
	SeeRM_MOD11 04h, reg_ah, regSP
	SeeRM_MOD11 05h, reg_ch, regBP
	SeeRM_MOD11 06h, reg_dh, regSI
	SeeRM_MOD11 07h, reg_bh, regDI
    ;------------------
    call printEnter
    jmp finish
    ;------------------
    
    
    ;kai mod ne 11
    analizeMOV:
    call printSpace
    ;------------------
    cmp check_mov, 1
    je true_mov1
    mov dx, offset printTEST
    jmp print_com1
    
    true_mov1:
    mov dx, offset printMOV
    
    print_com1:
    mov ah, 9
    int 21h
    ;------------------ 
   		
	
    cmp code_w, 0     ; nustatom ar tai word ar byte ptr
    je w_0
    ;------------------
    mov ah, 9
    mov dx, offset word_ptr
    int 21h
    ;------------------
    jmp analizeMOD_RM
    
    w_0:
    ;------------------
    mov ah, 9
    mov dx, offset byte_ptr
    int 21h
    ;------------------
    analizeMOD_RM:
    ;------------------ 
    mov ah, 2
    mov dl, "["
    int 21h
    ;------------------ 
    cmp code_mod, 0   ; ziurime ar mod ne 00( t.y ar turi poslinki)
    jne RM_only       ; jei turi, tikriname r/m atveji su mod 01 arba 10
    cmp code_rm, 6h   ; jei mod 00 ir r\m 110, tai tiesioginis adresas
    je word_offset    ; tiesioginis adresas visada 2 baitu, praleidzia reg
    ;------------------
    
    RM_only:
    
    SeeRM 00h, reg_bxsi
	SeeRM 01h, reg_bxdi
	SeeRM 02h, reg_bpsi
	SeeRM 03h, reg_bpdi       ;is lenteles, reg
	SeeRM 04h, reg_si         ; r/m * mod(00,01,10)
	SeeRM 05h, reg_di
	SeeRM 06h, reg_bp
	SeeRM 07h, reg_bx
    
    ;tikrinti ar yra poslinkis  
    cmp code_mod, 0  
    je skip
    
    mov ah, 2
    mov dl, "+"
    int 21h
    ;tikrinti ar poslinkis word dydzio
    cmp code_mod, 80h 
    je word_offset
    
    ;byte dydzio atvejis
    mov al, byte3
    call printAL
    jmp skip
    
    ;word dydzio atvejis
    word_offset:
    mov al, byte4
    call printAL
    mov al, byte3
    call printAL
   ;------------------ 
    mov ah, 2
    mov dl, "]"
    int 21h
    mov ah, 2
    mov dl, ","
    int 21h
    
    cmp code_w, 1
    je is_word
    mov al, byte5           ; tikriname koki atsakyma ivest pagal w
    call printAL
    jmp to_h1
    
    is_word:
    mov ah, byte6
    mov al, byte5
    call printAX
    
    to_h1:
    mov ah, 2
    mov dl, "h"
    int 21h 
   ;atskiras atvejis  (kai mod = 00 , r/m = 110)  
    cmp code_mod, 0
    jne Hex
    cmp code_rm, 6h
    je skip_Hex
    jmp Hex
    
   ;tarpinis 
    skip_Hex:
    jmp finish_enter
    
   ;------------------ 
    skip:  
    mov ah, 2
    mov dl, "]"
    int 21h
    mov ah, 2
    mov dl, ","
    int 21h
                            ; iki sios vietos ateina tik (mod 00;01)
    cmp code_mod, 40h       ; tikriname kuri baita reikia israsyti
    je is_byte              ; pagal tai ar turi poslinki ar ne
    
    cmp code_w, 1
    je w_is_1               ; ir tikriname atsakymo dydi
    mov al, byte3                  
    call printAL
    jmp to_h
    w_is_1:
    mov ah, byte4
    mov al, byte3
    call printAX
                        
    jmp to_h                
    is_byte:
    mov al, byte4
    call printAL
    
    to_h:
    mov ah, 2
    mov dl, "h"
    int 21h 
   ;------------------ 
    Hex:
    SeeRM_Reg_2 00h, reg_bx, regBX, reg_si, regSI
	SeeRM_Reg_2 01h, reg_bx, regBX, reg_di, regDI
	SeeRM_Reg_2 02h, reg_bp, regBP, reg_si, regSI      ;" reg = hex, [reg] = hex"
	SeeRM_Reg_2 03h, reg_bp, regBP, reg_di, regDI
	SeeRM_Reg 04h, reg_si, regSI
	SeeRM_Reg 05h, reg_di, regDI
	SeeRM_Reg 06h, reg_bp, regBP
	SeeRM_Reg 07h, reg_bx, regBX
    
    finish_enter:
    call printEnter
	
Finish:	   
	mov ax, regAX
	mov bx, regBX
	mov cx, regCX
	mov dx, regDX
	mov sp, regSP
	mov bp, regBP
	mov si, regSI
	mov di, regDI
	IRET
ReadInterupt ENDP

;------------------
printSpace:
	push ax
	push dx
		mov ah, 2
		mov dl, " "
		int 21h
	pop dx
	pop ax
RET
;------------------
printEnter:
	push ax
	push dx
		mov ah, 9
		mov dx, offset Enter
		int 21h
	pop dx
	pop ax
RET
;------------------
printAX:
	push ax
	mov al, ah
	call printAL
	pop ax
	call printAL
RET
;------------------
printAL:
	push ax
	push cx
		push ax
		mov cl, 4
		shr al, cl  ;lieka ah reiksme
		call PrintHex
		pop ax
		call PrintHex
	pop cx
	pop ax
RET
;------------------
PrintHex:
	push ax
	push dx
	
	and al, 0Fh ;ah = 0;
	cmp al, 9
	jbe Number
	
	Letter: 
	sub al, 10
	add al, 41h   ; ASCII
	mov dl, al
	mov ah, 2
	int 21h
	jmp PrintEnd	
	
	Number:
	mov dl, al
	add dl, 30h ; ASCII
	mov ah, 2
	int 21h
	
	PrintEnd:
	pop dx
	pop ax
RET
;-----------------
printRM proc
	cmp al, code_rm 
	jne EndPrint
	 
	push ax
	
	mov ah, 9   ;registro vardas kuris mov'inamas
	int 21h
	
	pop ax 
	
	cmp code_mod, 0C0h
	jne EndPrint
	
	;Vykgdoma jeigu mod = 11  "reg = hex"
	push dx
	mov ah, 2
	mov dl, ','
	int 21h
	call printSpace
	pop dx
	push ax
	push bx
	
	mov ah, 9  ;registro vardas
	int 21h
	
	mov ah, 2
	mov dl, '='
	int 21h
	
	cmp code_w, 1
	je size_w
	
	mov ax, bx
	call printAL
	jmp EndMod11
	
	size_w:
	mov ax, bx
	call printAL
	mov al, ah
	call printAL
	
	EndMod11:
	pop bx
	pop ax
	;-----------------
	EndPrint:
		ret
printRM endp
;-----------------	
IfAddress proc    
    
	push ax
	
	cmp al, code_rm 
	jne EndIf
	;----------------- " reg = hex, [reg] = hex"
	call printSpace
	push ax
	call printRM
	push dx
	
	mov ah, 2
	mov dl, '='
	int 21h
	
	mov ax, bx
	call printAX
	
	mov ah, 2
	mov dl, ','
	int 21h

	mov ah, 2
	mov dl, '['
	int 21h
	
	pop dx
	pop ax
	call printRM
	
    mov ah, 2
	mov dl, ']'
	int 21h 
	
	mov ah, 2
	mov dl, '='
	int 21h
	
	mov ax, [bx]
	call PrintAX
	;-----------------
	EndIf:
	pop ax
	ret
	
IfAddress endp

END Pradzia