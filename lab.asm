; ==========================================
; lab.asm 
; Author: Jiayuan Li
; 编译方法：nasm lab.asm -o lab.com
; ==========================================

%include    "pm.inc"

org 0100h
        jmp LABEL_BEGIN

[SECTION .gdt]
;*ANCHOR -  GDT
;*                                  段基址，             段界限，属性
LABEL_GDT:              Descriptor       0,                  0, 0,           ;* 空描述符
LABEL_DESC_NORMAL:      Descriptor       0,             0ffffh, DA_DRW       ;* Normal描述符
LABEL_DESC_CODE32:      Descriptor       0,   SegCode32Len - 1, DA_C + DA_32 ;* 非一致代码段,存在的只执行32位代码段，DPL为0
LABEL_DESC_CODE16:      Descriptor       0,             0ffffh, DA_C         ;* 非一致代码段，16位
LABEL_DESC_CODE_DEST:   Descriptor       0,   SegCodeDestLen-1, DA_C + DA_32 ;* 调用门的目标代码段
LABEL_DESC_CODE_RING3:  Descriptor       0,  SegCodeRing3Len-1, DA_C + DA_32 + DA_DPL3
LABEL_DESC_DATA:        Descriptor       0,          DataLen-1, DA_DRW      ;* 数据段
LABEL_DESC_STACK:       Descriptor       0,         TopOfStack, DA_DRWA + DA_32 ;* 32位stack
LABEL_DESC_STACK3:      Descriptor       0,        TopOfStack3, DA_DRWA + DA_32 + DA_DPL3 ;* 32位stack3
LABEL_DESC_LDT:         Descriptor       0,         LDTLen - 1, DA_LDT       ;* GDT中针对LDT的描述符
LABEL_DESC_LDT2:        Descriptor       0,        LDTLen2 - 1, DA_LDT       ;* GDT中针对LDT2的描述符
LABEL_DESC_TSS:         Descriptor       0,         TSSLen - 1, DA_386TSS    ;* TSS
LABEL_DESC_VIDEO:       Descriptor 0B8000h,             0ffffh, DA_DRW + DA_DPL3       ;* 显存首地址
;* GDT结束

;* 调用门                                   目标选择子       偏移    Dcount, 属性   
LABEL_CALL_GATE_TEST:   Gate        SelectorCodeDest,          0,       0, DA_386CGate + DA_DPL3

GdtLen          equ             $ - LABEL_GDT           ;*GDT长度
GdtPtr          dw              GdtLen - 1              ;*GDT界限，低位2字节
                dd              0                       ;*GDT基地址，高位4字节

;*ANCHOR -  GDT选择子
SelectorNormal             equ  LABEL_DESC_NORMAL - LABEL_GDT
SelectorCode32             equ  LABEL_DESC_CODE32 - LABEL_GDT
SelectorCode16             equ  LABEL_DESC_CODE16 - LABEL_GDT
SelectorCodeDest           equ  LABEL_DESC_CODE_DEST - LABEL_GDT
SelectorCodeRing3          equ  LABEL_DESC_CODE_RING3 - LABEL_GDT + SA_RPL3
SelectorData               equ  LABEL_DESC_DATA - LABEL_GDT
SelectorStack              equ  LABEL_DESC_STACK - LABEL_GDT
SelectorStack3             equ  LABEL_DESC_STACK3 - LABEL_GDT + SA_RPL3
SelectorLDT                equ  LABEL_DESC_LDT - LABEL_GDT
SelectorLDT2               equ  LABEL_DESC_LDT2 - LABEL_GDT    ;TODO: second ldt selector
SelectorTSS                equ  LABEL_DESC_TSS - LABEL_GDT
SelectorVideo              equ  LABEL_DESC_VIDEO - LABEL_GDT

SelectorCallGateTest       equ  LABEL_CALL_GATE_TEST - LABEL_GDT + SA_RPL3  ;* 测试调用门的选择子
;* End of [SECTION .gdt]

[SECTION .data1]     ;* 数据段
ALIGN	32
[BITS   32]
LABEL_DATA:
SpValueInRealMode       dw  0
PMMessage:              db  "In Protect Mode",0
OffsetPMMessage         equ PMMessage - $$
StrTest:                db  "ABCDEFGHIJKLMNOPQRSTUVWXYZ", 0
OffsetStrTest           equ StrTest - $$
DataLen                 equ $ - LABEL_DATA
;* End of [SECTION .data1]


;*ANCHOR - 全局堆栈段
[SECTION .gs]
ALIGN   32
[BITS   32]
LABEL_STACK:
    times 512 db 0

TopOfStack equ $ - LABEL_STACK - 1
;* End of [SECTION .gs]


;*ANCHOR - 堆栈段ring3
[SECTION .s3]
ALIGN   32
[BITS   32]
LABEL_STACK3:
    times 512 db 0
TopOfStack3 equ $ - LABEL_STACK3 - 1
;* End of [SECTION .s3]


;*ANCHOR - TSS
;* 初始化任务状态堆栈段TSS
[SECTION .tss]
ALIGN   32
[BITS   32]
LABEL_TSS:
        DD  0
        DD TopOfStack     ;* 0级堆栈
        DD SelectorStack
        DD  0             ;* 1级堆栈
        DD  0
        DD  0             ;* 2级堆栈
        DD  0
        DD	0			;* CR3
		DD	0			;* EIP
		DD	0			;* EFLAGS
		DD	0			;* EAX
		DD	0			;* ECX
		DD	0			;* EDX
		DD	0			;* EBX
		DD	0			;* ESP
		DD	0			;* EBP
		DD	0			;* ESI
		DD	0			;* EDI
		DD	0			;* ES
		DD	0			;* CS
		DD	0			;* SS
		DD	0			;* DS
		DD	0			;* FS
		DD	0			;* GS
		DD	0			;* LDT
		DW	0			;* 调试陷阱标志
		DW	$ - LABEL_TSS + 2	;* I/O位图基址
		DB	0ffh			;* I/O位图结束标志
TSSLen		equ	$ - LABEL_TSS   ;*求得段的大小

;*ANCHOR - 16位实模式初始工作部分
[SECTION .s16]    
[BITS   16]       ;*表明是16位代码段
LABEL_BEGIN:
        mov ax, cs
        mov ds, ax
        mov es, ax
        mov ss, ax
        mov sp, 0100h   ;???

        mov [LABEL_GO_BACK_TO_REAL+3], ax  ;! ax的值是实模式下的cs,把cs保存到Segment的位置
        mov [SpValueInRealMode], sp        ;! 保存实模式下的sp的值，用于后续的恢复

        ;* 初始化16位代码段描述符
        mov ax, cs
        movzx eax, ax
        shl eax, 4
        add eax, LABEL_SEG_CODE16
        mov word [LABEL_DESC_CODE16 + 2], ax
        shr eax, 16
        mov byte [LABEL_DESC_CODE16 + 4], al
        mov byte [LABEL_DESC_CODE16 + 7], ah

        ;* 初始化32位代码段描述符
        xor eax, eax
        mov ax, cs     ; 段地址向左移位4位加上偏移量
        shl eax, 4
        add eax, LABEL_SEG_CODE32   ; eax里面放的是基地址
        mov word [LABEL_DESC_CODE32 + 2], ax
        shr eax, 16
        mov byte [LABEL_DESC_CODE32 + 4], al
        mov byte [LABEL_DESC_CODE32 + 7], ah

        ;* 初始化ring3的32位代码段描述符
        xor eax, eax
        mov ax, ds
        shl eax, 4
        add eax, LABEL_CODE_RING3
        mov word [LABEL_DESC_CODE_RING3 + 2], ax
        shr eax, 16
        mov byte [LABEL_DESC_CODE_RING3 + 4], al
        mov byte [LABEL_DESC_CODE_RING3 + 7], ah

        ;* 初始化调用门目标代码段描述符
        xor eax, eax
        mov ax, cs
        shl eax, 4
        add eax, LABEL_SEG_CODE_DEST
        mov word [LABEL_DESC_CODE_DEST + 2], ax
        shr eax, 16
        mov byte [LABEL_DESC_CODE_DEST + 4], al
        mov byte [LABEL_DESC_CODE_DEST + 7], ah

        ;* 初始化数据段描述符
        xor eax,eax
        mov ax, ds
        shl eax, 4
        add eax, LABEL_DATA
        mov word [LABEL_DESC_DATA + 2], ax
        shr eax, 16
        mov byte [LABEL_DESC_DATA + 4], al
        mov byte [LABEL_DESC_DATA + 7], ah

        ;* 初始化堆栈段描述符
        xor eax,eax
        mov ax, ds
        shl eax, 4
        add eax, LABEL_STACK
        mov word [LABEL_DESC_STACK + 2], ax
        shr eax, 16
        mov byte [LABEL_DESC_STACK + 4], al
        mov byte [LABEL_DESC_STACK + 7], ah

        ;* 初始化ring3堆栈段描述符
        xor eax, eax
        mov ax, ds
        shl eax, 4
        add eax, LABEL_STACK3
        mov word [LABEL_DESC_STACK3 + 2], ax
        shr eax, 16
        mov byte [LABEL_DESC_STACK3 + 4], al
        mov byte [LABEL_DESC_STACK3 + 7], ah


        ;* 初始化LDT在GDT中的描述符
        xor eax, eax
        mov ax, ds
        shl eax, 4
        add eax, LABEL_LDT
        mov word [LABEL_DESC_LDT + 2], ax
        shr eax, 16
        mov byte [LABEL_DESC_LDT + 4], al
        mov byte [LABEL_DESC_LDT + 7], ah

        ;!test: 初始化LDT2在GDT中的描述符
        xor eax, eax
        mov ax, ds
        shl eax, 4
        add eax, LABEL_LDT2
        mov word [LABEL_DESC_LDT2 + 2], ax
        shr eax, 16
        mov byte [LABEL_DESC_LDT2 + 4], al
        mov byte [LABEL_DESC_LDT2 + 7], ah

        ;* 初始化LDT中的描述符
        xor eax, eax
        mov ax, ds
        shl eax, 4
        add eax, LABEL_CODE_A
        mov word [LABEL_LDT_DESC_CODEA + 2], ax
        shr eax, 16
        mov byte [LABEL_LDT_DESC_CODEA + 4], al
        mov byte [LABEL_LDT_DESC_CODEA + 7], ah

        ;TODO: 初始化LDT2中的描述符
        xor eax, eax
        mov ax, ds
        shl eax, 4
        add eax, LABEL_CODE_A_2
        mov word [LABEL_LDT2_DESC_CODEA + 2], ax
        shr eax, 16
        mov byte [LABEL_LDT2_DESC_CODEA + 4], al
        mov byte [LABEL_LDT2_DESC_CODEA + 7], ah

        ;* 初始化TSS描述符
        xor eax, eax
        mov ax, ds
        shl eax, 4
        add eax, LABEL_TSS
        mov word [LABEL_DESC_TSS + 2], ax
        shr eax, 16
        mov byte [LABEL_DESC_TSS + 4], al
        mov byte [LABEL_DESC_TSS + 7], ah

        ;* 为加载GDTR做准备
        xor eax, eax
        mov ax, ds
        shl eax, 4
        add eax, LABEL_GDT           ;* eax <- gdt基地址
        mov dword [GdtPtr + 2], eax  ;* [GdtPtr + 2] <- gdt基地址

        ;* 加载 GDTR
        lgdt [GdtPtr]

        ;! 关闭中断,保护模式下中断机制不同，需要关闭
        cli

        ;* 打开地址线A20
        in al, 92h
        or al, 00000010b
        out 92h, al

        ;* 准备切换到保护模式,cr0寄存器最低位置1
        mov eax, cr0
        or eax, 1
        mov cr0, eax

        ;* 真正进入保护模式
        jmp dword SelectorCode32:0     ;* 执行这一句将SelectorCode32装入cs
                                       ;* 并且跳转到SelectorCode32:0处
;* End of [SECTION .s16]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LABEL_REAL_ENTRY:
        mov ax, cs     ;! 跳回实模式之后，重新设置各个段寄存器的值，恢复sp的值，关闭A20，打开中断
        mov ds, ax     ;! 回到原来的样子
        mov es, ax
        mov ss, ax

        mov sp, [SpValueInRealMode]
        
        in al, 92h     ;* 关闭A20地址线
        and al, 11111101b 
        out 92h, al

        sti            ;* 跳回实模式之后打开中断

        mov ax, 4c00h  ;! 重新回到实模式下的DOS
        int 21h
;* End of [SECTION .s16]

;*ANCHOR -  32位代码段，由实模式跳入
[SECTION .s32] 
[BITS   32]        ;* 表明是32位代码段
LABEL_SEG_CODE32:
        mov ax, SelectorData ;! 跳到32位代码段之后的准备工作，ds->数据段，es->测试段，gs->显存，ss->堆栈
        mov ds, ax         ;*数据段选择子

        mov ax, SelectorVideo
        mov gs, ax         ;*视频段选择子

        mov ax, SelectorStack
        mov ss, ax         ;*堆栈段选择子
        mov esp, TopOfStack

        ;TODO: To be removed
        ;* 显示一个字符串, 表示此时已经处于保护模式中
        mov ah, 0Ch       ;* 0000:黑底，1100：红字
        xor esi, esi
        xor edi, edi
        mov esi, OffsetPMMessage   ;* 源数据偏移
        mov edi, (80 * 10 + 0) * 2 ;* 目标数据偏移，屏幕第10行第0列
        cld
    .1:
        lodsb    ;* load byte at address DS:(E)SI into AL register
        test al, al
        jz .2
        mov [gs:edi], ax
        add edi, 2
        jmp .1
    .2:         ;* 字符串显示完毕
        call DispReturn

        ;* Load TSS
        mov ax, SelectorTSS     ;! ltr是ring0级指令，只能运行在ring0级代码段
        ltr ax                  ;* 设置任务状态段寄存器tr

        push SelectorStack3     ;* 从ring0 -> ring3， 压入ss, sp, cs, ip
        push TopOfStack3
        push SelectorCodeRing3
        push 0
        retf

        ;* 测试使用调用门
        ;call SelectorCallGateTest:0

        ;* load LDT, 准备跳入局部任务
        ;mov ax, SelectorLDT
        ;lldt ax ;! lldt负责加载ldtr, 操作数是一个选择子，选择子对应的用来描述LDT的GDT中的描述符

        ;*在保护模式下32位代码段中跳入局部任务
        ;jmp SelectorLDTCodeA:0

; ------------------------------------------------------------------------
TestRead:
        xor esi, esi
        mov ecx, 8
    .loop:
        mov al, [es:esi]
        call DispAL
        inc esi
        loop .loop

        call DispReturn
        ret
; TestRead 结束-----------------------------------------------------------


; ------------------------------------------------------------------------
TestWrite:
        push esi
        push edi
        xor esi, esi
        xor edi, edi
        mov esi, OffsetStrTest
        cld
    .1:
        lodsb
        test al, al
        jz .2
        mov [es:edi], al
        inc edi
        jmp .1
    .2:
        pop edi
        pop esi
        ret
; TestWrite 结束----------------------------------------------------------


; ------------------------------------------------------------------------
; 显示 AL 中的数字
; 默认地:
;	数字已经存在 AL 中
;	edi 始终指向要显示的下一个字符的位置
; 被改变的寄存器:
;	ax, edi
; ------------------------------------------------------------------------
DispAL:
	push	ecx
	push	edx

	mov	ah, 0Ch			; 0000: 黑底    1100: 红字
	mov	dl, al
	shr	al, 4
	mov	ecx, 2
.begin:
	and	al, 01111b
	cmp	al, 9
	ja	.1
	add	al, '0'
	jmp	.2
.1:
	sub	al, 0Ah
	add	al, 'A'
.2:
	mov	[gs:edi], ax
	add	edi, 2

	mov	al, dl
	loop	.begin
	add	edi, 2

	pop	edx
	pop	ecx

	ret
; DispAL 结束-------------------------------------------------------------


; ------------------------------------------------------------------------
DispReturn:
	push	eax
	push	ebx
	mov	eax, edi
	mov	bl, 160
	div	bl
	and	eax, 0FFh
	inc	eax
	mov	bl, 160
	mul	bl
	mov	edi, eax
	pop	ebx
	pop	eax

	ret
; DispReturn 结束---------------------------------------------------------
SegCode32Len       equ   $ - LABEL_SEG_CODE32
;* End of [SECTION .s32]

;*ANCHOR - 调用门的目标代码段
[SECTION .sdest]
[BITS   32]
LABEL_SEG_CODE_DEST:      ;* ring0级代码段
    mov ax, SelectorVideo
    mov gs, ax
    mov edi, (80 * 12 + 0) * 2
    mov ah, 0Ch
    mov al, 'C'
    mov [gs:edi], ax

    mov ax, SelectorLDT    ;TODO: 使用调用门后跳到局部任务的代码段
    lldt ax

    jmp SelectorLDTCodeA:0

    ;retf            ;* 使用call调用，结尾用retf
SegCodeDestLen  equ $ - LABEL_SEG_CODE_DEST
;* End of [SECTION .sdest]


;*ANCHOR - 16位代码段，由32位代码段跳入，跳出后到实模式
[SECTION .s16code]
ALIGN 32
[BITS   16]
LABEL_SEG_CODE16:
    ;* 跳回实模式
    mov ax, SelectorNormal   ;! 准备结束保护模式回到实模式之前，需要加载一个合适的描述符选择子到有关段寄存器
    mov ds, ax               ;! 以使对应段描述符高速缓冲寄存器中有合适的段界限和属性。而且不能从32位代码段
    mov es, ax               ;! 返回实模式，只能从16位代码段中返回。因为无法实现从32位代码段返回时cs寄存器中
    mov fs, ax               ;! 的属性符合实模式的要求
    mov gs, ax
    mov ss, ax

    mov eax, cr0
    and al, 11111110b
    mov cr0, eax

LABEL_GO_BACK_TO_REAL:       ;! 段的地址会在程序开始处被设置成正确的值 -> jmp cs_real_mode:LABEL_REAL_ENTRY
    jmp 0:LABEL_REAL_ENTRY

Code16Len   equ $ - LABEL_SEG_CODE16
;* End of [SECTION .s16code]


;*ANCHOR -  LDT
[SECTION .ldt]
ALIGN   32
LABEL_LDT:           ;! 当前LDT中只有一个代码段描述符
;*                                               段基址                 段界限， 属性
LABEL_LDT_DESC_CODEA:           Descriptor            0,          CodeALen - 1, DA_C + DA_32

LDTLen      equ  $ - LABEL_LDT

;* LDT选择子
SelectorLDTCodeA        equ LABEL_LDT_DESC_CODEA - LABEL_LDT + SA_TIL
;* End of [SECTION .ldt]


;*ANCHOR - LDT的代码段 CodeA(LDT, 32位代码段）
[SECTION .la]
ALIGN   32
[BITS   32]
LABEL_CODE_A:
    mov ax, SelectorVideo
    mov gs, ax

    mov edi, (80 * 13 + 0) * 2   ;* 屏幕第十行，第0列
    mov ah, 0Ch
    mov al, 'L'
    mov [gs:edi], ax

    ; ;* 跳到16位代码段然后跳回实模式
    jmp SelectorCode16:0
    ; mov ax, SelectorLDT2
    ; lldt ax
    ; jmp SelectorLDT2CodeA:0   ;? 能否跳到第二个局部任务? -> OK

CodeALen       equ $ - LABEL_CODE_A
;* End of [SECTION .la]


;*ANCHOR - LDT2
[SECTION .ldt2]
ALIGN   32
LABEL_LDT2:
LABEL_LDT2_DESC_CODEA:          Descriptor              0,          CodeA2Len - 1, DA_C + DA_32

LDTLen2         equ  $ - LABEL_LDT2

;* LDT2选择子
SelectorLDT2CodeA      equ LABEL_LDT2_DESC_CODEA - LABEL_LDT2 + SA_TIL
;* End of [SECTION .ldt]

;*ANCHOR - LDT2的代码段 CodeA2(LDT2的32位代码段）
[SECTION    .la2]
ALIGN   32
[BITS   32]
LABEL_CODE_A_2:
    mov ax, SelectorVideo
    mov gs, ax

    mov edi, (80 * 15 + 0) * 2
    mov ah, 0Ch
    mov al, 'M'
    mov [gs:edi], ax

    ;* 跳到十六位代码段然后跳回实模式
    ; jmp SelectorCode16:0
    jmp $      ;* temporarily endless loop
CodeA2Len       equ   $ - LABEL_CODE_A_2
;* End of [SECTION .la2]

;*ANCHOR - Ring3的32位代码段
[SECTION .ring3]
ALIGN   32
[BITS   32]
LABEL_CODE_RING3:
    mov ax, SelectorVideo
    mov gs, ax
    mov edi, (80 * 14 + 0) * 2
    mov ah, 0Ch
    mov al, '3'     ;* 指示当前进入了ring3
    mov [gs:edi], ax

    call SelectorCallGateTest:0  ;* 在ring3下使用调用门跳转到ring0

    jmp $           ;TODO: to be modified
SegCodeRing3Len equ $ - LABEL_CODE_RING3
;* End of [SECTION .ring3]