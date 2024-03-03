%include	"pm.inc"	

PageDirBase0        equ 200000h 	;* 页目录开始地址 2M
PageTblBase0        equ 201000h 	;* 页表开始地址 2M + 4K
PageDirBase1        equ 210000h 	;* 页目录开始地址 2M + 64K
PageTblBase1        equ 211000h 	;* 页表开始地址 2M + 64K + 4K
LinearAddrDemo      equ 00401000h
ProcFoo             equ 00401000h   ;* 映射的第一个地址
ProcBar             equ 00501000h   ;* 映射的第二个地址

org	0100h
	jmp	LABEL_BEGIN

[SECTION .gdt]
;*ANCHOR -  GDT
;*                                  段基址，             段界限，属性
LABEL_GDT:				Descriptor      0,               	0, 0							;* 空描述符
LABEL_DESC_NORMAL:		Descriptor      0,             0ffffh, DA_DRW			       		;* Normal 描述符
LABEL_DESC_FLAT_C:		Descriptor      0,            0fffffh, DA_CR | DA_32 | DA_LIMIT_4K  ;* 0 ~ 4G, 用来执行
LABEL_DESC_FLAT_RW:		Descriptor      0,            0fffffh, DA_DRW | DA_LIMIT_4K	        ;* 0 ~ 4G， 用来读写
LABEL_DESC_CODE32:		Descriptor      0,     SegCode32Len-1, DA_CR | DA_32		        ;* 非一致代码段,存在的只执行32位代码段，DPL为0
LABEL_DESC_CODE16:		Descriptor      0,             0ffffh, DA_C			           		;* 非一致代码段，16位
LABEL_DESC_CODE_DEST:   Descriptor      0,   SegCodeDestLen-1, DA_C + DA_32 				;* 调用门的目标代码段
LABEL_DESC_CODE_RING3:  Descriptor      0,  SegCodeRing3Len-1, DA_C + DA_32 + DA_DPL3
LABEL_DESC_DATA:		Descriptor      0,          DataLen-1, DA_DRW			            ;* 数据段
LABEL_DESC_STACK:		Descriptor      0,         TopOfStack, DA_DRWA | DA_32		        ;* 32位stack
LABEL_DESC_STACK3:      Descriptor      0,        TopOfStack3, DA_DRWA + DA_32 + DA_DPL3 	;* 32位stack3
LABEL_DESC_LDT_A:       Descriptor      0,          LDTALen-1, DA_LDT		                ;* GDT中针对LDT的描述符
LABEL_DESC_LDT_B:       Descriptor      0,          LDTBLen-1, DA_LDT                       ;* GDT中针对LDT2的描述符
LABEL_DESC_TSS_A:       Descriptor      0,          TSSALen-1, DA_386TSS	                ;* TSSA
LABEL_DESC_TSS_B:       Descriptor      0,          TSSBLen-1, DA_386TSS                    ;* TSSB
LABEL_DESC_VIDEO:		Descriptor      0B8000h,       0ffffh, DA_DRW			            ;* 显存首地址

;* 调用门                                   目标选择子       偏移    Dcount, 属性   
LABEL_CALL_GATE_TEST:   Gate        SelectorCodeDest,          0,       0, DA_386CGate + DA_DPL3
;* GDT 结束

GdtLen		equ	$ - LABEL_GDT	;* GDT长度
GdtPtr		dw	GdtLen - 1	    ;* GDT界限
			dd	0		        ;* GDT基地址

;*ANCHOR -  GDT选择子
SelectorNormal	    equ	LABEL_DESC_NORMAL	- LABEL_GDT
SelectorFlatC		equ	LABEL_DESC_FLAT_C	- LABEL_GDT
SelectorFlatRW		equ	LABEL_DESC_FLAT_RW	- LABEL_GDT
SelectorCode32		equ	LABEL_DESC_CODE32	- LABEL_GDT
SelectorCode16		equ	LABEL_DESC_CODE16	- LABEL_GDT
SelectorCodeDest    equ LABEL_DESC_CODE_DEST - LABEL_GDT
SelectorCodeRing3   equ LABEL_DESC_CODE_RING3 - LABEL_GDT + SA_RPL3
SelectorData		equ	LABEL_DESC_DATA		- LABEL_GDT
SelectorStack		equ	LABEL_DESC_STACK	- LABEL_GDT
SelectorStack3      equ LABEL_DESC_STACK3 - LABEL_GDT + SA_RPL3
SelectorLDTA		equ	LABEL_DESC_LDT_A	- LABEL_GDT
SelectorLDTB		equ	LABEL_DESC_LDT_B	- LABEL_GDT
SelectorTSSA		equ	LABEL_DESC_TSS_A	- LABEL_GDT
SelectorTSSB		equ	LABEL_DESC_TSS_B	- LABEL_GDT
SelectorVideo		equ	LABEL_DESC_VIDEO	- LABEL_GDT

SelectorCallGateTest  equ  LABEL_CALL_GATE_TEST - LABEL_GDT + SA_RPL3  ;* 测试调用门的选择子
;* End of [SECTION .gdt]

;*ANCHOR - 全局数据段
[SECTION .data1] 
ALIGN	32
[BITS   32]
LABEL_DATA:
; SpValueInRealMode       dw  0
; PMMessage:              db  "In Protect Mode",0
; OffsetPMMessage         equ PMMessage - $$
; StrTest:                db  "ABCDEFGHIJKLMNOPQRSTUVWXYZ", 0
; OffsetStrTest           equ StrTest - $$
; DataLen                 equ $ - LABEL_DATA

;* 实模式下使用这些符号
;* Strings
_szPMMessage:			db	"In Protect Mode now.", 0Ah, 0Ah, 0	                        ; 进入保护模式后显示此字符串
_szMemChkTitle:			db	"BaseAddrL BaseAddrH LengthLow LengthHigh   Type", 0Ah, 0	; 进入保护模式后显示此字符串
_szRAMSize			    db	"RAM size:", 0
_szReturn			    db	0Ah, 0
;* Variables
_wSPValueInRealMode		dw	0
_dwMCRNumber:			dd	0	                ;* Memory Check Result
_dwDispPos:			    dd	(80 * 6 + 0) * 2	;* 屏幕第 6 行, 第 0 列。
_dwMemSize:			    dd	0
_ARDStruct:		       							;* Address Range Descriptor Structure -> 与读取内存信息有关
	_dwBaseAddrLow:		dd	0
	_dwBaseAddrHigh:	dd	0
	_dwLengthLow:		dd	0
	_dwLengthHigh:		dd	0
	_dwType:		    dd	0
_PageTableNumber:		dd	0
_SavedIDTR:			    dd	0					;* 用于保存 IDTR
				        dd	0
_SavedIMREG:			db	0					;* 中断屏蔽寄存器值
_workingTask:           db  0      
_MemChkBuf:	times	256	db	0

;* 保护模式下使用这些符号，因为保护模式下和实模式下机制不同
szPMMessage		    equ	_szPMMessage	- $$
szMemChkTitle		equ	_szMemChkTitle	- $$
szRAMSize		    equ	_szRAMSize	- $$
szReturn		    equ	_szReturn	- $$
dwDispPos		    equ	_dwDispPos	- $$
dwMemSize		    equ	_dwMemSize	- $$
dwMCRNumber		    equ	_dwMCRNumber	- $$
ARDStruct		    equ	_ARDStruct	- $$
	dwBaseAddrLow	equ	_dwBaseAddrLow	- $$
	dwBaseAddrHigh	equ	_dwBaseAddrHigh	- $$
	dwLengthLow	    equ	_dwLengthLow	- $$
	dwLengthHigh	equ	_dwLengthHigh	- $$
	dwType		    equ	_dwType		- $$
MemChkBuf		    equ	_MemChkBuf	- $$
SavedIDTR		    equ	_SavedIDTR	- $$
SavedIMREG		    equ	_SavedIMREG	- $$
PageTableNumber		equ	_PageTableNumber- $$
workingTask         equ     _workingTask        - $$ 

DataLen			    equ	$ - LABEL_DATA
;* End of [SECTION .data1]


;*ANCHOR - 全局堆栈段
[SECTION .gs]
ALIGN	32
[BITS	32]
LABEL_STACK:
	times 512 db 0

TopOfStack	equ	$ - LABEL_STACK - 1
;* End of [SECTION .gs]

;*ANCHOR -  IDT
[SECTION .idt]
ALIGN	32
[BITS	32]
LABEL_IDT:
%rep 32
			Gate	SelectorCode32, SpuriousHandler,      0, DA_386IGate
%endrep
.020h:		Gate	SelectorCode32,    ClockHandler,      0, DA_386IGate  ;* 20号时钟中断
%rep 95
			Gate	SelectorCode32, SpuriousHandler,      0, DA_386IGate
%endrep

IdtLen	equ	$ - LABEL_IDT
IdtPtr	dw	IdtLen - 1	; 段界限
		dd	0		; 基地址
;* END of [SECTION .idt]


;*ANCHOR - 堆栈段ring3
[SECTION .s3]
ALIGN   32
[BITS   32]
LABEL_STACK3:
    times 512 db 0
TopOfStack3 equ $ - LABEL_STACK3 - 1
;* End of [SECTION .s3]


;*ANCHOR - TSS-A
;* 初始化任务状态堆栈段TSS[SECTION .tss_a]
ALIGN	32
[BITS	32]
LABEL_TSS_A:
        DD	0		
        DD	0		        	; *0 级堆栈
        DD	0       		
        DD	0					;* 1 级堆栈
        DD	0			
        DD	0					;* 2 级堆栈
        DD	0			
        DD	PageDirBase0		;* CR3 -> 局部任务对应页目录的基址
        DD	0					;* EIP
        DD	0200h		        ;* EFLAGS
        DD	0					;* EAX
        DD	0					;* ECX
        DD	0					;* EDX
        DD	0					;* EBX
        DD	TopOfStackA			;* ESP
        DD	0					;* EBP
        DD	0					;* ESI
        DD	PageDirBase0    	;* EDI
        DD	SelectorFlatRW 		;* ES
        DD	SelectorLDTCodeA	;* CS -> 局部任务的代码段
        DD	SelectorLDTStackA	;* SS -> 局部任务的栈段
        DD	SelectorData		;* DS
        DD	0					;* FS
        DD	0					;* GS
        DD	SelectorLDTA		;* LDT -> GDT中对应LDT的描述符
        DW	0					; *调试陷阱标志
        DW	$ - LABEL_TSS_A + 2	;* I/O位图基址
        DB	0ffh				;* I/O位图结束标志
TSSALen		equ	$ - LABEL_TSS_A


;*ANCHOR - TSS-B
;* 初始化任务状态堆栈段TSS-BALIGN	32
[BITS	32]
LABEL_TSS_B:
		DD	0			
		DD	0		        	;* 0 级堆栈
		DD	0		        
		DD	0					;* 1 级堆栈
		DD	0			
		DD	0					;* 2 级堆栈
		DD	0			
		DD	PageDirBase1		;* CR3 -> 局部任务对应页目录的基址
		DD	0					;* EIP
		DD	0200h				;* EFLAGS
		DD	0					;* EAX
		DD	0					;* ECX
		DD	0					;* EDX
		DD	0					;* EBX
		DD	TopOfStackB			;* ESP
		DD	0					;* EBP
		DD	0					;* ESI
		DD	PageDirBase1		;* EDI
		DD	SelectorFlatRW      ;* ES
		DD	SelectorLDTCodeB	;* CS -> 局部任务的代码段
		DD	SelectorLDTStackB	;* SS -> 局部任务的栈段
		DD	SelectorData		;* DS
		DD	0					;* FS
		DD	0					;* GS
		DD	SelectorLDTB		;* LDT -> GDT中对应LDT的描述符
		DW	0					;* 调试陷阱标志
		DW	$ - LABEL_TSS_B + 2	;* I/O位图基址
		DB	0ffh				;* I/O位图结束标志
TSSBLen		equ	$ - LABEL_TSS_B

;*ANCHOR - 16位实模式初始工作部分
[SECTION .s16]
[BITS	16]			 ;*表明是16位代码段
LABEL_BEGIN:
    mov ax, cs
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0100h   ;???

	mov	[LABEL_GO_BACK_TO_REAL+3], ax	;! ax的值是实模式下的cs,把cs保存到Segment的位置
	mov	[_wSPValueInRealMode], sp		;! 保存实模式下的sp的值，用于后续的恢复

    ;* 得到内存数
	mov	ebx, 0          				;* 放置后续值，第一次调用时ebx为0
	mov	di, _MemChkBuf  				;* es:di指向一个地址范围描述符结构ARDS
.loop:
	mov	eax, 0E820h     				;* ax赋值为0E820H
	mov	ecx, 20		    				;* es:di所指向的地址范围描述符结构的大小，以字节为单位
	mov	edx, 0534D4150h 				;* 'SMAP'
	int	15h								;* int 15h中断
	jc	LABEL_MEM_CHK_FAIL  			;*CF标志位为1时跳转，表示出现错误
	add	di, 20
	inc	dword [_dwMCRNumber]
	cmp	ebx, 0
	jne	.loop
	jmp	LABEL_MEM_CHK_OK
LABEL_MEM_CHK_FAIL:
	mov	dword [_dwMCRNumber], 0
LABEL_MEM_CHK_OK:


	;* 初始化 16 位代码段描述符
	mov	ax, cs                                
	movzx	eax, ax                                 
	shl	eax, 4                                 
	add	eax, LABEL_SEG_CODE16                 
	mov	word [LABEL_DESC_CODE16 + 2], ax       
	shr	eax, 16
	mov	byte [LABEL_DESC_CODE16 + 4], al      
	mov	byte [LABEL_DESC_CODE16 + 7], ah

	;* 初始化 32 位代码段描述符
	xor	eax, eax
	mov	ax, cs		 			;* 段地址向左移位4位加上偏移量
	shl	eax, 4
	add	eax, LABEL_SEG_CODE32	;* eax里面放的是基地址
	mov	word [LABEL_DESC_CODE32 + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_CODE32 + 4], al
	mov	byte [LABEL_DESC_CODE32 + 7], ah

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
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_DATA
	mov	word [LABEL_DESC_DATA + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_DATA + 4], al
	mov	byte [LABEL_DESC_DATA + 7], ah

	;* 初始化堆栈段描述符
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_STACK
	mov	word [LABEL_DESC_STACK + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_STACK + 4], al
	mov	byte [LABEL_DESC_STACK + 7], ah

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
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_LDT_A
	mov	word [LABEL_DESC_LDT_A + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_LDT_A + 4], al
	mov	byte [LABEL_DESC_LDT_A + 7], ah

    ;* 初始化LDT中栈段的描述符
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_LDT_STACK_A
	mov	word [LABEL_LDT_DESC_STACK_A + 2], ax
	shr	eax, 16
	mov	byte [LABEL_LDT_DESC_STACK_A + 4], al
	mov	byte [LABEL_LDT_DESC_STACK_A + 7], ah

	;* 初始化LDT中代码段的描述符
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_CODE_A
	mov	word [LABEL_LDT_DESC_CODE_A + 2], ax
	shr	eax, 16
	mov	byte [LABEL_LDT_DESC_CODE_A + 4], al
	mov	byte [LABEL_LDT_DESC_CODE_A + 7], ah

    ;*初始化LDT2在GDT中的描述符
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_LDT_B
	mov	word [LABEL_DESC_LDT_B + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_LDT_B + 4], al
	mov	byte [LABEL_DESC_LDT_B + 7], ah

	;* 初始化LDT2中代码段的描述符
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_CODE_B
	mov	word [LABEL_LDT_DESC_CODE_B + 2], ax
	shr	eax, 16
	mov	byte [LABEL_LDT_DESC_CODE_B + 4], al
	mov	byte [LABEL_LDT_DESC_CODE_B + 7], ah

	;* 初始化LDT2中栈段的描述符        
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_LDT_STACK_B
	mov	word [LABEL_LDT_DESC_STACK_B + 2], ax
	shr	eax, 16
	mov	byte [LABEL_LDT_DESC_STACK_B + 4], al
	mov	byte [LABEL_LDT_DESC_STACK_B + 7], ah

	;* 初始化TSSA描述符
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_TSS_A
	mov	word [LABEL_DESC_TSS_A + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_TSS_A + 4], al
	mov	byte [LABEL_DESC_TSS_A + 7], ah

    ;* 初始化 TSSB 描述符
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_TSS_B
	mov	word [LABEL_DESC_TSS_B + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_TSS_B + 4], al
	mov	byte [LABEL_DESC_TSS_B + 7], ah

	;* 为加载GDTR做准备
    xor eax, eax
    mov ax, ds
    shl eax, 4
    add eax, LABEL_GDT           ;* eax <- gdt基地址
    mov dword [GdtPtr + 2], eax  ;* [GdtPtr + 2] <- gdt基地址

    ;* 为加载 IDTR 作准备
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_IDT			;* eax <- idt 基地址
	mov	dword [IdtPtr + 2], eax	;* [IdtPtr + 2] <- idt 基地址

	;* 保存IDIR
    sidt    [_SavedIDTR]

    ;* 保存中断屏蔽寄存器的值
    in al, 21h
    mov [_SavedIMREG], al

    ;* 加载 GDTR
    lgdt [GdtPtr]

    ;! 关闭中断,保护模式下中断机制不同，需要关闭
    cli

    ;* 加载IDTR
    lidt [IdtPtr]

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LABEL_REAL_ENTRY:
        mov ax, cs     ;! 跳回实模式之后，重新设置各个段寄存器的值，恢复sp的值，关闭A20，打开中断
        mov ds, ax     ;! 回到原来的样子
        mov es, ax
        mov ss, ax

        mov sp, [_wSPValueInRealMode]

        lidt  [_SavedIDTR]

        mov al, [_SavedIMREG]
        out 21h, al
        
        in al, 92h     ;* 关闭A20地址线
        and al, 11111101b 
        out 92h, al

        sti            ;* 跳回实模式之后打开中断

        mov ax, 4c00h  ;! 重新回到实模式下的DOS
        int 21h
;* End of [SECTION .s16]

;*ANCHOR -  32位代码段，由实模式跳入
[SECTION .s32]
[BITS	32]
LABEL_SEG_CODE32:
    mov ax, SelectorData ;! 跳到32位代码段之后的准备工作，ds->数据段，es->测试段，gs->显存，ss->堆栈
    mov ds, ax         ;*数据段选择子
    mov es, ax         ;*REVIEW - Is it necessary? -> Yes

    mov ax, SelectorVideo
    mov gs, ax         ;*视频段选择子

    mov ax, SelectorStack
    mov ss, ax         ;*堆栈段选择子
    mov esp, TopOfStack

    ;     ;TODO: To be removed
    ;     ;* 显示一个字符串, 表示此时已经处于保护模式中
    ;     mov ah, 0Ch       ;* 0000:黑底，1100：红字
    ;     xor esi, esi
    ;     xor edi, edi
    ;     mov esi, szPMMessage   ;* 源数据偏移
    ;     mov edi, (80 * 10 + 0) * 2 ;* 目标数据偏移，屏幕第10行第0列
    ;     cld
    ; .1:
    ;     lodsb    ;* load byte at address DS:(E)SI into AL register
    ;     test al, al
    ;     jz .2
    ;     mov [gs:edi], ax
    ;     add edi, 2
    ;     jmp .1
    ; .2:         ;* 字符串显示完毕
    ;     call DispReturn

    push szPMMessage
    call DispStr
    add esp, 4


    push szMemChkTitle
    call DispStr
    add esp, 4

    call DispMemSize   ;* 显示内存信息

    ;* 复制两个任务函数到指定的物理地址
    mov ax, cs
    mov ds, ax
    mov ax, SelectorFlatRW
    mov es, ax

    push LenFoo
    push OffsetFoo
    push ProcFoo
    call MemCpy
    add esp, 12

    push LenBar
    push OffsetBar
    push ProcBar
    call MemCpy
    add esp, 12

    mov ax, SelectorData
    mov ds, ax
     mov es, ax

    call SetupPaging   ;* 打开分页机制

	; mov byte [workingTask], 0

    ;* 加载LDT
    mov ax, SelectorLDTB   ;!! workingTask 的值初始默认为0
    lldt ax

    ;* 加载TSS
    mov ax, SelectorTSSB
    ltr ax

    ;* 设置8259A
    call Init8259A
        
	;* 打开中断
    sti 
    
	;* 跳到局部任务
    jmp SelectorLDTCodeB:0

    ;* 到此停止
    jmp SelectorCode16:0



;* Init8259A ---------------------------------------------------------------------------------------------
Init8259A:
	mov	al, 011h
	out	020h, al	; 主8259, ICW1.
	call	io_delay

	out	0A0h, al	; 从8259, ICW1.
	call	io_delay

	mov	al, 020h	; IRQ0 对应中断向量 0x20
	out	021h, al	; 主8259, ICW2.
	call	io_delay

	mov	al, 028h	; IRQ8 对应中断向量 0x28
	out	0A1h, al	; 从8259, ICW2.
	call	io_delay

	mov	al, 004h	; IR2 对应从8259
	out	021h, al	; 主8259, ICW3.
	call	io_delay

	mov	al, 002h	; 对应主8259的 IR2
	out	0A1h, al	; 从8259, ICW3.
	call	io_delay

	mov	al, 001h
	out	021h, al	; 主8259, ICW4.
	call	io_delay

	out	0A1h, al	; 从8259, ICW4.
	call	io_delay

	;mov	al, 11111111b	; 屏蔽主8259所有中断
	mov	al, 11111110b	; 仅仅开启定时器中断
	out	021h, al	; 主8259, OCW1.
	call	io_delay

	mov	al, 11111111b	; 屏蔽从8259所有中断
	out	0A1h, al	; 从8259, OCW1.
	call	io_delay

	ret
;* Init8259A ---------------------------------------------------------------------------------------------


;* SetRealmode8259A ---------------------------------------------------------------------------------------------
SetRealmode8259A:
	mov	ax, SelectorData
	mov	fs, ax

	mov	al, 017h
	out	020h, al	; 主8259, ICW1.
	call	io_delay

	mov	al, 008h	; IRQ0 对应中断向量 0x8
	out	021h, al	; 主8259, ICW2.
	call	io_delay

	mov	al, 001h
	out	021h, al	; 主8259, ICW4.
	call	io_delay

	mov	al, [fs:SavedIMREG]	; ┓恢复中断屏蔽寄存器(IMREG)的原值
	out	021h, al		; ┛
	call	io_delay

	ret
;* SetRealmode8259A ---------------------------------------------------------------------------------------------

io_delay:
	nop
	nop
	nop
	nop
	ret

;* int handler ---------------------------------------------------------------
_ClockHandler:
ClockHandler	equ	_ClockHandler - $$
	push eax
	mov eax, 1
	cmp [workingTask], eax
	jz .BAR

	mov byte [workingTask], 1	;! workingTask = 0时
	mov al, 20h
	out 20h, al			;* 发送 EOI
	jmp SelectorTSSA:0
	jmp .exit

.BAR:                           ;! workingTask = 1时
	mov byte [workingTask], 0
	mov al, 20h
	out 20h, al			;* 发送 EOI
	jmp SelectorTSSB:0
	jmp .exit

.exit:
	pop eax				;* 恢复eax寄存器的值
	iretd


_SpuriousHandler:
SpuriousHandler	equ	_SpuriousHandler - $$
	mov	ah, 0Ch				;* 0000: 黑底    1100: 红字
	mov	al, '!'
	mov	[gs:((80 * 0 + 75) * 2)], ax	;* 屏幕第 0 行, 第 75 列。
	jmp	$
	iretd
; ---------------------------------------------------------------------------

;* 开启分页机制 --------------------------------------------------------------
SetupPaging:
	;* 根据内存大小计算应初始化多少PDE以及多少页表
	xor	edx, edx
	mov	eax, [dwMemSize]
	mov	ebx, 400000h	; 400000h = 4M = 4096 * 1024, 一个页表对应的内存大小
	div	ebx
	mov	ecx, eax	; 此时 ecx 为页表的个数，也即 PDE 应该的个数
	test    edx, edx        ; # 测试页表除后是否有余数

	jz	.no_remainder   ; # 除尽了
	inc	ecx		; 如果余数不为 0 就需增加一个页表
.no_remainder:
	mov	[PageTableNumber], ecx	;* 暂存页表个数, ecx中存放的是页表的个数

	;* 为简化处理, 所有线性地址对应相等的物理地址. 并且不考虑内存空洞.
	;* 首先初始化第一个页目录
	mov	ax, SelectorFlatRW
	mov	es, ax
	mov	edi, PageDirBase0	;* 此段首地址为 PageDirBase0， es:edi就指向物理地址PageDirBase0处
	xor	eax, eax
	mov	eax, PageTblBase0 | PG_P  | PG_USU | PG_RWW
.1:
	stosd
	add	eax, 4096		; 为了简化, 所有页表在内存中是连续的.
	loop	.1

	;* 再初始化所有页表
	mov	eax, [PageTableNumber]	; 页表个数
	mov	ebx, 1024		; 每个页表 1024 个 PTE
	mul	ebx
	mov	ecx, eax		; PTE个数 = 页表个数 * 1024
	mov	edi, PageTblBase0	; 此段首地址为 PageTabBase1
	xor	eax, eax
	mov	eax, PG_P  | PG_USU | PG_RWW
.2:
	stosd
	add	eax, 4096		; 每一页指向 4K 的空间
	loop	.2

	mov	eax, PageDirBase0    ;* 首先让cr3寄存器指向页表PageDirBase0
	mov	cr3, eax
	mov	eax, cr0
	or	eax, 80000000h       ;* 设置cr0寄存器的最高位来打开分页机制
	mov	cr0, eax
	jmp	short .3
.3:
	nop

    ;* 初始化第二个页目录和页表
	mov	edi, PageDirBase1	; 此段首地址为 PageDirBase2
	xor	eax, eax
	mov	eax, PageTblBase1 | PG_P  | PG_USU | PG_RWW
	mov	ecx, [PageTableNumber]
.4:
	stosd
	add	eax, 4096		; 为了简化, 所有页表在内存中是连续的.
	loop	.4

	; 再初始化所有页表
	mov	eax, [PageTableNumber]	; 页表个数
	mov	ebx, 1024		; 每个页表 1024 个 PTE
	mul	ebx
	mov	ecx, eax		; PTE个数 = 页表个数 * 1024
	mov	edi, PageTblBase1	; 此段首地址为 PageTabBase2
	xor	eax, eax
	mov	eax, PG_P  | PG_USU | PG_RWW
.5:
	stosd
	add	eax, 4096		; 每一页指向 4K 的空间
	loop	.5

    ; 在此假设内存是大于 8M 的
	mov	eax, LinearAddrDemo
	shr	eax, 22    ;* 提取前10位
	mov	ebx, 4096
	mul	ebx
	mov	ecx, eax
	mov	eax, LinearAddrDemo
	shr	eax, 12     ;* 提取中间10位
	and	eax, 03FFh	;* 1111111111b (10 bits)
	mov	ebx, 4
	mul	ebx
	add	eax, ecx 
	add	eax, PageTblBase1   ;* 改变特定的线性地址和物理地址的映射关系
	mov	dword [es:eax], ProcBar | PG_P | PG_USU | PG_RWW

    nop

	ret
;* 分页机制启动完毕 ----------------------------------------------------------


;* Foo -----------------------------------------------------------------------
Foo:
OffsetFoo	equ	Foo - $$
	mov	ax, SelectorVideo
	mov	gs, ax			

    mov	ah, 0Ch				; 0000: 黑底    1100: 红字
	mov	al, 'H'
	mov	[gs:((80 * 17 + 0) * 2)], ax	; 屏幕第 17 行, 第 0 列。
	mov	al, 'U'
	mov	[gs:((80 * 17 + 1) * 2)], ax	; 屏幕第 17 行, 第 1 列。
    mov al, 'S'
	mov	[gs:((80 * 17 + 2) * 2)], ax	; 屏幕第 17 行, 第 2 列。
    mov al, 'T'
	mov	[gs:((80 * 17 + 3) * 2)], ax	; 屏幕第 17 行, 第 3 列

        retf
LenFoo	equ	$ - Foo
;* ---------------------------------------------------------------------------


;* Bar -----------------------------------------------------------------------
Bar:
OffsetBar	equ	Bar - $$
	mov	ax, SelectorVideo
	mov	gs, ax			
    mov	ah, 0Ch				; 0000: 黑底    1100: 红字
	mov	al, 'Y'
	mov	[gs:((80 * 17 + 0) * 2)], ax	; 屏幕第 17 行, 第 0 列。
	mov	al, 'U'
	mov	[gs:((80 * 17 + 1) * 2)], ax	; 屏幕第 17 行, 第 1 列。
	mov	al, 'A'
	mov	[gs:((80 * 17 + 2) * 2)], ax	; 屏幕第 17 行, 第 2 列。
    mov	al, 'N'
	mov	[gs:((80 * 17 + 3) * 2)], ax	; 屏幕第 17 行, 第 3 列。 
    retf
LenBar	equ	$ - Bar
; ---------------------------------------------------------------------------


%include	"lib.inc"
SegCode32Len	equ	$ - LABEL_SEG_CODE32
;* END of [SECTION .s32]


;*ANCHOR - 调用门的目标代码段
;??? The place is right?
[SECTION .sdest]
[BITS   32]
LABEL_SEG_CODE_DEST:      ;* ring0级代码段
    mov ax, SelectorVideo
    mov gs, ax
    mov edi, (80 * 12 + 0) * 2
    mov ah, 0Ch
    mov al, 'C'
    mov [gs:edi], ax

    mov ax, SelectorLDTA    ;TODO: 使用调用门后跳到局部任务的代码段
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
ALIGN	32
LABEL_LDT_A:
;*                                   段基址,         段界限,      属性
LABEL_LDT_DESC_CODE_A:	Descriptor      0,      CodeALen-1,     DA_C + DA_32	; Code, 32 位
LABEL_LDT_DESC_STACK_A: Descriptor      0,      TopOfStackA,    DA_DRWA | DA_32 ; Stack

LDTALen		equ	$ - LABEL_LDT_A

;* LDT_A 选择子
SelectorLDTCodeA        equ LABEL_LDT_DESC_CODE_A - LABEL_LDT_A + SA_TIL
SelectorLDTStackA       equ LABEL_LDT_DESC_STACK_A - LABEL_LDT_A + SA_TIL
;* End of [SECTION .ldt]


;*ANCHOR - LDT的栈段 StackA
[SECTION .ls]
ALIGN	32
[BITS	32]
LABEL_LDT_STACK_A:
	times 512 db 0
TopOfStackA	equ	$ - LABEL_LDT_STACK_A - 1
;* End of LDT stack


;*ANCHOR - LDT的代码段 CodeA(LDT, 32位代码段）
[SECTION .lc]
ALIGN	32
[BITS	32]
LABEL_CODE_A:
.loop:
    call	SelectorFlatC:LinearAddrDemo
    jmp     .loop

	; mov ax, SelectorVideo
    ; mov gs, ax

    ; mov edi, (80 * 13 + 0) * 2   ;* 屏幕第十行，第0列
    ; mov ah, 0Ch
    ; mov al, 'L'
    ; mov [gs:edi], ax

    ; ; ;* 跳到16位代码段然后跳回实模式
    ; jmp SelectorCode16:0
    ; ; mov ax, SelectorLDT2
    ; ; lldt ax
    ; ; jmp SelectorLDT2CodeA:0   ;? 能否跳到第二个局部任务? -> OK

CodeALen	equ	$ - LABEL_CODE_A
;* End of [SECTION .lc]


;*ANCHOR - LDT2
[SECTION .ldt2]
ALIGN	32
LABEL_LDT_B:
;                                   段基址,         段界限,      属性
LABEL_LDT_DESC_CODE_B:	Descriptor      0,      CodeBLen-1,     DA_C + DA_32	; Code, 32 位
LABEL_LDT_DESC_STACK_B: Descriptor      0,      TopOfStackB,    DA_DRWA | DA_32 ; Stack 

LDTBLen		equ	$ - LABEL_LDT_B

;* LDT_B 选择子
SelectorLDTCodeB	equ	LABEL_LDT_DESC_CODE_B - LABEL_LDT_B + SA_TIL
SelectorLDTStackB   equ LABEL_LDT_DESC_STACK_B - LABEL_LDT_B + SA_TIL
;* End of [SECTION .ldt2]


;*ANCHOR - LDT2的栈段 StackA2(LDT2的32位栈段)
[SECTION .ls2]
ALIGN	32
[BITS	32]
LABEL_LDT_STACK_B:
	times 512 db 0
TopOfStackB	equ	$ - LABEL_LDT_STACK_B - 1
;* End of LDT2 stack 

;*ANCHOR - LDT2的代码段 CodeA2(LDT2的32位代码段）
[SECTION .lc2]
ALIGN	32
[BITS	32]
LABEL_CODE_B:
.loop:
        call	SelectorFlatC:LinearAddrDemo
        jmp     .loop
CodeBLen	equ	$ - LABEL_CODE_B
;* End of [SECTION .lc2]


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