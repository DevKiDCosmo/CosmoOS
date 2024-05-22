org 0x7C00
bits 16

%define ENDL 0x0D, 0x0A

; FAT12 Boot Sector
jmp short start
nop

bdb_oem:                    db 'MSWIN4.1'           ; 8 bytes
bdb_bytes_per_sector:       dw 512
bdb_sectors_per_cluster:    db 1
bdb_reserved_sectors:       dw 1
bdb_fat_count:              db 2
bdb_dir_entries_count:      dw 0E0h
bdb_total_sectors:          dw 2880                 ; 2880 * 512 = 1.44MB
bdb_media_descriptor_type:  db 0F0h                 ; F0 = 3.5" floppy disk
bdb_sectors_per_fat:        dw 9                    ; 9 sectors/fat
bdb_sectors_per_track:      dw 18
bdb_heads:                  dw 2
bdb_hidden_sectors:         dd 0
bdb_large_sector_count:     dd 0

; extended boot record
ebr_drive_number:           db 0                    ; 0x00 floppy, 0x80 hdd, useless
                            db 0                    ; reserved
ebr_signature:              db 29h
ebr_volume_id:              db 42h, 41h, 62h, 45h   ; Anithing you want Serial Number
ebr_volume_label:           db 'COSMO OS   '        ; 11 bytes. Anything you want
ebr_file_system:            db 'FAT12   '           ; 8 bytes. Anything you want

start:

	; setup data segments
	mov ax, 0           ; can't write to ds/es directly
	mov ds, ax
	mov es, ax

	; setup stack
	mov ss, ax
	mov sp, 0x7C00      ; stack grows downwards from where we are loaded in memory

	; some BIOSes might start at 07C0:0000 instead of 0000:7C00. Make sure to be at expected location
	push es
	push word .after
	retf

.after:
	; read something from the floppy
	; BIOS set DL to drive number
	mov [ebr_drive_number], dl
	
	; print loading
	mov si, msg_loading
	call puts

	; read drive parameters instead of relying on data on formatted disk
	push es
	mov ah, 08h
	int 13h
	jc floppy_error
	pop es

	and cl, 0x3F ; remove top 2 bits
	xor ch, ch ; set ch to 0
	mov [bdb_sectors_per_track], cx ; sector count

	inc dh
	mov [bdb_heads], dh ; head count

	; read Fat root directory
	mov ax, [bdb_sectors_per_fat] ; LBA of root directory = reserved + fats * sectors_per_fat
	mov bl, [bdb_fat_count]
	xor bh, bh
	mul bx          ; ax = (sectors_per_fat * fat_count)
	add ax, [bdb_reserved_sectors] ; ax = (sectors_per_fat * fat_count) + reserved
	push ax

	; compute size of root directory = (32 * number_of_entries) / bytes_per_sector
	mov ax, [bdb_sectors_per_fat]
	shl ax, 5
	xor dx, dx
	div word [bdb_bytes_per_sector]

	test dx, dx
	jz .root_dir_after
	inc ax ; division remainder != 0, add 1

.root_dir_after:

	; read root directory
	mov cl, al ; cl = number of sectors to read
	pop ax     ; ax = LBA of root directory
	mov dl, [ebr_drive_number] ; dl = drive number
	mov bx, buffer             ; es:bx = buffer
	call disk_read

	; search for kernel.bin
	xor bx, bx
	mov di, buffer

.search_kernel:
	mov si, file_stage2_bin
	mov cx, 11 ; compare 11 bytes
	push di
	repe cmpsb
	pop di
	je .found_kernel

	add di, 32
	inc bx
	cmp bx, [bdb_dir_entries_count]
	jl .search_kernel

	jmp kernel_not_found_error

.found_kernel:

	; di should have the addr to the kernel
	mov ax, [di + 26] ; first logical cluster field (offset 26)
	mov [stage2_cluster], ax

	; load Fat from disk into memory
	mov ax, [bdb_reserved_sectors] ; LBA of first fat = reserved
	mov bx, buffer
	mov cl, [bdb_sectors_per_fat] ; number of sectors to read
	mov dl, [ebr_drive_number]
	call disk_read

	; read kernel and process FAT chain
	mov bx, KERNEL_LOAD_SEGMENT
	mov es, bx
	mov bx, KERNEL_LOAD_OFFSET

.load_kernel_load:
	; Read next cluster
	mov ax, [stage2_cluster]

	; Hardcoded values for FAT12. Fixed later in future.
	add ax, 31 ; first cluster = (cluster - 2) * sectors_per_cluster + data_start
	; start sector = reserved + fat_count * sectors_per_fat + (cluster - 2) * sectors_per_cluster
	mov cl, 1
	mov dl, [ebr_drive_number]
	call disk_read

	add bx, [bdb_bytes_per_sector]

	; compute location of next cluster
	mov ax, [stage2_cluster]
	mov cx, 3
	mul cx
	mov cx, 2
	div cx ; ax = index of FAT entry

	mov si, buffer
	add si, ax
	mov ax, [ds:si] ; read entry from FAT table

	or dx, dx
	jz .even

.odd:
	shr ax, 4
	jmp .next_cluster_after

.even:
	and ax, 0x0FFF

.next_cluster_after:
	cmp ax, 0x0FF8
	jae .read_finish

	mov [stage2_cluster], ax
	jmp .load_kernel_load

.read_finish:
	; jump to kernel
	mov dl, [ebr_drive_number] ; boot device in dl
	;set segment registers 
	mov ax, KERNEL_LOAD_SEGMENT
	mov ds, ax
	mov es, ax

	jmp KERNEL_LOAD_SEGMENT:KERNEL_LOAD_OFFSET

	; should never reach here
	jmp wait_key_and_reboot

	cli ; disable interrupts
	hlt

; Error handling
floppy_error:
	mov si, msg_read_failed
	call puts
	jmp wait_key_and_reboot

kernel_not_found_error:
	mov si, msg_stage2_not_found
	call puts
	jmp wait_key_and_reboot

wait_key_and_reboot:
	mov ah, 0
	int 16h        ; wait for key press
	jmp 0FFFFh:0   ; reboot

.halt:
	cli         ; disable interrupts
	jmp .halt

; prints a string to the screen.
; Params: - ds:si points to string
puts:
	; save registers we will modify
	push si
	push ax
	push bx

.loop:
	lodsb               ; loads next character in al
	or al, al           ; check if next char is null
	jz .done

	mov ah, 0x0e        ; Call Bios Interrupt
	mov bh, 0
	int 0x10
	jmp .loop

.done:
	pop bx
	pop ax
	pop si
	ret

; Disk routines

; Converts an LBA addr to a CHS addr
; Params: - ax: LBA addr
; Return: - cx [bits 0-5]: sector number
;         - cx [bits 6-15]: cylinder number
; 			  - dh: head number

lba_to_chs:
	push ax
	push dx

	xor dx, dx                       ; dx = 0
	div word [bdb_sectors_per_track] ; ax = LBA / SPT
			                             ; dx = LBA % SPT
	inc dx                           ; dx = LBA / SPT + 1 = sector
	mov cx, dx                       ; cx = dx

	xor dx, dx                       ; dx = 0
	div word [bdb_heads]             ; ax = LBA / (SPT * HPC) = cylinder number
			                             ; dx = LBA % (SPT * HPC) = head

	mov dh, dl                       ; dh = head
	mov ch, al											 ; ch = cylinder low byte
	shl ah, 6
	or cl, ah                        ; put upper 2 bits of cylinder in cl

	pop ax
	mov dl, al ; Restore DL
	pop ax
	ret

; Reads sectors from a disk
; Params:
;   - ax: LBA addr
;   - cl: number of sectors to read
;   - dl: drive number
;   - es:bx: mem addr to read to
disk_read:

	push ax ; Save register
	push bx
	push cx
	push dx
	push di

	push cx                          ; Don't remove else it won't work.
	call lba_to_chs                  ; compute CHS addr
	pop ax													 ; AX = number of sector
	
	mov ah, 02h
	mov di, 3  										   ; retry count

.retry:
	pusha                            ; save registers
	stc                              ; set carry flag
	int 13h                          ; carry flag = cleared
	jnc .done                        ; jump if no error
	
	; read failed
	popa
	call disk_reset

	dec di
	test di, di
	jnz .retry

.fail:
	; all attemps are exhausted
	jmp floppy_error

.done:
	popa

	pop di
	pop dx
	pop cx
	pop bx
	pop ax ; restore register
	ret

; Reset disk controller
; Params: dl: drive number
disk_reset:
	pusha
	mov ah, 0
	stc
	int 13h
	jc floppy_error
	popa
	ret

msg_loading:          db 'Loading...', ENDL, 0
msg_stage2_not_found: db 'STAGE2.BIN not found.', ENDL, 0
msg_read_failed:      db 'Read failed', ENDL, 0
file_stage2_bin:      db 'STAGE2  BIN',
stage2_cluster:       dw 0

KERNEL_LOAD_SEGMENT  equ 0x2000
KERNEL_LOAD_OFFSET   equ 0

times 510-($-$$) db 0
dw 0AA55h

buffer:
