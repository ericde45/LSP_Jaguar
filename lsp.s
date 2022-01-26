; LSP sur Jaguar
;
;	- routine de replay Paula en I2S
;	- routine de replay Paula en Timer 1
;	- convertir player LSP en Timer 1
;	- gérer le changement de bpm dans le timer 1
; OK - init : signed 8 bits samples => unsigned 8 bits samples
; OK - LSP Init : longueur sample en bytes
; OK - mettre en place la console


; - Timer 1 pour gestion LSP
; - I2S pour replay samples
;
; calcul bpm: FRQ=24/(60/bpm)= => 125 bpm=50hz
; 
; samples Amiga = 8 bits signed PCM

;CC (Carry Clear) = %00100
;CS (Carry Set)   = %01000
;EQ (Equal)       = %00010
;MI (Minus)       = %11000
;NE (Not Equal)   = %00001
;PL (Plus)        = %10100
;HI (Higher)      = %00101
;T (True)         = %00000

	include	"jaguar.inc"


CLEAR_BSS			.equ			1									; 1=efface toute la BSS jusqu'a la fin de la ram centrale
LSP_DSP_Audio_frequence					.equ			36000				; real hardware needs lower sample frequencies than emulators !


display_infos_debug				.equ			0
DSP_DEBUG						.equ			0
I2S_during_Timer1				.equ			0									; 0= I2S waits while timer 1 / 1=IMASK cleared while Timer 1
LSP_avancer_module				.equ			1									; 1=incremente position dans le module

channel_1		.equ		1
channel_2		.equ		1
channel_3		.equ		1
channel_4		.equ		1

; ----------------------------
; parametres affichage
;ob_liste_originale			equ		(ENDRAM-$4000)							; address of list (shadow)
ob_list_courante			equ		((ENDRAM-$4000)+$2000)				; address of read list
nb_octets_par_ligne			equ		320
nb_lignes					equ		256

curseur_Y_min		.equ		8


DSP_STACK_SIZE	equ	32	; long words
DSP_USP			equ		(D_ENDRAM-(4*DSP_STACK_SIZE))
DSP_ISP			equ		(DSP_USP-(4*DSP_STACK_SIZE))

.opt "~Oall"

.text



			.68000

	move.l		#INITSTACK, sp	
	move.w		#%0000011011000111, VMODE			; 320x256
	move.w		#$100,JOYSTICK

; clear BSS

	.if			CLEAR_BSS=1
	lea			DEBUT_BSS,a0
	lea			FIN_RAM,a1
	moveq		#0,d0
	
boucle_clean_BSS:
	move.b		d0,(a0)+
	cmp.l		a0,a1
	bne.s		boucle_clean_BSS
	.endif

; init LSP

	lea		LSP_module_music_data,a0
	lea		LSP_module_sound_bank,a1
	jsr		LSP_PlayerInit

;		Out : 	a0: music BPM pointer (16bits).w
;				d0: music len in tick count


;check ntsc ou pal:

	moveq		#0,d0
	move.w		JOYBUTS ,d0

	move.l		#26593900,frequence_Video_Clock			; PAL
	move.l		#415530,frequence_Video_Clock_divisee

	
	btst		#4,d0
	beq.s		jesuisenpal
jesuisenntsc:
	move.l		#26590906,frequence_Video_Clock			; NTSC
	move.l		#415483,frequence_Video_Clock_divisee
jesuisenpal:

    bsr     InitVideo               	; Setup our video registers.

	jsr     copy_olist              	; use Blitter to update active list from shadow

	move.l	#ob_list_courante,d0					; set the object list pointer
	swap	d0
	move.l	d0,OLP

	lea		CLUT,a2
	move.l	#255-2,d7
	moveq	#0,d0
	
copie_couleurs:
	move.w	d0,(a2)+
	addq.l	#5,d0
	dbf		d7,copie_couleurs

	lea		CLUT+2,a2
	move.w	#$F00F,(a2)+
	
	move.l  #VBL,LEVEL0     	; Install 68K LEVEL0 handler
	move.w  a_vde,d0                	; Must be ODD
	sub.w   #16,d0
	ori.w   #1,d0
	move.w  d0,VI

	move.w  #%01,INT1                 	; Enable video interrupts 11101


	;and.w   #%1111100011111111,sr				; 1111100011111111 => bits 8/9/10 = 0
	and.w   #$f8ff,sr

; CLS
	moveq	#0,d0
	bsr		print_caractere

; init DSP

	lea		chaine_LSP,a0
	bsr		print_string

	move.l	#0,D_CTRL

; copie du code DSP dans la RAM DSP

	lea		YM_DSP_debut,A0
	lea		D_RAM,A1
	move.l	#YM_DSP_fin-DSP_base_memoire,d0
	lsr.l	#2,d0
	sub.l	#1,D0
boucle_copie_bloc_DSP:
	move.l	(A0)+,(A1)+
	dbf		D0,boucle_copie_bloc_DSP




	; set timers
	move.l		#LSP_DSP_Audio_frequence,d0
	move.l		frequence_Video_Clock_divisee,d1
	lsl.l		#8,d1
	divu		d0,d1
	and.l		#$ffff,d1
	add.l		#128,d1			; +0.5 pour arrondir
	lsr.l		#8,d1
	subq.l		#1,d1
	move.l		d1,DSP_parametre_de_frequence_I2S

;calcul inverse
 	addq.l	#1,d1
	add.l	d1,d1		; * 2 
	add.l	d1,d1		; * 2 
	lsl.l	#4,d1		; * 16
	move.l	frequence_Video_Clock,d0
	divu	d1,d0			; 26593900 / ( (16*2*2*(+1))
	and.l		#$ffff,d0
	move.l	d0,DSP_frequence_de_replay_reelle_I2S


; launch DSP

	move.l	#REGPAGE,D_FLAGS
	move.l	#DSP_routine_init_DSP,D_PC
	move.l	#DSPGO,D_CTRL
	move.l	#0,vbl_counter





; calcul RAM DSP
	move.l		#D_ENDRAM,d0
	sub.l		debut_ram_libre_DSP,d0
	
	move.l		a0,-(sp)
	lea			chaine_RAM_DSP,a0
	bsr			print_string
	move.l		(sp)+,a0
	
	bsr			print_nombre_4_chiffres
; ligne suivante
	moveq		#10,d0
	bsr			print_caractere

	move.b		#85,couleur_char

; replay frequency
	move.l		a0,-(sp)
	lea			chaine_replay_frequency,a0
	bsr			print_string
	move.l		(sp)+,a0

	move.l		DSP_frequence_de_replay_reelle_I2S,d0
	bsr			print_nombre_5_chiffres

	move.l		a0,-(sp)
	lea			chaine_Hz_init_LSP,a0
	bsr			print_string
	move.l		(sp)+,a0

	move.b		#145,couleur_char
	
	move.l		a0,-(sp)
	lea			chaine_playing_LSP,a0
	bsr			print_string
	move.l		(sp)+,a0

	move.b		#245,couleur_char

	lea			chaine_entete_debug_module,a0
	bsr			print_string


toto:
	lea		LSPVars,a6
	lea		m_lspInstruments(A6),A6

; affiche les registres internes
	move.l	LSP_DSP_PAULA_internal_location0,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_increment0,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_offset0,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_length0,d0
	bsr		print_nombre_hexa_8_chiffres

	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

	move.l	LSP_DSP_PAULA_internal_location1,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_increment1,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_offset1,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_length1,d0
	bsr		print_nombre_hexa_8_chiffres


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

	move.l	LSP_DSP_PAULA_internal_location2,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_increment2,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_offset2,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_length2,d0
	bsr		print_nombre_hexa_8_chiffres


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere



	move.l	LSP_DSP_PAULA_internal_location3,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_increment3,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_offset3,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_length3,d0
	bsr		print_nombre_hexa_8_chiffres


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

; saute une ligne
	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere


	lea			chaine_entete_debug_module2,a0
	bsr			print_string


; affiche les registres externes
	move.l	LSP_DSP_PAULA_AUD0L,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD0LEN,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD0PER,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD0VOL,d0
	bsr		print_nombre_hexa_8_chiffres
	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

	move.l	LSP_DSP_PAULA_AUD1L,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD1LEN,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD1PER,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD1VOL,d0
	bsr		print_nombre_hexa_8_chiffres
	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

	move.l	LSP_DSP_PAULA_AUD2L,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD2LEN,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD2PER,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD2VOL,d0
	bsr		print_nombre_hexa_8_chiffres
	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

	move.l	LSP_DSP_PAULA_AUD3L,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD3LEN,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD3PER,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD3VOL,d0
	bsr		print_nombre_hexa_8_chiffres
	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere
	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere


	move.l	LSP_DSP_repeat_length0,d0
	bsr		print_nombre_hexa_8_chiffres

	.rept	11
	; retour a la ligne	 au dessus
	moveq	#8,d0
	bsr		print_caractere
	.endr


	bra			toto

	stop		#$2700


;-----------------------------------------------------------------------------------
;--------------------------
; VBL

VBL:
                movem.l d0-d7/a0-a6,-(a7)
				
				.if		display_infos_debug=1
				add.w		#1,BG					; debug pour voir si vivant
				.endif

                jsr     copy_olist              	; use Blitter to update active list from shadow

                addq.l	#1,vbl_counter

                ;move.w  #$101,INT1              	; Signal we're done
				move.w	#$101,INT1
                move.w  #$0,INT2
.exit:
                movem.l (a7)+,d0-d7/a0-a6
                rte

; ---------------------------------------
; imprime une chaine terminée par un zéro
; a0=pointeur sur chaine
print_string:
	movem.l d0-d7/a0-a6,-(a7)	

print_string_boucle:
	moveq	#0,d0
	move.b	(a0)+,d0
	cmp.w	#0,d0
	bne.s	print_string_pas_fin_de_chaine
	movem.l (a7)+,d0-d7/a0-a6
	rts
print_string_pas_fin_de_chaine:
	bsr		print_caractere
	bra.s	print_string_boucle

; ---------------------------------------
; imprime un nombre HEXA de 2 chiffres
print_nombre_hexa_2_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	lea		convert_hexa,a0
	move.l		d0,d1
	divu		#16,d0
	and.l		#$F,d0			; limite a 0-15
	move.l		d0,d2
	mulu		#16,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	move.l		d1,d0
	and.l		#$F,d0			; limite a 0-15
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts
	
convert_hexa:
	dc.b		48,49,50,51,52,53,54,55,56,57
	dc.b		65,66,67,68,69,70
	
; ---------------------------------------
; imprime un nombre de 2 chiffres
print_nombre_2_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	move.l		d0,d1
	divu		#10,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#10,d2
	sub.l		d2,d1
	cmp.l		#0,d0
	beq.s		.zap
	add.l		#48,d0
	bsr			print_caractere
.zap:
	move.l		d1,d0
	add.l		#48,d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts

; ---------------------------------------
; imprime un nombre de 3 chiffres
print_nombre_3_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	move.l		d0,d1

	divu		#100,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#100,d2
	sub.l		d2,d1
	cmp.l		#0,d0
	beq.s		.zap
	add.l		#48,d0
	bsr			print_caractere
.zap:
	move.l		d1,d0	
	divu		#10,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#10,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere
	
	move.l		d1,d0
	add.l		#48,d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts


; ---------------------------------------
; imprime un nombre de 2 chiffres , 00
print_nombre_2_chiffres_force:
	movem.l d0-d7/a0-a6,-(a7)
	move.l		d0,d1
	divu		#10,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#10,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere
	move.l		d1,d0
	add.l		#48,d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts

; ---------------------------------------
; imprime un nombre de 4 chiffres HEXA
print_nombre_hexa_4_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	move.l		d0,d1
	lea		convert_hexa,a0

	divu		#4096,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#4096,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere

	move.l		d1,d0
	divu		#256,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#256,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere


	move.l		d1,d0
	divu		#16,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#16,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	move.l		d1,d0
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts

; ---------------------------------------
; imprime un nombre de 6 chiffres HEXA ( pour les adresses memoire)
print_nombre_hexa_6_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	
	move.l		d0,d1
	lea		convert_hexa,a0

	move.l		d1,d0
	swap		d0
	and.l		#$F0,d0
	divu		#16,d0
	and.l		#$F,d0
	move.b		(a0,d0.w),d0
	and.l		#$FF,d0
	bsr			print_caractere

	move.l		d1,d0
	swap		d0
	and.l		#$F,d0
	move.b		(a0,d0.w),d0
	and.l		#$FF,d0
	bsr			print_caractere

	and.l		#$FFFF,d1
	move.l		d1,d0
	divu		#4096,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#4096,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere

	move.l		d1,d0
	divu		#256,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#256,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere


	move.l		d1,d0
	divu		#16,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#16,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	move.l		d1,d0
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts

; ---------------------------------------
; imprime un nombre de 8 chiffres HEXA ( pour les adresses memoire et les données en 16:16)
print_nombre_hexa_8_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	
	move.l		d0,d1
	lea		convert_hexa,a0

	move.l		d1,d0
	swap		d0
	and.l		#$F000,d0
	divu		#4096,d0
	and.l		#$F,d0
	move.b		(a0,d0.w),d0
	and.l		#$FF,d0
	bsr			print_caractere



	move.l		d1,d0
	swap		d0
	and.l		#$F00,d0
	divu		#256,d0
	and.l		#$F,d0
	move.b		(a0,d0.w),d0
	and.l		#$FF,d0
	bsr			print_caractere


	move.l		d1,d0
	swap		d0
	and.l		#$F0,d0
	divu		#16,d0
	and.l		#$F,d0
	move.b		(a0,d0.w),d0
	and.l		#$FF,d0
	bsr			print_caractere

	move.l		d1,d0
	swap		d0
	and.l		#$F,d0
	move.b		(a0,d0.w),d0
	and.l		#$FF,d0
	bsr			print_caractere

	and.l		#$FFFF,d1
	move.l		d1,d0
	divu		#4096,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#4096,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere

	move.l		d1,d0
	divu		#256,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#256,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere


	move.l		d1,d0
	divu		#16,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#16,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	move.l		d1,d0
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts


; ---------------------------------------
; imprime un nombre de 4 chiffres
print_nombre_4_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	move.l		d0,d1

	divu		#1000,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#1000,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere

	move.l		d1,d0
	divu		#100,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#100,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere


	move.l		d1,d0
	divu		#10,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#10,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere
	move.l		d1,d0
	add.l		#48,d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts

; ---------------------------------------
; imprime un nombre de 5 chiffres
print_nombre_5_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	move.l		d0,d1

	divu		#10000,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#10000,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere

	move.l		d1,d0
	divu		#1000,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#1000,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere

	move.l		d1,d0
	divu		#100,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#100,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere


	move.l		d1,d0
	divu		#10,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#10,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere
	move.l		d1,d0
	add.l		#48,d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts


; -----------------------------
; copie un caractere a l ecran
; d0.w=caractere

print_caractere:
	movem.l d0-d7/a0-a6,-(a7)



	cmp.b	#00,d0
	bne.s	print_caractere_pas_CLS
	move.l	#ecran1,A1_BASE			; = DEST
	move.l	#$0,A1_PIXEL
	move.l	#PIXEL16|XADDPHR|PITCH1,A1_FLAGS
	move.l	#ecran1+320*100,A2_BASE			; = source
	move.l	#$0,A2_PIXEL
	move.l	#PIXEL16|XADDPHR|PITCH1,A2_FLAGS
	
	move.w	#$00,B_PATD
	

	moveq	#0,d0
	move.w	#nb_octets_par_ligne,d0
	lsr.w	#1,d0
	move.w	#nb_lignes,d1
	mulu	d1,d0
	swap	d0
	move.w	#1,d0
	swap	d0
	;move.w	#65535,d0
	move.l	d0,B_COUNT
	move.l	#LFU_REPLACE|SRCEN|PATDSEL,B_CMD


	movem.l (a7)+,d0-d7/a0-a6
	rts
	
print_caractere_pas_CLS:

	cmp.b	#10,d0
	bne.s	print_caractere_pas_retourchariot
	move.w	#0,curseur_x
	add.w	#8,curseur_y
	movem.l (a7)+,d0-d7/a0-a6
	rts

print_caractere_pas_retourchariot:
	cmp.b	#09,d0
	bne.s	print_caractere_pas_retourdebutligne
	move.w	#0,curseur_x
	movem.l (a7)+,d0-d7/a0-a6
	rts

print_caractere_pas_retourdebutligne:
	cmp.b	#08,d0
	bne.s	print_caractere_pas_retourdebutligneaudessus
	move.w	#0,curseur_x
	sub.w	#8,curseur_y
	movem.l (a7)+,d0-d7/a0-a6
	rts


print_caractere_pas_retourdebutligneaudessus:

	lea		ecran1,a1
	moveq	#0,d1
	move.w	curseur_x,d1
	add.l	d1,a1
	moveq	#0,d1
	move.w	curseur_y,d1
	mulu	#nb_octets_par_ligne,d1
	add.l	d1,a1

	lsl.l	#3,d0		; * 8
	lea		fonte,a0
	add.l	d0,a0
	
	
; copie 1 lettre
	move.l	#8-1,d0
copieC_ligne:
	moveq	#8-1,d1
	move.b	(a0)+,d2
copieC_colonne:
	moveq	#0,d4
	btst	d1,d2
	beq.s	pixel_a_zero
	move.b	couleur_char,d4
pixel_a_zero:
	move.b	d4,(a1)+
	dbf		d1,copieC_colonne
	lea		nb_octets_par_ligne-8(a1),a1
	dbf		d0,copieC_ligne

	move.w	curseur_x,d0
	add.w	#8,d0
	cmp.w	#320,d0
	blt		curseur_pas_fin_de_ligne
	moveq	#0,d0
	add.w	#8,curseur_y
curseur_pas_fin_de_ligne:
	move.w	d0,curseur_x

	movem.l (a7)+,d0-d7/a0-a6

	rts


;----------------------------------
; recopie l'object list dans la courante

copy_olist:
				move.l	#ob_list_courante,A1_BASE			; = DEST
				move.l	#$0,A1_PIXEL
				move.l	#PIXEL16|XADDPHR|PITCH1,A1_FLAGS
				move.l	#ob_liste_originale,A2_BASE			; = source
				move.l	#$0,A2_PIXEL
				move.l	#PIXEL16|XADDPHR|PITCH1,A2_FLAGS
				move.w	#1,d0
				swap	d0
				move.l	#fin_ob_liste_originale-ob_liste_originale,d1
				move.w	d1,d0
				move.l	d0,B_COUNT
				move.l	#LFU_REPLACE|SRCEN,B_CMD
				rts


; ---------------------------------------
; allocation de mémoire
; malloc de d0, retour avec  un pointeur dans d0
; ---------------------------------------

YM_malloc:

	movem.l		d1-d3/a0,-(sp)

	move.l		debut_ram_libre,d1
	move.l		d1,a0
	move.l		d1,d3
; arrondit multiple de 2
    btst		#0,d0
	beq.s		YM_malloc_pas_d_arrondi
	addq.l		#1,d0
YM_malloc_pas_d_arrondi:
	add.l		d0,d1
	move.l		d1,debut_ram_libre
	
	move.l		d0,d2
	subq.l		#1,d2
	moveq.l		#0,d0

YM_malloc_boucle_clean_ram:
	move.b		d0,(a0)+
	dbf			d2,YM_malloc_boucle_clean_ram
	
	move.l		d3,d0

	movem.l		(sp)+,d1-d3/a0
	rts

; ---------------------------------------
; allocation de mémoire version RAM DSP
; malloc de d0, retour avec  un pointeur dans d0
; d0 => forcément un multiple de 4
; ---------------------------------------

YM_malloc_DSP:

	movem.l		d1-d3/a0,-(sp)

	move.l		debut_ram_libre_DSP,d1
	move.l		d1,a0
	move.l		d1,d3
	add.l		d0,d1
	move.l		d1,debut_ram_libre_DSP
	
	move.l		d0,d2
	moveq.l		#0,d0
	lsr.l		#2,d2		; 4 octets par 4 octets
	subq.l		#1,d2

YM_malloc_boucle_clean_ram_DSP:
	move.l		d0,(a0)+
	dbf			d2,YM_malloc_boucle_clean_ram_DSP
	
	move.l		d3,d0

	movem.l		(sp)+,d1-d3/a0
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Procedure: InitVideo (same as in vidinit.s)
;;            Build values for hdb, hde, vdb, and vde and store them.
;;

InitVideo:
                movem.l d0-d6,-(sp)

				
				move.w	#-1,ntsc_flag
				move.l	#50,_50ou60hertz
	
				move.w  CONFIG,d0                ; Also is joystick register
                andi.w  #VIDTYPE,d0              ; 0 = PAL, 1 = NTSC
                beq     .palvals
				move.w	#1,ntsc_flag
				move.l	#60,_50ou60hertz
	

.ntscvals:		move.w  #NTSC_HMID,d2
                move.w  #NTSC_WIDTH,d0

                move.w  #NTSC_VMID,d6
                move.w  #NTSC_HEIGHT,d4
				
                bra     calc_vals
.palvals:
				move.w #PAL_HMID,d2
				move.w #PAL_WIDTH,d0

				move.w #PAL_VMID,d6				
				move.w #PAL_HEIGHT,d4

				
calc_vals:		
                move.w  d0,width
                move.w  d4,height
                move.w  d0,d1
                asr     #1,d1                   ; Width/2
                sub.w   d1,d2                   ; Mid - Width/2
                add.w   #4,d2                   ; (Mid - Width/2)+4
                sub.w   #1,d1                   ; Width/2 - 1
                ori.w   #$400,d1                ; (Width/2 - 1)|$400
                move.w  d1,a_hde
                move.w  d1,HDE
                move.w  d2,a_hdb
                move.w  d2,HDB1
                move.w  d2,HDB2
                move.w  d6,d5
                sub.w   d4,d5
                add.w   #16,d5
                move.w  d5,a_vdb
                add.w   d4,d6
                move.w  d6,a_vde
			
			    move.w  a_vdb,VDB
				move.w  a_vde,VDE    
				
				
				move.l  #0,BORD1                ; Black border
                move.w  #0,BG                   ; Init line buffer to black
                movem.l (sp)+,d0-d6
                rts

; ------------------------------------
;          LSP
; ------------------------------------


; ------------------------------------
; Init

LSP_PlayerInit:
; a0: music data (any mem)
; a1: sound bank data (chip mem)
; (a2: 16bit DMACON word address)

;		Out:a0: music BPM pointer (16bits)
;			d0: music len in tick count

			cmpi.l		#'LSP1',(a0)+
			bne			.dataError
			move.l		(a0)+,d0		; unique id
			cmp.l		(a1),d0			; check that sample bank is this one
			bne			.dataError

			lea			LSPVars,a3
			cmpi.w		#$0105,(a0)+			; minimal major & minor version of latest compatible LSPConvert.exe		 = V 1.05
			blt			.dataError
			moveq		#0,d6
			move.w		(a0)+,d6
			move.l		d6,m_currentBpm(a3)		; default BPM
			move.w		(a0)+,d6
			move.l		d6,m_escCodeRewind(a3)		; tout en .L
			move.w		(a0)+,d6
			move.l		d6,m_escCodeSetBpm(a3)
			move.l		(a0)+,-(a7)
			;move.l	a2,m_dmaconPatch(a3)
			;move.w	#$8000,-1(a2)			; Be sure DMACon word is $8000 (note: a2 should be ODD address)
			moveq		#0,d0
			move.w		(a0)+,d0				; instrument count
			lea			-12(a0),a2				; LSP data has -12 offset on instrument tab ( to win 2 cycles in fast player :) )
			move.l		a2,m_lspInstruments(a3)	; instrument tab addr ( minus 4 )
			subq.w		#1,d0
			move.l		a1,d1

.relocLoop:	
			;bset.b		#0,3(a0)				; bit0 is relocation done flag
			;bne.s		.relocated

			add.l		d1,(a0)					; pointeur sample
			move.w		4(a0),d6				; taille en words
			add.w		d6,d6
			move.w		d6,4(a0)				; taille en bytes
			move.l		(a0),a4					
			bsr			LSP_unsigne_sample		; A4=sample location / d6=length
			
			add.l		d1,6(a0)				; pointeur repeat
			move.w		10(a0),d6				; taille repeat en words
			add.w		d6,d6
			move.w		d6,10(a0)				; taille repeat en bytes

.relocated:	
			lea			12(a0),a0
			dbf			d0,.relocLoop
			
			move.w		(a0)+,d0				; codes count (+2)
			move.l		a0,m_codeTableAddr(a3)	; code table
			add.w		d0,d0
			add.w		d0,a0
			move.l		(a0)+,d0				; word stream size
			move.l		(a0)+,d1				; byte stream loop point
			move.l		(a0)+,d2				; word stream loop point

			move.l		a0,m_wordStream(a3)
			lea			0(a0,d0.l),a1			; byte stream
			move.l		a1,m_byteStream(a3)
			add.l		d2,a0
			add.l		d1,a1
			move.l		a0,m_wordStreamLoop(a3)
			move.l		a1,m_byteStreamLoop(a3)
			;bset.b		#1,$bfe001				; disabling this fucking Low pass filter!!
			lea			m_currentBpm(a3),a0
			move.l		(a7)+,d0				; music len in frame ticks
			rts

.dataError:	illegal

LSP_unsigne_sample:
; A4=sample location / d6=length
	movem.l		d0/d6/a4,-(sp)
	subq.l		#1,d6
.boucle:
	move.b		(a4),d0				; -128 to 127
	sub.b		#$80,d0
	move.b		d0,(a4)+
	dbf			d6,.boucle

	movem.l		(sp)+,d0/d6/a4
	rts
.abs
																	;		DSP index
m_byteStream:		ds.l	1	;  0 :  byte stream							0
m_wordStream:		ds.l	1	;  4 :  word stream							1
m_codeTableAddr:	ds.l	1	;  8 :  code table addr						2
m_escCodeRewind:	ds.l	1	; 12 :  rewind special escape code			3
m_escCodeSetBpm:	ds.l	1	; 16 :  set BPM escape code					4
m_lspInstruments:	ds.l	1	; 20 :  LSP instruments table addr			5
m_relocDone:		ds.l	1	; 24 :  reloc done flag						6
m_currentBpm:		ds.l	1	; 28 :  current BPM							7
m_byteStreamLoop:	ds.l	1	; 32 :  byte stream loop point				8
m_wordStreamLoop:	ds.l	1	; 36 :  word stream loop point				9
sizeof_LSPVars		equ ^^abscount

;m_dmaconPatch:		ds.l	1	;  8 m_lfmDmaConPatch




	
	
	
	.text
	.phrase
LSPVars:	ds.l		10

	
	
	
	
	
;-------------------------------------
;
;     DSP
;
;-------------------------------------

	.phrase
YM_DSP_debut:

	.dsp
	.org	D_RAM
DSP_base_memoire:

; CPU interrupt
	.rept	8
		nop
	.endr
; I2S interrupt
	movei	#DSP_LSP_routine_interruption_I2S,r28						; 6 octets
	movei	#D_FLAGS,r30											; 6 octets
	jump	(r28)													; 2 octets
	load	(r30),r29	; read flags								; 2 octets = 16 octets
; Timer 1 interrupt
	movei	#DSP_LSP_routine_interruption_Timer1,r12						; 6 octets
	movei	#D_FLAGS,r16											; 6 octets
	jump	(r12)													; 2 octets
	load	(r16),r13	; read flags								; 2 octets = 16 octets
; Timer 2 interrupt	
	movei	#DSP_LSP_routine_interruption_Timer2,r28						; 6 octets
	movei	#D_FLAGS,r30											; 6 octets
	jump	(r28)													; 2 octets
	load	(r30),r29	; read flags								; 2 octets = 16 octets
; External 0 interrupt
	.rept	8
		nop
	.endr
; External 1 interrupt
	.rept	8
		nop
	.endr













; -------------------------------
; DSP : routines en interruption
; -------------------------------
; utilisés : 	R29/R30/R31
; 				R18/R19/R20/R21 /R22/R23/R24/R25/R26/R27/R28


; I2S : replay sample
;	- version simple, lit un octet à chaque fois
;	- puis version plus compleque : lit 1 long, et utilise ses octets
DSP_LSP_routine_interruption_I2S:

	.if		DSP_DEBUG
; change la couleur du fond
	movei	#$777,R26
	movei	#BG,r27
	storew	r26,(r27)
	.endif


; version simple

; channel 0
		movei		#LSP_DSP_PAULA_internal_offset0,R28
		movei		#LSP_DSP_PAULA_internal_increment0,R27
		load		(R28),R26										; R26=current offset 16:16
		load		(R27),R27										; R27=increment 16:16
		movei		#LSP_DSP_PAULA_internal_location0,R25			
		add			R27,R26											; R26=offset+increment
		movei		#LSP_DSP_PAULA_internal_length0,R23
		load		(R25),R24										; R24=current sample start
		load		(R23),R22										; R22=currrent sample length 16:16
		cmp			R22,R26
		jr			mi,DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel0
		nop
; fin de sample => on recharge les infos des registres externes
		movei		#LSP_DSP_PAULA_AUD0L,R21			; sample location
		movei		#LSP_DSP_PAULA_AUD0LEN,R22			; length en 16:16
		load		(R21),R24
		load		(R22),R22
		store		R24,(R25)							; upsate internal sample location
		store		R22,(R23)							; update internal sample length
		shlq		#16,R26								; virgule
		shrq		#16,R26								; offset partie entiere remise à zéro
		
DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel0:
		store		R26,(R28)							; stocke internal offset en 16:16
		movei		#LSP_DSP_PAULA_AUD0VOL,R23
		shrq		#16,R26								; partie entiere
		load		(R23),R23							; R23 = volume : 6 bits
		add			R26,R24								; internal location + offset partie entiere
		loadb		(R24),R18							; R18=sample channel 0 8 bits unsigned
		mult		R23,R18								; unsigned multiplication : unsigned sample * volume => 8bits + 6 bits = 14 bits
; R18=sample channel 0 on 14 bits
		
; channel 1
		movei		#LSP_DSP_PAULA_internal_offset1,R28
		movei		#LSP_DSP_PAULA_internal_increment1,R27
		load		(R28),R26										; R26=current offset 16:16
		load		(R27),R27										; R27=increment 16:16
		movei		#LSP_DSP_PAULA_internal_location1,R25			
		add			R27,R26											; R26=offset+increment
		movei		#LSP_DSP_PAULA_internal_length1,R23
		load		(R25),R24										; R24=current sample start
		load		(R23),R22										; R22=currrent sample length 16:16
		cmp			R22,R26
		jr			mi,DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel1
		nop
; fin de sample => on recharge les infos des registres externes
		movei		#LSP_DSP_PAULA_AUD1L,R21			; sample location
		movei		#LSP_DSP_PAULA_AUD1LEN,R22			; length en 16:16
		load		(R21),R24
		load		(R22),R22
		store		R24,(R25)							; upsate internal sample location
		store		R22,(R23)							; update internal sample length
		shlq		#16,R26								; virgule
		shrq		#16,R26								; offset partie entiere remise à zéro
		
DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel1:
		store		R26,(R28)							; stocke internal offset en 16:16
		movei		#LSP_DSP_PAULA_AUD1VOL,R23
		shrq		#16,R26								; partie entiere
		load		(R23),R23							; R23 = volume : 6 bits
		add			R26,R24								; internal location + offset partie entiere
		loadb		(R24),R19							; R18=sample channel 0 8 bits unsigned
		mult		R23,R19								; unsigned multiplication : unsigned sample * volume => 8bits + 6 bits = 14 bits
; R19=sample channel 1 on 14 bits

; channel 2
		movei		#LSP_DSP_PAULA_internal_offset2,R28
		movei		#LSP_DSP_PAULA_internal_increment2,R27
		load		(R28),R26										; R26=current offset 16:16
		load		(R27),R27										; R27=increment 16:16
		movei		#LSP_DSP_PAULA_internal_location2,R25			
		add			R27,R26											; R26=offset+increment
		movei		#LSP_DSP_PAULA_internal_length2,R23
		load		(R25),R24										; R24=current sample start
		load		(R23),R22										; R22=currrent sample length 16:16
		cmp			R22,R26
		jr			mi,DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel2
		nop
; fin de sample => on recharge les infos des registres externes
		movei		#LSP_DSP_PAULA_AUD2L,R21			; sample location
		movei		#LSP_DSP_PAULA_AUD2LEN,R22			; length en 16:16
		load		(R21),R24
		load		(R22),R22
		store		R24,(R25)							; upsate internal sample location
		store		R22,(R23)							; update internal sample length
		shlq		#16,R26								; virgule
		shrq		#16,R26								; offset partie entiere remise à zéro
		
DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel2:
		store		R26,(R28)							; stocke internal offset en 16:16
		movei		#LSP_DSP_PAULA_AUD2VOL,R23
		shrq		#16,R26								; partie entiere
		load		(R23),R23							; R23 = volume : 6 bits
		add			R26,R24								; internal location + offset partie entiere
		loadb		(R24),R20							; R18=sample channel 0 8 bits unsigned
		mult		R23,R20								; unsigned multiplication : unsigned sample * volume => 8bits + 6 bits = 14 bits
; R20=sample channel 2 on 14 bits

; channel 3
		movei		#LSP_DSP_PAULA_internal_offset3,R28
		movei		#LSP_DSP_PAULA_internal_increment3,R27
		load		(R28),R26										; R26=current offset 16:16
		load		(R27),R27										; R27=increment 16:16
		movei		#LSP_DSP_PAULA_internal_location3,R25			
		add			R27,R26											; R26=offset+increment
		movei		#LSP_DSP_PAULA_internal_length3,R23
		load		(R25),R24										; R24=current sample start
		load		(R23),R22										; R22=currrent sample length 16:16
		cmp			R22,R26
		jr			mi,DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel3
		nop
; fin de sample => on recharge les infos des registres externes
		movei		#LSP_DSP_PAULA_AUD3L,R21			; sample location
		movei		#LSP_DSP_PAULA_AUD3LEN,R22			; length en 16:16
		load		(R21),R24
		load		(R22),R22
		store		R24,(R25)							; upsate internal sample location
		store		R22,(R23)							; update internal sample length
		shlq		#16,R26								; virgule
		shrq		#16,R26								; offset partie entiere remise à zéro
		
DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel3:
		store		R26,(R28)							; stocke internal offset en 16:16
		movei		#LSP_DSP_PAULA_AUD3VOL,R23
		shrq		#16,R26								; partie entiere
		load		(R23),R23							; R23 = volume : 6 bits
		add			R26,R24								; internal location + offset partie entiere
		loadb		(R24),R21							; R18=sample channel 0 8 bits unsigned
		mult		R23,R21								; unsigned multiplication : unsigned sample * volume => 8bits + 6 bits = 14 bits
; R21=sample channel 3 on 14 bits

; Stéreo Amiga:
; les canaux 0 et 3 formant la voie stéréo gauche et 1 et 2 la voie stéréo droite
; R18=channel 0
; R19=channel 1
; R20=channel 2
; R21=channel 3
		.if			channel_1=0
			moveq	#0,R18
		.endif
		.if			channel_2=0
			moveq	#0,R19
		.endif
		.if			channel_3=0
			moveq	#0,R20
		.endif
		.if			channel_4=0
			moveq	#0,R21
		.endif

		movei		#$8000,R26
		add			R21,R18				; R18 = left 15 bits unsigned
		movei		#L_I2S,R27
		add			R20,R19				; R19 = right 15 bits unsigned
		shlq		#1,R18				; 16 bits unsigned
		shlq		#1,R19
		sub			R26,R18				; 16 bits signed
		movei		#L_I2S+4,R25
		sub			R26,R19
		store		R19,(R27)			; write right channel
		store		R18,(R25)			; write left channel

		

	.if		DSP_DEBUG
; change la couleur du fond
	movei	#$000,R26
	movei	#BG,r27
	storew	r26,(r27)
	.endif


;------------------------------------	
; return from interrupt I2S
	load	(r31),r28	; return address
	bset	#10,r29		; clear latch 1 = I2S
	;bset	#11,r29		; clear latch 1 = timer 1
	;bset	#12,r29		; clear latch 1 = timer 2
	bclr	#3,r29		; clear IMASK
	addq	#4,r31		; pop from stack
	addqt	#2,r28		; next instruction
	jump	t,(r28)		; return
	store	r29,(r30)	; restore flags


;--------------------------------------------
; ---------------- Timer 1 ------------------
;--------------------------------------------
; autorise interruptions, pour timer I2S
; 
; registres utilisés :
;		R13/R16   /R31
;		R0/R1/R2/R3/R4/R5/R6/R7/R8/R9  R12/R14


DSP_LSP_routine_interruption_Timer1:
	.if		I2S_during_Timer1=1
	bclr	#3,r13		; clear IMASK
	store	r13,(r16)	; restore flags
	.endif

; gestion replay LSP

	movei		#LSPVars,R14
	load		(R14),R0					; R0 = byte stream

DSP_LSP_Timer1_process:
	moveq		#0,R2
DSP_LSP_Timer1_cloop:

	loadb		(R0),R6						; R6 = byte code
	addq		#1,R0
	
	cmpq		#0,R6
	jr			ne,DSP_LSP_Timer1_swCode
	nop
	movei		#$0100,R3
	add			R3,R2
	jr			DSP_LSP_Timer1_cloop
	nop

DSP_LSP_Timer1_swCode:
	add			R2,R6
	move		R6,R2

	add			R2,R2
	load		(R14+(m_codeTableAddr/4)),R3			; R3=code table
	add			R2,R3
	movei		#DSP_LSP_Timer1_noInst,R12
	loadw		(R3),R2									; R2 = code
	cmpq		#0,R2
	jump		eq,(R12)
	nop
	load		(R14+(m_escCodeRewind/4)),R4			; R4=escape code rewind
	movei		#DSP_LSP_Timer1_r_rewind,R12
	cmp			R4,R2
	jump		eq,(R12)
	nop
	load		(R14+(m_escCodeSetBpm/4)),R4			; R4=escape code set bpm
	movei		#DSP_LSP_Timer1_r_chgbpm,R12
	cmp			R4,R2
	jump		eq,(R12)
	nop
; test volume canal 3
	btst		#7,R2
	jr			eq,DSP_LSP_Timer1_noVd
	nop
	loadb		(R0),R4
	movei		#LSP_DSP_PAULA_AUD3VOL,R5
	addq		#1,R0
	store		R4,(R5)
DSP_LSP_Timer1_noVd:
; test volume canal 2
	btst		#6,R2
	jr			eq,DSP_LSP_Timer1_noVc
	nop
	loadb		(R0),R4
	movei		#LSP_DSP_PAULA_AUD2VOL,R5
	addq		#1,R0
	store		R4,(R5)
DSP_LSP_Timer1_noVc:
; test volume canal 1
	btst		#5,R2
	jr			eq,DSP_LSP_Timer1_noVb
	nop
	loadb		(R0),R4
	movei		#LSP_DSP_PAULA_AUD1VOL,R5
	addq		#1,R0
	store		R4,(R5)
DSP_LSP_Timer1_noVb:
; test volume canal 0
	btst		#4,R2
	jr			eq,DSP_LSP_Timer1_noVa
	nop
	loadb		(R0),R4
	movei		#LSP_DSP_PAULA_AUD0VOL,R5
	addq		#1,R0
	store		R4,(R5)
DSP_LSP_Timer1_noVa:

	.if			LSP_avancer_module=1
	store		R0,(R14)									; store byte stream ptr
	.endif
	addq		#4,R14									; avance a word stream ptr
	load		(R14),R0									; R0 = word stream

; test period canal 3
	btst		#3,R2
	jr			eq,DSP_LSP_Timer1_noPd
	nop
	loadw		(R0),R4
	movei		#LSP_DSP_PAULA_AUD3PER,R5
	addq		#2,R0
	store		R4,(R5)
DSP_LSP_Timer1_noPd:
; test period canal 2
	btst		#2,R2
	jr			eq,DSP_LSP_Timer1_noPc
	nop
	loadw		(R0),R4
	movei		#LSP_DSP_PAULA_AUD2PER,R5
	addq		#2,R0
	store		R4,(R5)
DSP_LSP_Timer1_noPc:
; test period canal 1
	btst		#1,R2
	jr			eq,DSP_LSP_Timer1_noPb
	nop
	loadw		(R0),R4
	movei		#LSP_DSP_PAULA_AUD1PER,R5
	addq		#2,R0
	store		R4,(R5)
DSP_LSP_Timer1_noPb:
; test period canal 0
	btst		#0,R2
	jr			eq,DSP_LSP_Timer1_noPa
	nop
	loadw		(R0),R4
	movei		#LSP_DSP_PAULA_AUD0PER,R5
	addq		#2,R0
	store		R4,(R5)
DSP_LSP_Timer1_noPa:

; pas de test des 8 bits du haut en entier pour zapper la lecture des instruments
; tst.w	d0							; d0.w, avec d0.b qui a avancé ! / beq.s	.noInst

	load		(R14+((m_lspInstruments-4)/4)),R5		; R5= instrument table  ( =+$10)  = a2

;--- test instrument voie 3
	movei		#DSP_LSP_Timer1_setIns3,R12
	btst		#15,R2
	jump		ne,(R12)
	nop
	
	movei		#DSP_LSP_Timer1_skip3,R12
	btst		#14,R2
	jump		eq,(R12)
	nop

; repeat voie 3	
	movei		#LSP_DSP_repeat_pointeur3,R3
	movei		#LSP_DSP_repeat_length3,R4
	load		(R3),R3					; pointeur sauvegardé, sur infos de repeats
	load		(R4),R4
	movei		#LSP_DSP_PAULA_AUD3L,R7
	movei		#LSP_DSP_PAULA_AUD3LEN,R8
	store		R3,(R7)
	store		R4,(R8)					; stocke le pointeur sample de repeat dans LSP_DSP_PAULA_AUD3L
	jump		(R12)				; jump en DSP_LSP_Timer1_skip3
	nop

DSP_LSP_Timer1_setIns3:
	loadw		(R0),R3				; offset de l'instrument par rapport au precedent
; addition en .w
	btst		#15,R3
	jr			eq,.positif3
	nop
	movei		#$FFFF0000,R7
	or			R7,R3

.positif3:
	add			R3,R5				;R5=pointeur datas instruments
	addq		#2,R0


	movei		#LSP_DSP_PAULA_AUD3L,R7
	loadw		(R5),R6
	addq		#2,R5
	shlq		#16,R6
	loadw		(R5),R8
	or			R8,R6
	movei		#LSP_DSP_PAULA_AUD3LEN,R8
	store		R6,(R7)				; stocke le pointeur sample dans LSP_DSP_PAULA_AUD3L
	addq		#2,R5
	loadw		(R5),R9				; .w = R9 = taille du sample
	shlq		#16,R9				; en 16:16
	store		R9,(R8)				; stocke la nouvelle taille
	addq		#2,R5				; positionne sur pointeur de repeat
; repeat pointeur
	movei		#LSP_DSP_repeat_pointeur3,R7
	loadw		(R5),R4
	addq		#2,R5
	shlq		#16,R4
	loadw		(R5),R8
	or			R8,R4
	addq		#2,R5
	store		R4,(R7)
; repeat length
	movei		#LSP_DSP_repeat_length3,R7
	loadw		(R5),R8				; .w = R8 = taille du sample
	shlq		#16,R8				; en 16:16
	store		R8,(R7)				; stocke la nouvelle taille
	subq		#4,R5
	
; test le reset pour prise en compte immediate du changement de sample
	movei		#DSP_LSP_Timer1_noreset3,R12
	btst		#14,R2
	jump		eq,(R12)
	nop
; reset a travers le dmacon, il faut rafraichir : LSP_DSP_PAULA_internal_location3 & LSP_DSP_PAULA_internal_length3 & LSP_DSP_PAULA_internal_offset3=0
	movei		#LSP_DSP_PAULA_internal_location3,R7
	movei		#LSP_DSP_PAULA_internal_length3,R8
	store		R6,(R7)				; stocke le pointeur sample dans LSP_DSP_PAULA_internal_location3
	moveq		#0,R12
	movei		#LSP_DSP_PAULA_internal_offset3,R7
	store		R9,(R8)				; stocke la nouvelle taille en 16:16: dans LSP_DSP_PAULA_internal_length3
	store		R12,(R7)			; LSP_DSP_PAULA_internal_offset3=0
	

DSP_LSP_Timer1_noreset3:
DSP_LSP_Timer1_skip3:
	

	
;--- test instrument voie 2
	movei		#DSP_LSP_Timer1_setIns2,R12
	btst		#13,R2
	jump		ne,(R12)
	nop
	
	movei		#DSP_LSP_Timer1_skip2,R12
	btst		#12,R2
	jump		eq,(R12)
	nop

; repeat voie 2
	movei		#LSP_DSP_repeat_pointeur2,R3
	movei		#LSP_DSP_repeat_length2,R4
	load		(R3),R3					; pointeur sauvegardé, sur infos de repeats
	load		(R4),R4
	movei		#LSP_DSP_PAULA_AUD2L,R7
	movei		#LSP_DSP_PAULA_AUD2LEN,R8
	store		R3,(R7)
	store		R4,(R8)					; stocke le pointeur sample de repeat dans LSP_DSP_PAULA_AUD3L
	jump		(R12)				; jump en DSP_LSP_Timer1_skip3
	nop


DSP_LSP_Timer1_setIns2:
	loadw		(R0),R3				; offset de l'instrument par rapport au precedent
; addition en .w
	btst		#15,R3
	jr			eq,.positif2
	nop
	movei		#$FFFF0000,R7
	or			R7,R3
.positif2:
	add			R3,R5				;R5=pointeur datas instruments
	addq		#2,R0


	movei		#LSP_DSP_PAULA_AUD2L,R7
	loadw		(R5),R6
	addq		#2,R5
	shlq		#16,R6
	loadw		(R5),R8
	or			R8,R6
	movei		#LSP_DSP_PAULA_AUD2LEN,R8
	store		R6,(R7)				; stocke le pointeur sample dans LSP_DSP_PAULA_AUD3L
	addq		#2,R5
	loadw		(R5),R9				; .w = R9 = taille du sample
	shlq		#16,R9				; en 16:16
	store		R9,(R8)				; stocke la nouvelle taille
	addq		#2,R5				; positionne sur pointeur de repeat
; repeat pointeur
	movei		#LSP_DSP_repeat_pointeur2,R7
	loadw		(R5),R4
	addq		#2,R5
	shlq		#16,R4
	loadw		(R5),R8
	or			R8,R4
	addq		#2,R5
	store		R4,(R7)
; repeat length
	movei		#LSP_DSP_repeat_length2,R7
	loadw		(R5),R8				; .w = R8 = taille du sample
	shlq		#16,R8				; en 16:16
	store		R8,(R7)				; stocke la nouvelle taille
	subq		#4,R5


; test le reset pour prise en compte immediate du changement de sample
	movei		#DSP_LSP_Timer1_noreset2,R12
	btst		#12,R2
	jump		eq,(R12)
	nop
; reset a travers le dmacon, il faut rafraichir : LSP_DSP_PAULA_internal_location3 & LSP_DSP_PAULA_internal_length3 & LSP_DSP_PAULA_internal_offset3=0
	movei		#LSP_DSP_PAULA_internal_location2,R7
	movei		#LSP_DSP_PAULA_internal_length2,R8
	store		R6,(R7)				; stocke le pointeur sample dans LSP_DSP_PAULA_internal_location3
	moveq		#0,R12
	movei		#LSP_DSP_PAULA_internal_offset2,R7
	store		R9,(R8)				; stocke la nouvelle taille en 16:16: dans LSP_DSP_PAULA_internal_length3
	store		R12,(R7)			; LSP_DSP_PAULA_internal_offset3=0
	

DSP_LSP_Timer1_noreset2:
DSP_LSP_Timer1_skip2:
	
	
;--- test instrument voie 1
	movei		#DSP_LSP_Timer1_setIns1,R12
	btst		#11,R2
	jump		ne,(R12)
	nop
	
	movei		#DSP_LSP_Timer1_skip1,R12
	btst		#10,R2
	jump		eq,(R12)
	nop

; repeat voie 1
	movei		#LSP_DSP_repeat_pointeur1,R3
	movei		#LSP_DSP_repeat_length1,R4
	load		(R3),R3					; pointeur sauvegardé, sur infos de repeats
	load		(R4),R4
	movei		#LSP_DSP_PAULA_AUD1L,R7
	movei		#LSP_DSP_PAULA_AUD1LEN,R8
	store		R3,(R7)
	store		R4,(R8)					; stocke le pointeur sample de repeat dans LSP_DSP_PAULA_AUD3L
	jump		(R12)				; jump en DSP_LSP_Timer1_skip3
	nop


DSP_LSP_Timer1_setIns1:
	loadw		(R0),R3				; offset de l'instrument par rapport au precedent
; addition en .w
	btst		#15,R3
	jr			eq,.positif1
	nop
	movei		#$FFFF0000,R7
	or			R7,R3
.positif1:
	add			R3,R5				;R5=pointeur datas instruments
	addq		#2,R0

	movei		#LSP_DSP_PAULA_AUD1L,R7
	loadw		(R5),R6
	addq		#2,R5
	shlq		#16,R6
	loadw		(R5),R8
	or			R8,R6
	movei		#LSP_DSP_PAULA_AUD1LEN,R8
	store		R6,(R7)				; stocke le pointeur sample dans LSP_DSP_PAULA_AUD3L
	addq		#2,R5
	loadw		(R5),R9				; .w = R9 = taille du sample
	shlq		#16,R9				; en 16:16
	store		R9,(R8)				; stocke la nouvelle taille
	addq		#2,R5				; positionne sur pointeur de repeat
; repeat pointeur
	movei		#LSP_DSP_repeat_pointeur1,R7
	loadw		(R5),R4
	addq		#2,R5
	shlq		#16,R4
	loadw		(R5),R8
	or			R8,R4
	addq		#2,R5
	store		R4,(R7)
; repeat length
	movei		#LSP_DSP_repeat_length1,R7
	loadw		(R5),R8				; .w = R8 = taille du sample
	shlq		#16,R8				; en 16:16
	store		R8,(R7)				; stocke la nouvelle taille
	subq		#4,R5

	
; test le reset pour prise en compte immediate du changement de sample
	movei		#DSP_LSP_Timer1_noreset1,R12
	btst		#10,R2
	jump		eq,(R12)
	nop
; reset a travers le dmacon, il faut rafraichir : LSP_DSP_PAULA_internal_location3 & LSP_DSP_PAULA_internal_length3 & LSP_DSP_PAULA_internal_offset3=0
	movei		#LSP_DSP_PAULA_internal_location1,R7
	movei		#LSP_DSP_PAULA_internal_length1,R8
	store		R6,(R7)				; stocke le pointeur sample dans LSP_DSP_PAULA_internal_location3
	moveq		#0,R12
	movei		#LSP_DSP_PAULA_internal_offset1,R7
	store		R9,(R8)				; stocke la nouvelle taille en 16:16: dans LSP_DSP_PAULA_internal_length3
	store		R12,(R7)			; LSP_DSP_PAULA_internal_offset3=0
	

DSP_LSP_Timer1_noreset1:
DSP_LSP_Timer1_skip1:
	
;--- test instrument voie 0
	movei		#DSP_LSP_Timer1_setIns0,R12
	btst		#9,R2
	jump		ne,(R12)
	nop
	
	movei		#DSP_LSP_Timer1_skip0,R12
	btst		#8,R2
	jump		eq,(R12)
	nop

; repeat voie 0
	movei		#LSP_DSP_repeat_pointeur0,R3
	movei		#LSP_DSP_repeat_length0,R4
	load		(R3),R3					; pointeur sauvegardé, sur infos de repeats
	load		(R4),R4
	movei		#LSP_DSP_PAULA_AUD0L,R7
	movei		#LSP_DSP_PAULA_AUD0LEN,R8
	store		R3,(R7)
	store		R4,(R8)					; stocke le pointeur sample de repeat dans LSP_DSP_PAULA_AUD3L
	jump		(R12)				; jump en DSP_LSP_Timer1_skip3
	nop


DSP_LSP_Timer1_setIns0:
	loadw		(R0),R3				; offset de l'instrument par rapport au precedent
; addition en .w
	btst		#15,R3
	jr			eq,.positif
	nop
	movei		#$FFFF0000,R7
	or			R7,R3

.positif:
	add			R3,R5				;R5=pointeur datas instruments
	addq		#2,R0

	movei		#LSP_DSP_PAULA_AUD0L,R7
	loadw		(R5),R6
	addq		#2,R5
	shlq		#16,R6
	loadw		(R5),R8
	addq		#2,R5
	or			R8,R6
	store		R6,(R7)				; stocke le pointeur sample dans LSP_DSP_PAULA_AUD0L

	movei		#LSP_DSP_PAULA_AUD0LEN,R8
	loadw		(R5),R9				; .w = R9 = taille du sample
	shlq		#16,R9				; en 16:16
	store		R9,(R8)				; stocke la nouvelle taille
	addq		#2,R5				; positionne sur pointeur de repeat
; repeat pointeur
	movei		#LSP_DSP_repeat_pointeur0,R7
	loadw		(R5),R4
	addq		#2,R5
	shlq		#16,R4
	loadw		(R5),R8
	or			R8,R4
	addq		#2,R5
	store		R4,(R7)
; repeat length
	movei		#LSP_DSP_repeat_length0,R7
	loadw		(R5),R8				; .w = R8 = taille du sample
	shlq		#16,R8				; en 16:16
	store		R8,(R7)				; stocke la nouvelle taille
	subq		#4,R5

	
; test le reset pour prise en compte immediate du changement de sample
	movei		#DSP_LSP_Timer1_noreset0,R12
	btst		#8,R2
	jump		eq,(R12)
	nop
; reset a travers le dmacon, il faut rafraichir : LSP_DSP_PAULA_internal_location3 & LSP_DSP_PAULA_internal_length3 & LSP_DSP_PAULA_internal_offset3=0
	movei		#LSP_DSP_PAULA_internal_location0,R7
	movei		#LSP_DSP_PAULA_internal_length0,R8
	store		R6,(R7)				; stocke le pointeur sample dans LSP_DSP_PAULA_internal_location3
	moveq		#0,R12
	movei		#LSP_DSP_PAULA_internal_offset0,R7
	store		R9,(R8)				; stocke la nouvelle taille en 16:16: dans LSP_DSP_PAULA_internal_length3
	store		R12,(R7)			; LSP_DSP_PAULA_internal_offset3=0

DSP_LSP_Timer1_noreset0:
DSP_LSP_Timer1_skip0:
	
	
DSP_LSP_Timer1_noInst:
	.if			LSP_avancer_module=1
	store		R0,(R14)			; store word stream (or byte stream if coming from early out)
	.endif


; - fin de la conversion du player LSP

; elements d'emulation Paula
; calcul des increments
; calcul de l'increment a partir de la note Amiga : (3546895 / note) / frequence I2S

	movei		#DSP_frequence_de_replay_reelle_I2S,R0
	movei		#LSP_DSP_PAULA_internal_increment0,R1
	movei		#LSP_DSP_PAULA_AUD0PER,R2
	load		(R0),R0
	movei		#3546895,R3
	
	load		(R2),R2
	cmpq		#0,R2
	jr			ne,.1
	nop
	moveq		#0,R4
	jr			.2
	nop
.1:
	move		R3,R4
	div			R2,R4			; (3546895 / note)
	or			R4,R4
	shlq		#16,R4
	div			R0,R4			; (3546895 / note) / frequence I2S en 16:16
	or			R4,R4
.2:
	store		R4,(R1)

	movei		#LSP_DSP_PAULA_AUD1PER,R2
	movei		#LSP_DSP_PAULA_internal_increment1,R1
	move		R3,R4
	load		(R2),R2
	cmpq		#0,R2
	jr			ne,.12
	nop
	moveq		#0,R4
	jr			.22
	nop
.12:

	div			R2,R4			; (3546895 / note)
	or			R4,R4
	shlq		#16,R4
	div			R0,R4			; (3546895 / note) / frequence I2S en 16:16
	or			R4,R4
.22:
	store		R4,(R1)

	movei		#LSP_DSP_PAULA_AUD2PER,R2
	movei		#LSP_DSP_PAULA_internal_increment2,R1
	move		R3,R4
	load		(R2),R2
	cmpq		#0,R2
	jr			ne,.13
	nop
	moveq		#0,R4
	jr			.23
	nop
.13:
	div			R2,R4			; (3546895 / note)
	or			R4,R4
	shlq		#16,R4
	div			R0,R4			; (3546895 / note) / frequence I2S en 16:16
	or			R4,R4
.23:
	store		R4,(R1)

	movei		#LSP_DSP_PAULA_AUD3PER,R2
	movei		#LSP_DSP_PAULA_internal_increment3,R1
	move		R3,R4
	load		(R2),R2
	cmpq		#0,R2
	jr			ne,.14
	nop
	moveq		#0,R4
	jr			.24
	nop
.14:
	div			R2,R4			; (3546895 / note)
	or			R4,R4
	shlq		#16,R4
	div			R0,R4			; (3546895 / note) / frequence I2S en 16:16
	or			R4,R4
.24:
	store		R4,(R1)
	
	
;------------------------------------	
; return from interrupt Timer 1
	load	(r31),r12	; return address
	;bset	#10,r13		; clear latch 1 = I2S
	bset	#11,r13		; clear latch 1 = timer 1
	;bset	#12,r13		; clear latch 1 = timer 2
	bclr	#3,r13		; clear IMASK
	addq	#4,r31		; pop from stack
	addqt	#2,r12		; next instruction
	jump	t,(r12)		; return
	store	r13,(r16)	; restore flags


DSP_LSP_Timer1_r_rewind:
;	movei		#LSPVars,R14
;	load		(R14),R0					; R0 = byte stream
	load		(R14+(m_byteStreamLoop/4)),R0			; bouclage : R0 = byte stream
	movei		#DSP_LSP_Timer1_process,R12
	load		(R14+(m_wordStreamLoop/4)),R3
	jump		(R12)
	store		R3,(R14+(m_wordStream/4))
	
DSP_LSP_Timer1_r_chgbpm:
	movei		#DSP_LSP_Timer1_process,R12
	loadb		(R0),R3
	store		R3,(R14+(m_currentBpm/4))
	jump		(R12)
	addq		#1,R0




; ------------------- N/A ------------------
DSP_LSP_routine_interruption_Timer2:
; ------------------- N/A ------------------



; ------------- main DSP ------------------
DSP_routine_init_DSP:
; assume run from bank 1
	movei	#DSP_ISP+(DSP_STACK_SIZE*4),r31			; init isp
	moveq	#0,r1
	moveta	r31,r31									; ISP (bank 0)
	movei	#DSP_USP+(DSP_STACK_SIZE*4),r31			; init usp


; init I2S
	movei	#SCLK,r10
	movei	#SMODE,r11
	movei	#DSP_parametre_de_frequence_I2S,r12
	movei	#%001101,r13			; SMODE bascule sur RISING
	load	(r12),r12				; SCLK
	store	r12,(r10)
	store	r13,(r11)


; init Timer 1
; frq = 24/(60/bpm)
	movei	#LSP_BPM_frequence_replay,R11
	load	(R11),R11
	movei	#60,R10
	shlq	#8,R10				; 16 bits de virgule
	div		R11,R10				; 60/bpm
	or		R10,R10
	movei	#24,R9				; 24=> 5 bits
	shlq	#16,R9
	div		R10,R9				; R9=
	or		R9,R9
	shrq	#8,R9				; R9=frequence replay 
	
	move	R9,R11	
	

; frequence du timer 1
	movei	#182150,R10				; 26593900 / 146 = 182150
	div		R11,R10
	or		R10,R10
	move	R10,R13

	subq	#1,R13					; -1 pour parametrage du timer 1
	
	

; 26593900 / 50 = 531 878 => 2 × 73 × 3643 => 146*3643
	movei	#JPIT1,r10				; F10000
	;movei	#JPIT2,r11				; F10002
	movei	#146-1,r12				; Timer 1 Pre-scaler

;EDZ
	;movei	#3643-1,r13				; Timer 1 Divider  
	
	shlq	#16,r12
	or		R13,R12
	
	store	r12,(r10)				; JPIT1 & JPIT2


; init timer 2

;	movei	#JPIT3,r10				; F10004
;	movei	#JPIT4,r11				; F10006



; enable interrupts
	movei	#D_FLAGS,r28
	
	movei	#D_I2SENA|D_TIM1ENA|REGPAGE,r29			; I2S+Timer 1
	;movei	#D_I2SENA|REGPAGE,r29					; I2S only
	
	
	;movei	#D_TIM1ENA|REGPAGE,r29					; Timer 1 only
	;movei	#D_TIM2ENA|REGPAGE,r29					; Timer 2 only
	
	store	r29,(r28)

DSP_boucle_centrale:
	movei	#DSP_boucle_centrale,R20
	jump	(R20)
	nop
	
	
	.phrase

DSP_frequence_de_replay_reelle_I2S:					dc.l			0
DSP_UN_sur_frequence_de_replay_reelle_I2S:			dc.l			0
DSP_parametre_de_frequence_I2S:						dc.l			0

; variables Paula
; channel 0
LSP_DSP_PAULA_AUD0L:				dc.l			silence			; Audio channel 0 location
LSP_DSP_PAULA_AUD0LEN:				dc.l			$20000			; en bytes !
LSP_DSP_PAULA_AUD0PER:				dc.l			0				; period , a transformer en increment
LSP_DSP_PAULA_AUD0VOL:				dc.l			0				; volume
;LSP_DSP_PAULA_AUD0DAT:				dc.l			0
LSP_DSP_PAULA_internal_location0:	dc.l			silence			; internal register : location of the sample currently played
LSP_DSP_PAULA_internal_increment0:	dc.l			1				; internal register : increment linked to period 16:16
LSP_DSP_PAULA_internal_offset0:		dc.l			0				; internal register : current offset 16:16
LSP_DSP_PAULA_internal_length0:		dc.l			$20000			; internal register : length of the sample currently played
LSP_DSP_repeat_pointeur0:			dc.l			silence
LSP_DSP_repeat_length0:				dc.l			$20000
; channel 1
LSP_DSP_PAULA_AUD1L:				dc.l			silence			; Audio channel 1 location
LSP_DSP_PAULA_AUD1LEN:				dc.l			$20000			; en bytes 16:16
LSP_DSP_PAULA_AUD1PER:				dc.l			0				; period , a transformer en increment
LSP_DSP_PAULA_AUD1VOL:				dc.l			0				; volume
;LSP_DSP_PAULA_AUD1DAT:				dc.l			0
LSP_DSP_PAULA_internal_location1:	dc.l			silence			; internal register : location of the sample currently played
LSP_DSP_PAULA_internal_increment1:	dc.l			2				; internal register : increment linked to period 16:16
LSP_DSP_PAULA_internal_offset1:		dc.l			0				; internal register : current offset 16:16
LSP_DSP_PAULA_internal_length1:		dc.l			$20000			; internal register : length of the sample currently played
LSP_DSP_repeat_pointeur1:			dc.l			silence
LSP_DSP_repeat_length1:				dc.l			$20000
; channel 2
LSP_DSP_PAULA_AUD2L:				dc.l			silence			; Audio channel 0 location
LSP_DSP_PAULA_AUD2LEN:				dc.l			$20000			; en bytes 16:16
LSP_DSP_PAULA_AUD2PER:				dc.l			0				; period , a transformer en increment
LSP_DSP_PAULA_AUD2VOL:				dc.l			0				; volume
;LSP_DSP_PAULA_AUD2DAT:				dc.l			0
LSP_DSP_PAULA_internal_location2:	dc.l			silence			; internal register : location of the sample currently played
LSP_DSP_PAULA_internal_increment2:	dc.l			3				; internal register : increment linked to period 16:16
LSP_DSP_PAULA_internal_offset2:		dc.l			0				; internal register : current offset 16:16
LSP_DSP_PAULA_internal_length2:		dc.l			$20000			; internal register : length of the sample currently played
LSP_DSP_repeat_pointeur2:			dc.l			silence
LSP_DSP_repeat_length2:				dc.l			$20000
; channel 3
LSP_DSP_PAULA_AUD3L:				dc.l			silence			; Audio channel 0 location
LSP_DSP_PAULA_AUD3LEN:				dc.l			$20000			; en bytes !
LSP_DSP_PAULA_AUD3PER:				dc.l			0				; period , a transformer en increment
LSP_DSP_PAULA_AUD3VOL:				dc.l			0				; volume
;LSP_DSP_PAULA_AUD3DAT:				dc.l			0
LSP_DSP_PAULA_internal_location3:	dc.l			silence			; internal register : location of the sample currently played
LSP_DSP_PAULA_internal_increment3:	dc.l			4				; internal register : increment linked to period 16:16
LSP_DSP_PAULA_internal_offset3:		dc.l			0				; internal register : current offset 16:16
LSP_DSP_PAULA_internal_length3:		dc.l			$20000			; internal register : length of the sample currently played
LSP_DSP_repeat_pointeur3:			dc.l			silence
LSP_DSP_repeat_length3:				dc.l			$20000



silence:		dc.l			0

EDZTMP1:		dc.l			0

;---------------------
; FIN DE LA RAM DSP
YM_DSP_fin:
;---------------------


SOUND_DRIVER_SIZE			.equ			YM_DSP_fin-DSP_base_memoire
	.print	"--- Sound driver code size (DSP): ", /u SOUND_DRIVER_SIZE, " bytes / 8192 ---"


	
	.68000


	.even
	.phrase
couleur_char:				dc.b		25

	even
fonte:	
	.include	"fonte1plan.s"
	even
	
            .dphrase
stoplist:		dc.l	0,4

debut_ram_libre_DSP:		dc.l			YM_DSP_fin
debut_ram_libre:			dc.l			FIN_RAM


        .68000
		.dphrase
ob_liste_originale:           				 ; This is the label you will use to address this in 68K code
        .objproc 							   ; Engage the OP assembler
		.dphrase

        .org    ob_list_courante			 ; Tell the OP assembler where the list will execute
;
        branch      VC < 0, .stahp    			 ; Branch to the STOP object if VC < 0
        branch      VC > 265, .stahp   			 ; Branch to the STOP object if VC > 241
			; bitmap data addr, xloc, yloc, dwidth, iwidth, iheight, bpp, pallete idx, flags, firstpix, pitch
        bitmap      ecran1, 16, 26, nb_octets_par_ligne/8, nb_octets_par_ligne/8, 246-26,3
		;bitmap		ecran1,16,24,40,40,255,3
        jump        .haha
.stahp:
        stop
.haha:
        jump        .stahp
		
		.68000
		.dphrase
fin_ob_liste_originale:



















			.data
	.dphrase
LSP_BPM_frequence_replay:		dc.l			125

curseur_x:	dc.w		0
curseur_y:	dc.w		curseur_Y_min

chaine_LSP:						dc.b	"LSP player for Jaguar",10,0
chaine_playing_LSP:				dc.b	"Now playing module",10,0
chaine_BPM_init_LSP:			dc.b	" bpm.",0
chaine_Hz_init_LSP:			dc.b	" Hz.",10,0
chaine_replay_frequency:		dc.b	"Replay frequency : ",0
chaine_RAM_DSP:					dc.b	"DSP RAM available while running : ",0
chaine_entete_debug_module:		dc.b	"location incremen offset   length  ",10,0
chaine_entete_debug_module2:	dc.b	"location length   period   volume",10,0
		even

	.phrase
LSP_module_music_data:
	.incbin			"LSP/elysium.lsmusic"
	;.incbin			"LSP/d.lsmusic"
	;.incbin			"LSP/testsamples4v.lsmusic"
	.phrase
LSP_module_sound_bank:
	.incbin			"LSP/elysium.lsbank"
	;.incbin			"LSP/d.lsbank"
	;.incbin			"LSP/testsamples4v.lsbank"
	.phrase

	even
	.phrase
	.bss
	.phrase
DEBUT_BSS:
;EDZ_compteur_reset_offset_entier_voie_A:			ds.l	1

frequence_Video_Clock:					ds.l				1
frequence_Video_Clock_divisee :			.ds.l				1



_50ou60hertz:			ds.l	1
ntsc_flag:				ds.w	1
a_hdb:          		ds.w   1
a_hde:          		ds.w   1
a_vdb:          		ds.w   1
a_vde:          		ds.w   1
width:          		ds.w   1
height:         		ds.w   1
taille_liste_OP:		ds.l	1
vbl_counter:			ds.l	1

            .dphrase
ecran1:				ds.b		320*256				; 8 bitplanes
	.phrase
FIN_RAM:
