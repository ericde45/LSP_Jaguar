; LSP sur Jaguar
; version 4 bytes reading at once
;
; OK - routine de replay Paula en I2S
; OK - routine de replay Paula en Timer 1
; OK - convertir player LSP en Timer 1
; OK - gérer le changement de bpm dans le timer 1
; OK - init : signed 8 bits samples => unsigned 8 bits samples
; OK - LSP Init : longueur sample en bytes
; OK - mettre en place la console
; OK - basculer tout en 21:11 : 
; OK - lire 4 octets par 4 octets de sample / attention à l'alignement du sample, lire les premiers octets en loadb et/ou loadw !
; - optimiser lecture lsp etc dans Timer 1
; - ajouter 4 voies samples avec frequence
; OK - gerer le joypad : xxxxxxCx xxBx2580 147*oxAP 369#RLDU
; - stocker le resultat de chaque voie, ne recalculer que si increment a avancé de 1 entier
; - interpolation entre samples, sur 8 octets stockés
; -  mono/stereo playback
; - master volume for mod/sfx
; - "stop module"
; - "shut down DSP"
; - module - play once/loop 
; - samples : play once/loop

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


CLEAR_BSS			.equ			1									; 1=efface toute la BSS jusqu'a la fin de la ram utilisée
LSP_DSP_Audio_frequence					.equ			46000				; real hardware needs lower sample frequencies than emulators 
nb_bits_virgule_offset					.equ			11					; 11 ok DRAM/ 8 avec samples en ram DSP

display_infos_debug				.equ			1
DSP_DEBUG						.equ			0
I2S_during_Timer1				.equ			0									; 0= I2S waits while timer 1 / 1=IMASK cleared while Timer 1
LSP_avancer_module				.equ			1								; 1=incremente position dans le module

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
; clear BSS
	lea			DEBUT_BSS,a0
	lea			FIN_RAM,a1
	moveq		#0,d0
	
boucle_clean_BSS:
	move.b		d0,(a0)+
	cmp.l		a0,a1
	bne.s		boucle_clean_BSS
; clear stack
	lea			INITSTACK-100,a0
	lea			INITSTACK,a1
	moveq		#0,d0
	
boucle_clean_BSS2:
	move.b		d0,(a0)+
	cmp.l		a0,a1
	bne.s		boucle_clean_BSS2

; clear object list
	lea			ob_list_courante,a0
	lea			ENDRAM,a1
	moveq		#0,d0
	
boucle_clean_BSS3:
	move.b		d0,(a0)+
	cmp.l		a0,a1
	bne.s		boucle_clean_BSS3
	.endif

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

; init LSP

	lea		LSP_module_music_data,a0
	lea		LSP_module_sound_bank,a1
	jsr		LSP_PlayerInit
	move.l	m_lspInstruments,a0

;		Out : 	a0: music BPM pointer (16bits).w
;				d0: music len in tick count

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


; launch DSP

	move.l	#REGPAGE,D_FLAGS
	move.l	#DSP_routine_init_DSP,D_PC
	move.l	#DSPGO,D_CTRL
	move.l	#0,vbl_counter


; calcul RAM DSP
	
	lea			chaine_RAM_DSP,a0
	bsr			print_string
	move.l		#D_ENDRAM,d0
	sub.l		debut_ram_libre_DSP,d0
	bsr			print_nombre_4_chiffres
; ligne suivante
	moveq		#10,d0
	bsr			print_caractere



; on attend le DSP
	moveq	#5,d7
.boucle_attente:
	move.l		vbl_counter,d0
.waitVBL2:
	move.l		vbl_counter,d1
	cmp.l		d0,d1
	beq		.waitVBL2
	dbf		d7,.boucle_attente


	move.w		#85,couleur_char




; replay frequency
	lea			chaine_replay_frequency,a0
	bsr			print_string

	move.l		DSP_frequence_de_replay_reelle_I2S,d0
	bsr			print_nombre_5_chiffres

	lea			chaine_Hz_init_LSP,a0
	bsr			print_string

	move.w		#145,couleur_char
	
	lea			chaine_playing_LSP,a0
	bsr			print_string
	
	move.w		#245,couleur_char

	lea			chaine_entete_debug_module,a0
	bsr			print_string

	lea			LSP_module_sound_bank,a0
	move.l		a0,EDZTMP1

toto:

	.if			display_infos_debug=1
; affiche les registres internes
	move.l	LSP_DSP_PAULA_internal_location0,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_increment0,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	#0,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_length0,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres

	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

	move.l	LSP_DSP_PAULA_internal_location1,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_increment1,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	#0,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_length1,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

	move.l	LSP_DSP_PAULA_internal_location2,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_increment2,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	#0,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_length2,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere


	move.l	LSP_DSP_PAULA_internal_location3,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_increment3,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	#0,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_internal_length3,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

; saute une ligne
	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

;-------------- deuxieme groupe d'infos


	lea			chaine_entete_debug_module2,a0
	bsr			print_string


; affiche les registres externes
	move.l	LSP_DSP_PAULA_AUD0L,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere

	move.l	LSP_DSP_PAULA_AUD0LEN,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere

	move.l	LSP_DSP_repeat_pointeur0,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif	
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere

	move.l	LSP_DSP_repeat_length0,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif	
	bsr		print_nombre_hexa_8_chiffres

	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

	move.l	LSP_DSP_PAULA_AUD1L,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD1LEN,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_repeat_pointeur1,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif	
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere

	move.l	LSP_DSP_repeat_length1,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif	
	bsr		print_nombre_hexa_8_chiffres
	
	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

	move.l	LSP_DSP_PAULA_AUD2L,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD2LEN,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_repeat_pointeur2,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif	
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere

	move.l	LSP_DSP_repeat_length2,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif	
	bsr		print_nombre_hexa_8_chiffres
	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

	move.l	LSP_DSP_PAULA_AUD3L,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD3LEN,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_repeat_pointeur3,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif	
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere

	move.l	LSP_DSP_repeat_length3,d0
	.if		nb_bits_virgule_offset<9
		lsr.l	#nb_bits_virgule_offset,d0
	.else
		lsr.l	#8,d0
		lsr.l	#nb_bits_virgule_offset-8,d0
	.endif	
	bsr		print_nombre_hexa_8_chiffres
	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere
	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere


	move.l	EDZTMP1,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD1DAT,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD2DAT,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l	LSP_DSP_PAULA_AUD3DAT,d0
	bsr		print_nombre_hexa_8_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere
	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

	.endif

; 2 lignes de status pad
	bsr		print_pads_status

	.if		display_infos_debug=1
	.rept	13
	; retour a la ligne	 au dessus
	moveq	#8,d0
	bsr		print_caractere
	.endr
	.endif
	.rept	2
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
				;add.w		#1,BG					; debug pour voir si vivant
				.endif

                jsr     copy_olist              	; use Blitter to update active list from shadow

                addq.l	#1,vbl_counter

                move.w  #$101,INT1              	; Signal we're done
				move.w  #$0,INT2
.exit:
                movem.l (a7)+,d0-d7/a0-a6
                rte

; ---------------------------------------
; print pads status 
; Pads : mask = xxxxxxCx xxBx2580 147*oxAP 369#RLDU
print_pads_status:
	
	move.l		DSP_pad1,d1
	lea			string_pad_status,a0
	move.l		#31,d6

.boucle:	
	moveq		#0,d0
	btst.l		d6,d1
	beq.s		.print_space
	move.b		(a0)+,d0
	bsr			print_caractere
	bra.s		.ok
.print_space:
	move.b		#'.',d0
	bsr			print_caractere
	lea			1(a0),a0
.ok:
	dbf			d6,.boucle

; ligne suivante
	moveq		#10,d0
	bsr			print_caractere
	
print_pads_status_pad2:
; pad2
	move.l		DSP_pad2,d1
	lea			string_pad_status,a0
	move.l		#31,d6

.boucle2:	
	moveq		#0,d0
	btst.l		d6,d1
	beq.s		.print_space2
	move.b		(a0)+,d0
	bsr			print_caractere
	bra.s		.ok2
.print_space2:
	move.b		#'.',d0
	bsr			print_caractere
	lea			1(a0),a0
.ok2:
	dbf			d6,.boucle2

; ligne suivante
	moveq		#10,d0
	bsr			print_caractere
	

	rts

string_pad_status:		dc.b		"......CE..BD2580147*oFAp369#RLDU"
		even

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
	even
	
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
	moveq	#0,d4
	move.w	couleur_char,d4
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


	.if			1=0
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


	.endif
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
			move.l		d6,m_currentBpm-LSPVars(a3)		; default BPM
			move.l		d6,LSP_BPM_frequence_replay
			move.w		(a0)+,d6
			move.l		d6,m_escCodeRewind-LSPVars(a3)		; tout en .L
			move.w		(a0)+,d6
			move.l		d6,m_escCodeSetBpm-LSPVars(a3)
			move.l		(a0)+,-(a7)							; nb de ticks du module en tout = temps de replay ( /BPM)
			;move.l	a2,m_dmaconPatch(a3)
			;move.w	#$8000,-1(a2)			; Be sure DMACon word is $8000 (note: a2 should be ODD address)
			moveq		#0,d0
			move.w		(a0)+,d0				; instrument count
			lea			-12(a0),a2				; LSP data has -12 offset on instrument tab ( to win 2 cycles in fast player :) )
			move.l		a2,m_lspInstruments-LSPVars(a3)	; instrument tab addr ( minus 4 )
			subq.w		#1,d0
			move.l		a1,d1

.relocLoop:	
			;bset.b		#0,3(a0)				; bit0 is relocation done flag
			;bne.s		.relocated
			
			move.l		(a0),d5					; pointeur sample
			add.l		d1,d5					; passage de relatif en absolu
			;lsl.l		#nb_bits_virgule_offset,d6
			move.l		d5,(a0)					; pointeur sample

			
			moveq		#0,d6
			move.w		4(a0),d6				; taille en words
			add.l		d6,d6
			move.w		d6,4(a0)				; taille en bytes

			move.l		(a0),a4					
			;bsr.s		LSP_unsigne_sample		; A4=sample location / d6=length


			move.l		6(a0),d6					; pointeur sample repeat
			add.l		d1,d6					; passage de relatif en absolu
			cmp.l		d5,d6					; corrige pointeur de repeat avant le debut de l'instrument
			bge.s		.ok_loop
			move.l		d5,d6
.ok_loop:
			;lsl.l		#nb_bits_virgule_offset,d6
			move.l		d6,6(a0)					; pointeur sample repeat
			
			moveq		#0,d6
			move.w		10(a0),d6				; taille repeat en words
			add.l		d6,d6
			move.w		d6,10(a0)				; taille repeat en bytes

.relocated:	
			lea			12(a0),a0
			dbf.w		d0,.relocLoop
		
			move.w		(a0)+,d0				; codes count (+2)
			move.l		a0,m_codeTableAddr-LSPVars(a3)	; code table
			add.w		d0,d0
			add.w		d0,a0
			move.l		(a0)+,d0				; word stream size
			move.l		(a0)+,d1				; byte stream loop point
			move.l		(a0)+,d2				; word stream loop point

			move.l		a0,m_wordStream-LSPVars(a3)
			lea			0(a0,d0.l),a1			; byte stream
			move.l		a1,m_byteStream-LSPVars(a3)
			add.l		d2,a0
			add.l		d1,a1
			move.l		a0,m_wordStreamLoop-LSPVars(a3)
			move.l		a1,m_byteStreamLoop-LSPVars(a3)
			;bset.b		#1,$bfe001				; disabling this fucking Low pass filter!!
			lea			m_currentBpm-LSPVars(a3),a0
			move.l		(a7)+,d0				; music len in frame ticks
			rts

.dataError:	illegal

LSP_unsigne_sample:
; A4=sample location / d6=length
	movem.l		d0/d6/a4,-(sp)
	subq.l		#1,d6
	cmp.l		#0,d6
	blt.s		.quit
.boucle:
	move.b		(a4),d0				; -128 to 127
	sub.b		#$80,d0
	move.b		d0,(a4)+
	dbf			d6,.boucle

.quit:
	movem.l		(sp)+,d0/d6/a4
	rts

	
	
	.text
	
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
	movei	#DSP_LSP_routine_interruption_Timer2,r12						; 6 octets
	movei	#D_FLAGS,r16											; 6 octets
	jump	(r12)													; 2 octets
	load	(r16),r13	; read flags								; 2 octets = 16 octets
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


; version complexe avec stockage de 4 octets
; channel 3
		movei		#LSP_DSP_PAULA_internal_location3,R28						; adresse sample actuelle, a virgule
		movei		#LSP_DSP_PAULA_internal_increment3,R27
		load		(R28),R26										; R26=current pointeur sample 16:16
		load		(R27),R27										; R27=increment 16:16
		move		R26,R17											; R17 = pointeur sample a virgule avant increment
		movei		#LSP_DSP_PAULA_internal_length3,R25				; =FIN
		add			R27,R26											; R26=adresse+increment , a virgule
		load		(R25),R23
		movei		#$FFFFFFFC,R22
		cmp			R23,R26
		jr			mi,DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel3
		;nop
		shrq		#nb_bits_virgule_offset,R17								; ancien pointeur adresse sample partie entiere

; fin de sample => on recharge les infos des registres externes
		shlq		#32-nb_bits_virgule_offset,R26
		movei		#LSP_DSP_PAULA_AUD3LEN,R27			; fin, a virgule 
		shrq		#32-nb_bits_virgule_offset,R26		; on ne garde que la virgule
		movei		#LSP_DSP_PAULA_AUD3L,R24			; sample location a virgule
		load		(R27),R27
		load		(R24),R23
		store		R27,(R25)							; update internal sample end, a virgule
		or			R23,R26								; on garde la virgule en cours
		
DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel3:
		store		R26,(R28)							; stocke internal sample pointeur, a virgule
		shrq		#nb_bits_virgule_offset,R26								; nouveau pointeur adresse sample partie entiere
		;shrq		#nb_bits_virgule_offset,R17								; ancien pointeur adresse sample partie entiere
		move		R26,R25								; R25 = nouveau pointeur sample 
		and			R22,R17								; ancien pointeur sample modulo 4
		and			R22,R26								; nouveau pointeur sample modulo 4
		movei		#LSP_DSP_PAULA_AUD3DAT,R28			; 4 octets actuels
		not			R22									; => %11
		load		(R28),R21							; R21 = octets actuels en stock
		and			R22,R25								; R25 = position octet à lire
		cmp			R17,R26
		jr			eq,DSP_LSP_routine_interruption_I2S_pas_nouveau_long_word3
		;nop
		shlq		#3,R25					; numero d'octet à lire * 8

; il faut rafraichir R21
		load		(R26),R21							; lit 4 nouveaux octets de sample
		store		R21,(R28)							; rafraichit le stockage des 4 octets

DSP_LSP_routine_interruption_I2S_pas_nouveau_long_word3:
		movei		#LSP_DSP_PAULA_AUD3VOL,R23
		neg			R25									; -0 -8 -16 -24
; R25=numero d'octet à lire
; ch2
		movei		#LSP_DSP_PAULA_internal_increment2,R27

		sh			R25,R21								; shift les 4 octets en stock vers la gauche, pour positionner l'octet à lire en haut
		load		(R23),R23							; R23 = volume : 6 bits
		sharq		#24,R21								; descends l'octet à lire
; ch2
		movei		#LSP_DSP_PAULA_internal_location2,R28						; adresse sample actuelle, a virgule

		imult		R23,R21								; unsigned multiplication : unsigned sample * volume => 8bits + 6 bits = 14 bits

; R21=sample channel 3 on 14 bits

; ----------
; channel 2
		;movei		#LSP_DSP_PAULA_internal_location2,R28						; adresse sample actuelle, a virgule
		;movei		#LSP_DSP_PAULA_internal_increment2,R27
		load		(R28),R26										; R26=current pointeur sample 16:16
		load		(R27),R27										; R27=increment 16:16
		move		R26,R17											; R17 = pointeur sample a virgule avant increment
		movei		#LSP_DSP_PAULA_internal_length2,R25				; =FIN
		add			R27,R26											; R26=adresse+increment , a virgule
		load		(R25),R23
		movei		#$FFFFFFFC,R22
		cmp			R23,R26
		jr			mi,DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel2
		;nop
		shrq		#nb_bits_virgule_offset,R17								; ancien pointeur adresse sample partie entiere

; fin de sample => on recharge les infos des registres externes
		shlq		#32-nb_bits_virgule_offset,R26
		movei		#LSP_DSP_PAULA_AUD2LEN,R27			; fin, a virgule 
		shrq		#32-nb_bits_virgule_offset,R26		; on ne garde que la virgule
		movei		#LSP_DSP_PAULA_AUD2L,R24			; sample location a virgule
		load		(R27),R27
		load		(R24),R23
		store		R27,(R25)							; update internal sample end, a virgule
		or			R23,R26								; on garde la virgule en cours
		
DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel2:
		store		R26,(R28)							; stocke internal sample pointeur, a virgule
		shrq		#nb_bits_virgule_offset,R26								; nouveau pointeur adresse sample partie entiere
		;shrq		#nb_bits_virgule_offset,R17								; ancien pointeur adresse sample partie entiere
		move		R26,R25								; R25 = nouveau pointeur sample 
		and			R22,R17								; ancien pointeur sample modulo 4
		and			R22,R26								; nouveau pointeur sample modulo 4
		movei		#LSP_DSP_PAULA_AUD2DAT,R28			; 4 octets actuels
		not			R22									; => %11
		load		(R28),R20							; R20 = octets actuels en stock
		and			R22,R25								; R25 = position octet à lire
		cmp			R17,R26
		jr			eq,DSP_LSP_routine_interruption_I2S_pas_nouveau_long_word2
		;nop
		shlq		#3,R25					; numero d'octet à lire * 8

; il faut rafraichir R20
		load		(R26),R20							; lit 4 nouveaux octets de sample
		store		R20,(R28)							; rafraichit le stockage des 4 octets

DSP_LSP_routine_interruption_I2S_pas_nouveau_long_word2:
		movei		#LSP_DSP_PAULA_AUD2VOL,R23
		neg			R25									; -0 -8 -16 -24
; R25=numero d'octet à lire
; ch1
		movei		#LSP_DSP_PAULA_internal_increment1,R27

		sh			R25,R20								; shift les 4 octets en stock vers la gauche, pour positionner l'octet à lire en haut
		load		(R23),R23							; R23 = volume : 6 bits
		sharq		#24,R20								; descends l'octet à lire
; ch1
		movei		#LSP_DSP_PAULA_internal_location1,R28						; adresse sample actuelle, a virgule

		imult		R23,R20								; unsigned multiplication : unsigned sample * volume => 8bits + 6 bits = 14 bits

; R20=sample channel 2 on 14 bits

; ----------
; channel 1
		;movei		#LSP_DSP_PAULA_internal_location1,R28						; adresse sample actuelle, a virgule
		;movei		#LSP_DSP_PAULA_internal_increment1,R27
		load		(R28),R26										; R26=current pointeur sample 16:16
		load		(R27),R27										; R27=increment 16:16
		move		R26,R17											; R17 = pointeur sample a virgule avant increment
		movei		#LSP_DSP_PAULA_internal_length1,R25				; =FIN
		add			R27,R26											; R26=adresse+increment , a virgule
		load		(R25),R23
		movei		#$FFFFFFFC,R22
		cmp			R23,R26
		jr			mi,DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel1
		;nop
		shrq		#nb_bits_virgule_offset,R17								; ancien pointeur adresse sample partie entiere

; fin de sample => on recharge les infos des registres externes
		shlq		#32-nb_bits_virgule_offset,R26
		movei		#LSP_DSP_PAULA_AUD1LEN,R27			; fin, a virgule 
		shrq		#32-nb_bits_virgule_offset,R26		; on ne garde que la virgule
		movei		#LSP_DSP_PAULA_AUD1L,R24			; sample location a virgule
		load		(R27),R27
		load		(R24),R23
		store		R27,(R25)							; update internal sample end, a virgule
		or			R23,R26								; on garde la virgule en cours
		
DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel1:
		store		R26,(R28)							; stocke internal sample pointeur, a virgule
		shrq		#nb_bits_virgule_offset,R26								; nouveau pointeur adresse sample partie entiere
		;shrq		#nb_bits_virgule_offset,R17								; ancien pointeur adresse sample partie entiere
		move		R26,R25								; R25 = nouveau pointeur sample 
		and			R22,R17								; ancien pointeur sample modulo 4
		and			R22,R26								; nouveau pointeur sample modulo 4
		movei		#LSP_DSP_PAULA_AUD1DAT,R28			; 4 octets actuels
		not			R22									; => %11
		load		(R28),R19							; R19 = octets actuels en stock
		and			R22,R25								; R25 = position octet à lire
		cmp			R17,R26
		jr			eq,DSP_LSP_routine_interruption_I2S_pas_nouveau_long_word1
		;nop
		shlq		#3,R25					; numero d'octet à lire * 8

; il faut rafraichir R19
		load		(R26),R19							; lit 4 nouveaux octets de sample
		store		R19,(R28)							; rafraichit le stockage des 4 octets

DSP_LSP_routine_interruption_I2S_pas_nouveau_long_word1:
		movei		#LSP_DSP_PAULA_AUD1VOL,R23
		neg			R25									; -0 -8 -16 -24
; R25=numero d'octet à lire
; ch0
		movei		#LSP_DSP_PAULA_internal_increment0,R27

		sh			R25,R19								; shift les 4 octets en stock vers la gauche, pour positionner l'octet à lire en haut
		load		(R23),R23							; R23 = volume : 6 bits
		sharq		#24,R19								; descends l'octet à lire
; ch0
		movei		#LSP_DSP_PAULA_internal_location0,R28						; adresse sample actuelle, a virgule

		imult		R23,R19								; unsigned multiplication : unsigned sample * volume => 8bits + 6 bits = 14 bits

; R19=sample channel 1 on 14 bits

; ----------
; channel 0
		;movei		#LSP_DSP_PAULA_internal_location0,R28						; adresse sample actuelle, a virgule
		;movei		#LSP_DSP_PAULA_internal_increment0,R27
		load		(R28),R26										; R26=current pointeur sample 16:16
		load		(R27),R27										; R27=increment 16:16
		move		R26,R17											; R17 = pointeur sample a virgule avant increment
		movei		#LSP_DSP_PAULA_internal_length0,R25				; =FIN
		add			R27,R26											; R26=adresse+increment , a virgule
		load		(R25),R23
		movei		#$FFFFFFFC,R22
		cmp			R23,R26
		jr			mi,DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel0
		;nop
		shrq		#nb_bits_virgule_offset,R17								; ancien pointeur adresse sample partie entiere

; fin de sample => on recharge les infos des registres externes
		shlq		#32-nb_bits_virgule_offset,R26
		movei		#LSP_DSP_PAULA_AUD0LEN,R27			; fin, a virgule 
		shrq		#32-nb_bits_virgule_offset,R26		; on ne garde que la virgule
		movei		#LSP_DSP_PAULA_AUD0L,R24			; sample location a virgule
		load		(R27),R27
		load		(R24),R23
		store		R27,(R25)							; update internal sample end, a virgule
		or			R23,R26								; on garde la virgule en cours
		
DSP_LSP_routine_interruption_I2S_pas_fin_de_sample_channel0:
		store		R26,(R28)							; stocke internal sample pointeur, a virgule
		shrq		#nb_bits_virgule_offset,R26								; nouveau pointeur adresse sample partie entiere
		;shrq		#nb_bits_virgule_offset,R17								; ancien pointeur adresse sample partie entiere
		move		R26,R25								; R25 = nouveau pointeur sample 
		and			R22,R17								; ancien pointeur sample modulo 4
		and			R22,R26								; nouveau pointeur sample modulo 4
		movei		#LSP_DSP_PAULA_AUD0DAT,R28			; 4 octets actuels
		not			R22									; => %11
		load		(R28),R18							; R18 = octets actuels en stock
		and			R22,R25								; R25 = position octet à lire
		cmp			R17,R26
		jr			eq,DSP_LSP_routine_interruption_I2S_pas_nouveau_long_word0
		;nop
		shlq		#3,R25					; numero d'octet à lire * 8

; il faut rafraichir R18
		load		(R26),R18							; lit 4 nouveaux octets de sample
		store		R18,(R28)							; rafraichit le stockage des 4 octets

DSP_LSP_routine_interruption_I2S_pas_nouveau_long_word0:
		movei		#LSP_DSP_PAULA_AUD0VOL,R23
		neg			R25									; -0 -8 -16 -24
; R25=numero d'octet à lire


; suite
		.if			channel_2=0
			moveq	#0,R19
		.endif
		.if			channel_3=0
			moveq	#0,R20
		.endif
		add			R20,R19				; R19 = right 15 bits unsigned
;--

		sh			R25,R18								; shift les 4 octets en stock vers la gauche, pour positionner l'octet à lire en haut
		load		(R23),R23							; R23 = volume : 6 bits
		sharq		#24,R18								; descends l'octet à lire

; suite
		movei		#$8000,R26

		imult		R23,R18								; unsigned multiplication : unsigned sample * volume => 8bits + 6 bits = 14 bits

; R20=sample channel 2 on 14 bits




; Stéreo Amiga:
; les canaux 0 et 3 formant la voie stéréo gauche et 1 et 2 la voie stéréo droite
; R18=channel 0
; R19=channel 1
; R20=channel 2
; R21=channel 3
		.if			channel_1=0
			moveq	#0,R18
		.endif
		.if			channel_4=0
			moveq	#0,R21
		.endif

		;movei		#$8000,R26
		add			R21,R18				; R18 = left 15 bits unsigned
		movei		#L_I2S,R27
		;add			R20,R19				; R19 = right 15 bits unsigned
		shlq		#1,R18				; 16 bits unsigned
		shlq		#1,R19
		
		;sub			R26,R18				; 16 bits signed
		movei		#L_I2S+4,R25
		;sub			R26,R19
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
	load		(R14+2),R3			; R3=code table / m_codeTableAddr
	add			R2,R3
	movei		#DSP_LSP_Timer1_noInst,R12
	loadw		(R3),R2									; R2 = code
	cmpq		#0,R2
	jump		eq,(R12)
	nop
	load		(R14+3),R4			; R4=escape code rewind / m_escCodeRewind
	movei		#DSP_LSP_Timer1_r_rewind,R12
	cmp			R4,R2
	jump		eq,(R12)
	nop
	load		(R14+4),R4			; R4=escape code set bpm / m_escCodeSetBpm
	movei		#DSP_LSP_Timer1_r_chgbpm,R12
	cmp			R4,R2
	jump		eq,(R12)
	nop

;--------------------------
; gestion des volumes
;--------------------------
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

;--------------------------
; gestion des notes
;--------------------------
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

	load		(R14+4),R5		; R5= instrument table  ( =+$10)  = a2   / m_lspInstruments-1 = 5-1

;--------------------------
; gestion des instruments
;--------------------------
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
; passage en .L
	shlq		#16,R3
	sharq		#16,R3
	add			R3,R5				;R5=pointeur datas instruments
	addq		#2,R0


	movei		#LSP_DSP_PAULA_AUD3L,R7
	loadw		(R5),R6
	addq		#2,R5
	shlq		#16,R6
	loadw		(R5),R8
	or			R8,R6
	movei		#LSP_DSP_PAULA_AUD3LEN,R8
	shlq		#nb_bits_virgule_offset,R6		
	store		R6,(R7)				; stocke le pointeur sample a virgule dans LSP_DSP_PAULA_AUD3L
	addq		#2,R5
	loadw		(R5),R9				; .w = R9 = taille du sample
	shlq		#nb_bits_virgule_offset,R9				; en 16:16
	add			R6,R9				; taille devient fin du sample, a virgule
	store		R9,(R8)				; stocke la nouvelle fin a virgule
	addq		#2,R5				; positionne sur pointeur de repeat
; repeat pointeur
	movei		#LSP_DSP_repeat_pointeur3,R7
	loadw		(R5),R4
	addq		#2,R5
	shlq		#16,R4
	loadw		(R5),R8
	or			R8,R4
	addq		#2,R5
	shlq		#nb_bits_virgule_offset,R4	
	store		R4,(R7)				; pointeur sample repeat, a virgule
; repeat length
	movei		#LSP_DSP_repeat_length3,R7
	loadw		(R5),R8				; .w = R8 = taille du sample
	shlq		#nb_bits_virgule_offset,R8				; en 16:16
	add			R4,R8
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
	store		R9,(R8)				; stocke la nouvelle taille en 16:16: dans LSP_DSP_PAULA_internal_length3
; remplace les 4 octets en stock
	move		R6,R12
	shrq		#nb_bits_virgule_offset+2,R12	; enleve la virgule  + 2 bits du bas
	movei		#LSP_DSP_PAULA_AUD3DAT,R8
	shlq		#2,R12
	load		(R12),R7
	store		R7,(R8)
	

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
; passage en .L
	shlq		#16,R3
	sharq		#16,R3
	add			R3,R5				;R5=pointeur datas instruments
	addq		#2,R0


	movei		#LSP_DSP_PAULA_AUD2L,R7
	loadw		(R5),R6
	addq		#2,R5
	shlq		#16,R6
	loadw		(R5),R8
	or			R8,R6
	movei		#LSP_DSP_PAULA_AUD2LEN,R8
	shlq		#nb_bits_virgule_offset,R6		
	store		R6,(R7)				; stocke le pointeur sample a virgule dans LSP_DSP_PAULA_AUD3L
	addq		#2,R5
	loadw		(R5),R9				; .w = R9 = taille du sample
	shlq		#nb_bits_virgule_offset,R9				; en 16:16
	add			R6,R9				; taille devient fin du sample, a virgule
	store		R9,(R8)				; stocke la nouvelle fin a virgule
	addq		#2,R5				; positionne sur pointeur de repeat
; repeat pointeur
	movei		#LSP_DSP_repeat_pointeur2,R7
	loadw		(R5),R4
	addq		#2,R5
	shlq		#16,R4
	loadw		(R5),R8
	or			R8,R4
	addq		#2,R5
	shlq		#nb_bits_virgule_offset,R4	
	store		R4,(R7)				; pointeur sample repeat, a virgule
; repeat length
	movei		#LSP_DSP_repeat_length2,R7
	loadw		(R5),R8				; .w = R8 = taille du sample
	shlq		#nb_bits_virgule_offset,R8				; en 16:16
	add			R4,R8
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
	store		R9,(R8)				; stocke la nouvelle taille en 16:16: dans LSP_DSP_PAULA_internal_length3
; remplace les 4 octets en stock
	move		R6,R12
	shrq		#nb_bits_virgule_offset+2,R12	; enleve la virgule  + 2 bits du bas
	movei		#LSP_DSP_PAULA_AUD2DAT,R8
	shlq		#2,R12
	load		(R12),R7
	store		R7,(R8)
	

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
; passage en .L
	shlq		#16,R3
	sharq		#16,R3
	add			R3,R5				;R5=pointeur datas instruments
	addq		#2,R0


	movei		#LSP_DSP_PAULA_AUD1L,R7
	loadw		(R5),R6
	addq		#2,R5
	shlq		#16,R6
	loadw		(R5),R8
	or			R8,R6
	movei		#LSP_DSP_PAULA_AUD1LEN,R8
	shlq		#nb_bits_virgule_offset,R6		
	store		R6,(R7)				; stocke le pointeur sample a virgule dans LSP_DSP_PAULA_AUD3L
	addq		#2,R5
	loadw		(R5),R9				; .w = R9 = taille du sample
	shlq		#nb_bits_virgule_offset,R9				; en 16:16
	add			R6,R9				; taille devient fin du sample, a virgule
	store		R9,(R8)				; stocke la nouvelle fin a virgule
	addq		#2,R5				; positionne sur pointeur de repeat
; repeat pointeur
	movei		#LSP_DSP_repeat_pointeur1,R7
	loadw		(R5),R4
	addq		#2,R5
	shlq		#16,R4
	loadw		(R5),R8
	or			R8,R4
	addq		#2,R5
	shlq		#nb_bits_virgule_offset,R4	
	store		R4,(R7)				; pointeur sample repeat, a virgule
; repeat length
	movei		#LSP_DSP_repeat_length1,R7
	loadw		(R5),R8				; .w = R8 = taille du sample
	shlq		#nb_bits_virgule_offset,R8				; en 16:16
	add			R4,R8
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
	store		R9,(R8)				; stocke la nouvelle taille en 16:16: dans LSP_DSP_PAULA_internal_length3
; remplace les 4 octets en stock
	move		R6,R12
	shrq		#nb_bits_virgule_offset+2,R12	; enleve la virgule  + 2 bits du bas
	movei		#LSP_DSP_PAULA_AUD1DAT,R8
	shlq		#2,R12
	load		(R12),R7
	store		R7,(R8)
	

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
; passage en .L
	shlq		#16,R3
	sharq		#16,R3
	add			R3,R5				;R5=pointeur datas instruments
	addq		#2,R0


	movei		#LSP_DSP_PAULA_AUD0L,R7
	loadw		(R5),R6
	addq		#2,R5
	shlq		#16,R6
	loadw		(R5),R8
	or			R8,R6
	movei		#LSP_DSP_PAULA_AUD0LEN,R8
	shlq		#nb_bits_virgule_offset,R6		
	store		R6,(R7)				; stocke le pointeur sample a virgule dans LSP_DSP_PAULA_AUD3L
	addq		#2,R5
	loadw		(R5),R9				; .w = R9 = taille du sample
	shlq		#nb_bits_virgule_offset,R9				; en 16:16
	add			R6,R9				; taille devient fin du sample, a virgule
	store		R9,(R8)				; stocke la nouvelle fin a virgule
	addq		#2,R5				; positionne sur pointeur de repeat
; repeat pointeur
	movei		#LSP_DSP_repeat_pointeur0,R7
	loadw		(R5),R4
	addq		#2,R5
	shlq		#16,R4
	loadw		(R5),R8
	or			R8,R4
	addq		#2,R5
	shlq		#nb_bits_virgule_offset,R4	
	store		R4,(R7)				; pointeur sample repeat, a virgule
; repeat length
	movei		#LSP_DSP_repeat_length0,R7
	loadw		(R5),R8				; .w = R8 = taille du sample
	shlq		#nb_bits_virgule_offset,R8				; en 16:16
	add			R4,R8
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
	store		R9,(R8)				; stocke la nouvelle taille en 16:16: dans LSP_DSP_PAULA_internal_length3

; remplace les 4 octets en stock
	move		R6,R12
	shrq		#nb_bits_virgule_offset+2,R12	; enleve la virgule  + 2 bits du bas
	movei		#LSP_DSP_PAULA_AUD0DAT,R8
	shlq		#2,R12
	load		(R12),R7
	store		R7,(R8)
	

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

; conversion period => increment voie 0
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
	shlq		#nb_bits_virgule_offset,R4
	div			R0,R4			; (3546895 / note) / frequence I2S en 16:16
	or			R4,R4
.2:
	store		R4,(R1)
; conversion period => increment voie 1
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
	shlq		#nb_bits_virgule_offset,R4
	div			R0,R4			; (3546895 / note) / frequence I2S en 16:16
	or			R4,R4
.22:
	store		R4,(R1)

; conversion period => increment voie 2
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
	shlq		#nb_bits_virgule_offset,R4
	div			R0,R4			; (3546895 / note) / frequence I2S en 16:16
	or			R4,R4
.23:
	store		R4,(R1)

; conversion period => increment voie 3
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
	shlq		#nb_bits_virgule_offset,R4
	div			R0,R4			; (3546895 / note) / frequence I2S en 16:16
	or			R4,R4
.24:
	store		R4,(R1)

;--------------------------------------------------




	
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

;------------------------------------	
;rewind
DSP_LSP_Timer1_r_rewind:
;	movei		#LSPVars,R14
;	load		(R14),R0					; R0 = byte stream
	load		(R14+8),R0			; bouclage : R0 = byte stream / m_byteStreamLoop = 8
	movei		#DSP_LSP_Timer1_process,R12
	load		(R14+9),R3			; m_wordStreamLoop=9
	jump		(R12)
	store		R3,(R14+1)				; m_wordStream=1

;------------------------------------	
; change bpm
DSP_LSP_Timer1_r_chgbpm:
	movei		#DSP_LSP_Timer1_process,R12
	loadb		(R0),R11
	store		R11,(R14+7)		; R3=nouveau bpm / m_currentBpm = 7
;application nouveau bpm dans Timer 1
	movei	#60*256,R10
	;shlq	#8,R10				; 16 bits de virgule
	div		R11,R10				; 60/bpm
	movei	#24*65536,R9				; 24=> 5 bits
	or		R10,R10
	;shlq	#16,R9
	div		R10,R9				; R9=
	or		R9,R9
	shrq	#8,R9				; R9=frequence replay 
	;move	R9,R11	
; frequence du timer 1
	movei	#182150,R10				; 26593900 / 146 = 182150
	div		R9,R10
	or		R10,R10
	move	R10,R14
	subq	#1,R14					; -1 pour parametrage du timer 1
; 26593900 / 50 = 531 878 => 2 × 73 × 3643 => 146*3643
	movei	#JPIT1,r10				; F10000
	movei	#145*65536,r9				; Timer 1 Pre-scaler
	;shlq	#16,r12
	or		R14,R9
	store	r9,(r10)				; JPIT1 & JPIT2



	jump		(R12)
	addq		#1,R0




; ------------------- N/A ------------------
DSP_LSP_routine_interruption_Timer2:
; ------------------- N/A ------------------

;DSP_pad1
;DSP_pad2
; lecture des 2 pads
; Pads : mask = xxxx xxCx xxBx 2580 147* oxAP 369# RLDU
; dispos : R0 à R12
	movei		#DSP_pad1,R11
	movei		#DSP_pad2,R12
	movei		#JOYSTICK,R0

	movei		#%00001111000000000000000000000000,R2		; mask port 1
	movei		#%00000000000000000000000000000011,R3		; mask port 1

	movei		#%11110000000000000000000000000000,R5		; mask port 2
	movei		#%00000000000000000000000000001100,R6		; mask port 2



; row 0
	MOVEI		#$817e,R1			; =81<<8 + 0111 1110 = (A Pause) + (Right Left Down Up) / 81 pour bit 15 pour output + bit 8 pour  conserver le son ON : pad 1 & 2
									; 1110 = row 0 of joypad = Pause A Up Down Left Right
	storew		R1,(R0)				; lecture row 0
	nop
	load		(R0),R1
	;movei		#$F000000C,R3		; mask port 2
	
; row0 = Pause A Up Down Left Right
; 0000 1111 0000 0000 0000 0000 0000 0011
;      RLDU                            Ap
	move		R1,R10				; stocke pour lecture port 2
	
	move		R1,R4
	move		R10,R7
	and			R3,R4		
	and			R6,R7		
	and			R2,R1				
	and			R5,R10				
	shlq		#8,R4				; R4=Ap xxxx xxxx
	shlq		#6,R7				; R4=Ap xxxx xxxx
	shrq		#24,R1				; R1=RLDU
	shrq		#28,R10				; R10=RLDU
	or			R4,R1
	or			R7,R10
	move		R1,R8
	move		R10,R9



; row 1
	MOVEI		#$81BD,R1			; #($81 << 8)|(%1011 << 4)|(%1101),(a2) ; (B D) + (1 4 7 *)
	storew		R1,(R0)				; lecture row 1
	nop
	load		(R0),R1
; row1 = 
; 0000 1111 0000 0000 0000 0000 0000 0011
;      147*                            BD
	move		R1,R10				; stocke pour lecture port 2
;row1 port 1&2

	move		R1,R4
	move		R10,R7
	and			R3,R4
	and			R6,R7		
	shlq		#20,R4
	shlq		#18,R7				
	and			R2,R1				
	and			R5,R10				
	shrq		#12,R1				; R1=147*
	shrq		#16,R10				; R10=147*
	or			R1,R4
	or			R7,R10
	or			R4,R8				; R8= BD xxxx 147* xxAp xxxx RLDU
	or			R10,R9


; row 2
	MOVEI		#$81DB,R1			; #($81 << 8)|(%1101 << 4)|(%1011),(a2) ; (C E) + (2 5 8 0)
	storew		R1,(R0)				; lecture row 2
	nop
	load		(R0),R1
	move		R1,R10				; stocke pour lecture port 2

; row2 = 
; 0000 1111 0000 0000 0000 0000 0000 0011
;      2580                            CE
; 24,8,22,12
	move		R1,R4
	move		R10,R7
	and			R3,R4
	and			R6,R7		
	shlq		#24,R4
	shlq		#22,R7				
	and			R2,R1				
	and			R5,R10				
	shrq		#8,R1				; R1=147*
	shrq		#12,R10				; R10=147*
	or			R1,R4
	or			R7,R10
	or			R4,R8				; R8= BD xxxx 147* xxAp xxxx RLDU
	or			R10,R9



; row 3
	MOVEI		#$81E7,R1			; #($81 << 8)|(%1110 << 4)|(%0111),(a2) ; (Option F) + (3 6 9 #)
	storew		R1,(R0)				; lecture row 3
	nop
	load		(R0),R1
; row3 = 
; 0000 1111 0000 0000 0000 0000 0000 0011
;      369#                            oF
; l10,r20,l8,r24
	move		R1,R10				; stocke pour lecture port 2

	move		R1,R4
	move		R10,R7
	and			R3,R4
	and			R6,R7		
	shlq		#10,R4
	shlq		#8,R7				
	and			R2,R1				
	and			R5,R10				
	shrq		#20,R1				; R1=147*
	shrq		#24,R10				; R10=147*
	or			R1,R4
	or			R7,R10
	or			R4,R8				; R8= BD xxxx 147* xxAp xxxx RLDU
	or			R10,R9

	
	
	not			R8
	not			R9
	store		R8,(R11)
	store		R9,(R12)
	
	
									
									
									
;------------------------------------	
; return from interrupt Timer 2
	load	(r31),r12	; return address
	;bset	#10,r13		; clear latch 1 = I2S
	;bset	#11,r13		; clear latch 1 = timer 1
	bset	#12,r13		; clear latch 1 = timer 2
	bclr	#3,r13		; clear IMASK
	addq	#4,r31		; pop from stack
	addqt	#2,r12		; next instruction
	jump	t,(r12)		; return
	store	r13,(r16)	; restore flags



; ------------- main DSP ------------------
DSP_routine_init_DSP:
; assume run from bank 1
	movei	#DSP_ISP+(DSP_STACK_SIZE*4),r31			; init isp
	moveq	#0,r1
	moveta	r31,r31									; ISP (bank 0)
	nop
	movei	#DSP_USP+(DSP_STACK_SIZE*4),r31			; init usp

; calculs des frequences deplacé dans DSP
; sclk I2S
	movei	#LSP_DSP_Audio_frequence,R0
	movei	#frequence_Video_Clock_divisee,R1
	load	(R1),R1
	shlq	#8,R1
	div		R0,R1
	movei	#128,R2
	add		R2,R1			; +128 = +0.5
	shrq	#8,R1
	subq	#1,R1
	movei	#DSP_parametre_de_frequence_I2S,r2
	store	R1,(R2)
;calcul inverse
	addq	#1,R1
	add		R1,R1			; *2
	add		R1,R1			; *2
	shlq	#4,R1			; *16
	movei	#frequence_Video_Clock,R0
	load	(R0),R0
	div		R1,R0
	movei	#DSP_frequence_de_replay_reelle_I2S,R2
	store	R0,(R2)
	

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
	movei	#60*256,R10
	;shlq	#8,R10				; 16 bits de virgule
	div		R11,R10				; 60/bpm
	movei	#24*65536,R9				; 24=> 5 bits
	or		R10,R10
	;shlq	#16,R9
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
	movei	#145*65536,r12				; Timer 1 Pre-scaler
	;shlq	#16,r12
	or		R13,R12
	
	store	r12,(r10)				; JPIT1 & JPIT2


; init timer 2
	movei	#JPIT3,r10				; F10004
	;movei	#JPIT4,r11				; F10006
	movei	#145*65536,r12			; Timer 1 Pre-scaler
	movei	#955-1,r13				; 951=200hz
	or		R13,R12
	store	r12,(r10)				; JPIT1 & JPIT2


; enable interrupts
	movei	#D_FLAGS,r28
	
	movei	#D_I2SENA|D_TIM1ENA|D_TIM2ENA|REGPAGE,r29			; I2S+Timer 1+timer 2
	;movei	#D_I2SENA|D_TIM1ENA|REGPAGE,r29			; I2S+Timer 1
	;movei	#D_I2SENA|REGPAGE,r29					; I2S only
	
	
	;movei	#D_TIM1ENA|REGPAGE,r29					; Timer 1 only
	;movei	#D_TIM2ENA|REGPAGE,r29					; Timer 2 only

	store	r29,(r28)

DSP_boucle_centrale:
	movei	#DSP_boucle_centrale,R20
	jump	(R20)
	nop
	
	
	.phrase

EDZTMP1:											dc.l			$120771

DSP_frequence_de_replay_reelle_I2S:					dc.l			0
DSP_UN_sur_frequence_de_replay_reelle_I2S:			dc.l			0
DSP_parametre_de_frequence_I2S:						dc.l			0

; variables Paula
; channel 0
LSP_DSP_PAULA_AUD0L:				dc.l			silence<<nb_bits_virgule_offset			; Audio channel 0 location
LSP_DSP_PAULA_AUD0LEN:				dc.l			(silence+4)<<nb_bits_virgule_offset			; en bytes !
LSP_DSP_PAULA_AUD0PER:				dc.l			0				; period , a transformer en increment
LSP_DSP_PAULA_AUD0VOL:				dc.l			0				; volume
LSP_DSP_PAULA_AUD0DAT:				dc.l			0				; long word en cours d'utilisation / stocké / buffering
LSP_DSP_PAULA_internal_location0:	dc.l			silence<<nb_bits_virgule_offset				; internal register : location of the sample currently played
LSP_DSP_PAULA_internal_increment0:	dc.l			0				; internal register : increment linked to period 16:16
LSP_DSP_PAULA_internal_length0:		dc.l			(silence+4)<<nb_bits_virgule_offset			; internal register : length of the sample currently played
LSP_DSP_repeat_pointeur0:			dc.l			silence<<nb_bits_virgule_offset
LSP_DSP_repeat_length0:				dc.l			(silence+4)<<nb_bits_virgule_offset
; channel 1
LSP_DSP_PAULA_AUD1L:				dc.l			silence<<nb_bits_virgule_offset			; Audio channel 0 location
LSP_DSP_PAULA_AUD1LEN:				dc.l			(silence+4)<<nb_bits_virgule_offset			; en bytes !
LSP_DSP_PAULA_AUD1PER:				dc.l			0				; period , a transformer en increment
LSP_DSP_PAULA_AUD1VOL:				dc.l			0				; volume
LSP_DSP_PAULA_AUD1DAT:				dc.l			0				; long word en cours d'utilisation / stocké / buffering
LSP_DSP_PAULA_internal_location1:	dc.l			silence<<nb_bits_virgule_offset				; internal register : location of the sample currently played
LSP_DSP_PAULA_internal_increment1:	dc.l			0				; internal register : increment linked to period 16:16
LSP_DSP_PAULA_internal_length1:		dc.l			(silence+4)<<nb_bits_virgule_offset			; internal register : length of the sample currently played
LSP_DSP_repeat_pointeur1:			dc.l			silence<<nb_bits_virgule_offset
LSP_DSP_repeat_length1:				dc.l			(silence+4)<<nb_bits_virgule_offset
; channel 2
LSP_DSP_PAULA_AUD2L:				dc.l			silence<<nb_bits_virgule_offset			; Audio channel 0 location
LSP_DSP_PAULA_AUD2LEN:				dc.l			(silence+4)<<nb_bits_virgule_offset			; en bytes !
LSP_DSP_PAULA_AUD2PER:				dc.l			0				; period , a transformer en increment
LSP_DSP_PAULA_AUD2VOL:				dc.l			0				; volume
LSP_DSP_PAULA_AUD2DAT:				dc.l			0				; long word en cours d'utilisation / stocké / buffering
LSP_DSP_PAULA_internal_location2:	dc.l			silence<<nb_bits_virgule_offset				; internal register : location of the sample currently played
LSP_DSP_PAULA_internal_increment2:	dc.l			0				; internal register : increment linked to period 16:16
LSP_DSP_PAULA_internal_length2:		dc.l			(silence+4)<<nb_bits_virgule_offset			; internal register : length of the sample currently played
LSP_DSP_repeat_pointeur2:			dc.l			silence<<nb_bits_virgule_offset
LSP_DSP_repeat_length2:				dc.l			(silence+4)<<nb_bits_virgule_offset
; channel 3
LSP_DSP_PAULA_AUD3L:				dc.l			silence<<nb_bits_virgule_offset			; Audio channel 0 location
LSP_DSP_PAULA_AUD3LEN:				dc.l			(silence+4)<<nb_bits_virgule_offset			; en bytes !
LSP_DSP_PAULA_AUD3PER:				dc.l			0				; period , a transformer en increment
LSP_DSP_PAULA_AUD3VOL:				dc.l			0				; volume
LSP_DSP_PAULA_AUD3DAT:				dc.l			0				; long word en cours d'utilisation / stocké / buffering
LSP_DSP_PAULA_internal_location3:	dc.l			silence<<nb_bits_virgule_offset				; internal register : location of the sample currently played
LSP_DSP_PAULA_internal_increment3:	dc.l			0				; internal register : increment linked to period 16:16
LSP_DSP_PAULA_internal_length3:		dc.l			(silence+4)<<nb_bits_virgule_offset			; internal register : length of the sample currently played
LSP_DSP_repeat_pointeur3:			dc.l			silence<<nb_bits_virgule_offset
LSP_DSP_repeat_length3:				dc.l			(silence+4)<<nb_bits_virgule_offset


LSPVars:
m_byteStream:		dc.l	0	;  0 :  byte stream							0
m_wordStream:		dc.l	0	;  4 :  word stream							1
m_codeTableAddr:	dc.l	0	;  8 :  code table addr						2
m_escCodeRewind:	dc.l	0	; 12 :  rewind special escape code			3
m_escCodeSetBpm:	dc.l	0	; 16 :  set BPM escape code					4
m_lspInstruments:	dc.l	0	; 20 :  LSP instruments table addr			5
m_relocDone:		dc.l	0	; 24 :  reloc done flag						6
m_currentBpm:		dc.l	0	; 28 :  current BPM							7
m_byteStreamLoop:	dc.l	0	; 32 :  byte stream loop point				8
m_wordStreamLoop:	dc.l	0	; 36 :  word stream loop point				9



LSP_BPM_frequence_replay:		dc.l			25

; pads
; Pads : mask = xxxxxxCx xxBx2580 147*oxAP 369#RLDU
; U235 format
;------------------------------------------------------------------------------------------------ Joypad Section

										; Pads : mask = xxxxxxCx xxBx2580 147*oxAP 369#RLDU

; 												Bit numbers for buttons in the mask for testing individual bits
U235SE_BBUT_UP			EQU		0		; Up
U235SE_BBUT_U			EQU		0
U235SE_BBUT_DOWN		EQU		1		; Down
U235SE_BBUT_D			EQU		1
U235SE_BBUT_LEFT		EQU		2		; Left
U235SE_BBUT_L			EQU		2
U235SE_BBUT_RIGHT		EQU		3		; Right
U235SE_BBUT_R			EQU		3		
U235SE_BBUT_HASH		EQU		4		; Hash (#)
U235SE_BBUT_9			EQU		5		; 9
U235SE_BBUT_6			EQU		6		; 6
U235SE_BBUT_3			EQU		7		; 3
U235SE_BBUT_PAUSE		EQU		8		; Pause
U235SE_BBUT_A			EQU		9		; A button
U235SE_BBUT_OPTION		EQU		11		; Option
U235SE_BBUT_STAR		EQU		12		; Star 
U235SE_BBUT_7			EQU		13		; 7
U235SE_BBUT_4			EQU		14		; 4
U235SE_BBUT_1			EQU		15		; 1
U235SE_BBUT_0			EQU		16		; 0 (zero)
U235SE_BBUT_8			EQU		17		; 8
U235SE_BBUT_5			EQU		18		; 5
U235SE_BBUT_2			EQU		19		; 2
U235SE_BBUT_B			EQU		21		; B button
U235SE_BBUT_C			EQU		25		; C button

; 												Numerical representations
U235SE_BUT_UP			EQU		1		; Up
U235SE_BUT_U			EQU		1
U235SE_BUT_DOWN			EQU		2		; Down
U235SE_BUT_D			EQU		2
U235SE_BUT_LEFT			EQU		4		; Left
U235SE_BUT_L			EQU		4
U235SE_BUT_RIGHT		EQU		8		; Right
U235SE_BUT_R			EQU		8		
U235SE_BUT_HASH			EQU		16		; Hash (#)
U235SE_BUT_9			EQU		32		; 9
U235SE_BUT_6			EQU		64		; 6
U235SE_BUT_3			EQU		$80		; 3
U235SE_BUT_PAUSE		EQU		$100	; Pause
U235SE_BUT_A			EQU		$200	; A button
U235SE_BUT_OPTION		EQU		$800	; Option
U235SE_BUT_STAR			EQU		$1000	; Star 
U235SE_BUT_7			EQU		$2000	; 7
U235SE_BUT_4			EQU		$4000	; 4
U235SE_BUT_1			EQU		$8000	; 1
U235SE_BUT_0			EQU		$10000	; 0 (zero)
U235SE_BUT_8			EQU		$20000	; 8
U235SE_BUT_5			EQU		$40000	; 5
U235SE_BUT_2			EQU		$80000	; 2
U235SE_BUT_B			EQU		$200000	; B button
U235SE_BUT_C			EQU		$2000000; C button

; xxxxxxCx xxBx2580 147*oxAP 369#RLDU
DSP_pad1:				dc.l		0
DSP_pad2:				dc.l		0


	.phrase

;---------------------
; FIN DE LA RAM DSP
YM_DSP_fin:
;---------------------


SOUND_DRIVER_SIZE			.equ			YM_DSP_fin-DSP_base_memoire
	.print	"--- Sound driver code size (DSP): ", /u SOUND_DRIVER_SIZE, " bytes / 8192 ---"


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

stoplist:		dc.l	0,4

silence:		
		dc.b			$0,$0,$0,$0
		dc.b			$0,$0,$0,$0
		dc.b			$0,$0,$0,$0


debut_ram_libre_DSP:		dc.l			YM_DSP_fin
debut_ram_libre:			dc.l			FIN_RAM
	even

fonte:	
	.include	"fonte1plan.s"
	even

couleur_char:				dc.w		25
curseur_x:					dc.w		0
curseur_y:					dc.w		curseur_Y_min
		even
		
chaine_LSP:						dc.b	"LSP player for Jaguar",10,0
chaine_playing_LSP:				dc.b	"Now playing module",10,0
chaine_BPM_init_LSP:			dc.b	" bpm.",0
chaine_Hz_init_LSP:				dc.b	" Hz.",10,0
chaine_replay_frequency:		dc.b	"Replay frequency : ",0
chaine_RAM_DSP:					dc.b	"DSP RAM available while running : ",0
chaine_entete_debug_module:		dc.b	"location incremen offset   end  ",10,0
chaine_entete_debug_module2:	dc.b	"location length   repeat_s rep end",10,0
		even

	.phrase
LSP_module_music_data:
	.incbin			"LSP/elysium.lsmusic"
	;.incbin			"LSP/d.lsmusic"
	;.incbin			"LSP/k.lsmusic"
	;.incbin			"LSP/testsamples4v.lsmusic"
	
	.phrase
LSP_module_sound_bank:
	.incbin			"LSP/elysium.lsbank"
	;.incbin			"LSP/d.lsbank"
	;.incbin			"LSP/k.lsbank"
	;.incbin			"LSP/testsamples4v.lsbank"
	
	.phrase

	.bss
	.phrase
DEBUT_BSS:
;EDZ_compteur_reset_offset_entier_voie_A:			ds.l	1

	.phrase
frequence_Video_Clock:					ds.l				1
frequence_Video_Clock_divisee :			ds.l				1



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



