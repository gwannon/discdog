 processor 6502
 include "vcs.h"
 include "macro.h"
 include "2600basic.h"
 include "2600basic_variable_redefs.h"
 ifconst bankswitch
  if bankswitch == 8
     ORG $1000
     RORG $D000
  endif
  if bankswitch == 16
     ORG $1000
     RORG $9000
  endif
  if bankswitch == 32
     ORG $1000
     RORG $1000
  endif
 else
   ORG $F000
 endif
; This is a 2-line kernel!
kernel
 sta WSYNC
 lda #255
 sta TIM64T

 lda #1
 sta VDELBL
 sta VDELP0
 ldx ballheight
 inx
 inx
 stx temp4
 lda player1y
 sta temp3

 ifconst shakescreen
   jsr doshakescreen
 else
   ldx missile0height
   inx
 endif

 inx
 stx stack1

 lda bally
 sta stack2

 lda player0y
 ldx #0
 sta WSYNC
 stx GRP0
 stx GRP1
 stx PF1
 stx PF2
 stx CXCLR
 ifconst readpaddle
   stx paddle
 else
   sleep 3
 endif

 sta temp2,x

 ;store these so they can be retrieved later
 ifnconst pfres
   ldx #128-44
 else
   ldx #132-pfres*4
 endif

 inc player1y

 lda missile0y
 sta temp5
 lda missile1y
 sta temp6

 lda playfieldpos
 sta temp1
 
 ifconst pfrowheight
 lda #pfrowheight+2
 else
 ifnconst pfres
   lda #10
 else
   lda #(96/pfres)+2 ; try to come close to the real size
 endif
 endif

 clc
 sbc playfieldpos
 sta playfieldpos
 jmp .startkernel

.skipDrawP0
 lda #0
 tay
 jmp .continueP0

.skipDrawP1
 lda #0
 tay
 jmp .continueP1

.kerloop ; enter at cycle 59??

continuekernel
 sleep 2
continuekernel2
 lda ballheight
 
 ifconst pfres
 ldy playfield+pfres*4-132,x
 sty PF1 ;3
 ldy playfield+pfres*4-131,x
 sty PF2 ;3
 ldy playfield+pfres*4-129,x
 sty PF1 ; 3 too early?
 ldy playfield+pfres*4-130,x
 sty PF2 ;3
 else
 ldy playfield+44-128,x ;4
 sty PF1 ;3
 ldy playfield+45-128,x ;4
 sty PF2 ;3
 ldy playfield+47-128,x ;4
 sty PF1 ; 3 too early?
 ldy playfield+46-128,x;4
 sty PF2 ;3
 endif

 dcp bally
 rol
 rol
; rol
; rol
goback
 sta ENABL 
.startkernel
 lda player1height ;3
 dcp player1y ;5
 bcc .skipDrawP1 ;2
 ldy player1y ;3
 lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
			; so it doesn't cross a page boundary!

.continueP1
 sta GRP1 ;3

 ifnconst player1colors
   lda missile1height ;3
   dcp missile1y ;5
   rol;2
   rol;2
   sta ENAM1 ;3
 else
   lda (player1color),y
   sta COLUP1
 ifnconst playercolors
   sleep 7
 else
   lda.w player0colorstore
   sta COLUP0
 endif
 endif

 ifconst pfres
 lda playfield+pfres*4-132,x 
 sta PF1 ;3
 lda playfield+pfres*4-131,x 
 sta PF2 ;3
 lda playfield+pfres*4-129,x 
 sta PF1 ; 3 too early?
 lda playfield+pfres*4-130,x 
 sta PF2 ;3
 else
 lda playfield+44-128,x ;4
 sta PF1 ;3
 lda playfield+45-128,x ;4
 sta PF2 ;3
 lda playfield+47-128,x ;4
 sta PF1 ; 3 too early?
 lda playfield+46-128,x;4
 sta PF2 ;3
 endif 
; sleep 3

 lda player0height
 dcp player0y
 bcc .skipDrawP0
 ldy player0y
 lda (player0pointer),y
.continueP0
 sta GRP0

 ifnconst no_blank_lines
 ifnconst playercolors
   lda missile0height ;3
   dcp missile0y ;5
   sbc stack1
   sta ENAM0 ;3
 else
   lda (player0color),y
   sta player0colorstore
   sleep 6
 endif
   dec temp1
   bne continuekernel
 else
   dec temp1
   beq altkernel2
 ifconst readpaddle
   ldy currentpaddle
   lda INPT0,y
   bpl noreadpaddle
   inc paddle
   jmp continuekernel2
noreadpaddle
   sleep 2
   jmp continuekernel
 else
 ifnconst playercolors 
 ifconst PFcolors
   txa
   tay
   lda (pfcolortable),y
 ifnconst backgroundchange
   sta COLUPF
 else
   sta COLUBK
 endif
   jmp continuekernel
 else
   sleep 12
 endif
 else
   lda (player0color),y
   sta player0colorstore
   sleep 4
 endif
   jmp continuekernel
 endif
altkernel2
   txa
   sbx #252
   bmi lastkernelline
 ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif
   sta temp1
   jmp continuekernel
 endif

altkernel

 ifconst PFmaskvalue
   lda #PFmaskvalue
 else
   lda #0
 endif
 sta PF1
 sta PF2


 ;sleep 3

 ;28 cycles to fix things
 ;minus 11=17

; lax temp4
; clc
 txa
 sbx #252

 bmi lastkernelline

 ifconst PFcolorandheight
   ldy playfieldcolorandheight-87,x
 ifnconst backgroundchange
   sty COLUPF
 else
   sty COLUBK
 endif
   lda playfieldcolorandheight-88,x
   sta.w temp1
 endif
 ifconst PFheights
   lsr
   lsr
   tay
   lda (pfheighttable),y
   sta.w temp1
 endif
 ifconst PFcolors
   tay
   lda (pfcolortable),y
 ifnconst backgroundchange
   sta COLUPF
 else
   sta COLUBK
 endif
 ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif
   sta temp1
 endif
 ifnconst PFcolorandheight
 ifnconst PFcolors
 ifnconst PFheights
 ifnconst no_blank_lines
 ; read paddle 0
 ; lo-res paddle read
  ; bit INPT0
  ; bmi paddleskipread
  ; inc paddle0
;donepaddleskip
   sleep 10
 ifconst pfrowheight
   lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif
   sta temp1
 endif
 endif
 endif
 endif
 

 lda ballheight
 dcp bally
 sbc temp4


 jmp goback


 ifnconst no_blank_lines
lastkernelline
 ifnconst PFcolors
   sleep 10
 else
   ldy #124
   lda (pfcolortable),y
   sta COLUPF
 endif

 ifconst PFheights
 ldx #1
 sleep 4
 else
 ldx playfieldpos
 sleep 3
 endif

 jmp enterlastkernel

 else
lastkernelline
 
 ifconst PFheights
 ldx #1
 sleep 5
 else
   ldx playfieldpos
 sleep 4
 endif

   cpx #1
   bne .enterfromNBL
   jmp no_blank_lines_bailout
 endif

 if ((<*)>$d5)
 align 256
 endif
 ; this is a kludge to prevent page wrapping - fix!!!

.skipDrawlastP1
 sleep 2
 lda #0
 jmp .continuelastP1

.endkerloop ; enter at cycle 59??
 
 nop

.enterfromNBL
 ifconst pfres
 ldy.w playfield+pfres*4-4
 sty PF1 ;3
 ldy.w playfield+pfres*4-3
 sty PF2 ;3
 ldy.w playfield+pfres*4-1
 sty PF1 ; possibly too early?
 ldy.w playfield+pfres*4-2
 sty PF2 ;3
 else
 ldy.w playfield+44
 sty PF1 ;3
 ldy.w playfield+45
 sty PF2 ;3
 ldy.w playfield+47
 sty PF1 ; possibly too early?
 ldy.w playfield+46
 sty PF2 ;3
 endif

enterlastkernel
 lda ballheight

; tya
 dcp bally
; sleep 4

; sbc stack3
 rol
 rol
 sta ENABL 

 lda player1height ;3
 dcp player1y ;5
 bcc .skipDrawlastP1
 ldy player1y ;3
 lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
			; so it doesn't cross a page boundary!

.continuelastP1
 sta GRP1 ;3

 ifnconst player1colors
   lda missile1height ;3
   dcp missile1y ;5
 else
   lda (player1color),y
   sta COLUP1
 endif

 dex
 ;dec temp4 ; might try putting this above PF writes
 beq endkernel


 ifconst pfres
 ldy.w playfield+pfres*4-4
 sty PF1 ;3
 ldy.w playfield+pfres*4-3
 sty PF2 ;3
 ldy.w playfield+pfres*4-1
 sty PF1 ; possibly too early?
 ldy.w playfield+pfres*4-2
 sty PF2 ;3
 else
 ldy.w playfield+44
 sty PF1 ;3
 ldy.w playfield+45
 sty PF2 ;3
 ldy.w playfield+47
 sty PF1 ; possibly too early?
 ldy.w playfield+46
 sty PF2 ;3
 endif

 ifnconst player1colors
   rol;2
   rol;2
   sta ENAM1 ;3
 else
 ifnconst playercolors
   sleep 7
 else
   lda.w player0colorstore
   sta COLUP0
 endif
 endif
 
 lda.w player0height
 dcp player0y
 bcc .skipDrawlastP0
 ldy player0y
 lda (player0pointer),y
.continuelastP0
 sta GRP0



 ifnconst no_blank_lines
   lda missile0height ;3
   dcp missile0y ;5
   sbc stack1
   sta ENAM0 ;3
   jmp .endkerloop
 else
 ifconst readpaddle
   ldy currentpaddle
   lda INPT0,y
   bpl noreadpaddle2
   inc paddle
   jmp .endkerloop
noreadpaddle2
   sleep 4
   jmp .endkerloop
 else ; no_blank_lines and no paddle reading
 sleep 14
 jmp .endkerloop
 endif
 endif


;  ifconst donepaddleskip
;paddleskipread
 ; this is kind of lame, since it requires 4 cycles from a page boundary crossing
 ; plus we get a lo-res paddle read
; bmi donepaddleskip
;  endif

.skipDrawlastP0
 sleep 2
 lda #0
 jmp .continuelastP0

 ifconst no_blank_lines
no_blank_lines_bailout
 ldx #0
 endif

endkernel
 ; 6 digit score routine
 stx PF1
 stx PF2
 stx PF0
 clc

 ifconst pfrowheight
 lda #pfrowheight+2
 else
 ifnconst pfres
   lda #10
 else
   lda #(96/pfres)+2 ; try to come close to the real size
 endif
 endif

 sbc playfieldpos
 sta playfieldpos
 txa

 ifconst shakescreen
   bit shakescreen
   bmi noshakescreen2
   ldx #$3D
noshakescreen2
 endif

   sta WSYNC,x

;                STA WSYNC ;first one, need one more
 sta REFP0
 sta REFP1
                STA GRP0
                STA GRP1
 ;               STA PF1
   ;             STA PF2
 sta HMCLR
 sta ENAM0
 sta ENAM1
 sta ENABL

 lda temp2 ;restore variables that were obliterated by kernel
 sta player0y
 lda temp3
 sta player1y
 ifnconst player1colors
   lda temp6
   sta missile1y
 endif
 ifnconst playercolors
 ifnconst readpaddle
   lda temp5
   sta missile0y
 endif
 endif
 lda stack2
 sta bally

 ifconst no_blank_lines
 sta WSYNC
 endif

 lda INTIM
 clc
 ifnconst vblank_time
 adc #43+12+87
 else
 adc #vblank_time+12+87
 endif
; sta WSYNC
 sta TIM64T

 ifconst minikernel
 jsr minikernel
 endif

 ; now reassign temp vars for score pointers

; score pointers contain:
; score1-5: lo1,lo2,lo3,lo4,lo5,lo6
; swap lo2->temp1
; swap lo4->temp3
; swap lo6->temp5
 ifnconst noscore
 lda scorepointers+1
; ldy temp1
 sta temp1
; sty scorepointers+1

 lda scorepointers+3
; ldy temp3
 sta temp3
; sty scorepointers+3


 sta HMCLR
 tsx
 stx stack1 
 ldx #$10
 stx HMP0

 sta WSYNC
 ldx #0
                STx GRP0
                STx GRP1 ; seems to be needed because of vdel

 lda scorepointers+5
; ldy temp5
 sta temp5,x
; sty scorepointers+5
 lda #>scoretable
 sta scorepointers+1
 sta scorepointers+3
 sta scorepointers+5,x
 sta temp2,x
 sta temp4,x
 sta temp6,x
                LDY #7
                STA RESP0
                STA RESP1


        LDA #$03
        STA NUSIZ0
        STA NUSIZ1,x
        STA VDELP0
        STA VDELP1
        LDA #$20
        STA HMP1
               LDA scorecolor 
;               STA HMCLR
;               STA WSYNC; second one
                STA HMOVE ; cycle 73 ?

                STA COLUP0
                STA COLUP1
 lda  (scorepointers),y
 sta  GRP0
 ifconst pfscore
 lda pfscorecolor
 sta COLUPF
 endif
 lda  (scorepointers+8),y
 sta WSYNC
 sleep 2
 jmp beginscore

 if ((<*)>$d4)
 align 256 ; kludge that potentially wastes space!  should be fixed!
 endif

loop2
 lda  (scorepointers),y     ;+5  68  204
 sta  GRP0            ;+3  71  213      D1     --      --     --
 ifconst pfscore
 lda.w pfscore1
 sta PF1
 else
 sleep 7
 endif
 ; cycle 0
 lda  (scorepointers+$8),y  ;+5   5   15
beginscore
 sta  GRP1            ;+3   8   24      D1     D1      D2     --
 lda  (scorepointers+$6),y  ;+5  13   39
 sta  GRP0            ;+3  16   48      D3     D1      D2     D2
 lax  (scorepointers+$2),y  ;+5  29   87
 txs
 lax  (scorepointers+$4),y  ;+5  36  108
 sleep 3

 ifconst pfscore
 lda pfscore2
 sta PF1
 else
 sleep 6
 endif

 lda  (scorepointers+$A),y  ;+5  21   63
 stx  GRP1            ;+3  44  132      D3     D3      D4     D2!
 tsx
 stx  GRP0            ;+3  47  141      D5     D3!     D4     D4
 sta  GRP1            ;+3  50  150      D5     D5      D6     D4!
 sty  GRP0            ;+3  53  159      D4*    D5!     D6     D6
 dey
 bpl  loop2           ;+2  60  180

 ldx stack1 
 txs
; lda scorepointers+1
 ldy temp1
; sta temp1
 sty scorepointers+1

                LDA #0   
 sta PF1
               STA GRP0
                STA GRP1
        STA VDELP0
        STA VDELP1;do we need these
        STA NUSIZ0
        STA NUSIZ1

; lda scorepointers+3
 ldy temp3
; sta temp3
 sty scorepointers+3

; lda scorepointers+5
 ldy temp5
; sta temp5
 sty scorepointers+5
 endif ;noscore
 LDA #%11000010
 sta WSYNC
 STA VBLANK
 RETURN

 ifconst shakescreen
doshakescreen
   bit shakescreen
   bmi noshakescreen
   sta WSYNC
noshakescreen
   ldx missile0height
   inx
   rts
 endif

start
 sei
 cld
 ldy #0
 lda $D0
 cmp #$2C               ;check RAM location #1
 bne MachineIs2600
 lda $D1
 cmp #$A9               ;check RAM location #2
 bne MachineIs2600
 dey
MachineIs2600
 ldx #0
 txa
clearmem
 inx
 txs
 pha
 bne clearmem
 sty temp1
 ifconst pfrowheight
 lda pfrowheight
 else
 ifconst pfres
 lda #(96/pfres)
 else
 lda #8
 endif
 endif
 sta playfieldpos
 ldx #5
initscore
 lda #<scoretable
 sta scorepointers,x 
 dex
 bpl initscore
 lda #1
 sta CTRLPF
 ora INTIM
 sta rand

 ifconst multisprite
   jsr multisprite_setup
 endif

 ifnconst bankswitch
   jmp game
 else
   lda #>(game-1)
   pha
   lda #<(game-1)
   pha
   pha
   pha
   ldx #1
   jmp BS_jsr
 endif
; playfield drawing routines
; you get a 32x12 bitmapped display in a single color :)
; 0-31 and 0-11

pfclear ; clears playfield - or fill with pattern
 ifconst pfres
 ldx #pfres*4-1
 else
 ldx #47
 endif
pfclear_loop
 ifnconst superchip
 sta playfield,x
 else
 sta playfield-128,x
 endif
 dex
 bpl pfclear_loop
 RETURN
 
setuppointers
 stx temp2 ; store on.off.flip value
 tax ; put x-value in x 
 lsr
 lsr
 lsr ; divide x pos by 8 
 sta temp1
 tya
 asl
 asl ; multiply y pos by 4
 clc
 adc temp1 ; add them together to get actual memory location offset
 tay ; put the value in y
 lda temp2 ; restore on.off.flip value
 rts

pfread
;x=xvalue, y=yvalue
 jsr setuppointers
 lda setbyte,x
 and playfield,y
 eor setbyte,x
; beq readzero
; lda #1
; readzero
 RETURN

pfpixel
;x=xvalue, y=yvalue, a=0,1,2
 jsr setuppointers

 ifconst bankswitch
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon_r  ; if "on" go to on
 lsr
 bcs pixeloff_r ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixelon_r
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixeloff_r
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN

 else
 jmp plotpoint
 endif

pfhline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 jmp noinc
keepgoing
 inx
 txa
 and #7
 bne noinc
 iny
noinc
 jsr plotpoint
 cpx temp3
 bmi keepgoing
 RETURN

pfvline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 sty temp1 ; store memory location offset
 inc temp3 ; increase final x by 1 
 lda temp3
 asl
 asl ; multiply by 4
 sta temp3 ; store it
 ; Thanks to Michael Rideout for fixing a bug in this code
 ; right now, temp1=y=starting memory location, temp3=final
 ; x should equal original x value
keepgoingy
 jsr plotpoint
 iny
 iny
 iny
 iny
 cpy temp3
 bmi keepgoingy
 RETURN

plotpoint
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon  ; if "on" go to on
 lsr
 bcs pixeloff ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
  ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixelon
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixeloff
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts

setbyte
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
pfscroll ;(a=0 left, 1 right, 2 up, 4 down, 6=upup, 12=downdown)
 bne notleft
;left
 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
leftloop
 lda playfield-1,x
 lsr

 ifconst superchip
 lda playfield-2,x
 rol
 sta playfield-130,x
 lda playfield-3,x
 ror
 sta playfield-131,x
 lda playfield-4,x
 rol
 sta playfield-132,x
 lda playfield-1,x
 ror
 sta playfield-129,x
 else
 rol playfield-2,x
 ror playfield-3,x
 rol playfield-4,x
 ror playfield-1,x
 endif

 txa
 sbx #4
 bne leftloop
 RETURN

notleft
 lsr
 bcc notright
;right

 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
rightloop
 lda playfield-4,x
 lsr
 ifconst superchip
 lda playfield-3,x
 rol
 sta playfield-131,x
 lda playfield-2,x
 ror
 sta playfield-130,x
 lda playfield-1,x
 rol
 sta playfield-129,x
 lda playfield-4,x
 ror
 sta playfield-132,x
 else
 rol playfield-3,x
 ror playfield-2,x
 rol playfield-1,x
 ror playfield-4,x
 endif
 txa
 sbx #4
 bne rightloop
  RETURN

notright
 lsr
 bcc notup
;up
 lsr
 bcc onedecup
 dec playfieldpos
onedecup
 dec playfieldpos
 beq shiftdown 
 bpl noshiftdown2 
shiftdown
  ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif

 sta playfieldpos
 lda playfield+3
 sta temp4
 lda playfield+2
 sta temp3
 lda playfield+1
 sta temp2
 lda playfield
 sta temp1
 ldx #0
up2
 lda playfield+4,x
 ifconst superchip
 sta playfield-128,x
 lda playfield+5,x
 sta playfield-127,x
 lda playfield+6,x
 sta playfield-126,x
 lda playfield+7,x
 sta playfield-125,x
 else
 sta playfield,x
 lda playfield+5,x
 sta playfield+1,x
 lda playfield+6,x
 sta playfield+2,x
 lda playfield+7,x
 sta playfield+3,x
 endif
 txa
 sbx #252
 ifconst pfres
 cpx #(pfres-1)*4
 else
 cpx #44
 endif
 bne up2

 lda temp4
 
 ifconst superchip
 ifconst pfres
 sta playfield+pfres*4-129
 lda temp3
 sta playfield+pfres*4-130
 lda temp2
 sta playfield+pfres*4-131
 lda temp1
 sta playfield+pfres*4-132
 else
 sta playfield+47-128
 lda temp3
 sta playfield+46-128
 lda temp2
 sta playfield+45-128
 lda temp1
 sta playfield+44-128
 endif
 else
 ifconst pfres
 sta playfield+pfres*4-1
 lda temp3
 sta playfield+pfres*4-2
 lda temp2
 sta playfield+pfres*4-3
 lda temp1
 sta playfield+pfres*4-4
 else
 sta playfield+47
 lda temp3
 sta playfield+46
 lda temp2
 sta playfield+45
 lda temp1
 sta playfield+44
 endif
 endif
noshiftdown2
 RETURN


notup
;down
 lsr
 bcs oneincup
 inc playfieldpos
oneincup
 inc playfieldpos
 lda playfieldpos

  ifconst pfrowheight
 cmp #pfrowheight+1
 else
 ifnconst pfres
   cmp #9
 else
   cmp #(96/pfres)+1 ; try to come close to the real size
 endif
 endif

 bcc noshiftdown 
 lda #1
 sta playfieldpos

 ifconst pfres
 lda playfield+pfres*4-1
 sta temp4
 lda playfield+pfres*4-2
 sta temp3
 lda playfield+pfres*4-3
 sta temp2
 lda playfield+pfres*4-4
 else
 lda playfield+47
 sta temp4
 lda playfield+46
 sta temp3
 lda playfield+45
 sta temp2
 lda playfield+44
 endif

 sta temp1

 ifconst pfres
 ldx #(pfres-1)*4
 else
 ldx #44
 endif
down2
 lda playfield-1,x
 ifconst superchip
 sta playfield-125,x
 lda playfield-2,x
 sta playfield-126,x
 lda playfield-3,x
 sta playfield-127,x
 lda playfield-4,x
 sta playfield-128,x
 else
 sta playfield+3,x
 lda playfield-2,x
 sta playfield+2,x
 lda playfield-3,x
 sta playfield+1,x
 lda playfield-4,x
 sta playfield,x
 endif
 txa
 sbx #4
 bne down2

 lda temp4
 ifconst superchip
 sta playfield-125
 lda temp3
 sta playfield-126
 lda temp2
 sta playfield-127
 lda temp1
 sta playfield-128
 else
 sta playfield+3
 lda temp3
 sta playfield+2
 lda temp2
 sta playfield+1
 lda temp1
 sta playfield
 endif
noshiftdown
 RETURN
;standard routines needed for pretty much all games
; just the random number generator is left - maybe we should remove this asm file altogether?
; repositioning code and score pointer setup moved to overscan
; read switches, joysticks now compiler generated (more efficient)

randomize
	lda rand
	lsr
 ifconst rand16
	rol rand16
 endif
	bcc noeor
	eor #$B4
noeor
	sta rand
 ifconst rand16
	eor rand16
 endif
	RETURN
drawscreen
 ifconst debugscore
   ldx #14
   lda INTIM ; display # cycles left in the score

 ifconst mincycles
 lda mincycles 
 cmp INTIM
 lda mincycles
 bcc nochange
 lda INTIM
 sta mincycles
nochange
 endif

;   cmp #$2B
;   bcs no_cycles_left
   bmi cycles_left
   ldx #64
   eor #$ff ;make negative
cycles_left
   stx scorecolor
   and #$7f ; clear sign bit
   tax
   lda scorebcd,x
   sta score+2
   lda scorebcd1,x
   sta score+1
   jmp done_debugscore   
scorebcd
 .byte $00, $64, $28, $92, $56, $20, $84, $48, $12, $76, $40
 .byte $04, $68, $32, $96, $60, $24, $88, $52, $16, $80, $44
 .byte $08, $72, $36, $00, $64, $28, $92, $56, $20, $84, $48
 .byte $12, $76, $40, $04, $68, $32, $96, $60, $24, $88
scorebcd1
 .byte 0, 0, 1, 1, 2, 3, 3, 4, 5, 5, 6
 .byte 7, 7, 8, 8, 9, $10, $10, $11, $12, $12, $13
 .byte $14, $14, $15, $16, $16, $17, $17, $18, $19, $19, $20
 .byte $21, $21, $22, $23, $23, $24, $24, $25, $26, $26
done_debugscore
 endif

 ifconst debugcycles
   lda INTIM ; if we go over, it mucks up the background color
;   cmp #$2B
;   BCC overscan
   bmi overscan
   sta COLUBK
   bcs doneoverscan
 endif

 
overscan
 lda INTIM ;wait for sync
 bmi overscan
doneoverscan
;do VSYNC
 lda #2
 sta WSYNC
 sta VSYNC
 STA WSYNC
 STA WSYNC
 LDA #0
 STA WSYNC
 STA VSYNC
 sta VBLANK
 ifnconst overscan_time
 lda #37+128
 else
 lda #overscan_time+128
 endif
 sta TIM64T

 ifconst legacy
 if legacy < 100
 ldx #4
adjustloop
 lda player0x,x
 sec
 sbc #14 ;?
 sta player0x,x
 dex
 bpl adjustloop
 endif
 endif
 if (<*)>$F0
 align 256, $EA
 endif
  sta WSYNC
  ldx #4
  SLEEP 3
HorPosLoop       ;     5
  lda player0x,X  ;+4   9
  sec           ;+2  11
DivideLoop
  sbc #15
  bcs DivideLoop;+4  15
  sta temp1,X    ;+4  19
  sta RESP0,X   ;+4  23
  sta WSYNC
  dex
  bpl HorPosLoop;+5   5
                ;     4

  ldx #4
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 18

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 32

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 46

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 60

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 74

  sta WSYNC
 
  sta HMOVE     ;+3   3


 ifconst legacy
 if legacy < 100
 ldx #4
adjustloop2
 lda player0x,x
 clc
 adc #14 ;?
 sta player0x,x
 dex
 bpl adjustloop2
 endif
 endif




;set score pointers
 lax score+2
 jsr scorepointerset
 sty scorepointers+5
 stx scorepointers+2
 lax score+1
 jsr scorepointerset
 sty scorepointers+4
 stx scorepointers+1
 lax score
 jsr scorepointerset
 sty scorepointers+3
 stx scorepointers

vblk
; run possible vblank bB code
 ifconst vblank_bB_code
   jsr vblank_bB_code
 endif
vblk2
 LDA INTIM
 bmi vblk2
 jmp kernel
 

    .byte $80,$70,$60,$50,$40,$30,$20,$10,$00
    .byte $F0,$E0,$D0,$C0,$B0,$A0,$90
repostable

scorepointerset
 and #$0F
 asl
 asl
 asl
 adc #<scoretable
 tay 
 txa
; and #$F0
; lsr
 asr #$F0
 adc #<scoretable
 tax
 rts
game
.L00 ;  rem DiscDog

.
 ; 

.L01 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel0
PF_data0
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %01010100, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %10000000
	.byte %11111111, %11111111, %11111111, %11111111
pflabel0
	lda PF_data0,x
	sta playfield,x
	dex
	bpl pflabel0
.
 ; 

.L02 ;  player0:

	LDA #<playerL02_0

	STA player0pointerlo
	LDA #>playerL02_0

	STA player0pointerhi
	LDA #8
	STA player0height
.
 ; 

.L03 ;  player1:

	LDA #<playerL03_1

	STA player1pointerlo
	LDA #>playerL03_1

	STA player1pointerhi
	LDA #2
	STA player1height
.
 ; 

.L04 ;  COLUPF  =  176

	LDA #176
	STA COLUPF
.L05 ;  scorecolor  =  52

	LDA #52
	STA scorecolor
.L06 ;  score  =  0

	LDA #$00
	STA score+2
	LDA #$00
	STA score+1
	LDA #$00
	STA score
.
 ; 

.L07 ;  player0x  =  21

	LDA #21
	STA player0x
.L08 ;  player0y  =  80

	LDA #80
	STA player0y
.
 ; 

.L09 ;  player1x  =  138

	LDA #138
	STA player1x
.L010 ;  player1y  =  65

	LDA #65
	STA player1y
.
 ; 

.L011 ;  missile0x  =  82

	LDA #82
	STA missile0x
.L012 ;  missile0y  =  79

	LDA #79
	STA missile0y
.L013 ;  missile0height  =  5

	LDA #5
	STA missile0height
.
 ; 

.L014 ;  dim perrosalto  =  a

.L015 ;  dim discodireccion  =  b

.L016 ;  dim discocogido  =  c

.L017 ;  dim puntos  =  d

.L018 ;  dim perrodireccion  =  e

.L019 ;  dim discovelocidad  =  f

.L020 ;  dim aleatorio  =  g

.L021 ;  dim discoaltura  =  h

.L022 ;  dim perrovidas  =  i

.L023 ;  dim cuentaatras  =  j

.L024 ;  dim discoalturasube  =  k

.L025 ;  dim discoalturabaja  =  l

.L026 ;  dim discoalturapaso  =  m

.
 ; 

.L027 ;  puntos  =  0

	LDA #0
	STA puntos
.L028 ;  discocogido  =  1

	LDA #1
	STA discocogido
.L029 ;  discodireccion  =  1

	LDA #1
	STA discodireccion
.L030 ;  perrodireccion  =  1

	LDA #1
	STA perrodireccion
.L031 ;  discovelocidad  =  2

	LDA #2
	STA discovelocidad
.L032 ;  perrovidas  =  3

	LDA #3
	STA perrovidas
.L033 ;  cuentaatras  =  0

	LDA #0
	STA cuentaatras
.L034 ;  discoaltura  =   ( rand  &  10 )   +  1

; complex statement detected
 jsr randomize
	AND #10
	CLC
	ADC #1
	STA discoaltura
.L035 ;  discoalturasube  =  18  +  discoaltura

	LDA #18
	CLC
	ADC discoaltura
	STA discoalturasube
.L036 ;  discoalturabaja  =  138  -  discoaltura

	LDA #138
	SEC
	SBC discoaltura
	STA discoalturabaja
.L037 ;  discoalturapaso  =  4

	LDA #4
	STA discoalturapaso
.
 ; 

.mainloop
 ; mainloop

.
 ; 

.L038 ;  AUDV0  =  0

	LDA #0
	STA AUDV0
.L039 ;  COLUP0  =  4

	LDA #4
	STA COLUP0
.L040 ;  COLUP1  =  132

	LDA #132
	STA COLUP1
.
 ; 

.L041 ;  if perrovidas  =  0 then COLUPF  =  52  :  gosub limpiarpantalla

	LDA perrovidas
	CMP #0
     BNE .skipL041
.condpart0
	LDA #52
	STA COLUPF
 jsr .limpiarpantalla

.skipL041
.L042 ;  if perrovidas  =  0 then gosub game

	LDA perrovidas
	CMP #0
     BNE .skipL042
.condpart1
 jsr .game

.skipL042
.L043 ;  if perrovidas  =  0 then gosub over

	LDA perrovidas
	CMP #0
     BNE .skipL043
.condpart2
 jsr .over

.skipL043
.
 ; 

.L044 ;  if joy0left  &&  player0x  >  21  &&  perrovidas  >  0 then gosub moverizquierda

 lda #$40
 bit SWCHA
	BNE .skipL044
.condpart3
	LDA #21
	CMP player0x
     BCS .skip3then
.condpart4
	LDA #0
	CMP perrovidas
     BCS .skip4then
.condpart5
 jsr .moverizquierda

.skip4then
.skip3then
.skipL044
.L045 ;  if joy0right  &&  player0x  <  133  &&  perrovidas  >  0 then gosub moverderecha

 lda #$80
 bit SWCHA
	BNE .skipL045
.condpart6
	LDA player0x
	CMP #133
     BCS .skip6then
.condpart7
	LDA #0
	CMP perrovidas
     BCS .skip7then
.condpart8
 jsr .moverderecha

.skip7then
.skip6then
.skipL045
.L046 ;  if joy0up  &&  perrosalto  =  0  &&  perrovidas  >  0 then perrosalto  =  1

 lda #$10
 bit SWCHA
	BNE .skipL046
.condpart9
	LDA perrosalto
	CMP #0
     BNE .skip9then
.condpart10
	LDA #0
	CMP perrovidas
     BCS .skip10then
.condpart11
	LDA #1
	STA perrosalto
.skip10then
.skip9then
.skipL046
.
 ; 

.L047 ;  if cuentaatras  =  25 then pfpixel 21 1 on  :  missile0height  =  6

	LDA cuentaatras
	CMP #25
     BNE .skipL047
.condpart12
	LDA #21
	LDY #1
	LDX #0
 jsr pfpixel
	LDA #6
	STA missile0height
.skipL047
.L048 ;  if cuentaatras  =  50 then pfpixel 22 1 on

	LDA cuentaatras
	CMP #50
     BNE .skipL048
.condpart13
	LDA #22
	LDY #1
	LDX #0
 jsr pfpixel
.skipL048
.L049 ;  if cuentaatras  =  75 then pfpixel 23 1 on  :  missile0height  =  7

	LDA cuentaatras
	CMP #75
     BNE .skipL049
.condpart14
	LDA #23
	LDY #1
	LDX #0
 jsr pfpixel
	LDA #7
	STA missile0height
.skipL049
.L050 ;  if cuentaatras  =  100 then pfpixel 24 1 on

	LDA cuentaatras
	CMP #100
     BNE .skipL050
.condpart15
	LDA #24
	LDY #1
	LDX #0
 jsr pfpixel
.skipL050
.L051 ;  if cuentaatras  =  125 then pfpixel 25 1 on  :  missile0height  =  8

	LDA cuentaatras
	CMP #125
     BNE .skipL051
.condpart16
	LDA #25
	LDY #1
	LDX #0
 jsr pfpixel
	LDA #8
	STA missile0height
.skipL051
.L052 ;  if cuentaatras  =  150 then pfpixel 26 1 on

	LDA cuentaatras
	CMP #150
     BNE .skipL052
.condpart17
	LDA #26
	LDY #1
	LDX #0
 jsr pfpixel
.skipL052
.L053 ;  if cuentaatras  =  175 then pfpixel 27 1 on  :  missile0height  =  9

	LDA cuentaatras
	CMP #175
     BNE .skipL053
.condpart18
	LDA #27
	LDY #1
	LDX #0
 jsr pfpixel
	LDA #9
	STA missile0height
.skipL053
.L054 ;  if cuentaatras  =  200 then pfpixel 28 1 on

	LDA cuentaatras
	CMP #200
     BNE .skipL054
.condpart19
	LDA #28
	LDY #1
	LDX #0
 jsr pfpixel
.skipL054
.L055 ;  if cuentaatras  =  225 then pfpixel 29 1 on  :  missile0height  =  10

	LDA cuentaatras
	CMP #225
     BNE .skipL055
.condpart20
	LDA #29
	LDY #1
	LDX #0
 jsr pfpixel
	LDA #10
	STA missile0height
.skipL055
.L056 ;  if cuentaatras  =  250 then pfpixel 30 1 on

	LDA cuentaatras
	CMP #250
     BNE .skipL056
.condpart21
	LDA #30
	LDY #1
	LDX #0
 jsr pfpixel
.skipL056
.
 ; 

.L057 ;  if cuentaatras  =  250 then perrovidas  =  0

	LDA cuentaatras
	CMP #250
     BNE .skipL057
.condpart22
	LDA #0
	STA perrovidas
.skipL057
.
 ; 

.L058 ;  if discocogido  =  2 then gosub cogerdisco else gosub moverdisco

	LDA discocogido
	CMP #2
     BNE .skipL058
.condpart23
 jsr .cogerdisco
 jmp .skipelse0
.skipL058
 jsr .moverdisco

.skipelse0
.
 ; 

.L059 ;  if perrosalto  =  1 then gosub saltarsubida

	LDA perrosalto
	CMP #1
     BNE .skipL059
.condpart24
 jsr .saltarsubida

.skipL059
.L060 ;  if perrosalto  =  2 then gosub saltarbajada

	LDA perrosalto
	CMP #2
     BNE .skipL060
.condpart25
 jsr .saltarbajada

.skipL060
.
 ; 

.L061 ;  if collision(player0,player1) then discocogido  =  2

	BIT CXPPMM
	BPL .skipL061
.condpart26
	LDA #2
	STA discocogido
.skipL061
.
 ; 

.L062 ;  if player0x  =  21  &&  collision(playfield,player1)  &&  discocogido  =  2 then gosub lanzardisco2

	LDA player0x
	CMP #21
     BNE .skipL062
.condpart27
	BIT CXP1FB
	BPL .skip27then
.condpart28
	LDA discocogido
	CMP #2
     BNE .skip28then
.condpart29
 jsr .lanzardisco2

.skip28then
.skip27then
.skipL062
.L063 ;  if player0x  =  133  &&  collision(playfield,player1)  &&  discocogido  =  2 then gosub lanzardisco1

	LDA player0x
	CMP #133
     BNE .skipL063
.condpart30
	BIT CXP1FB
	BPL .skip30then
.condpart31
	LDA discocogido
	CMP #2
     BNE .skip31then
.condpart32
 jsr .lanzardisco1

.skip31then
.skip30then
.skipL063
.
 ; 

.L064 ;  if collision(player0,missile0) then AUDV0  =  15  :  AUDC0  =  6  :  AUDF0  =  4  :  player0x  =  21  :  perrovidas  =  perrovidas  -  1

	BIT CXM0P
	BVC .skipL064
.condpart33
	LDA #15
	STA AUDV0
	LDA #6
	STA AUDC0
	LDA #4
	STA AUDF0
	LDA #21
	STA player0x
	DEC perrovidas
.skipL064
.
 ; 

.L065 ;  if perrovidas  =  2 then pfpixel 5 1 off

	LDA perrovidas
	CMP #2
     BNE .skipL065
.condpart34
	LDA #5
	LDY #1
	LDX #1
 jsr pfpixel
.skipL065
.L066 ;  if perrovidas  =  1 then pfpixel 3 1 off

	LDA perrovidas
	CMP #1
     BNE .skipL066
.condpart35
	LDA #3
	LDY #1
	LDX #1
 jsr pfpixel
.skipL066
.L067 ;  if perrovidas  =  0 then pfpixel 1 1 off

	LDA perrovidas
	CMP #0
     BNE .skipL067
.condpart36
	LDA #1
	LDY #1
	LDX #1
 jsr pfpixel
.skipL067
.
 ; 

.L068 ;  drawscreen

 jsr drawscreen
.L069 ;  goto mainloop

 jmp .mainloop

.
 ; 

.lanzardisco1
 ; lanzardisco1

.L070 ;  pfpixel 31 9 off

	LDA #31
	LDY #9
	LDX #1
 jsr pfpixel
.L071 ;  pfpixel 0 9 on

	LDA #0
	LDY #9
	LDX #0
 jsr pfpixel
.L072 ;  AUDV0  =  5  :  AUDC0  =  12  :  AUDF0  =  4

	LDA #5
	STA AUDV0
	LDA #12
	STA AUDC0
	LDA #4
	STA AUDF0
.L073 ;  discocogido  =  1

	LDA #1
	STA discocogido
.L074 ;  player1y  =  65

	LDA #65
	STA player1y
.L075 ;  player1x  =  18

	LDA #18
	STA player1x
.L076 ;  discodireccion  =  2

	LDA #2
	STA discodireccion
.L077 ;  aleatorio  =   ( rand  &  3 )   +  1

; complex statement detected
 jsr randomize
	AND #3
	CLC
	ADC #1
	STA aleatorio
.L078 ;  if aleatorio  =  4 then discovelocidad  =  4

	LDA aleatorio
	CMP #4
     BNE .skipL078
.condpart37
	LDA #4
	STA discovelocidad
.skipL078
.L079 ;  if aleatorio  =  3 then discovelocidad  =  2

	LDA aleatorio
	CMP #3
     BNE .skipL079
.condpart38
	LDA #2
	STA discovelocidad
.skipL079
.L080 ;  if aleatorio  =  2 then discovelocidad  =  1

	LDA aleatorio
	CMP #2
     BNE .skipL080
.condpart39
	LDA #1
	STA discovelocidad
.skipL080
.L081 ;  if aleatorio  =  1 then discovelocidad  =  1

	LDA aleatorio
	CMP #1
     BNE .skipL081
.condpart40
	LDA #1
	STA discovelocidad
.skipL081
.L082 ;  score  =  score  +  100

	SED
	CLC
	LDA score+2
	ADC #$00
	STA score+2
	LDA score+1
	ADC #$01
	STA score+1
	LDA score
	ADC #$00
	STA score
	CLD
.L083 ;  puntos  =  puntos  +  100

	LDA puntos
	CLC
	ADC #100
	STA puntos
.L084 ;  cuentaatras  =  cuentaatras  +  1

	INC cuentaatras
.L085 ;  discoaltura  =   ( rand  &  10 )   +  1

; complex statement detected
 jsr randomize
	AND #10
	CLC
	ADC #1
	STA discoaltura
.L086 ;  discoalturasube  =  138  -   ( discoaltura  *  4 ) 

; complex statement detected
	LDA #138
	PHA
	LDA discoaltura
	asl
	asl
	TAY
	PLA
	TSX
	STY $00,x
	SEC
	SBC $100,x
	STA discoalturasube
.L087 ;  discoalturabaja  =  18  +   ( discoaltura  *  4 ) 

; complex statement detected
	LDA #18
	PHA
	LDA discoaltura
	asl
	asl
	TSX
	INX
	TXS
	CLC
	ADC $100,x
	STA discoalturabaja
.L088 ;  discoalturapaso  =  4

	LDA #4
	STA discoalturapaso
.L089 ;  return

	RTS
.
 ; 

.lanzardisco2
 ; lanzardisco2

.L090 ;  pfpixel 31 9 on

	LDA #31
	LDY #9
	LDX #0
 jsr pfpixel
.L091 ;  pfpixel 0 9 off

	LDA #0
	LDY #9
	LDX #1
 jsr pfpixel
.L092 ;  AUDV0  =  5  :  AUDC0  =  12  :  AUDF0  =  4

	LDA #5
	STA AUDV0
	LDA #12
	STA AUDC0
	LDA #4
	STA AUDF0
.L093 ;  discocogido  =  1

	LDA #1
	STA discocogido
.L094 ;  player1y  =  65

	LDA #65
	STA player1y
.L095 ;  player1x  =  138

	LDA #138
	STA player1x
.L096 ;  discodireccion  =  1

	LDA #1
	STA discodireccion
.L097 ;  aleatorio  =   ( rand  &  3 )   +  1

; complex statement detected
 jsr randomize
	AND #3
	CLC
	ADC #1
	STA aleatorio
.L098 ;  if aleatorio  =  4 then discovelocidad  =  4

	LDA aleatorio
	CMP #4
     BNE .skipL098
.condpart41
	LDA #4
	STA discovelocidad
.skipL098
.L099 ;  if aleatorio  =  3 then discovelocidad  =  2

	LDA aleatorio
	CMP #3
     BNE .skipL099
.condpart42
	LDA #2
	STA discovelocidad
.skipL099
.L0100 ;  if aleatorio  =  2 then discovelocidad  =  1

	LDA aleatorio
	CMP #2
     BNE .skipL0100
.condpart43
	LDA #1
	STA discovelocidad
.skipL0100
.L0101 ;  if aleatorio  =  1 then discovelocidad  =  1

	LDA aleatorio
	CMP #1
     BNE .skipL0101
.condpart44
	LDA #1
	STA discovelocidad
.skipL0101
.L0102 ;  score  =  score  +  100

	SED
	CLC
	LDA score+2
	ADC #$00
	STA score+2
	LDA score+1
	ADC #$01
	STA score+1
	LDA score
	ADC #$00
	STA score
	CLD
.L0103 ;  puntos  =  puntos  +  100

	LDA puntos
	CLC
	ADC #100
	STA puntos
.L0104 ;  cuentaatras  =  cuentaatras  +  1

	INC cuentaatras
.L0105 ;  discoaltura  =   ( rand  &  10 )   +  1

; complex statement detected
 jsr randomize
	AND #10
	CLC
	ADC #1
	STA discoaltura
.L0106 ;  discoalturabaja  =  18  +   ( discoaltura  *  4 ) 

; complex statement detected
	LDA #18
	PHA
	LDA discoaltura
	asl
	asl
	TSX
	INX
	TXS
	CLC
	ADC $100,x
	STA discoalturabaja
.L0107 ;  discoalturasube  =  138  -   ( discoaltura  *  4 ) 

; complex statement detected
	LDA #138
	PHA
	LDA discoaltura
	asl
	asl
	TAY
	PLA
	TSX
	STY $00,x
	SEC
	SBC $100,x
	STA discoalturasube
.L0108 ;  discoalturapaso  =  4

	LDA #4
	STA discoalturapaso
.L0109 ;  return

	RTS
.
 ; 

.moverizquierda
 ; moverizquierda

.L0110 ;  player0:

	LDA #<playerL0110_0

	STA player0pointerlo
	LDA #>playerL0110_0

	STA player0pointerhi
	LDA #8
	STA player0height
.L0111 ;  perrodireccion  =  2

	LDA #2
	STA perrodireccion
.L0112 ;  player0x  =  player0x  -  1

	DEC player0x
.L0113 ;  return

	RTS
.
 ; 

.moverderecha
 ; moverderecha

.L0114 ;  player0:

	LDA #<playerL0114_0

	STA player0pointerlo
	LDA #>playerL0114_0

	STA player0pointerhi
	LDA #8
	STA player0height
.L0115 ;  player0x  =  player0x  +  1

	INC player0x
.L0116 ;  perrodireccion  =  1

	LDA #1
	STA perrodireccion
.L0117 ;  return

	RTS
.
 ; 

.saltarsubida
 ; saltarsubida

.L0118 ;  player0y  =  player0y  -  1

	DEC player0y
.L0119 ;  if player0y  =  62 then perrosalto  =  2

	LDA player0y
	CMP #62
     BNE .skipL0119
.condpart45
	LDA #2
	STA perrosalto
.skipL0119
.L0120 ;  if puntos  >=  10  &&  perrosalto  =  2 then score  =  score  -  10  :  puntos  =  puntos  -  10

	LDA puntos
	CMP #10
     BCC .skipL0120
.condpart46
	LDA perrosalto
	CMP #2
     BNE .skip46then
.condpart47
	SED
	SEC
	LDA score+2
	SBC #$10
	STA score+2
	LDA score+1
	SBC #$00
	STA score+1
	LDA score
	SBC #$00
	STA score
	CLD
	LDA puntos
	SEC
	SBC #10
	STA puntos
.skip46then
.skipL0120
.L0121 ;  return

	RTS
.
 ; 

.saltarbajada
 ; saltarbajada

.L0122 ;  player0y  =  player0y  +  1

	INC player0y
.L0123 ;  if player0y  =  80 then perrosalto  =  0

	LDA player0y
	CMP #80
     BNE .skipL0123
.condpart48
	LDA #0
	STA perrosalto
.skipL0123
.
 ; 

.L0124 ;  return

	RTS
.
 ; 

.moverdisco
 ; moverdisco

.
 ; 

.L0125 ;  if discoalturapaso  >  1 then discoalturapaso  =  discoalturapaso  -  1

	LDA #1
	CMP discoalturapaso
     BCS .skipL0125
.condpart49
	DEC discoalturapaso
.skipL0125
.
 ; 

.
 ; 

.L0126 ;  if discoalturapaso  =  1  &&  player1x  <=  discoalturabaja  &&  discovelocidad  =  1  &&  discodireccion  =  1 then player1y  =  player1y  +  1  :  discoalturapaso  =  4

	LDA discoalturapaso
	CMP #1
     BNE .skipL0126
.condpart50
	LDA discoalturabaja
	CMP player1x
     BCC .skip50then
.condpart51
	LDA discovelocidad
	CMP #1
     BNE .skip51then
.condpart52
	LDA discodireccion
	CMP #1
     BNE .skip52then
.condpart53
	INC player1y
	LDA #4
	STA discoalturapaso
.skip52then
.skip51then
.skip50then
.skipL0126
.L0127 ;  if discoalturapaso  =  1  &&  player1x  >=  discoalturasube  &&  discovelocidad  =  1  &&  discodireccion  =  1 then player1y  =  player1y  -  1  :  discoalturapaso  =  4

	LDA discoalturapaso
	CMP #1
     BNE .skipL0127
.condpart54
	LDA player1x
	CMP discoalturasube
     BCC .skip54then
.condpart55
	LDA discovelocidad
	CMP #1
     BNE .skip55then
.condpart56
	LDA discodireccion
	CMP #1
     BNE .skip56then
.condpart57
	DEC player1y
	LDA #4
	STA discoalturapaso
.skip56then
.skip55then
.skip54then
.skipL0127
.
 ; 

.L0128 ;  if discoalturapaso  =  1  &&  player1x  >=  discoalturasube  &&  discovelocidad  =  1  &&  discodireccion  =  2 then player1y  =  player1y  +  1  :  discoalturapaso  =  4

	LDA discoalturapaso
	CMP #1
     BNE .skipL0128
.condpart58
	LDA player1x
	CMP discoalturasube
     BCC .skip58then
.condpart59
	LDA discovelocidad
	CMP #1
     BNE .skip59then
.condpart60
	LDA discodireccion
	CMP #2
     BNE .skip60then
.condpart61
	INC player1y
	LDA #4
	STA discoalturapaso
.skip60then
.skip59then
.skip58then
.skipL0128
.L0129 ;  if discoalturapaso  =  1  &&  player1x  <=  discoalturabaja  &&  discovelocidad  =  1  &&  discodireccion  =  2 then player1y  =  player1y  -  1 :  discoalturapaso  =  4

	LDA discoalturapaso
	CMP #1
     BNE .skipL0129
.condpart62
	LDA discoalturabaja
	CMP player1x
     BCC .skip62then
.condpart63
	LDA discovelocidad
	CMP #1
     BNE .skip63then
.condpart64
	LDA discodireccion
	CMP #2
     BNE .skip64then
.condpart65
	DEC player1y
	LDA #4
	STA discoalturapaso
.skip64then
.skip63then
.skip62then
.skipL0129
.
 ; 

.L0130 ;  if player1x  <=  138  &&  discodireccion  =  1 then player1x  =  player1x  -  discovelocidad

	LDA #138
	CMP player1x
     BCC .skipL0130
.condpart66
	LDA discodireccion
	CMP #1
     BNE .skip66then
.condpart67
	LDA player1x
	SEC
	SBC discovelocidad
	STA player1x
.skip66then
.skipL0130
.L0131 ;  if player1x  >=  18  &&  discodireccion  =  2 then player1x  =  player1x  +  discovelocidad

	LDA player1x
	CMP #18
     BCC .skipL0131
.condpart68
	LDA discodireccion
	CMP #2
     BNE .skip68then
.condpart69
	LDA player1x
	CLC
	ADC discovelocidad
	STA player1x
.skip68then
.skipL0131
.L0132 ;  if player1x  <=  18 then discodireccion  =  2  :  cuentaatras  =  cuentaatras  +  1  :  player1y  =  65

	LDA #18
	CMP player1x
     BCC .skipL0132
.condpart70
	LDA #2
	STA discodireccion
	INC cuentaatras
	LDA #65
	STA player1y
.skipL0132
.L0133 ;  if player1x  >=  138 then discodireccion  =  1  :  cuentaatras  =  cuentaatras  +  1  :  player1y  =  65

	LDA player1x
	CMP #138
     BCC .skipL0133
.condpart71
	LDA #1
	STA discodireccion
	INC cuentaatras
	LDA #65
	STA player1y
.skipL0133
.
 ; 

.
 ; 

.L0134 ;  AUDV0  =  0

	LDA #0
	STA AUDV0
.L0135 ;  return

	RTS
.
 ; 

.cogerdisco
 ; cogerdisco

.L0136 ;  if perrodireccion  =  1 then player1x  =  player0x  +  6

	LDA perrodireccion
	CMP #1
     BNE .skipL0136
.condpart72
	LDA player0x
	CLC
	ADC #6
	STA player1x
.skipL0136
.L0137 ;  if perrodireccion  =  2 then player1x  =  player0x  -  6

	LDA perrodireccion
	CMP #2
     BNE .skipL0137
.condpart73
	LDA player0x
	SEC
	SBC #6
	STA player1x
.skipL0137
.L0138 ;  player1y  =  player0y  -  5

	LDA player0y
	SEC
	SBC #5
	STA player1y
.L0139 ;  return

	RTS
.
 ; 

.limpiarpantalla
 ; limpiarpantalla

.L0140 ;  pfpixel 5 1 off

	LDA #5
	LDY #1
	LDX #1
 jsr pfpixel
.L0141 ;  pfpixel 3 1 off

	LDA #3
	LDY #1
	LDX #1
 jsr pfpixel
.L0142 ;  pfpixel 1 1 off

	LDA #1
	LDY #1
	LDX #1
 jsr pfpixel
.L0143 ;  pfpixel 22 1 off

	LDA #22
	LDY #1
	LDX #1
 jsr pfpixel
.L0144 ;  pfpixel 23 1 off

	LDA #23
	LDY #1
	LDX #1
 jsr pfpixel
.L0145 ;  pfpixel 24 1 off

	LDA #24
	LDY #1
	LDX #1
 jsr pfpixel
.L0146 ;  pfpixel 25 1 off

	LDA #25
	LDY #1
	LDX #1
 jsr pfpixel
.L0147 ;  pfpixel 26 1 off

	LDA #26
	LDY #1
	LDX #1
 jsr pfpixel
.L0148 ;  pfpixel 27 1 off

	LDA #27
	LDY #1
	LDX #1
 jsr pfpixel
.L0149 ;  pfpixel 28 1 off

	LDA #28
	LDY #1
	LDX #1
 jsr pfpixel
.L0150 ;  pfpixel 29 1 off

	LDA #29
	LDY #1
	LDX #1
 jsr pfpixel
.L0151 ;  pfpixel 30 1 off

	LDA #30
	LDY #1
	LDX #1
 jsr pfpixel
.L0152 ;  pfpixel 31 1 off

	LDA #31
	LDY #1
	LDX #1
 jsr pfpixel
.L0153 ;  drawscreen

 jsr drawscreen
.L0154 ;  return

	RTS
.
 ; 

.game
 ; game

.L0155 ;  pfpixel 6 0 on

	LDA #6
	LDY #0
	LDX #0
 jsr pfpixel
.L0156 ;  pfpixel 7 0 on

	LDA #7
	LDY #0
	LDX #0
 jsr pfpixel
.L0157 ;  pfpixel 8 0 on

	LDA #8
	LDY #0
	LDX #0
 jsr pfpixel
.L0158 ;  pfpixel 11 0 on

	LDA #11
	LDY #0
	LDX #0
 jsr pfpixel
.L0159 ;  pfpixel 12 0 on

	LDA #12
	LDY #0
	LDX #0
 jsr pfpixel
.L0160 ;  pfpixel 13 0 on

	LDA #13
	LDY #0
	LDX #0
 jsr pfpixel
.L0161 ;  pfpixel 15 0 on

	LDA #15
	LDY #0
	LDX #0
 jsr pfpixel
.L0162 ;  pfpixel 19 0 on

	LDA #19
	LDY #0
	LDX #0
 jsr pfpixel
.L0163 ;  pfpixel 21 0 on

	LDA #21
	LDY #0
	LDX #0
 jsr pfpixel
.L0164 ;  pfpixel 22 0 on

	LDA #22
	LDY #0
	LDX #0
 jsr pfpixel
.L0165 ;  pfpixel 23 0 on

	LDA #23
	LDY #0
	LDX #0
 jsr pfpixel
.L0166 ;  pfpixel 24 0 on

	LDA #24
	LDY #0
	LDX #0
 jsr pfpixel
.
 ; 

.L0167 ;  pfpixel 5 1 on

	LDA #5
	LDY #1
	LDX #0
 jsr pfpixel
.L0168 ;  pfpixel 10 1 on

	LDA #10
	LDY #1
	LDX #0
 jsr pfpixel
.L0169 ;  pfpixel 13 1 on

	LDA #13
	LDY #1
	LDX #0
 jsr pfpixel
.L0170 ;  pfpixel 15 1 on

	LDA #15
	LDY #1
	LDX #0
 jsr pfpixel
.L0171 ;  pfpixel 16 1 on

	LDA #16
	LDY #1
	LDX #0
 jsr pfpixel
.L0172 ;  pfpixel 18 1 on

	LDA #18
	LDY #1
	LDX #0
 jsr pfpixel
.L0173 ;  pfpixel 19 1 on

	LDA #19
	LDY #1
	LDX #0
 jsr pfpixel
.L0174 ;  pfpixel 21 1 on

	LDA #21
	LDY #1
	LDX #0
 jsr pfpixel
.
 ; 

.L0175 ;  pfpixel 5 2 on

	LDA #5
	LDY #2
	LDX #0
 jsr pfpixel
.L0176 ;  pfpixel 7 2 on

	LDA #7
	LDY #2
	LDX #0
 jsr pfpixel
.L0177 ;  pfpixel 8 2 on

	LDA #8
	LDY #2
	LDX #0
 jsr pfpixel
.L0178 ;  pfpixel 10 2 on

	LDA #10
	LDY #2
	LDX #0
 jsr pfpixel
.L0179 ;  pfpixel 13 2 on

	LDA #13
	LDY #2
	LDX #0
 jsr pfpixel
.L0180 ;  pfpixel 15 2 on

	LDA #15
	LDY #2
	LDX #0
 jsr pfpixel
.L0181 ;  pfpixel 17 2 on

	LDA #17
	LDY #2
	LDX #0
 jsr pfpixel
.L0182 ;  pfpixel 19 2 on

	LDA #19
	LDY #2
	LDX #0
 jsr pfpixel
.L0183 ;  pfpixel 21 2 on

	LDA #21
	LDY #2
	LDX #0
 jsr pfpixel
.L0184 ;  pfpixel 22 2 on

	LDA #22
	LDY #2
	LDX #0
 jsr pfpixel
.
 ; 

.L0185 ;  pfpixel 5 3 on

	LDA #5
	LDY #3
	LDX #0
 jsr pfpixel
.L0186 ;  pfpixel 8 3 on

	LDA #8
	LDY #3
	LDX #0
 jsr pfpixel
.L0187 ;  pfpixel 10 3 on

	LDA #10
	LDY #3
	LDX #0
 jsr pfpixel
.L0188 ;  pfpixel 11 3 on

	LDA #11
	LDY #3
	LDX #0
 jsr pfpixel
.L0189 ;  pfpixel 12 3 on

	LDA #12
	LDY #3
	LDX #0
 jsr pfpixel
.L0190 ;  pfpixel 13 3 on

	LDA #13
	LDY #3
	LDX #0
 jsr pfpixel
.L0191 ;  pfpixel 15 3 on

	LDA #15
	LDY #3
	LDX #0
 jsr pfpixel
.L0192 ;  pfpixel 19 3 on

	LDA #19
	LDY #3
	LDX #0
 jsr pfpixel
.L0193 ;  pfpixel 21 3 on

	LDA #21
	LDY #3
	LDX #0
 jsr pfpixel
.
 ; 

.L0194 ;  pfpixel 6 4 on

	LDA #6
	LDY #4
	LDX #0
 jsr pfpixel
.L0195 ;  pfpixel 7 4 on

	LDA #7
	LDY #4
	LDX #0
 jsr pfpixel
.L0196 ;  pfpixel 10 4 on

	LDA #10
	LDY #4
	LDX #0
 jsr pfpixel
.L0197 ;  pfpixel 13 4 on

	LDA #13
	LDY #4
	LDX #0
 jsr pfpixel
.L0198 ;  pfpixel 15 4 on

	LDA #15
	LDY #4
	LDX #0
 jsr pfpixel
.L0199 ;  pfpixel 19 4 on

	LDA #19
	LDY #4
	LDX #0
 jsr pfpixel
.L0200 ;  pfpixel 21 4 on

	LDA #21
	LDY #4
	LDX #0
 jsr pfpixel
.L0201 ;  pfpixel 22 4 on

	LDA #22
	LDY #4
	LDX #0
 jsr pfpixel
.L0202 ;  pfpixel 23 4 on

	LDA #23
	LDY #4
	LDX #0
 jsr pfpixel
.L0203 ;  pfpixel 24 4 on

	LDA #24
	LDY #4
	LDX #0
 jsr pfpixel
.L0204 ;  drawscreen

 jsr drawscreen
.L0205 ;  return

	RTS
.
 ; 

.over
 ; over

.L0206 ;  pfpixel 6 6 on

	LDA #6
	LDY #6
	LDX #0
 jsr pfpixel
.L0207 ;  pfpixel 7 6 on

	LDA #7
	LDY #6
	LDX #0
 jsr pfpixel
.L0208 ;  pfpixel 10 6 on

	LDA #10
	LDY #6
	LDX #0
 jsr pfpixel
.L0209 ;  pfpixel 14 6 on

	LDA #14
	LDY #6
	LDX #0
 jsr pfpixel
.L0210 ;  pfpixel 16 6 on

	LDA #16
	LDY #6
	LDX #0
 jsr pfpixel
.L0211 ;  pfpixel 17 6 on

	LDA #17
	LDY #6
	LDX #0
 jsr pfpixel
.L0212 ;  pfpixel 18 6 on

	LDA #18
	LDY #6
	LDX #0
 jsr pfpixel
.L0213 ;  pfpixel 19 6 on

	LDA #19
	LDY #6
	LDX #0
 jsr pfpixel
.L0214 ;  pfpixel 21 6 on

	LDA #21
	LDY #6
	LDX #0
 jsr pfpixel
.L0215 ;  pfpixel 22 6 on

	LDA #22
	LDY #6
	LDX #0
 jsr pfpixel
.L0216 ;  pfpixel 23 6 on

	LDA #23
	LDY #6
	LDX #0
 jsr pfpixel
.L0217 ;  pfpixel 24 6 on

	LDA #24
	LDY #6
	LDX #0
 jsr pfpixel
.
 ; 

.L0218 ;  pfpixel 5 7 on

	LDA #5
	LDY #7
	LDX #0
 jsr pfpixel
.L0219 ;  pfpixel 8 7 on

	LDA #8
	LDY #7
	LDX #0
 jsr pfpixel
.L0220 ;  pfpixel 10 7 on

	LDA #10
	LDY #7
	LDX #0
 jsr pfpixel
.L0221 ;  pfpixel 14 7 on

	LDA #14
	LDY #7
	LDX #0
 jsr pfpixel
.L0222 ;  pfpixel 16 7 on

	LDA #16
	LDY #7
	LDX #0
 jsr pfpixel
.L0223 ;  pfpixel 21 7 on

	LDA #21
	LDY #7
	LDX #0
 jsr pfpixel
.L0224 ;  pfpixel 24 7 on

	LDA #24
	LDY #7
	LDX #0
 jsr pfpixel
.
 ; 

.L0225 ;  pfpixel 5 8 on

	LDA #5
	LDY #8
	LDX #0
 jsr pfpixel
.L0226 ;  pfpixel 8 8 on

	LDA #8
	LDY #8
	LDX #0
 jsr pfpixel
.L0227 ;  pfpixel 10 8 on

	LDA #10
	LDY #8
	LDX #0
 jsr pfpixel
.L0228 ;  pfpixel 14 8 on

	LDA #14
	LDY #8
	LDX #0
 jsr pfpixel
.L0229 ;  pfpixel 16 8 on

	LDA #16
	LDY #8
	LDX #0
 jsr pfpixel
.L0230 ;  pfpixel 17 8 on

	LDA #17
	LDY #8
	LDX #0
 jsr pfpixel
.L0231 ;  pfpixel 21 8 on

	LDA #21
	LDY #8
	LDX #0
 jsr pfpixel
.L0232 ;  pfpixel 22 8 on

	LDA #22
	LDY #8
	LDX #0
 jsr pfpixel
.L0233 ;  pfpixel 23 8 on

	LDA #23
	LDY #8
	LDX #0
 jsr pfpixel
.
 ; 

.L0234 ;  pfpixel 5 9 on

	LDA #5
	LDY #9
	LDX #0
 jsr pfpixel
.L0235 ;  pfpixel 8 9 on

	LDA #8
	LDY #9
	LDX #0
 jsr pfpixel
.L0236 ;  pfpixel 11 9 on

	LDA #11
	LDY #9
	LDX #0
 jsr pfpixel
.L0237 ;  pfpixel 13 9 on

	LDA #13
	LDY #9
	LDX #0
 jsr pfpixel
.L0238 ;  pfpixel 16 9 on

	LDA #16
	LDY #9
	LDX #0
 jsr pfpixel
.L0239 ;  pfpixel 21 9 on

	LDA #21
	LDY #9
	LDX #0
 jsr pfpixel
.L0240 ;  pfpixel 23 9 on

	LDA #23
	LDY #9
	LDX #0
 jsr pfpixel
.
 ; 

.L0241 ;  pfpixel 4 10 off

	LDA #4
	LDY #10
	LDX #1
 jsr pfpixel
.L0242 ;  pfpixel 5 10 off

	LDA #5
	LDY #10
	LDX #1
 jsr pfpixel
.L0243 ;  pfpixel 8 10 off

	LDA #8
	LDY #10
	LDX #1
 jsr pfpixel
.L0244 ;  pfpixel 9 10 off

	LDA #9
	LDY #10
	LDX #1
 jsr pfpixel
.L0245 ;  pfpixel 10 10 off

	LDA #10
	LDY #10
	LDX #1
 jsr pfpixel
.L0246 ;  pfpixel 11 10 off

	LDA #11
	LDY #10
	LDX #1
 jsr pfpixel
.L0247 ;  pfpixel 13 10 off

	LDA #13
	LDY #10
	LDX #1
 jsr pfpixel
.L0248 ;  pfpixel 14 10 off

	LDA #14
	LDY #10
	LDX #1
 jsr pfpixel
.L0249 ;  pfpixel 15 10 off

	LDA #15
	LDY #10
	LDX #1
 jsr pfpixel
.L0250 ;  pfpixel 20 10 off

	LDA #20
	LDY #10
	LDX #1
 jsr pfpixel
.L0251 ;  pfpixel 22 10 off

	LDA #22
	LDY #10
	LDX #1
 jsr pfpixel
.L0252 ;  pfpixel 23 10 off

	LDA #23
	LDY #10
	LDX #1
 jsr pfpixel
.L0253 ;  pfpixel 25 10 off

	LDA #25
	LDY #10
	LDX #1
 jsr pfpixel
.L0254 ;  drawscreen

 jsr drawscreen
.L0255 ;  return

	RTS
.
 ; 

 if (<*) > (<(*+9))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL02_0

	.byte 0
	.byte  %01000100
	.byte  %01000100
	.byte  %01111100
	.byte  %01111100
	.byte  %01111100
	.byte  %10000111
	.byte  %00000111
	.byte  %00000100
 if (<*) > (<(*+3))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL03_1

	.byte 0
	.byte  %01111110
	.byte  %00011000
 if (<*) > (<(*+9))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0110_0

	.byte 0
	.byte  %00100010
	.byte  %00100010
	.byte  %00111110
	.byte  %00111110
	.byte  %00111111
	.byte  %11100000
	.byte  %11100000
	.byte  %00100000
 if (<*) > (<(*+9))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0114_0

	.byte 0
	.byte  %01000100
	.byte  %01000100
	.byte  %01111100
	.byte  %01111100
	.byte  %01111100
	.byte  %10000111
	.byte  %00000111
	.byte  %00000100
       echo "    ",[(scoretable - *)]d , "bytes of ROM space left")
 
 
 
; feel free to modify the score graphics - just keep each digit 8 high
; and keep the conditional compilation stuff intact
 ifconst ROM2k
   ORG $F7AC
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 16
       ORG $4F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 32
       ORG $8F94-bscode_length
       RORG $FF94-bscode_length
     endif
   else
     ORG $FF9C
   endif
 endif


scoretable
       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %01111110
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00111000
       .byte %00011000
       .byte %00001000

       .byte %01111110
       .byte %01100000
       .byte %01100000
       .byte %00111100
       .byte %00000110
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00011100
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00001100
       .byte %00001100
       .byte %01111110
       .byte %01001100
       .byte %01001100
       .byte %00101100
       .byte %00011100
       .byte %00001100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00111100
       .byte %01100000
       .byte %01100000
       .byte %01111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01111100
       .byte %01100000
       .byte %01100010
       .byte %00111100

       .byte %00110000
       .byte %00110000
       .byte %00110000
       .byte %00011000
       .byte %00001100
       .byte %00000110
       .byte %01000010
       .byte %00111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00111110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100 


 ifconst ROM2k
   ORG $F7FC
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 16
       ORG $4FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 32
       ORG $8FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
   else
     ORG $FFFC
   endif
 endif
 ifconst bankswitch
   if bankswitch == 8
     ORG $2FFC
     RORG $FFFC
   endif
   if bankswitch == 16
     ORG $4FFC
     RORG $FFFC
   endif
   if bankswitch == 32
     ORG $8FFC
     RORG $FFFC
   endif
 else
   ifconst ROM2k
     ORG $F7FC
   else
     ORG $FFFC
   endif
 endif
 .word start
 .word start
