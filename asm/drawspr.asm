;$00 - offset to vine Y coordinate adder
;$02 - offset to sprite data

VineYPosAdder:
      .byte $00, $30

DrawVine:
         sty $00                    ;save offset here
         lda Enemy_Rel_YPos         ;get relative vertical coordinate
         clc
         adc VineYPosAdder,y        ;add value using offset in Y to get value
         ldx VineObjOffset,y        ;get offset to vine
         ldy Enemy_SprDataOffset,x  ;get sprite data offset
         sty $02                    ;store sprite data offset here
         jsr SixSpriteStacker       ;stack six sprites on top of each other vertically
         lda Enemy_Rel_XPos         ;get relative horizontal coordinate
         sta Sprite_X_Position,y    ;store in first, third and fifth sprites
         sta Sprite_X_Position+8,y
         sta Sprite_X_Position+16,y
         clc
         adc #$06                   ;add six pixels to second, fourth and sixth sprites
         sta Sprite_X_Position+4,y  ;to give characteristic staggered vine shape to
         sta Sprite_X_Position+12,y ;our vertical stack of sprites
         sta Sprite_X_Position+20,y
         lda #%00100001             ;set bg priority and palette attribute bits
         sta Sprite_Attributes,y    ;set in first, third and fifth sprites
         sta Sprite_Attributes+8,y
         sta Sprite_Attributes+16,y
         ora #%01000000             ;additionally, set horizontal flip bit
         sta Sprite_Attributes+4,y  ;for second, fourth and sixth sprites
         sta Sprite_Attributes+12,y
         sta Sprite_Attributes+20,y
         ldx #$05                   ;set tiles for six sprites
VineTL:  lda #$e1                   ;set tile number for sprite
         sta Sprite_Tilenumber,y
         iny                        ;move offset to next sprite data
         iny
         iny
         iny
         dex                        ;move onto next sprite
         bpl VineTL                 ;loop until all sprites are done
         ldy $02                    ;get original offset
         lda $00                    ;get offset to vine adding data
         bne SkpVTop                ;if offset not zero, skip this part
         lda #$e0
         sta Sprite_Tilenumber,y    ;set other tile number for top of vine
SkpVTop: ldx #$00                   ;start with the first sprite again
ChkFTop: lda VineStart_Y_Position   ;get original starting vertical coordinate
         sec
         sbc Sprite_Y_Position,y    ;subtract top-most sprite's Y coordinate
         cmp #$64                   ;if two coordinates are less than 100/$64 pixels
         bcc NextVSp                ;apart, skip this to leave sprite alone
         lda #$f8
         sta Sprite_Y_Position,y    ;otherwise move sprite offscreen
NextVSp: iny                        ;move offset to next OAM data
         iny
         iny
         iny
         inx                        ;move onto next sprite
         cpx #$06                   ;do this until all sprites are checked
         bne ChkFTop
         ldy $00                    ;return offset set earlier
         rts

SixSpriteStacker:
       ldx #$06           ;do six sprites
StkLp: sta Sprite_Data,y  ;store X or Y coordinate into OAM data
       clc
       adc #$08           ;add eight pixels
       iny
       iny                ;move offset four bytes forward
       iny
       iny
       dex                ;do another sprite
       bne StkLp          ;do this until all sprites are done
       ldy $02            ;get saved OAM data offset and leave
       rts

;-------------------------------------------------------------------------------------

FirstSprXPos:
      .byte $04, $00, $04, $00

FirstSprYPos:
      .byte $00, $04, $00, $04

SecondSprXPos:
      .byte $00, $08, $00, $08

SecondSprYPos:
      .byte $08, $00, $08, $00

FirstSprTilenum:
      .byte $80, $82, $81, $83

SecondSprTilenum:
      .byte $81, $83, $80, $82

HammerSprAttrib:
      .byte $03, $03, $c3, $c3

DrawHammer:
            ldy Misc_SprDataOffset,x    ;get misc object OAM data offset
            lda TimerControl
            bne ForceHPose              ;if master timer control set, skip this part
            lda Misc_State,x            ;otherwise get hammer's state
            and #%01111111              ;mask out d7
            cmp #$01                    ;check to see if set to 1 yet
            beq GetHPose                ;if so, branch
ForceHPose: ldx #$00                    ;reset offset here
            beq RenderH                 ;do unconditional branch to rendering part
GetHPose:   lda FrameCounter            ;get frame counter
            lsr                         ;move d3-d2 to d1-d0
            lsr
            and #%00000011              ;mask out all but d1-d0 (changes every four frames)
            tax                         ;use as timing offset
RenderH:    lda Misc_Rel_YPos           ;get relative vertical coordinate
            clc
            adc FirstSprYPos,x          ;add first sprite vertical adder based on offset
            sta Sprite_Y_Position,y     ;store as sprite Y coordinate for first sprite
            clc
            adc SecondSprYPos,x         ;add second sprite vertical adder based on offset
            sta Sprite_Y_Position+4,y   ;store as sprite Y coordinate for second sprite
            lda Misc_Rel_XPos           ;get relative horizontal coordinate
            clc
            adc FirstSprXPos,x          ;add first sprite horizontal adder based on offset
            sta Sprite_X_Position,y     ;store as sprite X coordinate for first sprite
            clc
            adc SecondSprXPos,x         ;add second sprite horizontal adder based on offset
            sta Sprite_X_Position+4,y   ;store as sprite X coordinate for second sprite
            lda FirstSprTilenum,x
            sta Sprite_Tilenumber,y     ;get and store tile number of first sprite
            lda SecondSprTilenum,x
            sta Sprite_Tilenumber+4,y   ;get and store tile number of second sprite
            lda HammerSprAttrib,x
            sta Sprite_Attributes,y     ;get and store attribute bytes for both
            sta Sprite_Attributes+4,y   ;note in this case they use the same data
            ldx ObjectOffset            ;get misc object offset
            lda Misc_OffscreenBits
            and #%11111100              ;check offscreen bits
            beq NoHOffscr               ;if all bits clear, leave object alone
            lda #$00
            sta Misc_State,x            ;otherwise nullify misc object state
            lda #$f8
            jsr DumpTwoSpr              ;do sub to move hammer sprites offscreen
NoHOffscr:  rts                         ;leave

;-------------------------------------------------------------------------------------
;$00-$01 - used to hold tile numbers ($01 addressed in draw floatey number part)
;$02 - used to hold Y coordinate for floatey number
;$03 - residual byte used for flip (but value set here affects nothing)
;$04 - attribute byte for floatey number
;$05 - used as X coordinate for floatey number

FlagpoleScoreNumTiles:
      .byte $f9, $50
      .byte $f7, $50
      .byte $fa, $fb
      .byte $f8, $fb
      .byte $f6, $fb

FlagpoleGfxHandler:
      ldy Enemy_SprDataOffset,x      ;get sprite data offset for flagpole flag
      lda Enemy_Rel_XPos             ;get relative horizontal coordinate
      sta Sprite_X_Position,y        ;store as X coordinate for first sprite
      clc
      adc #$08                       ;add eight pixels and store
      sta Sprite_X_Position+4,y      ;as X coordinate for second and third sprites
      sta Sprite_X_Position+8,y
      clc
      adc #$0c                       ;add twelve more pixels and
      sta $05                        ;store here to be used later by floatey number
      lda Enemy_Y_Position,x         ;get vertical coordinate
      jsr DumpTwoSpr                 ;and do sub to dump into first and second sprites
      adc #$08                       ;add eight pixels
      sta Sprite_Y_Position+8,y      ;and store into third sprite
      lda FlagpoleFNum_Y_Pos         ;get vertical coordinate for floatey number
      sta $02                        ;store it here
      lda #$01
      sta $03                        ;set value for flip which will not be used, and
      sta $04                        ;attribute byte for floatey number
      sta Sprite_Attributes,y        ;set attribute bytes for all three sprites
      sta Sprite_Attributes+4,y
      sta Sprite_Attributes+8,y
      lda #$7e
      sta Sprite_Tilenumber,y        ;put triangle shaped tile
      sta Sprite_Tilenumber+8,y      ;into first and third sprites
      lda #$7f
      sta Sprite_Tilenumber+4,y      ;put skull tile into second sprite
      lda FlagpoleCollisionYPos      ;get vertical coordinate at time of collision
      beq ChkFlagOffscreen           ;if zero, branch ahead
      tya
      clc                            ;add 12 bytes to sprite data offset
      adc #$0c
      tay                            ;put back in Y
      lda FlagpoleScore              ;get offset used to award points for touching flagpole
      asl                            ;multiply by 2 to get proper offset here
      tax
      lda FlagpoleScoreNumTiles,x    ;get appropriate tile data
      sta $00
      lda FlagpoleScoreNumTiles+1,x
      jsr DrawOneSpriteRow           ;use it to render floatey number

ChkFlagOffscreen:
      ldx ObjectOffset               ;get object offset for flag
      ldy Enemy_SprDataOffset,x      ;get OAM data offset
      lda Enemy_OffscreenBits        ;get offscreen bits
      and #%00001110                 ;mask out all but d3-d1
      beq ExitDumpSpr                ;if none of these bits set, branch to leave

;-------------------------------------------------------------------------------------

MoveSixSpritesOffscreen:
      lda #$f8                  ;set offscreen coordinate if jumping here

DumpSixSpr:
      sta Sprite_Data+20,y      ;dump A contents
      sta Sprite_Data+16,y      ;into third row sprites

DumpFourSpr:
      sta Sprite_Data+12,y      ;into second row sprites

DumpThreeSpr:
      sta Sprite_Data+8,y

DumpTwoSpr:
      sta Sprite_Data+4,y       ;and into first row sprites
      sta Sprite_Data,y

ExitDumpSpr:
      rts

;-------------------------------------------------------------------------------------

DrawLargePlatform:
      ldy Enemy_SprDataOffset,x   ;get OAM data offset
      sty $02                     ;store here
      iny                         ;add 3 to it for offset
      iny                         ;to X coordinate
      iny
      lda Enemy_Rel_XPos          ;get horizontal relative coordinate
      jsr SixSpriteStacker        ;store X coordinates using A as base, stack horizontally
      ldx ObjectOffset
      lda Enemy_Y_Position,x      ;get vertical coordinate
      jsr DumpFourSpr             ;dump into first four sprites as Y coordinate
      ldy AreaType
      cpy #$03                    ;check for castle-type level
      beq ShrinkPlatform
      ldy SecondaryHardMode       ;check for secondary hard mode flag set
      beq SetLast2Platform        ;branch if not set elsewhere

ShrinkPlatform:
      lda #$f8                    ;load offscreen coordinate if flag set or castle-type level

SetLast2Platform:
      ldy Enemy_SprDataOffset,x   ;get OAM data offset
      sta Sprite_Y_Position+16,y  ;store vertical coordinate or offscreen
      sta Sprite_Y_Position+20,y  ;coordinate into last two sprites as Y coordinate
      lda #$5b                    ;load default tile for platform (girder)
      ldx CloudTypeOverride
      beq SetPlatformTilenum      ;if cloud level override flag not set, use
      lda #$75                    ;otherwise load other tile for platform (puff)

SetPlatformTilenum:
        ldx ObjectOffset            ;get enemy object buffer offset
        iny                         ;increment Y for tile offset
        jsr DumpSixSpr              ;dump tile number into all six sprites
        lda #$02                    ;set palette controls
        iny                         ;increment Y for sprite attributes
        jsr DumpSixSpr              ;dump attributes into all six sprites
        inx                         ;increment X for enemy objects
        jsr GetXOffscreenBits       ;get offscreen bits again
        dex
        ldy Enemy_SprDataOffset,x   ;get OAM data offset
        asl                         ;rotate d7 into carry, save remaining
        pha                         ;bits to the stack
        bcc SChk2
        lda #$f8                    ;if d7 was set, move first sprite offscreen
        sta Sprite_Y_Position,y
SChk2:  pla                         ;get bits from stack
        asl                         ;rotate d6 into carry
        pha                         ;save to stack
        bcc SChk3
        lda #$f8                    ;if d6 was set, move second sprite offscreen
        sta Sprite_Y_Position+4,y
SChk3:  pla                         ;get bits from stack
        asl                         ;rotate d5 into carry
        pha                         ;save to stack
        bcc SChk4
        lda #$f8                    ;if d5 was set, move third sprite offscreen
        sta Sprite_Y_Position+8,y
SChk4:  pla                         ;get bits from stack
        asl                         ;rotate d4 into carry
        pha                         ;save to stack
        bcc SChk5
        lda #$f8                    ;if d4 was set, move fourth sprite offscreen
        sta Sprite_Y_Position+12,y
SChk5:  pla                         ;get bits from stack
        asl                         ;rotate d3 into carry
        pha                         ;save to stack
        bcc SChk6
        lda #$f8                    ;if d3 was set, move fifth sprite offscreen
        sta Sprite_Y_Position+16,y
SChk6:  pla                         ;get bits from stack
        asl                         ;rotate d2 into carry
        bcc SLChk                   ;save to stack
        lda #$f8
        sta Sprite_Y_Position+20,y  ;if d2 was set, move sixth sprite offscreen
SLChk:  lda Enemy_OffscreenBits     ;check d7 of offscreen bits
        asl                         ;and if d7 is not set, skip sub
        bcc ExDLPl
        jsr MoveSixSpritesOffscreen ;otherwise branch to move all sprites offscreen
ExDLPl: rts

;-------------------------------------------------------------------------------------

DrawFloateyNumber_Coin:
          lda FrameCounter          ;get frame counter
          lsr                       ;divide by 2
          bcs NotRsNum              ;branch if d0 not set to raise number every other frame
          dec Misc_Y_Position,x     ;otherwise, decrement vertical coordinate
NotRsNum: lda Misc_Y_Position,x     ;get vertical coordinate
          jsr DumpTwoSpr            ;dump into both sprites
          lda Misc_Rel_XPos         ;get relative horizontal coordinate
          sta Sprite_X_Position,y   ;store as X coordinate for first sprite
          clc
          adc #$08                  ;add eight pixels
          sta Sprite_X_Position+4,y ;store as X coordinate for second sprite
          lda #$02
          sta Sprite_Attributes,y   ;store attribute byte in both sprites
          sta Sprite_Attributes+4,y
          lda #$f7
          sta Sprite_Tilenumber,y   ;put tile numbers into both sprites
          lda #$fb                  ;that resemble "200"
          sta Sprite_Tilenumber+4,y
          jmp ExJCGfx               ;then jump to leave (why not an rts here instead?)

JumpingCoinTiles:
      .byte $60, $61, $62, $63

JCoinGfxHandler:
         ldy Misc_SprDataOffset,x    ;get coin/floatey number's OAM data offset
         lda Misc_State,x            ;get state of misc object
         cmp #$02                    ;if 2 or greater, 
         bcs DrawFloateyNumber_Coin  ;branch to draw floatey number
         lda Misc_Y_Position,x       ;store vertical coordinate as
         sta Sprite_Y_Position,y     ;Y coordinate for first sprite
         clc
         adc #$08                    ;add eight pixels
         sta Sprite_Y_Position+4,y   ;store as Y coordinate for second sprite
         lda Misc_Rel_XPos           ;get relative horizontal coordinate
         sta Sprite_X_Position,y
         sta Sprite_X_Position+4,y   ;store as X coordinate for first and second sprites
         lda FrameCounter            ;get frame counter
         lsr                         ;divide by 2 to alter every other frame
         and #%00000011              ;mask out d2-d1
         tax                         ;use as graphical offset
         lda JumpingCoinTiles,x      ;load tile number
         iny                         ;increment OAM data offset to write tile numbers
         jsr DumpTwoSpr              ;do sub to dump tile number into both sprites
         dey                         ;decrement to get old offset
         lda #$02
         sta Sprite_Attributes,y     ;set attribute byte in first sprite
         lda #$82
         sta Sprite_Attributes+4,y   ;set attribute byte with vertical flip in second sprite
         ldx ObjectOffset            ;get misc object offset
ExJCGfx: rts                         ;leave

;-------------------------------------------------------------------------------------
;$00-$01 - used to hold tiles for drawing the power-up, $00 also used to hold power-up type
;$02 - used to hold bottom row Y position
;$03 - used to hold flip control (not used here)
;$04 - used to hold sprite attributes
;$05 - used to hold X position
;$07 - counter

;tiles arranged in top left, right, bottom left, right order
PowerUpGfxTable:
      .byte $76, $77, $78, $79 ;regular mushroom
      .byte $d6, $d6, $d9, $d9 ;fire flower
      .byte $8d, $8d, $e4, $e4 ;star
      .byte $76, $77, $78, $79 ;1-up mushroom

PowerUpAttributes:
      .byte $02, $01, $02, $01

DrawPowerUp:
      ldy Enemy_SprDataOffset+5  ;get power-up's sprite data offset
      lda Enemy_Rel_YPos         ;get relative vertical coordinate
      clc
      adc #$08                   ;add eight pixels
      sta $02                    ;store result here
      lda Enemy_Rel_XPos         ;get relative horizontal coordinate
      sta $05                    ;store here
      ldx PowerUpType            ;get power-up type
      lda PowerUpAttributes,x    ;get attribute data for power-up type
      ora Enemy_SprAttrib+5      ;add background priority bit if set
      sta $04                    ;store attributes here
      txa
      pha                        ;save power-up type to the stack
      asl
      asl                        ;multiply by four to get proper offset
      tax                        ;use as X
      lda #$01
      sta $07                    ;set counter here to draw two rows of sprite object
      sta $03                    ;init d1 of flip control

PUpDrawLoop:
        lda PowerUpGfxTable,x      ;load left tile of power-up object
        sta $00
        lda PowerUpGfxTable+1,x    ;load right tile
        jsr DrawOneSpriteRow       ;branch to draw one row of our power-up object
        dec $07                    ;decrement counter
        bpl PUpDrawLoop            ;branch until two rows are drawn
        ldy Enemy_SprDataOffset+5  ;get sprite data offset again
        pla                        ;pull saved power-up type from the stack
        beq PUpOfs                 ;if regular mushroom, branch, do not change colors or flip
        cmp #$03
        beq PUpOfs                 ;if 1-up mushroom, branch, do not change colors or flip
        sta $00                    ;store power-up type here now
        lda FrameCounter           ;get frame counter
        lsr                        ;divide by 2 to change colors every two frames
        and #%00000011             ;mask out all but d1 and d0 (previously d2 and d1)
        ora Enemy_SprAttrib+5      ;add background priority bit if any set
        sta Sprite_Attributes,y    ;set as new palette bits for top left and
        sta Sprite_Attributes+4,y  ;top right sprites for fire flower and star
        ldx $00
        dex                        ;check power-up type for fire flower
        beq FlipPUpRightSide       ;if found, skip this part
        sta Sprite_Attributes+8,y  ;otherwise set new palette bits  for bottom left
        sta Sprite_Attributes+12,y ;and bottom right sprites as well for star only

FlipPUpRightSide:
        lda Sprite_Attributes+4,y
        ora #%01000000             ;set horizontal flip bit for top right sprite
        sta Sprite_Attributes+4,y
        lda Sprite_Attributes+12,y
        ora #%01000000             ;set horizontal flip bit for bottom right sprite
        sta Sprite_Attributes+12,y ;note these are only done for fire flower and star power-ups
PUpOfs: jmp SprObjectOffscrChk     ;jump to check to see if power-up is offscreen at all, then leave

;-------------------------------------------------------------------------------------
;$00-$01 - used in DrawEnemyObjRow to hold sprite tile numbers
;$02 - used to store Y position
;$03 - used to store moving direction, used to flip enemies horizontally
;$04 - used to store enemy's sprite attributes
;$05 - used to store X position
;$eb - used to hold sprite data offset
;$ec - used to hold either altered enemy state or special value used in gfx handler as condition
;$ed - used to hold enemy state from buffer 
;$ef - used to hold enemy code used in gfx handler (may or may not resemble Enemy_ID values)

;tiles arranged in top left, right, middle left, right, bottom left, right order
EnemyGraphicsTable:
      .byte $fc, $fc, $aa, $ab, $ac, $ad  ;buzzy beetle frame 1
      .byte $fc, $fc, $ae, $af, $b0, $b1  ;             frame 2
      .byte $fc, $a5, $a6, $a7, $a8, $a9  ;koopa troopa frame 1
      .byte $fc, $a0, $a1, $a2, $a3, $a4  ;             frame 2
      .byte $69, $a5, $6a, $a7, $a8, $a9  ;koopa paratroopa frame 1
      .byte $6b, $a0, $6c, $a2, $a3, $a4  ;                 frame 2
      .byte $fc, $fc, $96, $97, $98, $99  ;spiny frame 1
      .byte $fc, $fc, $9a, $9b, $9c, $9d  ;      frame 2
      .byte $fc, $fc, $8f, $8e, $8e, $8f  ;spiny's egg frame 1
      .byte $fc, $fc, $95, $94, $94, $95  ;            frame 2
      .byte $fc, $fc, $dc, $dc, $df, $df  ;bloober frame 1
      .byte $dc, $dc, $dd, $dd, $de, $de  ;        frame 2
      .byte $fc, $fc, $b2, $b3, $b4, $b5  ;cheep-cheep frame 1
      .byte $fc, $fc, $b6, $b3, $b7, $b5  ;            frame 2
      .byte $fc, $fc, $70, $71, $72, $73  ;goomba
      .byte $fc, $fc, $6e, $6e, $6f, $6f  ;koopa shell frame 1 (upside-down)
      .byte $fc, $fc, $6d, $6d, $6f, $6f  ;            frame 2
      .byte $fc, $fc, $6f, $6f, $6e, $6e  ;koopa shell frame 1 (rightsideup)
      .byte $fc, $fc, $6f, $6f, $6d, $6d  ;            frame 2
      .byte $fc, $fc, $f4, $f4, $f5, $f5  ;buzzy beetle shell frame 1 (rightsideup)
      .byte $fc, $fc, $f4, $f4, $f5, $f5  ;                   frame 2
      .byte $fc, $fc, $f5, $f5, $f4, $f4  ;buzzy beetle shell frame 1 (upside-down)
      .byte $fc, $fc, $f5, $f5, $f4, $f4  ;                   frame 2
      .byte $fc, $fc, $fc, $fc, $ef, $ef  ;defeated goomba
      .byte $b9, $b8, $bb, $ba, $bc, $bc  ;lakitu frame 1
      .byte $fc, $fc, $bd, $bd, $bc, $bc  ;       frame 2
      .byte $7a, $7b, $da, $db, $d8, $d8  ;princess
      .byte $cd, $cd, $ce, $ce, $cf, $cf  ;mushroom retainer
      .byte $7d, $7c, $d1, $8c, $d3, $d2  ;hammer bro frame 1
      .byte $7d, $7c, $89, $88, $8b, $8a  ;           frame 2
      .byte $d5, $d4, $e3, $e2, $d3, $d2  ;           frame 3
      .byte $d5, $d4, $e3, $e2, $8b, $8a  ;           frame 4
      .byte $e5, $e5, $e6, $e6, $eb, $eb  ;piranha plant frame 1
      .byte $ec, $ec, $ed, $ed, $ee, $ee  ;              frame 2
      .byte $fc, $fc, $d0, $d0, $d7, $d7  ;podoboo
      .byte $bf, $be, $c1, $c0, $c2, $fc  ;bowser front frame 1
      .byte $c4, $c3, $c6, $c5, $c8, $c7  ;bowser rear frame 1
      .byte $bf, $be, $ca, $c9, $c2, $fc  ;       front frame 2
      .byte $c4, $c3, $c6, $c5, $cc, $cb  ;       rear frame 2
      .byte $fc, $fc, $e8, $e7, $ea, $e9  ;bullet bill
      .byte $f2, $f2, $f3, $f3, $f2, $f2  ;jumpspring frame 1
      .byte $f1, $f1, $f1, $f1, $fc, $fc  ;           frame 2
      .byte $f0, $f0, $fc, $fc, $fc, $fc  ;           frame 3

EnemyGfxTableOffsets:
      .byte $0c, $0c, $00, $0c, $0c, $a8, $54, $3c
      .byte $ea, $18, $48, $48, $cc, $c0, $18, $18
      .byte $18, $90, $24, $ff, $48, $9c, $d2, $d8
      .byte $f0, $f6, $fc

EnemyAttributeData:
      .byte $01, $02, $03, $02, $01, $01, $03, $03
      .byte $03, $01, $01, $02, $02, $21, $01, $02
      .byte $01, $01, $02, $ff, $02, $02, $01, $01
      .byte $02, $02, $02

EnemyAnimTimingBMask:
      .byte $08, $18

JumpspringFrameOffsets:
      .byte $18, $19, $1a, $19, $18

EnemyGfxHandler:
      lda Enemy_Y_Position,x      ;get enemy object vertical position
      sta $02
      lda Enemy_Rel_XPos          ;get enemy object horizontal position
      sta $05                     ;relative to screen
      ldy Enemy_SprDataOffset,x
      sty $eb                     ;get sprite data offset
      lda #$00
      sta VerticalFlipFlag        ;initialize vertical flip flag by default
      lda Enemy_MovingDir,x
      sta $03                     ;get enemy object moving direction
      lda Enemy_SprAttrib,x
      sta $04                     ;get enemy object sprite attributes
      lda Enemy_ID,x
      cmp #PiranhaPlant           ;is enemy object piranha plant?
      bne CheckForRetainerObj     ;if not, branch
      ldy PiranhaPlant_Y_Speed,x
      bmi CheckForRetainerObj     ;if piranha plant moving upwards, branch
      ldy EnemyFrameTimer,x
      beq CheckForRetainerObj     ;if timer for movement expired, branch
      rts                         ;if all conditions fail, leave

CheckForRetainerObj:
      lda Enemy_State,x           ;store enemy state
      sta $ed
      and #%00011111              ;nullify all but 5 LSB and use as Y
      tay
      lda Enemy_ID,x              ;check for mushroom retainer/princess object
      cmp #RetainerObject
      bne CheckForBulletBillCV    ;if not found, branch
      ldy #$00                    ;if found, nullify saved state in Y
      lda #$01                    ;set value that will not be used
      sta $03
      lda #$15                    ;set value $15 as code for mushroom retainer/princess object

CheckForBulletBillCV:
       cmp #BulletBill_CannonVar   ;otherwise check for bullet bill object
       bne CheckForJumpspring      ;if not found, branch again
       dec $02                     ;decrement saved vertical position
       lda #$03
       ldy EnemyFrameTimer,x       ;get timer for enemy object
       beq SBBAt                   ;if expired, do not set priority bit
       ora #%00100000              ;otherwise do so
SBBAt: sta $04                     ;set new sprite attributes
       ldy #$00                    ;nullify saved enemy state both in Y and in
       sty $ed                     ;memory location here
       lda #$08                    ;set specific value to unconditionally branch once

CheckForJumpspring:
      cmp #JumpspringObject        ;check for jumpspring object
      bne CheckForPodoboo
      ldy #$03                     ;set enemy state -2 MSB here for jumpspring object
      ldx JumpspringAnimCtrl       ;get current frame number for jumpspring object
      lda JumpspringFrameOffsets,x ;load data using frame number as offset

CheckForPodoboo:
      sta $ef                 ;store saved enemy object value here
      sty $ec                 ;and Y here (enemy state -2 MSB if not changed)
      ldx ObjectOffset        ;get enemy object offset
      cmp #$0c                ;check for podoboo object
      bne CheckBowserGfxFlag  ;branch if not found
      lda Enemy_Y_Speed,x     ;if moving upwards, branch
      bmi CheckBowserGfxFlag
      inc VerticalFlipFlag    ;otherwise, set flag for vertical flip

CheckBowserGfxFlag:
             lda BowserGfxFlag   ;if not drawing bowser at all, skip to something else
             beq CheckForGoomba
             ldy #$16            ;if set to 1, draw bowser's front
             cmp #$01
             beq SBwsrGfxOfs
             iny                 ;otherwise draw bowser's rear
SBwsrGfxOfs: sty $ef

CheckForGoomba:
          ldy $ef               ;check value for goomba object
          cpy #Goomba
          bne CheckBowserFront  ;branch if not found
          lda Enemy_State,x
          cmp #$02              ;check for defeated state
          bcc GmbaAnim          ;if not defeated, go ahead and animate
          ldx #$04              ;if defeated, write new value here
          stx $ec
GmbaAnim: and #%00100000        ;check for d5 set in enemy object state 
          ora TimerControl      ;or timer disable flag set
          bne CheckBowserFront  ;if either condition true, do not animate goomba
          lda FrameCounter
          and #%00001000        ;check for every eighth frame
          bne CheckBowserFront
          lda $03
          eor #%00000011        ;invert bits to flip horizontally every eight frames
          sta $03               ;leave alone otherwise

CheckBowserFront:
             lda EnemyAttributeData,y    ;load sprite attribute using enemy object
             ora $04                     ;as offset, and add to bits already loaded
             sta $04
             lda EnemyGfxTableOffsets,y  ;load value based on enemy object as offset
             tax                         ;save as X
             ldy $ec                     ;get previously saved value
             lda BowserGfxFlag
             beq CheckForSpiny           ;if not drawing bowser object at all, skip all of this
             cmp #$01
             bne CheckBowserRear         ;if not drawing front part, branch to draw the rear part
             lda BowserBodyControls      ;check bowser's body control bits
             bpl ChkFrontSte             ;branch if d7 not set (control's bowser's mouth)      
             ldx #$de                    ;otherwise load offset for second frame
ChkFrontSte: lda $ed                     ;check saved enemy state
             and #%00100000              ;if bowser not defeated, do not set flag
             beq DrawBowser

FlipBowserOver:
      stx VerticalFlipFlag  ;set vertical flip flag to nonzero

DrawBowser:
      jmp DrawEnemyObject   ;draw bowser's graphics now

CheckBowserRear:
            lda BowserBodyControls  ;check bowser's body control bits
            and #$01
            beq ChkRearSte          ;branch if d0 not set (control's bowser's feet)
            ldx #$e4                ;otherwise load offset for second frame
ChkRearSte: lda $ed                 ;check saved enemy state
            and #%00100000          ;if bowser not defeated, do not set flag
            beq DrawBowser
            lda $02                 ;subtract 16 pixels from
            sec                     ;saved vertical coordinate
            sbc #$10
            sta $02
            jmp FlipBowserOver      ;jump to set vertical flip flag

CheckForSpiny:
        cpx #$24               ;check if value loaded is for spiny
        bne CheckForLakitu     ;if not found, branch
        cpy #$05               ;if enemy state set to $05, do this,
        bne NotEgg             ;otherwise branch
        ldx #$30               ;set to spiny egg offset
        lda #$02
        sta $03                ;set enemy direction to reverse sprites horizontally
        lda #$05
        sta $ec                ;set enemy state
NotEgg: jmp CheckForHammerBro  ;skip a big chunk of this if we found spiny but not in egg

CheckForLakitu:
        cpx #$90                  ;check value for lakitu's offset loaded
        bne CheckUpsideDownShell  ;branch if not loaded
        lda $ed
        and #%00100000            ;check for d5 set in enemy state
        bne NoLAFr                ;branch if set
        lda FrenzyEnemyTimer
        cmp #$10                  ;check timer to see if we've reached a certain range
        bcs NoLAFr                ;branch if not
        ldx #$96                  ;if d6 not set and timer in range, load alt frame for lakitu
NoLAFr: jmp CheckDefeatedState    ;skip this next part if we found lakitu but alt frame not needed

CheckUpsideDownShell:
      lda $ef                    ;check for enemy object => $04
      cmp #$04
      bcs CheckRightSideUpShell  ;branch if true
      cpy #$02
      bcc CheckRightSideUpShell  ;branch if enemy state < $02
      ldx #$5a                   ;set for upside-down koopa shell by default
      ldy $ef
      cpy #BuzzyBeetle           ;check for buzzy beetle object
      bne CheckRightSideUpShell
      ldx #$7e                   ;set for upside-down buzzy beetle shell if found
      inc $02                    ;increment vertical position by one pixel

CheckRightSideUpShell:
      lda $ec                ;check for value set here
      cmp #$04               ;if enemy state < $02, do not change to shell, if
      bne CheckForHammerBro  ;enemy state => $02 but not = $04, leave shell upside-down
      ldx #$72               ;set right-side up buzzy beetle shell by default
      inc $02                ;increment saved vertical position by one pixel
      ldy $ef
      cpy #BuzzyBeetle       ;check for buzzy beetle object
      beq CheckForDefdGoomba ;branch if found
      ldx #$66               ;change to right-side up koopa shell if not found
      inc $02                ;and increment saved vertical position again

CheckForDefdGoomba:
      cpy #Goomba            ;check for goomba object (necessary if previously
      bne CheckForHammerBro  ;failed buzzy beetle object test)
      ldx #$54               ;load for regular goomba
      lda $ed                ;note that this only gets performed if enemy state => $02
      and #%00100000         ;check saved enemy state for d5 set
      bne CheckForHammerBro  ;branch if set
      ldx #$8a               ;load offset for defeated goomba
      dec $02                ;set different value and decrement saved vertical position

CheckForHammerBro:
      ldy ObjectOffset
      lda $ef                  ;check for hammer bro object
      cmp #HammerBro
      bne CheckForBloober      ;branch if not found
      lda $ed
      beq CheckToAnimateEnemy  ;branch if not in normal enemy state
      and #%00001000
      beq CheckDefeatedState   ;if d3 not set, branch further away
      ldx #$b4                 ;otherwise load offset for different frame
      bne CheckToAnimateEnemy  ;unconditional branch

CheckForBloober:
      cpx #$48                 ;check for cheep-cheep offset loaded
      beq CheckToAnimateEnemy  ;branch if found
      lda EnemyIntervalTimer,y
      cmp #$05
      bcs CheckDefeatedState   ;branch if some timer is above a certain point
      cpx #$3c                 ;check for bloober offset loaded
      bne CheckToAnimateEnemy  ;branch if not found this time
      cmp #$01
      beq CheckDefeatedState   ;branch if timer is set to certain point
      inc $02                  ;increment saved vertical coordinate three pixels
      inc $02
      inc $02
      jmp CheckAnimationStop   ;and do something else

CheckToAnimateEnemy:
      lda $ef                  ;check for specific enemy objects
      cmp #Goomba
      beq CheckDefeatedState   ;branch if goomba
      cmp #$08
      beq CheckDefeatedState   ;branch if bullet bill (note both variants use $08 here)
      cmp #Podoboo
      beq CheckDefeatedState   ;branch if podoboo
      cmp #$18                 ;branch if => $18
      bcs CheckDefeatedState
      ldy #$00    
      cmp #$15                 ;check for mushroom retainer/princess object
      bne CheckForSecondFrame  ;which uses different code here, branch if not found
      iny                      ;residual instruction
      lda WorldNumber          ;are we on world 8?
      cmp #World8
      bcs CheckDefeatedState   ;if so, leave the offset alone (use princess)
      ldx #$a2                 ;otherwise, set for mushroom retainer object instead
      lda #$03                 ;set alternate state here
      sta $ec
      bne CheckDefeatedState   ;unconditional branch

CheckForSecondFrame:
      lda FrameCounter            ;load frame counter
      and EnemyAnimTimingBMask,y  ;mask it (partly residual, one byte not ever used)
      bne CheckDefeatedState      ;branch if timing is off

CheckAnimationStop:
      lda $ed                 ;check saved enemy state
      and #%10100000          ;for d7 or d5, or check for timers stopped
      ora TimerControl
      bne CheckDefeatedState  ;if either condition true, branch
      txa
      clc
      adc #$06                ;add $06 to current enemy offset
      tax                     ;to animate various enemy objects

CheckDefeatedState:
      lda $ed               ;check saved enemy state
      and #%00100000        ;for d5 set
      beq DrawEnemyObject   ;branch if not set
      lda $ef
      cmp #$04              ;check for saved enemy object => $04
      bcc DrawEnemyObject   ;branch if less
      ldy #$01
      sty VerticalFlipFlag  ;set vertical flip flag
      dey
      sty $ec               ;init saved value here

DrawEnemyObject:
      ldy $eb                    ;load sprite data offset
      jsr DrawEnemyObjRow        ;draw six tiles of data
      jsr DrawEnemyObjRow        ;into sprite data
      jsr DrawEnemyObjRow
      ldx ObjectOffset           ;get enemy object offset
      ldy Enemy_SprDataOffset,x  ;get sprite data offset
      lda $ef
      cmp #$08                   ;get saved enemy object and check
      bne CheckForVerticalFlip   ;for bullet bill, branch if not found

SkipToOffScrChk:
      jmp SprObjectOffscrChk     ;jump if found

CheckForVerticalFlip:
      lda VerticalFlipFlag       ;check if vertical flip flag is set here
      beq CheckForESymmetry      ;branch if not
      lda Sprite_Attributes,y    ;get attributes of first sprite we dealt with
      ora #%10000000             ;set bit for vertical flip
      iny
      iny                        ;increment two bytes so that we store the vertical flip
      jsr DumpSixSpr             ;in attribute bytes of enemy obj sprite data
      dey
      dey                        ;now go back to the Y coordinate offset
      tya
      tax                        ;give offset to X
      lda $ef
      cmp #HammerBro             ;check saved enemy object for hammer bro
      beq FlipEnemyVertically
      cmp #Lakitu                ;check saved enemy object for lakitu
      beq FlipEnemyVertically    ;branch for hammer bro or lakitu
      cmp #$15
      bcs FlipEnemyVertically    ;also branch if enemy object => $15
      txa
      clc
      adc #$08                   ;if not selected objects or => $15, set
      tax                        ;offset in X for next row

FlipEnemyVertically:
      lda Sprite_Tilenumber,x     ;load first or second row tiles
      pha                         ;and save tiles to the stack
      lda Sprite_Tilenumber+4,x
      pha
      lda Sprite_Tilenumber+16,y  ;exchange third row tiles
      sta Sprite_Tilenumber,x     ;with first or second row tiles
      lda Sprite_Tilenumber+20,y
      sta Sprite_Tilenumber+4,x
      pla                         ;pull first or second row tiles from stack
      sta Sprite_Tilenumber+20,y  ;and save in third row
      pla
      sta Sprite_Tilenumber+16,y

CheckForESymmetry:
        lda BowserGfxFlag           ;are we drawing bowser at all?
        bne SkipToOffScrChk         ;branch if so
        lda $ef       
        ldx $ec                     ;get alternate enemy state
        cmp #$05                    ;check for hammer bro object
        bne ContES
        jmp SprObjectOffscrChk      ;jump if found
ContES: cmp #Bloober                ;check for bloober object
        beq MirrorEnemyGfx
        cmp #PiranhaPlant           ;check for piranha plant object
        beq MirrorEnemyGfx
        cmp #Podoboo                ;check for podoboo object
        beq MirrorEnemyGfx          ;branch if either of three are found
        cmp #Spiny                  ;check for spiny object
        bne ESRtnr                  ;branch closer if not found
        cpx #$05                    ;check spiny's state
        bne CheckToMirrorLakitu     ;branch if not an egg, otherwise
ESRtnr: cmp #$15                    ;check for princess/mushroom retainer object
        bne SpnySC
        lda #$42                    ;set horizontal flip on bottom right sprite
        sta Sprite_Attributes+20,y  ;note that palette bits were already set earlier
SpnySC: cpx #$02                    ;if alternate enemy state set to 1 or 0, branch
        bcc CheckToMirrorLakitu

MirrorEnemyGfx:
        lda BowserGfxFlag           ;if enemy object is bowser, skip all of this
        bne CheckToMirrorLakitu
        lda Sprite_Attributes,y     ;load attribute bits of first sprite
        and #%10100011
        sta Sprite_Attributes,y     ;save vertical flip, priority, and palette bits
        sta Sprite_Attributes+8,y   ;in left sprite column of enemy object OAM data
        sta Sprite_Attributes+16,y
        ora #%01000000              ;set horizontal flip
        cpx #$05                    ;check for state used by spiny's egg
        bne EggExc                  ;if alternate state not set to $05, branch
        ora #%10000000              ;otherwise set vertical flip
EggExc: sta Sprite_Attributes+4,y   ;set bits of right sprite column
        sta Sprite_Attributes+12,y  ;of enemy object sprite data
        sta Sprite_Attributes+20,y
        cpx #$04                    ;check alternate enemy state
        bne CheckToMirrorLakitu     ;branch if not $04
        lda Sprite_Attributes+8,y   ;get second row left sprite attributes
        ora #%10000000
        sta Sprite_Attributes+8,y   ;store bits with vertical flip in
        sta Sprite_Attributes+16,y  ;second and third row left sprites
        ora #%01000000
        sta Sprite_Attributes+12,y  ;store with horizontal and vertical flip in
        sta Sprite_Attributes+20,y  ;second and third row right sprites

CheckToMirrorLakitu:
        lda $ef                     ;check for lakitu enemy object
        cmp #Lakitu
        bne CheckToMirrorJSpring    ;branch if not found
        lda VerticalFlipFlag
        bne NVFLak                  ;branch if vertical flip flag not set
        lda Sprite_Attributes+16,y  ;save vertical flip and palette bits
        and #%10000001              ;in third row left sprite
        sta Sprite_Attributes+16,y
        lda Sprite_Attributes+20,y  ;set horizontal flip and palette bits
        ora #%01000001              ;in third row right sprite
        sta Sprite_Attributes+20,y
        ldx FrenzyEnemyTimer        ;check timer
        cpx #$10
        bcs SprObjectOffscrChk      ;branch if timer has not reached a certain range
        sta Sprite_Attributes+12,y  ;otherwise set same for second row right sprite
        and #%10000001
        sta Sprite_Attributes+8,y   ;preserve vertical flip and palette bits for left sprite
        bcc SprObjectOffscrChk      ;unconditional branch
NVFLak: lda Sprite_Attributes,y     ;get first row left sprite attributes
        and #%10000001
        sta Sprite_Attributes,y     ;save vertical flip and palette bits
        lda Sprite_Attributes+4,y   ;get first row right sprite attributes
        ora #%01000001              ;set horizontal flip and palette bits
        sta Sprite_Attributes+4,y   ;note that vertical flip is left as-is

CheckToMirrorJSpring:
      lda $ef                     ;check for jumpspring object (any frame)
      cmp #$18
      bcc SprObjectOffscrChk      ;branch if not jumpspring object at all
      lda #$82
      sta Sprite_Attributes+8,y   ;set vertical flip and palette bits of 
      sta Sprite_Attributes+16,y  ;second and third row left sprites
      ora #%01000000
      sta Sprite_Attributes+12,y  ;set, in addition to those, horizontal flip
      sta Sprite_Attributes+20,y  ;for second and third row right sprites

SprObjectOffscrChk:
         ldx ObjectOffset          ;get enemy buffer offset
         lda Enemy_OffscreenBits   ;check offscreen information
         lsr
         lsr                       ;shift three times to the right
         lsr                       ;which puts d2 into carry
         pha                       ;save to stack
         bcc LcChk                 ;branch if not set
         lda #$04                  ;set for right column sprites
         jsr MoveESprColOffscreen  ;and move them offscreen
LcChk:   pla                       ;get from stack
         lsr                       ;move d3 to carry
         pha                       ;save to stack
         bcc Row3C                 ;branch if not set
         lda #$00                  ;set for left column sprites,
         jsr MoveESprColOffscreen  ;move them offscreen
Row3C:   pla                       ;get from stack again
         lsr                       ;move d5 to carry this time
         lsr
         pha                       ;save to stack again
         bcc Row23C                ;branch if carry not set
         lda #$10                  ;set for third row of sprites
         jsr MoveESprRowOffscreen  ;and move them offscreen
Row23C:  pla                       ;get from stack
         lsr                       ;move d6 into carry
         pha                       ;save to stack
         bcc AllRowC
         lda #$08                  ;set for second and third rows
         jsr MoveESprRowOffscreen  ;move them offscreen
AllRowC: pla                       ;get from stack once more
         lsr                       ;move d7 into carry
         bcc ExEGHandler
         jsr MoveESprRowOffscreen  ;move all sprites offscreen (A should be 0 by now)
         lda Enemy_ID,x
         cmp #Podoboo              ;check enemy identifier for podoboo
         beq ExEGHandler           ;skip this part if found, we do not want to erase podoboo!
         lda Enemy_Y_HighPos,x     ;check high byte of vertical position
         cmp #$02                  ;if not yet past the bottom of the screen, branch
         bne ExEGHandler
         jsr EraseEnemyObject      ;what it says

ExEGHandler:
      rts

DrawEnemyObjRow:
      lda EnemyGraphicsTable,x    ;load two tiles of enemy graphics
      sta $00
      lda EnemyGraphicsTable+1,x

DrawOneSpriteRow:
      sta $01
      jmp DrawSpriteObject        ;draw them

MoveESprRowOffscreen:
      clc                         ;add A to enemy object OAM data offset
      adc Enemy_SprDataOffset,x
      tay                         ;use as offset
      lda #$f8
      jmp DumpTwoSpr              ;move first row of sprites offscreen

MoveESprColOffscreen:
      clc                         ;add A to enemy object OAM data offset
      adc Enemy_SprDataOffset,x
      tay                         ;use as offset
      jsr MoveColOffscreen        ;move first and second row sprites in column offscreen
      sta Sprite_Data+16,y        ;move third row sprite in column offscreen
      rts

;-------------------------------------------------------------------------------------
;$00-$01 - tile numbers
;$02 - relative Y position
;$03 - horizontal flip flag (not used here)
;$04 - attributes
;$05 - relative X position

DefaultBlockObjTiles:
      .byte $85, $85, $86, $86             ;brick w/ line (these are sprite tiles, not BG!)

DrawBlock:
           lda Block_Rel_YPos            ;get relative vertical coordinate of block object
           sta $02                       ;store here
           lda Block_Rel_XPos            ;get relative horizontal coordinate of block object
           sta $05                       ;store here
           lda #$03
           sta $04                       ;set attribute byte here
           lsr
           sta $03                       ;set horizontal flip bit here (will not be used)
           ldy Block_SprDataOffset,x     ;get sprite data offset
           ldx #$00                      ;reset X for use as offset to tile data
DBlkLoop:  lda DefaultBlockObjTiles,x    ;get left tile number
           sta $00                       ;set here
           lda DefaultBlockObjTiles+1,x  ;get right tile number
           jsr DrawOneSpriteRow          ;do sub to write tile numbers to first row of sprites
           cpx #$04                      ;check incremented offset
           bne DBlkLoop                  ;and loop back until all four sprites are done
           ldx ObjectOffset              ;get block object offset
           ldy Block_SprDataOffset,x     ;get sprite data offset
           lda AreaType
           cmp #$01                      ;check for ground level type area
           beq ChkRep                    ;if found, branch to next part
           lda #$86
           sta Sprite_Tilenumber,y       ;otherwise remove brick tiles with lines
           sta Sprite_Tilenumber+4,y     ;and replace then with lineless brick tiles
ChkRep:    lda Block_Metatile,x          ;check replacement metatile
           cmp #$c4                      ;if not used block metatile, then
           bne BlkOffscr                 ;branch ahead to use current graphics
           lda #$87                      ;set A for used block tile
           iny                           ;increment Y to write to tile bytes
           jsr DumpFourSpr               ;do sub to dump into all four sprites
           dey                           ;return Y to original offset
           lda #$03                      ;set palette bits
           ldx AreaType
           dex                           ;check for ground level type area again
           beq SetBFlip                  ;if found, use current palette bits
           lsr                           ;otherwise set to $01
SetBFlip:  ldx ObjectOffset              ;put block object offset back in X
           sta Sprite_Attributes,y       ;store attribute byte as-is in first sprite
           ora #%01000000
           sta Sprite_Attributes+4,y     ;set horizontal flip bit for second sprite
           ora #%10000000
           sta Sprite_Attributes+12,y    ;set both flip bits for fourth sprite
           and #%10000011
           sta Sprite_Attributes+8,y     ;set vertical flip bit for third sprite
BlkOffscr: lda Block_OffscreenBits       ;get offscreen bits for block object
           pha                           ;save to stack
           and #%00000100                ;check to see if d2 in offscreen bits are set
           beq PullOfsB                  ;if not set, branch, otherwise move sprites offscreen
           lda #$f8                      ;move offscreen two OAMs
           sta Sprite_Y_Position+4,y     ;on the right side
           sta Sprite_Y_Position+12,y
PullOfsB:  pla                           ;pull offscreen bits from stack
ChkLeftCo: and #%00001000                ;check to see if d3 in offscreen bits are set
           beq ExDBlk                    ;if not set, branch, otherwise move sprites offscreen

MoveColOffscreen:
        lda #$f8                   ;move offscreen two OAMs
        sta Sprite_Y_Position,y    ;on the left side (or two rows of enemy on either side
        sta Sprite_Y_Position+8,y  ;if branched here from enemy graphics handler)
ExDBlk: rts

;-------------------------------------------------------------------------------------
;$00 - used to hold palette bits for attribute byte or relative X position

DrawBrickChunks:
         lda #$02                   ;set palette bits here
         sta $00
         lda #$75                   ;set tile number for ball (something residual, likely)
         ldy GameEngineSubroutine
         cpy #$05                   ;if end-of-level routine running,
         beq DChunks                ;use palette and tile number assigned
         lda #$03                   ;otherwise set different palette bits
         sta $00
         lda #$84                   ;and set tile number for brick chunks
DChunks: ldy Block_SprDataOffset,x  ;get OAM data offset
         iny                        ;increment to start with tile bytes in OAM
         jsr DumpFourSpr            ;do sub to dump tile number into all four sprites
         lda FrameCounter           ;get frame counter
         asl
         asl
         asl                        ;move low nybble to high
         asl
         and #$c0                   ;get what was originally d3-d2 of low nybble
         ora $00                    ;add palette bits
         iny                        ;increment offset for attribute bytes
         jsr DumpFourSpr            ;do sub to dump attribute data into all four sprites
         dey
         dey                        ;decrement offset to Y coordinate
         lda Block_Rel_YPos         ;get first block object's relative vertical coordinate
         jsr DumpTwoSpr             ;do sub to dump current Y coordinate into two sprites
         lda Block_Rel_XPos         ;get first block object's relative horizontal coordinate
         sta Sprite_X_Position,y    ;save into X coordinate of first sprite
         lda Block_Orig_XPos,x      ;get original horizontal coordinate
         sec
         sbc ScreenLeft_X_Pos       ;subtract coordinate of left side from original coordinate
         sta $00                    ;store result as relative horizontal coordinate of original
         sec
         sbc Block_Rel_XPos         ;get difference of relative positions of original - current
         adc $00                    ;add original relative position to result
         adc #$06                   ;plus 6 pixels to position second brick chunk correctly
         sta Sprite_X_Position+4,y  ;save into X coordinate of second sprite
         lda Block_Rel_YPos+1       ;get second block object's relative vertical coordinate
         sta Sprite_Y_Position+8,y
         sta Sprite_Y_Position+12,y ;dump into Y coordinates of third and fourth sprites
         lda Block_Rel_XPos+1       ;get second block object's relative horizontal coordinate
         sta Sprite_X_Position+8,y  ;save into X coordinate of third sprite
         lda $00                    ;use original relative horizontal position
         sec
         sbc Block_Rel_XPos+1       ;get difference of relative positions of original - current
         adc $00                    ;add original relative position to result
         adc #$06                   ;plus 6 pixels to position fourth brick chunk correctly
         sta Sprite_X_Position+12,y ;save into X coordinate of fourth sprite
         lda Block_OffscreenBits    ;get offscreen bits for block object
         jsr ChkLeftCo              ;do sub to move left half of sprites offscreen if necessary
         lda Block_OffscreenBits    ;get offscreen bits again
         asl                        ;shift d7 into carry
         bcc ChnkOfs                ;if d7 not set, branch to last part
         lda #$f8
         jsr DumpTwoSpr             ;otherwise move top sprites offscreen
ChnkOfs: lda $00                    ;if relative position on left side of screen,
         bpl ExBCDr                 ;go ahead and leave
         lda Sprite_X_Position,y    ;otherwise compare left-side X coordinate
         cmp Sprite_X_Position+4,y  ;to right-side X coordinate
         bcc ExBCDr                 ;branch to leave if less
         lda #$f8                   ;otherwise move right half of sprites offscreen
         sta Sprite_Y_Position+4,y
         sta Sprite_Y_Position+12,y
ExBCDr:  rts                        ;leave

;-------------------------------------------------------------------------------------

DrawFireball:
      ldy FBall_SprDataOffset,x  ;get fireball's sprite data offset
      lda Fireball_Rel_YPos      ;get relative vertical coordinate
      sta Sprite_Y_Position,y    ;store as sprite Y coordinate
      lda Fireball_Rel_XPos      ;get relative horizontal coordinate
      sta Sprite_X_Position,y    ;store as sprite X coordinate, then do shared code

DrawFirebar:
       lda FrameCounter         ;get frame counter
       lsr                      ;divide by four
       lsr
       pha                      ;save result to stack
       and #$01                 ;mask out all but last bit
       eor #$64                 ;set either tile $64 or $65 as fireball tile
       sta Sprite_Tilenumber,y  ;thus tile changes every four frames
       pla                      ;get from stack
       lsr                      ;divide by four again
       lsr
       lda #$02                 ;load value $02 to set palette in attrib byte
       bcc FireA                ;if last bit shifted out was not set, skip this
       ora #%11000000           ;otherwise flip both ways every eight frames
FireA: sta Sprite_Attributes,y  ;store attribute byte and leave
       rts

;-------------------------------------------------------------------------------------

ExplosionTiles:
      .byte $68, $67, $66

DrawExplosion_Fireball:
      ldy Alt_SprDataOffset,x  ;get OAM data offset of alternate sort for fireball's explosion
      lda Fireball_State,x     ;load fireball state
      inc Fireball_State,x     ;increment state for next frame
      lsr                      ;divide by 2
      and #%00000111           ;mask out all but d3-d1
      cmp #$03                 ;check to see if time to kill fireball
      bcs KillFireBall         ;branch if so, otherwise continue to draw explosion

DrawExplosion_Fireworks:
      tax                         ;use whatever's in A for offset
      lda ExplosionTiles,x        ;get tile number using offset
      iny                         ;increment Y (contains sprite data offset)
      jsr DumpFourSpr             ;and dump into tile number part of sprite data
      dey                         ;decrement Y so we have the proper offset again
      ldx ObjectOffset            ;return enemy object buffer offset to X
      lda Fireball_Rel_YPos       ;get relative vertical coordinate
      sec                         ;subtract four pixels vertically
      sbc #$04                    ;for first and third sprites
      sta Sprite_Y_Position,y
      sta Sprite_Y_Position+8,y
      clc                         ;add eight pixels vertically
      adc #$08                    ;for second and fourth sprites
      sta Sprite_Y_Position+4,y
      sta Sprite_Y_Position+12,y
      lda Fireball_Rel_XPos       ;get relative horizontal coordinate
      sec                         ;subtract four pixels horizontally
      sbc #$04                    ;for first and second sprites
      sta Sprite_X_Position,y
      sta Sprite_X_Position+4,y
      clc                         ;add eight pixels horizontally
      adc #$08                    ;for third and fourth sprites
      sta Sprite_X_Position+8,y
      sta Sprite_X_Position+12,y
      lda #$02                    ;set palette attributes for all sprites, but
      sta Sprite_Attributes,y     ;set no flip at all for first sprite
      lda #$82
      sta Sprite_Attributes+4,y   ;set vertical flip for second sprite
      lda #$42
      sta Sprite_Attributes+8,y   ;set horizontal flip for third sprite
      lda #$c2
      sta Sprite_Attributes+12,y  ;set both flips for fourth sprite
      rts                         ;we are done

KillFireBall:
      lda #$00                    ;clear fireball state to kill it
      sta Fireball_State,x
      rts

;-------------------------------------------------------------------------------------

DrawSmallPlatform:
       ldy Enemy_SprDataOffset,x   ;get OAM data offset
       lda #$5b                    ;load tile number for small platforms
       iny                         ;increment offset for tile numbers
       jsr DumpSixSpr              ;dump tile number into all six sprites
       iny                         ;increment offset for attributes
       lda #$02                    ;load palette controls
       jsr DumpSixSpr              ;dump attributes into all six sprites
       dey                         ;decrement for original offset
       dey
       lda Enemy_Rel_XPos          ;get relative horizontal coordinate
       sta Sprite_X_Position,y
       sta Sprite_X_Position+12,y  ;dump as X coordinate into first and fourth sprites
       clc
       adc #$08                    ;add eight pixels
       sta Sprite_X_Position+4,y   ;dump into second and fifth sprites
       sta Sprite_X_Position+16,y
       clc
       adc #$08                    ;add eight more pixels
       sta Sprite_X_Position+8,y   ;dump into third and sixth sprites
       sta Sprite_X_Position+20,y
       lda Enemy_Y_Position,x      ;get vertical coordinate
       tax
       pha                         ;save to stack
       cpx #$20                    ;if vertical coordinate below status bar,
       bcs TopSP                   ;do not mess with it
       lda #$f8                    ;otherwise move first three sprites offscreen
TopSP: jsr DumpThreeSpr            ;dump vertical coordinate into Y coordinates
       pla                         ;pull from stack
       clc
       adc #$80                    ;add 128 pixels
       tax
       cpx #$20                    ;if below status bar (taking wrap into account)
       bcs BotSP                   ;then do not change altered coordinate
       lda #$f8                    ;otherwise move last three sprites offscreen
BotSP: sta Sprite_Y_Position+12,y  ;dump vertical coordinate + 128 pixels
       sta Sprite_Y_Position+16,y  ;into Y coordinates
       sta Sprite_Y_Position+20,y
       lda Enemy_OffscreenBits     ;get offscreen bits
       pha                         ;save to stack
       and #%00001000              ;check d3
       beq SOfs
       lda #$f8                    ;if d3 was set, move first and
       sta Sprite_Y_Position,y     ;fourth sprites offscreen
       sta Sprite_Y_Position+12,y
SOfs:  pla                         ;move out and back into stack
       pha
       and #%00000100              ;check d2
       beq SOfs2
       lda #$f8                    ;if d2 was set, move second and
       sta Sprite_Y_Position+4,y   ;fifth sprites offscreen
       sta Sprite_Y_Position+16,y
SOfs2: pla                         ;get from stack
       and #%00000010              ;check d1
       beq ExSPl
       lda #$f8                    ;if d1 was set, move third and
       sta Sprite_Y_Position+8,y   ;sixth sprites offscreen
       sta Sprite_Y_Position+20,y
ExSPl: ldx ObjectOffset            ;get enemy object offset and leave
       rts

;-------------------------------------------------------------------------------------

DrawBubble:
        ldy Player_Y_HighPos        ;if player's vertical high position
        dey                         ;not within screen, skip all of this
        bne ExDBub
        lda Bubble_OffscreenBits    ;check air bubble's offscreen bits
        and #%00001000
        bne ExDBub                  ;if bit set, branch to leave
        ldy Bubble_SprDataOffset,x  ;get air bubble's OAM data offset
        lda Bubble_Rel_XPos         ;get relative horizontal coordinate
        sta Sprite_X_Position,y     ;store as X coordinate here
        lda Bubble_Rel_YPos         ;get relative vertical coordinate
        sta Sprite_Y_Position,y     ;store as Y coordinate here
        lda #$74
        sta Sprite_Tilenumber,y     ;put air bubble tile into OAM data
        lda #$02
        sta Sprite_Attributes,y     ;set attribute byte
ExDBub: rts                         ;leave

;-------------------------------------------------------------------------------------
;$00 - used to store player's vertical offscreen bits

PlayerGfxTblOffsets:
      .byte $20, $28, $c8, $18, $00, $40, $50, $58
      .byte $80, $88, $b8, $78, $60, $a0, $b0, $b8

;tiles arranged in order, 2 tiles per row, top to bottom

PlayerGraphicsTable:
;big player table
      .byte $00, $01, $02, $03, $04, $05, $06, $07 ;walking frame 1
      .byte $08, $09, $0a, $0b, $0c, $0d, $0e, $0f ;        frame 2
      .byte $10, $11, $12, $13, $14, $15, $16, $17 ;        frame 3
      .byte $18, $19, $1a, $1b, $1c, $1d, $1e, $1f ;skidding
      .byte $20, $21, $22, $23, $24, $25, $26, $27 ;jumping
      .byte $08, $09, $28, $29, $2a, $2b, $2c, $2d ;swimming frame 1
      .byte $08, $09, $0a, $0b, $0c, $30, $2c, $2d ;         frame 2
      .byte $08, $09, $0a, $0b, $2e, $2f, $2c, $2d ;         frame 3
      .byte $08, $09, $28, $29, $2a, $2b, $5c, $5d ;climbing frame 1
      .byte $08, $09, $0a, $0b, $0c, $0d, $5e, $5f ;         frame 2
      .byte $fc, $fc, $08, $09, $58, $59, $5a, $5a ;crouching
      .byte $08, $09, $28, $29, $2a, $2b, $0e, $0f ;fireball throwing

;small player table
      .byte $fc, $fc, $fc, $fc, $32, $33, $34, $35 ;walking frame 1
      .byte $fc, $fc, $fc, $fc, $36, $37, $38, $39 ;        frame 2
      .byte $fc, $fc, $fc, $fc, $3a, $37, $3b, $3c ;        frame 3
      .byte $fc, $fc, $fc, $fc, $3d, $3e, $3f, $40 ;skidding
      .byte $fc, $fc, $fc, $fc, $32, $41, $42, $43 ;jumping
      .byte $fc, $fc, $fc, $fc, $32, $33, $44, $45 ;swimming frame 1
      .byte $fc, $fc, $fc, $fc, $32, $33, $44, $47 ;         frame 2
      .byte $fc, $fc, $fc, $fc, $32, $33, $48, $49 ;         frame 3
      .byte $fc, $fc, $fc, $fc, $32, $33, $90, $91 ;climbing frame 1
      .byte $fc, $fc, $fc, $fc, $3a, $37, $92, $93 ;         frame 2
      .byte $fc, $fc, $fc, $fc, $9e, $9e, $9f, $9f ;killed

;used by both player sizes
      .byte $fc, $fc, $fc, $fc, $3a, $37, $4f, $4f ;small player standing
      .byte $fc, $fc, $00, $01, $4c, $4d, $4e, $4e ;intermediate grow frame
      .byte $00, $01, $4c, $4d, $4a, $4a, $4b, $4b ;big player standing

SwimKickTileNum:
      .byte $31, $46

PlayerGfxHandler:
        lda InjuryTimer             ;if player's injured invincibility timer
        beq CntPl                   ;not set, skip checkpoint and continue code
        lda FrameCounter
        lsr                         ;otherwise check frame counter and branch
        bcs ExPGH                   ;to leave on every other frame (when d0 is set)
CntPl:  lda GameEngineSubroutine    ;if executing specific game engine routine,
        cmp #$0b                    ;branch ahead to some other part
        beq PlayerKilled
        lda PlayerChangeSizeFlag    ;if grow/shrink flag set
        bne DoChangeSize            ;then branch to some other code
        ldy SwimmingFlag            ;if swimming flag set, branch to
        beq FindPlayerAction        ;different part, do not return
        lda Player_State
        cmp #$00                    ;if player status normal,
        beq FindPlayerAction        ;branch and do not return
        jsr FindPlayerAction        ;otherwise jump and return
        lda FrameCounter
        and #%00000100              ;check frame counter for d2 set (8 frames every
        bne ExPGH                   ;eighth frame), and branch if set to leave
        tax                         ;initialize X to zero
        ldy Player_SprDataOffset    ;get player sprite data offset
        lda PlayerFacingDir         ;get player's facing direction
        lsr
        bcs SwimKT                  ;if player facing to the right, use current offset
        iny
        iny                         ;otherwise move to next OAM data
        iny
        iny
SwimKT: lda PlayerSize              ;check player's size
        beq BigKTS                  ;if big, use first tile
        lda Sprite_Tilenumber+24,y  ;check tile number of seventh/eighth sprite
        cmp SwimTileRepOffset       ;against tile number in player graphics table
        beq ExPGH                   ;if spr7/spr8 tile number = value, branch to leave
        inx                         ;otherwise increment X for second tile
BigKTS: lda SwimKickTileNum,x       ;overwrite tile number in sprite 7/8
        sta Sprite_Tilenumber+24,y  ;to animate player's feet when swimming
ExPGH:  rts                         ;then leave

FindPlayerAction:
      jsr ProcessPlayerAction       ;find proper offset to graphics table by player's actions
      jmp PlayerGfxProcessing       ;draw player, then process for fireball throwing

DoChangeSize:
      jsr HandleChangeSize          ;find proper offset to graphics table for grow/shrink
      jmp PlayerGfxProcessing       ;draw player, then process for fireball throwing

PlayerKilled:
      ldy #$0e                      ;load offset for player killed
      lda PlayerGfxTblOffsets,y     ;get offset to graphics table

PlayerGfxProcessing:
       sta PlayerGfxOffset           ;store offset to graphics table here
       lda #$04
       jsr RenderPlayerSub           ;draw player based on offset loaded
       jsr ChkForPlayerAttrib        ;set horizontal flip bits as necessary
       lda FireballThrowingTimer
       beq PlayerOffscreenChk        ;if fireball throw timer not set, skip to the end
       ldy #$00                      ;set value to initialize by default
       lda PlayerAnimTimer           ;get animation frame timer
       cmp FireballThrowingTimer     ;compare to fireball throw timer
       sty FireballThrowingTimer     ;initialize fireball throw timer
       bcs PlayerOffscreenChk        ;if animation frame timer => fireball throw timer skip to end
       sta FireballThrowingTimer     ;otherwise store animation timer into fireball throw timer
       ldy #$07                      ;load offset for throwing
       lda PlayerGfxTblOffsets,y     ;get offset to graphics table
       sta PlayerGfxOffset           ;store it for use later
       ldy #$04                      ;set to update four sprite rows by default
       lda Player_X_Speed
       ora Left_Right_Buttons        ;check for horizontal speed or left/right button press
       beq SUpdR                     ;if no speed or button press, branch using set value in Y
       dey                           ;otherwise set to update only three sprite rows
SUpdR: tya                           ;save in A for use
       jsr RenderPlayerSub           ;in sub, draw player object again

PlayerOffscreenChk:
           lda Player_OffscreenBits      ;get player's offscreen bits
           lsr
           lsr                           ;move vertical bits to low nybble
           lsr
           lsr
           sta $00                       ;store here
           ldx #$03                      ;check all four rows of player sprites
           lda Player_SprDataOffset      ;get player's sprite data offset
           clc
           adc #$18                      ;add 24 bytes to start at bottom row
           tay                           ;set as offset here
PROfsLoop: lda #$f8                      ;load offscreen Y coordinate just in case
           lsr $00                       ;shift bit into carry
           bcc NPROffscr                 ;if bit not set, skip, do not move sprites
           jsr DumpTwoSpr                ;otherwise dump offscreen Y coordinate into sprite data
NPROffscr: tya
           sec                           ;subtract eight bytes to do
           sbc #$08                      ;next row up
           tay
           dex                           ;decrement row counter
           bpl PROfsLoop                 ;do this until all sprite rows are checked
           rts                           ;then we are done!

;-------------------------------------------------------------------------------------

IntermediatePlayerData:
        .byte $58, $01, $00, $60, $ff, $04

DrawPlayer_Intermediate:
          ldx #$05                       ;store data into zero page memory
PIntLoop: lda IntermediatePlayerData,x   ;load data to display player as he always
          sta $02,x                      ;appears on world/lives display
          dex
          bpl PIntLoop                   ;do this until all data is loaded
          ldx #$b8                       ;load offset for small standing
          ldy #$04                       ;load sprite data offset
          jsr DrawPlayerLoop             ;draw player accordingly
          lda Sprite_Attributes+36       ;get empty sprite attributes
          ora #%01000000                 ;set horizontal flip bit for bottom-right sprite
          sta Sprite_Attributes+32       ;store and leave
          rts

;-------------------------------------------------------------------------------------
;$00-$01 - used to hold tile numbers, $00 also used to hold upper extent of animation frames
;$02 - vertical position
;$03 - facing direction, used as horizontal flip control
;$04 - attributes
;$05 - horizontal position
;$07 - number of rows to draw
;these also used in IntermediatePlayerData

RenderPlayerSub:
        sta $07                      ;store number of rows of sprites to draw
        lda Player_Rel_XPos
        sta Player_Pos_ForScroll     ;store player's relative horizontal position
        sta $05                      ;store it here also
        lda Player_Rel_YPos
        sta $02                      ;store player's vertical position
        lda PlayerFacingDir
        sta $03                      ;store player's facing direction
        lda Player_SprAttrib
        sta $04                      ;store player's sprite attributes
        ldx PlayerGfxOffset          ;load graphics table offset
        ldy Player_SprDataOffset     ;get player's sprite data offset

DrawPlayerLoop:
        lda PlayerGraphicsTable,x    ;load player's left side
        sta $00
        lda PlayerGraphicsTable+1,x  ;now load right side
        jsr DrawOneSpriteRow
        dec $07                      ;decrement rows of sprites to draw
        bne DrawPlayerLoop           ;do this until all rows are drawn
        rts

ProcessPlayerAction:
        lda Player_State      ;get player's state
        cmp #$03
        beq ActionClimbing    ;if climbing, branch here
        cmp #$02
        beq ActionFalling     ;if falling, branch here
        cmp #$01
        bne ProcOnGroundActs  ;if not jumping, branch here
        lda SwimmingFlag
        bne ActionSwimming    ;if swimming flag set, branch elsewhere
        ldy #$06              ;load offset for crouching
        lda CrouchingFlag     ;get crouching flag
        bne NonAnimatedActs   ;if set, branch to get offset for graphics table
        ldy #$00              ;otherwise load offset for jumping
        jmp NonAnimatedActs   ;go to get offset to graphics table

ProcOnGroundActs:
        ldy #$06                   ;load offset for crouching
        lda CrouchingFlag          ;get crouching flag
        bne NonAnimatedActs        ;if set, branch to get offset for graphics table
        ldy #$02                   ;load offset for standing
        lda Player_X_Speed         ;check player's horizontal speed
        ora Left_Right_Buttons     ;and left/right controller bits
        beq NonAnimatedActs        ;if no speed or buttons pressed, use standing offset
        lda Player_XSpeedAbsolute  ;load walking/running speed
        cmp #$09
        bcc ActionWalkRun          ;if less than a certain amount, branch, too slow to skid
        lda Player_MovingDir       ;otherwise check to see if moving direction
        and PlayerFacingDir        ;and facing direction are the same
        bne ActionWalkRun          ;if moving direction = facing direction, branch, don't skid
        iny                        ;otherwise increment to skid offset ($03)

NonAnimatedActs:
        jsr GetGfxOffsetAdder      ;do a sub here to get offset adder for graphics table
        lda #$00
        sta PlayerAnimCtrl         ;initialize animation frame control
        lda PlayerGfxTblOffsets,y  ;load offset to graphics table using size as offset
        rts

ActionFalling:
        ldy #$04                  ;load offset for walking/running
        jsr GetGfxOffsetAdder     ;get offset to graphics table
        jmp GetCurrentAnimOffset  ;execute instructions for falling state

ActionWalkRun:
        ldy #$04               ;load offset for walking/running
        jsr GetGfxOffsetAdder  ;get offset to graphics table
        jmp FourFrameExtent    ;execute instructions for normal state

ActionClimbing:
        ldy #$05               ;load offset for climbing
        lda Player_Y_Speed     ;check player's vertical speed
        beq NonAnimatedActs    ;if no speed, branch, use offset as-is
        jsr GetGfxOffsetAdder  ;otherwise get offset for graphics table
        jmp ThreeFrameExtent   ;then skip ahead to more code

ActionSwimming:
        ldy #$01               ;load offset for swimming
        jsr GetGfxOffsetAdder
        lda JumpSwimTimer      ;check jump/swim timer
        ora PlayerAnimCtrl     ;and animation frame control
        bne FourFrameExtent    ;if any one of these set, branch ahead
        lda A_B_Buttons
        asl                    ;check for A button pressed
        bcs FourFrameExtent    ;branch to same place if A button pressed

GetCurrentAnimOffset:
        lda PlayerAnimCtrl         ;get animation frame control
        jmp GetOffsetFromAnimCtrl  ;jump to get proper offset to graphics table

FourFrameExtent:
        lda #$03              ;load upper extent for frame control
        jmp AnimationControl  ;jump to get offset and animate player object

ThreeFrameExtent:
        lda #$02              ;load upper extent for frame control for climbing

AnimationControl:
          sta $00                   ;store upper extent here
          jsr GetCurrentAnimOffset  ;get proper offset to graphics table
          pha                       ;save offset to stack
          lda PlayerAnimTimer       ;load animation frame timer
          bne ExAnimC               ;branch if not expired
          lda PlayerAnimTimerSet    ;get animation frame timer amount
          sta PlayerAnimTimer       ;and set timer accordingly
          lda PlayerAnimCtrl
          clc                       ;add one to animation frame control
          adc #$01
          cmp $00                   ;compare to upper extent
          bcc SetAnimC              ;if frame control + 1 < upper extent, use as next
          lda #$00                  ;otherwise initialize frame control
SetAnimC: sta PlayerAnimCtrl        ;store as new animation frame control
ExAnimC:  pla                       ;get offset to graphics table from stack and leave
          rts

GetGfxOffsetAdder:
        lda PlayerSize  ;get player's size
        beq SzOfs       ;if player big, use current offset as-is
        tya             ;for big player
        clc             ;otherwise add eight bytes to offset
        adc #$08        ;for small player
        tay
SzOfs:  rts             ;go back

ChangeSizeOffsetAdder:
        .byte $00, $01, $00, $01, $00, $01, $02, $00, $01, $02
        .byte $02, $00, $02, $00, $02, $00, $02, $00, $02, $00

HandleChangeSize:
         ldy PlayerAnimCtrl           ;get animation frame control
         lda FrameCounter
         and #%00000011               ;get frame counter and execute this code every
         bne GorSLog                  ;fourth frame, otherwise branch ahead
         iny                          ;increment frame control
         cpy #$0a                     ;check for preset upper extent
         bcc CSzNext                  ;if not there yet, skip ahead to use
         ldy #$00                     ;otherwise initialize both grow/shrink flag
         sty PlayerChangeSizeFlag     ;and animation frame control
CSzNext: sty PlayerAnimCtrl           ;store proper frame control
GorSLog: lda PlayerSize               ;get player's size
         bne ShrinkPlayer             ;if player small, skip ahead to next part
         lda ChangeSizeOffsetAdder,y  ;get offset adder based on frame control as offset
         ldy #$0f                     ;load offset for player growing

GetOffsetFromAnimCtrl:
        asl                        ;multiply animation frame control
        asl                        ;by eight to get proper amount
        asl                        ;to add to our offset
        adc PlayerGfxTblOffsets,y  ;add to offset to graphics table
        rts                        ;and return with result in A

ShrinkPlayer:
        tya                          ;add ten bytes to frame control as offset
        clc
        adc #$0a                     ;this thing apparently uses two of the swimming frames
        tax                          ;to draw the player shrinking
        ldy #$09                     ;load offset for small player swimming
        lda ChangeSizeOffsetAdder,x  ;get what would normally be offset adder
        bne ShrPlF                   ;and branch to use offset if nonzero
        ldy #$01                     ;otherwise load offset for big player swimming
ShrPlF: lda PlayerGfxTblOffsets,y    ;get offset to graphics table based on offset loaded
        rts                          ;and leave

ChkForPlayerAttrib:
           ldy Player_SprDataOffset    ;get sprite data offset
           lda GameEngineSubroutine
           cmp #$0b                    ;if executing specific game engine routine,
           beq KilledAtt               ;branch to change third and fourth row OAM attributes
           lda PlayerGfxOffset         ;get graphics table offset
           cmp #$50
           beq C_S_IGAtt               ;if crouch offset, either standing offset,
           cmp #$b8                    ;or intermediate growing offset,
           beq C_S_IGAtt               ;go ahead and execute code to change 
           cmp #$c0                    ;fourth row OAM attributes only
           beq C_S_IGAtt
           cmp #$c8
           bne ExPlyrAt                ;if none of these, branch to leave
KilledAtt: lda Sprite_Attributes+16,y
           and #%00111111              ;mask out horizontal and vertical flip bits
           sta Sprite_Attributes+16,y  ;for third row sprites and save
           lda Sprite_Attributes+20,y
           and #%00111111  
           ora #%01000000              ;set horizontal flip bit for second
           sta Sprite_Attributes+20,y  ;sprite in the third row
C_S_IGAtt: lda Sprite_Attributes+24,y
           and #%00111111              ;mask out horizontal and vertical flip bits
           sta Sprite_Attributes+24,y  ;for fourth row sprites and save
           lda Sprite_Attributes+28,y
           and #%00111111
           ora #%01000000              ;set horizontal flip bit for second
           sta Sprite_Attributes+28,y  ;sprite in the fourth row
ExPlyrAt:  rts                         ;leave

;-------------------------------------------------------------------------------------
;$00 - used in adding to get proper offset

RelativePlayerPosition:
        ldx #$00      ;set offsets for relative cooordinates
        ldy #$00      ;routine to correspond to player object
        jmp RelWOfs   ;get the coordinates

RelativeBubblePosition:
        ldy #$01                ;set for air bubble offsets
        jsr GetProperObjOffset  ;modify X to get proper air bubble offset
        ldy #$03
        jmp RelWOfs             ;get the coordinates

RelativeFireballPosition:
         ldy #$00                    ;set for fireball offsets
         jsr GetProperObjOffset      ;modify X to get proper fireball offset
         ldy #$02
RelWOfs: jsr GetObjRelativePosition  ;get the coordinates
         ldx ObjectOffset            ;return original offset
         rts                         ;leave

RelativeMiscPosition:
        ldy #$02                ;set for misc object offsets
        jsr GetProperObjOffset  ;modify X to get proper misc object offset
        ldy #$06
        jmp RelWOfs             ;get the coordinates

RelativeEnemyPosition:
        lda #$01                     ;get coordinates of enemy object 
        ldy #$01                     ;relative to the screen
        jmp VariableObjOfsRelPos

RelativeBlockPosition:
        lda #$09                     ;get coordinates of one block object
        ldy #$04                     ;relative to the screen
        jsr VariableObjOfsRelPos
        inx                          ;adjust offset for other block object if any
        inx
        lda #$09
        iny                          ;adjust other and get coordinates for other one

VariableObjOfsRelPos:
        stx $00                     ;store value to add to A here
        clc
        adc $00                     ;add A to value stored
        tax                         ;use as enemy offset
        jsr GetObjRelativePosition
        ldx ObjectOffset            ;reload old object offset and leave
        rts

GetObjRelativePosition:
        lda SprObject_Y_Position,x  ;load vertical coordinate low
        sta SprObject_Rel_YPos,y    ;store here
        lda SprObject_X_Position,x  ;load horizontal coordinate
        sec                         ;subtract left edge coordinate
        sbc ScreenLeft_X_Pos
        sta SprObject_Rel_XPos,y    ;store result here
        rts

;-------------------------------------------------------------------------------------
;$00 - used as temp variable to hold offscreen bits

GetPlayerOffscreenBits:
        ldx #$00                 ;set offsets for player-specific variables
        ldy #$00                 ;and get offscreen information about player
        jmp GetOffScreenBitsSet

GetFireballOffscreenBits:
        ldy #$00                 ;set for fireball offsets
        jsr GetProperObjOffset   ;modify X to get proper fireball offset
        ldy #$02                 ;set other offset for fireball's offscreen bits
        jmp GetOffScreenBitsSet  ;and get offscreen information about fireball

GetBubbleOffscreenBits:
        ldy #$01                 ;set for air bubble offsets
        jsr GetProperObjOffset   ;modify X to get proper air bubble offset
        ldy #$03                 ;set other offset for airbubble's offscreen bits
        jmp GetOffScreenBitsSet  ;and get offscreen information about air bubble

GetMiscOffscreenBits:
        ldy #$02                 ;set for misc object offsets
        jsr GetProperObjOffset   ;modify X to get proper misc object offset
        ldy #$06                 ;set other offset for misc object's offscreen bits
        jmp GetOffScreenBitsSet  ;and get offscreen information about misc object

ObjOffsetData:
        .byte $07, $16, $0d

GetProperObjOffset:
        txa                  ;move offset to A
        clc
        adc ObjOffsetData,y  ;add amount of bytes to offset depending on setting in Y
        tax                  ;put back in X and leave
        rts

GetEnemyOffscreenBits:
        lda #$01                 ;set A to add 1 byte in order to get enemy offset
        ldy #$01                 ;set Y to put offscreen bits in Enemy_OffscreenBits
        jmp SetOffscrBitsOffset

GetBlockOffscreenBits:
        lda #$09       ;set A to add 9 bytes in order to get block obj offset
        ldy #$04       ;set Y to put offscreen bits in Block_OffscreenBits

SetOffscrBitsOffset:
        stx $00
        clc           ;add contents of X to A to get
        adc $00       ;appropriate offset, then give back to X
        tax

GetOffScreenBitsSet:
        tya                         ;save offscreen bits offset to stack for now
        pha
        jsr RunOffscrBitsSubs
        asl                         ;move low nybble to high nybble
        asl
        asl
        asl
        ora $00                     ;mask together with previously saved low nybble
        sta $00                     ;store both here
        pla                         ;get offscreen bits offset from stack
        tay
        lda $00                     ;get value here and store elsewhere
        sta SprObject_OffscrBits,y
        ldx ObjectOffset
        rts

RunOffscrBitsSubs:
        jsr GetXOffscreenBits  ;do subroutine here
        lsr                    ;move high nybble to low
        lsr
        lsr
        lsr
        sta $00                ;store here
        jmp GetYOffscreenBits

;--------------------------------
;(these apply to these three subsections)
;$04 - used to store proper offset
;$05 - used as adder in DividePDiff
;$06 - used to store preset value used to compare to pixel difference in $07
;$07 - used to store difference between coordinates of object and screen edges

XOffscreenBitsData:
        .byte $7f, $3f, $1f, $0f, $07, $03, $01, $00
        .byte $80, $c0, $e0, $f0, $f8, $fc, $fe, $ff

DefaultXOnscreenOfs:
        .byte $07, $0f, $07

GetXOffscreenBits:
          stx $04                     ;save position in buffer to here
          ldy #$01                    ;start with right side of screen
XOfsLoop: lda ScreenEdge_X_Pos,y      ;get pixel coordinate of edge
          sec                         ;get difference between pixel coordinate of edge
          sbc SprObject_X_Position,x  ;and pixel coordinate of object position
          sta $07                     ;store here
          lda ScreenEdge_PageLoc,y    ;get page location of edge
          sbc SprObject_PageLoc,x     ;subtract from page location of object position
          ldx DefaultXOnscreenOfs,y   ;load offset value here
          cmp #$00      
          bmi XLdBData                ;if beyond right edge or in front of left edge, branch
          ldx DefaultXOnscreenOfs+1,y ;if not, load alternate offset value here
          cmp #$01      
          bpl XLdBData                ;if one page or more to the left of either edge, branch
          lda #$38                    ;if no branching, load value here and store
          sta $06
          lda #$08                    ;load some other value and execute subroutine
          jsr DividePDiff
XLdBData: lda XOffscreenBitsData,x    ;get bits here
          ldx $04                     ;reobtain position in buffer
          cmp #$00                    ;if bits not zero, branch to leave
          bne ExXOfsBS
          dey                         ;otherwise, do left side of screen now
          bpl XOfsLoop                ;branch if not already done with left side
ExXOfsBS: rts

;--------------------------------

YOffscreenBitsData:
        .byte $00, $08, $0c, $0e
        .byte $0f, $07, $03, $01
        .byte $00

DefaultYOnscreenOfs:
        .byte $04, $00, $04

HighPosUnitData:
        .byte $ff, $00

GetYOffscreenBits:
          stx $04                      ;save position in buffer to here
          ldy #$01                     ;start with top of screen
YOfsLoop: lda HighPosUnitData,y        ;load coordinate for edge of vertical unit
          sec
          sbc SprObject_Y_Position,x   ;subtract from vertical coordinate of object
          sta $07                      ;store here
          lda #$01                     ;subtract one from vertical high byte of object
          sbc SprObject_Y_HighPos,x
          ldx DefaultYOnscreenOfs,y    ;load offset value here
          cmp #$00
          bmi YLdBData                 ;if under top of the screen or beyond bottom, branch
          ldx DefaultYOnscreenOfs+1,y  ;if not, load alternate offset value here
          cmp #$01
          bpl YLdBData                 ;if one vertical unit or more above the screen, branch
          lda #$20                     ;if no branching, load value here and store
          sta $06
          lda #$04                     ;load some other value and execute subroutine
          jsr DividePDiff
YLdBData: lda YOffscreenBitsData,x     ;get offscreen data bits using offset
          ldx $04                      ;reobtain position in buffer
          cmp #$00
          bne ExYOfsBS                 ;if bits not zero, branch to leave
          dey                          ;otherwise, do bottom of the screen now
          bpl YOfsLoop
ExYOfsBS: rts

;--------------------------------

DividePDiff:
          sta $05       ;store current value in A here
          lda $07       ;get pixel difference
          cmp $06       ;compare to preset value
          bcs ExDivPD   ;if pixel difference >= preset value, branch
          lsr           ;divide by eight
          lsr
          lsr
          and #$07      ;mask out all but 3 LSB
          cpy #$01      ;right side of the screen or top?
          bcs SetOscrO  ;if so, branch, use difference / 8 as offset
          adc $05       ;if not, add value to difference / 8
SetOscrO: tax           ;use as offset
ExDivPD:  rts           ;leave

;-------------------------------------------------------------------------------------
;$00-$01 - tile numbers
;$02 - Y coordinate
;$03 - flip control
;$04 - sprite attributes
;$05 - X coordinate

DrawSpriteObject:
         lda $03                    ;get saved flip control bits
         lsr
         lsr                        ;move d1 into carry
         lda $00
         bcc NoHFlip                ;if d1 not set, branch
         sta Sprite_Tilenumber+4,y  ;store first tile into second sprite
         lda $01                    ;and second into first sprite
         sta Sprite_Tilenumber,y
         lda #$40                   ;activate horizontal flip OAM attribute
         bne SetHFAt                ;and unconditionally branch
NoHFlip: sta Sprite_Tilenumber,y    ;store first tile into first sprite
         lda $01                    ;and second into second sprite
         sta Sprite_Tilenumber+4,y
         lda #$00                   ;clear bit for horizontal flip
SetHFAt: ora $04                    ;add other OAM attributes if necessary
         sta Sprite_Attributes,y    ;store sprite attributes
         sta Sprite_Attributes+4,y
         lda $02                    ;now the y coordinates
         sta Sprite_Y_Position,y    ;note because they are
         sta Sprite_Y_Position+4,y  ;side by side, they are the same
         lda $05       
         sta Sprite_X_Position,y    ;store x coordinate, then
         clc                        ;add 8 pixels and store another to
         adc #$08                   ;put them side by side
         sta Sprite_X_Position+4,y
         lda $02                    ;add eight pixels to the next y
         clc                        ;coordinate
         adc #$08
         sta $02
         tya                        ;add eight to the offset in Y to
         clc                        ;move to the next two sprites
         adc #$08
         tay
         inx                        ;increment offset to return it to the
         inx                        ;routine that called this subroutine
         rts

