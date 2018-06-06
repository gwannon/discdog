 rem DiscDog

 playfield:
 ................................
 .X.X.X..........................
 ................................
 ................................
 ................................
 ................................
 ................................
 ................................
 ................................
 ...............................X
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end

 player0:
 %01000100
 %01000100
 %01111100
 %01111100
 %01111100
 %10000111
 %00000111
 %00000100
end

 player1:
 %01111110
 %00011000
end

 COLUPF = 176
 scorecolor = 52
 score = 0
 
 player0x = 21
 player0y = 80

 player1x = 138
 player1y = 65

 missile0x = 82
 missile0y = 79
 missile0height = 5

 dim perrosalto = a
 dim discodireccion = b
 dim discocogido = c
 dim puntos = d
 dim perrodireccion = e
 dim discovelocidad = f
 dim aleatorio = g
 dim discoaltura = h
 dim perrovidas = i
 dim cuentaatras = j
 dim discoalturasube = k
 dim discoalturabaja = l
 dim discoalturapaso = m

 puntos = 0
 discocogido = 1
 discodireccion = 1
 perrodireccion = 1
 discovelocidad = 2
 perrovidas = 3
 cuentaatras = 0
 discoaltura = (rand & 10) + 1
 discoalturasube = 18 + discoaltura
 discoalturabaja = 138 - discoaltura
 discoalturapaso = 4

mainloop

 AUDV0 = 0
 COLUP0 = 4
 COLUP1 = 132

 if perrovidas = 0 then COLUPF = 52 : gosub limpiarpantalla
 if perrovidas = 0 then gosub game
 if perrovidas = 0 then gosub over 

 if joy0left && player0x > 21 && perrovidas > 0 then gosub moverizquierda
 if joy0right && player0x < 133 && perrovidas > 0 then gosub moverderecha
 if joy0up && perrosalto = 0 && perrovidas > 0 then perrosalto = 1

 if cuentaatras = 25 then pfpixel 21 1 on : missile0height = 6 
 if cuentaatras = 50 then pfpixel 22 1 on 
 if cuentaatras = 75 then pfpixel 23 1 on : missile0height = 7
 if cuentaatras = 100 then pfpixel 24 1 on 
 if cuentaatras = 125 then pfpixel 25 1 on : missile0height = 8
 if cuentaatras = 150 then pfpixel 26 1 on 
 if cuentaatras = 175 then pfpixel 27 1 on : missile0height = 9
 if cuentaatras = 200 then pfpixel 28 1 on 
 if cuentaatras = 225 then pfpixel 29 1 on : missile0height = 10
 if cuentaatras = 250 then pfpixel 30 1 on 

 if cuentaatras = 250 then perrovidas = 0 

 if discocogido = 2 then gosub cogerdisco else gosub moverdisco

 if perrosalto = 1 then gosub saltarsubida
 if perrosalto = 2 then gosub saltarbajada

 if collision(player0,player1) then discocogido = 2

 if player0x = 21 && collision(playfield,player1) && discocogido = 2 then gosub lanzardisco2
 if player0x = 133 && collision(playfield,player1) && discocogido = 2 then gosub lanzardisco1

 if collision(player0,missile0) then AUDV0 = 15 : AUDC0 = 6 : AUDF0 = 4 : player0x = 21 : perrovidas = perrovidas - 1

 if perrovidas = 2 then pfpixel 5 1 off 
 if perrovidas = 1 then pfpixel 3 1 off
 if perrovidas = 0 then pfpixel 1 1 off

 drawscreen
 goto mainloop

lanzardisco1
 pfpixel 31 9 off
 pfpixel 0 9 on
 AUDV0 = 5 : AUDC0 = 12 : AUDF0 = 4
 discocogido = 1
 player1y = 65
 player1x = 18
 discodireccion = 2
 aleatorio = (rand & 3) + 1
 if aleatorio = 4 then discovelocidad = 4
 if aleatorio = 3 then discovelocidad = 2
 if aleatorio = 2 then discovelocidad = 1
 if aleatorio = 1 then discovelocidad = 1
 score = score + 100
 puntos = puntos + 100
 cuentaatras = cuentaatras + 1
 discoaltura = (rand & 10) + 1
 discoalturasube =  138 - (discoaltura * 4) 
 discoalturabaja = 18 + (discoaltura * 4)
 discoalturapaso = 4
 return

lanzardisco2
 pfpixel 31 9 on
 pfpixel 0 9 off
 AUDV0 = 5 : AUDC0 = 12 : AUDF0 = 4
 discocogido = 1
 player1y = 65
 player1x = 138
 discodireccion = 1
 aleatorio = (rand & 3) + 1
 if aleatorio = 4 then discovelocidad = 4
 if aleatorio = 3 then discovelocidad = 2
 if aleatorio = 2 then discovelocidad = 1
 if aleatorio = 1 then discovelocidad = 1
 score = score + 100
 puntos = puntos + 100
 cuentaatras = cuentaatras + 1
 discoaltura = (rand & 10) + 1
 discoalturabaja = 18 + (discoaltura * 4)
 discoalturasube = 138 - (discoaltura * 4)
 discoalturapaso = 4
 return

moverizquierda
 player0:
 %00100010
 %00100010
 %00111110
 %00111110
 %00111111
 %11100000
 %11100000
 %00100000
end
 perrodireccion = 2
 player0x = player0x - 1
 return

moverderecha
 player0:
 %01000100
 %01000100
 %01111100
 %01111100
 %01111100
 %10000111
 %00000111
 %00000100
end
 player0x = player0x + 1
 perrodireccion = 1	
 return

saltarsubida
 player0y = player0y - 1
 if player0y = 62 then perrosalto = 2
 if puntos >= 10 && perrosalto = 2 then score = score - 10 : puntos = puntos - 10
 return

saltarbajada
 player0y = player0y + 1
 if player0y = 80 then perrosalto = 0
 
 return

moverdisco

 if discoalturapaso > 1 then discoalturapaso = discoalturapaso - 1

 
 if discoalturapaso = 1 && player1x <= discoalturabaja && discovelocidad = 1 && discodireccion = 1 then player1y = player1y + 1 : discoalturapaso = 4
 if discoalturapaso = 1 && player1x >= discoalturasube && discovelocidad = 1 && discodireccion = 1 then player1y = player1y - 1 : discoalturapaso = 4 

 if discoalturapaso = 1 && player1x >= discoalturasube && discovelocidad = 1 && discodireccion = 2 then player1y = player1y + 1 : discoalturapaso = 4
 if discoalturapaso = 1 && player1x <= discoalturabaja && discovelocidad = 1 && discodireccion = 2 then player1y = player1y - 1: discoalturapaso = 4

 if player1x <= 138 && discodireccion = 1 then player1x = player1x - discovelocidad
 if player1x >= 18 && discodireccion = 2 then player1x = player1x + discovelocidad 
 if player1x <= 18 then discodireccion = 2 : cuentaatras = cuentaatras + 1 : player1y = 65
 if player1x >= 138 then discodireccion = 1 : cuentaatras = cuentaatras + 1 : player1y = 65


 AUDV0 = 0
 return

cogerdisco
 if perrodireccion = 1 then player1x = player0x + 6
 if perrodireccion = 2 then player1x = player0x - 6
 player1y = player0y - 5
 return

limpiarpantalla
 pfpixel 5 1 off 
 pfpixel 3 1 off
 pfpixel 1 1 off
 pfpixel 22 1 off
 pfpixel 23 1 off
 pfpixel 24 1 off
 pfpixel 25 1 off
 pfpixel 26 1 off
 pfpixel 27 1 off
 pfpixel 28 1 off
 pfpixel 29 1 off
 pfpixel 30 1 off
 pfpixel 31 1 off
 drawscreen
 return

game
 pfpixel 6 0 on	
 pfpixel 7 0 on	
 pfpixel 8 0 on	
 pfpixel 11 0 on	
 pfpixel 12 0 on	
 pfpixel 13 0 on
 pfpixel 15 0 on	
 pfpixel 19 0 on	
 pfpixel 21 0 on	
 pfpixel 22 0 on	
 pfpixel 23 0 on	
 pfpixel 24 0 on

 pfpixel 5 1 on	
 pfpixel 10 1 on	
 pfpixel 13 1 on	
 pfpixel 15 1 on	
 pfpixel 16 1 on	
 pfpixel 18 1 on	
 pfpixel 19 1 on	
 pfpixel 21 1 on

 pfpixel 5 2 on
 pfpixel 7 2 on
 pfpixel 8 2 on
 pfpixel 10 2 on	
 pfpixel 13 2 on
 pfpixel 15 2 on	
 pfpixel 17 2 on
 pfpixel 19 2 on	
 pfpixel 21 2 on
 pfpixel 22 2 on

 pfpixel 5 3 on	
 pfpixel 8 3 on	
 pfpixel 10 3 on	
 pfpixel 11 3 on	
 pfpixel 12 3 on	
 pfpixel 13 3 on	
 pfpixel 15 3 on	
 pfpixel 19 3 on
 pfpixel 21 3 on

 pfpixel 6 4 on	
 pfpixel 7 4 on
 pfpixel 10 4 on		
 pfpixel 13 4 on
 pfpixel 15 4 on
 pfpixel 19 4 on
 pfpixel 21 4 on
 pfpixel 22 4 on
 pfpixel 23 4 on
 pfpixel 24 4 on
 drawscreen
 return

over
 pfpixel 6 6 on
 pfpixel 7 6 on
 pfpixel 10 6 on
 pfpixel 14 6 on
 pfpixel 16 6 on
 pfpixel 17 6 on
 pfpixel 18 6 on
 pfpixel 19 6 on
 pfpixel 21 6 on
 pfpixel 22 6 on
 pfpixel 23 6 on
 pfpixel 24 6 on

 pfpixel 5 7 on
 pfpixel 8 7 on
 pfpixel 10 7 on
 pfpixel 14 7 on
 pfpixel 16 7 on
 pfpixel 21 7 on
 pfpixel 24 7 on

 pfpixel 5 8 on
 pfpixel 8 8 on
 pfpixel 10 8 on
 pfpixel 14 8 on
 pfpixel 16 8 on
 pfpixel 17 8 on
 pfpixel 21 8 on
 pfpixel 22 8 on
 pfpixel 23 8 on

 pfpixel 5 9 on
 pfpixel 8 9 on
 pfpixel 11 9 on
 pfpixel 13 9 on
 pfpixel 16 9 on
 pfpixel 21 9 on
 pfpixel 23 9 on

 pfpixel 4 10 off
 pfpixel 5 10 off
 pfpixel 8 10 off
 pfpixel 9 10 off
 pfpixel 10 10 off
 pfpixel 11 10 off
 pfpixel 13 10 off
 pfpixel 14 10 off
 pfpixel 15 10 off
 pfpixel 20 10 off
 pfpixel 22 10 off
 pfpixel 23 10 off
 pfpixel 25 10 off
 drawscreen	
 return

