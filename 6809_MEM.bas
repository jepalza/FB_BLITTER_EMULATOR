'******************************************
' Funciones de lectura/escritura de RAM/ROM
'*******************************************
Sub pokeb(PT As integer, PV As Integer)
	
	' ROM no escribible 
	If PT>=&hD000 Then Exit Sub 
	
	' WATCHDOG: deshabilita IRQ o no se que COÑO hace aun
   'If PT=&hC900 Then ccf=1:cci=1

	
	If (PT>&hC7ff And PT<&HCC00) Then
			'If pt=&hcbc1 Then Locate 18,1:Print "Seleccionamos banco:";Hex(PT);" ";Hex(pv);"   "
		 	'If pt=&hc800 Then Locate 19,1:Print "Seleccionamos banco:";Hex(PT);" ";Hex(pv);"   "
		 	'If pt=&hcbc0 Then Locate 20,1:Print "Seleccionamos banco:";Hex(PT);" ";Hex(pv);"   "
		 	'If pt=&hcbd0 Then Locate 21,1:Print "Seleccionamos banco:";Hex(PT);" ";Hex(pv);"   "
		 	'If pt=&hcbe0 Then Locate 22,1:Print "Seleccionamos banco:";Hex(PT);" ";Hex(pv);"   "
		 	'If pt=&hc880 Then Locate 23,1:Print "Seleccionamos banco:";Hex(PT);" ";Hex(pv);"   "
	 	   'If pt=&hc881 Then Locate 24,1:Print "Seleccionamos banco:";Hex(PT);" ";Hex(pv);"   "
	 	   'If pt=&hc882 Then Locate 25,1:Print "Seleccionamos banco:";Hex(PT);" ";Hex(pv);"   "
	 	   'If pt=&hc883 Then Locate 26,1:Print "Seleccionamos banco:";Hex(PT);" ";Hex(pv);"   "
	EndIf
	
	' ENTRE LA CBC2 Y LA CC00 aparecen los CBD0 y CBE0, que solo escriben 0 y 3 a ratos.....	
	
	' manejo del BLITTER, el chip encargado de dibujar (mas bien mover) los bloques graficos entre ROM y VRAM
	If (PT>=&HCA00 And PT<=&HCA07) Then 
		 If PT=&hCA06 Then PV=PV Xor 4
		 If PT=&hCA07 Then PV=PV Xor 4
		 Blitter_reg(PT-&hCA00)=PV
		 RAM(PT)=PV
		 If PT=&hCA00 Then Blitter()
		 Exit Sub
	EndIf



	' RAM DE VIDEO????? (paginada)
	If (PT<&HA000) Then
		VRAM(pt)=pv:Exit sub
	EndIf
	
	' RAM: los cuatro primeros chips de 2k, 8k al total (o son 6k solo, (2+2+2))
	If (PT>=&hA000 And PT<&HC000) Then
      RAM(PT)=PV 
	EndIf
	
	' ni idea
	'If (PT>=&hC000 And PT<&HC800) Then 
	'	RAM(PT)=PV
	'EndIf
   
   ' zona de puertos I/O (de la C000 a la C800 "creo" que no hay nada)
	If (PT>=&hC800 And PT<&HCC00) Then 
		RAM(PT)=PV
	EndIf
	
	' CMOS (RAM con BATERIA): 1k justo
	If (PT>=&hCC00 And PT<&HD000) Then 
		RAM(PT)=PV
	EndIf
	
	' 2k de RAM superior
	If (PT>=&hD000 And PT<&HE000) Then
      RAM(PT)=PV 
	EndIf


	'If ( PT<&Ha000 ) Then Exit Sub ' ROM no escribible 
End Sub

Function peekb(PT As integer) As Integer	
	PV = RAM(PT)

   
	Return PV
End Function




'*******************************************
' cogemos o ponemos dos bytes (word)
Sub pokew(PT As integer, PV As Integer) 
	pokeb(PT  ,(PV Shr 8) )
	pokeb(PT+1, PV And &hff)
End Sub

Function peekw(PT As integer) As Integer
	PV = peekb(PT+1) Or ( peekb(PT) Shl 8 )
	Return PV
End Function
'*******************************************




'*******************************************
' coge un byte o palabra segun el modo de direccionamiento
Function peekxb() As Integer 
	Return peekb(get_modob())
End Function

Function peekxw() As Integer 
   Return peekw(get_modow())
End Function
'*******************************************





'*******************************************
' coge un byte o palabra e incrementa PC
Function get_byte() As Integer
  PV = peekb(PC)
  PC += 1
  Return PV And &hff
end Function

function get_word() As Integer
  PV = peekw(PC)
  PC += 2
  Return PV And &hffff
end Function
'******************************************





' rutinas de obtencion de datos en 8 o 16bits
function get_rd() As Integer    
	PR = ((ra And &hff) shl 8) or (rb And &hff)
	Return PR 'And &hffff
End Function

sub set_rd(vt As Integer) 
	ra = (vt shr 8) And &hff
	rb =  vt And &hff
End Sub

Function nib5(PV As integer) As Integer
	If (PV and &h10) Then nib5 = (PV or &hffe0) Else nib5 = (PV and &h000f)
End Function

Function get_CC() As Integer
  PV=(ccc or (ccv shl 1) or (ccz shl 2) or (ccn Shl 3) _ 
                             Or(cci shl 4) or (cch shl 5) or (ccf Shl 6) or (cce Shl 7))   
  Return PV And &hff            
end function

Sub set_CC(PV As Integer)

  ccc = cogebit(PV, &h01)
  ccv = cogebit(PV, &h02)
  ccz = cogebit(PV, &h04)
  ccn = cogebit(PV, &h08)
  cci = cogebit(PV, &h10)
  cch = cogebit(PV, &h20)
  ccf = cogebit(PV, &h40)
  cce = cogebit(PV, &h80)

End Sub




' ****************************************************************
'                        rutinas PUSH y PULL 
' ****************************************************************

Sub Push(d1 As integer, d2 As integer, PR As Integer)
	
	' nota: el orden aqui es importante: no alteralo, ya que se almacena segun va (como en FIFO)
	If (PR And &h80) Then d1 -= 2 : pokew(d1, PC     ) : cicloscpu += 2
	If (PR And &h40) Then d1 -= 2 : pokew(d1, d2     ) : cicloscpu += 2 ' U o S segun sea PSHS o PSHU	
	if (PR And &h20) Then d1 -= 2 : pokew(d1, ry     ) : cicloscpu += 2
	if (PR And &h10) Then d1 -= 2 : pokew(d1, rx     ) : cicloscpu += 2
	
	if (PR And &h08) Then d1 -= 1 : pokeb(d1, rDP    ) : cicloscpu += 1	
	If (PR and &h04) Then d1 -= 1 : pokeb(d1, rb     ) : cicloscpu += 1	
	If (PR and &h02) Then d1 -= 1 : pokeb(d1, ra     ) : cicloscpu += 1	
   If (PR and &h01) Then d1 -= 1 : pokeb(d1, get_CC()): cicloscpu += 1
    
   d1temp=d1 ' devolvemos el estado final de la pila (Bien sea U o S)
   'd2temp=d2 ' y el valor de U o S segun se lo pida D2 (no se altera, por lo que lo anulo por ahora)
    
End Sub

Sub Pull(d1 As integer, d2 As integer, PR As Integer)
' en realidad, la variable D2 no sirve de nada como dato de entrada
' ya que, la leemos aqui, y la devolvemos en d2temp
' pero queda mas claro y mas real asi construido
' vamos, que queda igual a PUSH y queda mas "vistoso"

	' nota: el orden aqui es importante: no alteralo, ya que se recupera segun va
	if (PR And &h01) Then set_CC(peekb(d1)): d1 += 1 : cicloscpu += 1
	if (PR And &h02) Then ra   = peekb(d1) : d1 += 1 : cicloscpu += 1
	if (PR and &h04) Then rb   = peekb(d1) : d1 += 1 : cicloscpu += 1
	if (PR And &h08) Then rDP  = peekb(d1) : d1 += 1 : cicloscpu += 1
	
	if (PR And &h10) Then rx   = peekw(d1) : d1 += 2 : cicloscpu += 2
	if (PR And &h20) Then ry   = peekw(d1) : d1 += 2 : cicloscpu += 2
	if (PR And &h40) Then d2   = peekw(d1) : d1 += 2 : cicloscpu += 2  ' U o S segun sea PSHS o PSHU
	if (PR And &h80) Then PC   = peekw(d1) : d1 += 2 : cicloscpu += 2
	
   d1temp=d1 ' devolvemos el estado final de la pila (Bien sea U o S)
   d2temp=d2 ' y el valor de U o S segun se lo pida D2

end Sub
