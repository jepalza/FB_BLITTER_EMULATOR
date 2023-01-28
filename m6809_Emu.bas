' emulador de 6809 , por Joseba Epalza
' jepalza@gmail.com, www.ingepal.es/com, "Sin copirrites"-2011

' solo es cambiar el nombre de la ROM a cargar
' modificar el modulo 6809_MEM en PEEKB y POKEB las propiedades de las direcciones o puertos
' y en este mismo modulo, abajo del todo, cambiar la emulacion de teclas, joystick y pantalla

' modo de pantalla 800x600
Screen 19 ' 800x600

' necesario para el MULTIKEY
' ademas, si usamos compilacion FB, se necesita el "USING FB"
#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB 
#EndIf

' necesarios para el "OpenDialog" y selecionar un fichero
#Define WIN_INCLUDEALL
#include once "windows.bi"

' la rutina de "OpenDialog" en si misma
function getname( byval hWnd as HWND ) as string

        dim inputname as OPENFILENAME
        dim filename as zstring * MAX_PATH+1
        
        with inputname
                .lStructSize         = sizeof( OPENFILENAME )
                .hwndOwner           = hWnd
                .hInstance           = GetModuleHandle( NULL )
                .lpstrFilter         = strptr( !"Cintas *.K7\0*.K7\0Cartuchos *.Mo5\0*.Mo5" )
                .lpstrCustomFilter   = NULL
                .nMaxCustFilter      = 0
                .nFilterIndex        = 1
                .lpstrFile           = @filename
                .nMaxFile            = sizeof( filename )
                .lpstrFileTitle      = NULL
                .nMaxFileTitle       = 0
                .lpstrInitialDir     = NULL
                .lpstrTitle          = @"Abrir cintas K7 o Cartuchos Mo5"
                .Flags               = OFN_EXPLORER or OFN_FILEMUSTEXIST or OFN_PATHMUSTEXIST
                .nFileOffset         = 0
                .nFileExtension      = 0
                .lpstrDefExt         = NULL
                .lCustData           = 0
                .lpfnHook            = NULL
                .lpTemplateName      = NULL
        end with
        
        if( GetOpenFileName( @inputname ) = FALSE ) then
                return ""
        else
                return filename
        end If
end Function
Dim NombreK7 As String
' ------------------------------------------------------------
Declare Sub LeeROM(nombre As String, zona As Integer)
Declare Sub pantalla()
Declare Sub Blitter_pixel(direccion As Integer, valor As Integer, dato As Integer, mascara As Integer, solido As integer)
Declare Sub Blitter()	


' ******************************************************************
' llamada a modulo desensamblador, borrar una vez depurado
  Declare Sub M6809_DIS(direccion As integer, longitud As Integer)
' ******************************************************************
Dim shared slee As Integer=0
' control de ciclos y tiempos para emulacion real
Dim Shared ciclos_ejecutados As Integer=0 ' almacena los ciclos ejecutados en EXECUTE
Dim Shared tiempo As Double

' variables  para lectura de ficheros, como la ROM
Dim linea As String*32
Dim ini As Integer ' inicio de la rom, para su lectura secuencial
Dim inirom As Integer
Dim contador As Integer
Dim Shared blitter_accesos As Integer=0 ' mantiene un registro de la ultima mascara empleada en el BLITTER

' ******************************
' *** nombre de ROM a emular ***
' ******************************
Dim nombreROM As String="TODO.ROM" '"R35U52.CPU"
' ******************************



		' definicion de espacios RAM y ROM
		Dim Shared RAM  (&h10000)   As Integer ' RAM general: en realidad, el espacio donde se trabaja
		'Dim Shared BRAM (5,&h800)  As Integer ' 5 bancos de RAM de 2k cada uno, para la CPU (10k)
	   'Dim Shared QRAM (1,1)      As Integer ' necesitamos guardar el estado de los bancos de RAM
		'Dim Shared CRAM (&h400)    As Integer ' RAM de CMOS de 1k para la configuracion
		Dim Shared BROM (2,&h10000) As Integer ' Memorias ROM
		                                      
		Dim Shared VRAM (&ha000)   As Integer ' RAM de video, 32k pixels, 16k colores?????

		' Borramos la RAM a ceros (solo por precaucion)
		For ini=0 To &hFFFF: ram(ini)=&hff:Next
		For ini=0 To &h9FFF:vram(ini)=&hff:Next



' ****************************************************************************************
' *********************   exclusivo para la emulacion de ...........  ********************
' ****************************************************************************************

' variables temporales 
Dim AA As Integer
Dim BB As Integer
Dim CC As Integer
Dim DD As String
Dim EE As Integer
Dim FF As Integer
Dim XX As Integer
Dim YY As Integer
Dim SS As String

Declare Sub BancoROM(nbanco As Integer,zonaram As Integer)
Declare Sub BancoRAM(nbanco As Integer,zonaram As integer)

Dim Shared Blitter_reg(&h10) As integer ' almacen de bytes para el emulador de Blitter


Dim Shared PtT As Integer=20000 ' PRUEBAS SOLO
Dim Shared Accel As Integer=0 ' PRUEBAS acelerador
Dim Shared Steer As Integer=0 ' PRUEBAS manillar
Dim Shared BancoROMG As Integer=0 ' banco actual de ROM grafica
Dim SHARED YA1 As Integer =0 ' PRUEBAS

' ****************************************************************************************

' Incluimos en este punto el emulador de 6809
' debe ir aqui, para que reconozca las variables anteriores
#Include "6809_CPU.bas" ' --> este a su vez, incluye TODOS los modulos del 6809
#Include "6809_DIS.bas" ' incluimos el modulo desensamblador, quitar en la version final

Palette 0,0,0,0
Palette 1,255,255,255
Palette 2,255,0,0
Palette 3,187,0,0
Palette 4,0,255,0
Palette 5,255,0,255
Palette 6,0,0,255
Palette 7,0,0,170
Palette 8,150,150,170
Palette 9,0,190,255
Palette 10,150,70,0
Palette 11,0,70,170
Palette 12,255,255,0
Palette 13,35,70,85
Palette 14,70,35,0
Palette 15,255,150,85

' leemos las ROMS de la CPU en su banco correspondiente
	LeeROM("BLITTEST.BIN",1) ' 8=E000-FFFF:8k : principal (inicio)
	
' 8k : graficos de textos	
	LeeROM("roms/R25U46.ROM",&h2)  

' PIF: Processor Interface Board
' anulado, hasta saber como va: de momento, lleva su propia CPU
	'LeeROM("roms/R26U3.ROM ",49)

' SND: Sound Board
' anulado, hasta saber como va: de momento, lleva su propia CPU
	'LeeROM("roms/R27U11.ROM",50)

' ponemos el banco principal, el 5, en su sitio
' y de paso, los que creo que van paginados
	'BancoROM(3,&h0000)
	'BancoROM(5,&h4000)
	'BancoROM(7,&h8000)
	BancoROM(1,&h0000)

' inicio de la emulacion
	m6809_reset()

' bucle infinito de ejecuciones: solo sale con "ESC"
While 1 
  tiempo=Timer()
  
  ' ejecutamos el M6809, una instruccion cada vez y sumamos los ciclos empleados
  ciclos_ejecutados += m6809_execute() 
     
   ' se ejecutan acciones HARDWARE cada 'x' ciclos
   'If (ciclos_ejecutados > 20000) Then 
   	ciclos_ejecutados=0
   	
   	' teclas de FIN: OJO QUE SALE SIN PEDIR CONFIRMACION  
      If MultiKey(SC_ESCAPE) Then End             
   	
   	' Comprobamos interrupciones solo si el estado de CC lo permite
   	m6809_irq()  ' IRQ
   	m6809_firq() ' FIRQ
   	'm6809_NMI() ' NMI no enmascarable
		 
 
   'End If

  
' repetimos todo el proceso otra vez
Wend
' aqui solo llegamos al pulsar "ESC", o fin de emulacion


' intercambios de bancos: lo que hago es copiar el solicitado dentro del general
' se podria hacer mas rápido seleccionandolo sin mas, pero la emulacion de la RAM queda peor
Sub BancoROM(nbanco As Integer,zonaram As integer)
	
	Var f=0
	Var tambanco=65536 ' tamaño del banco a trasferir
	
	'If nbanco>3 Then tambanco=8192 ' si son los dos ultimos, son de 8k, en lugar de 16k
	
	For f=0 To tambanco-1
		RAM(f+zonaram)=BROM(nbanco,f)
	Next
	
End Sub

' en el caso del banqueo de RAM la cosa es mas lenta aun, ya que primero
' debemos dejar en su sitio la original, antes de mover nada....
Sub BancoRAM(nbanco As Integer,zonaram As integer)
	
	'Var f=0

	'For f=0 To &h2000-1
	'	RAM(f+zonaram)=BRAM(nbanco,f)
	'Next
	
End Sub

' su nombre lo dice: lee un archivo de ROM en su banco correspondiente
Sub LeeROM(nombre As String,zona As Integer)
   ' leemos la BIOS (ROM) de un Monitor de 6809 (que incluye FORTH en 8000 y BASIC en B800)
	Var ini=1 ' posiciones en el fichero a leer
	Var rom=0 ' posiciones en ROM
	Var contador=0 ' contador de la linea leida del fichero
	Var linea="                " ' leemeos 16 caracteres de golpe
	
	Open nombre For Binary Access read As 1
	Var inirom=Lof(1)
	While Not Eof(1)
		Get #1,ini,linea
		For contador=1 To Len(linea)
			BROM(zona,rom)=Asc(Mid(linea,contador,1))
			rom+=1
		Next
		ini+=len(linea)
	Wend
	Close 1
	
End Sub

Sub Blitter_pixel(direccion As Integer, valor As Integer, dato As Integer, mascara As Integer, solido As integer)
   Dim pixel As Integer
   If direccion<&ha000 Then Pixel=VRAM(direccion) Else Pixel=0 'RAM(direccion)
	If dato And &h8 Then
		If (valor And &hf0)=0 Then mascara=mascara Or &hf0
		If (valor And &h0f)=0 Then mascara=mascara Or &h0f
	EndIf
	pixel=pixel And mascara
	If (dato And &h10) Then
		pixel=pixel Or (solido And (255 Xor mascara))
	Else
		pixel=pixel Or (valor  And (255 Xor mascara))
	EndIf
	If direccion < &ha000 Then vram(direccion)=pixel
End Sub


Sub Blitter ()
	' manejo del BLITTER, el chip encargado de dibujar (mas bien mover) los bloques graficos entre ROM y VRAM

		 Dim dato    As Integer=blitter_reg(0)
		 Dim mascara As Integer=blitter_reg(1)
		 Dim origen2  As Integer=blitter_reg(2)*256+blitter_reg(3)
		 Dim destino2 As Integer=blitter_reg(4)*256+blitter_reg(5)
		 Dim anchoB  As Integer=blitter_reg(6)
		 Dim altoB   As Integer=blitter_reg(7)
		 	
		  Dim FF As Integer
		  Dim DD As Integer	
		  
		  Dim sxadv As integer
		  Dim syadv As Integer
		  Dim dxadv As Integer
		  Dim dyadv As Integer	
		  
		  Dim origen  As Integer=origen2
		  Dim destino As Integer=destino2
		  	  
		  Dim mask As Integer=0
		 
		 		 	If blitter_reg(4)=&h66 And blitter_reg(5)=&h66 Then pantalla:Sleep:end
		 
		If (dato And 1) Then sxadv=256:syadv=1 Else sxadv=1:syadv=anchob
		If (dato And 2) Then dxadv=256:dyadv=1 Else dxadv=1:dyadv=anchob

		If (dato And &h80) Then mask=mask Or &hf0
		If (dato And &h40) Then mask=mask Or &h0f
		If mask=&hff Then Exit Sub ' no se porque, pero debe salir...
		
      If (dato And &h20)=0 Then
      ' caso normal, sin rotaciones
		 	For ff=0 To altob-1
		 		origen = origen2 And &hffff
            destino=destino2 And &hffff
		    	For dd=anchob-1 To 0 Step -1	
		         Blitter_pixel(destino,ram(origen),dato,mask,mascara)
		         blitter_accesos+=2
		         origen = (origen + sxadv) And &hffff
               destino=(destino + dxadv) And &hffff
		    	Next
		    	origen2+=syadv
		    	If (dato And 2) Then 
		    		destino2=(destino2 And &hff00) Or ((destino2 + dyadv) And &hff)
		    	Else
		    		destino2+=dyadv
		    	EndIf
		 	Next
		 	
      Else
      ' caso con rotaciones
      	Dim pix As Integer
      	mask=((mask And &hf0) Shr 4) Or ((mask and &h0f) Shl 4)
      	mascara=((mascara and &hf0) Shr 4) Or ((mascara and &h0f) Shl 4)
      	
		 	For ff=0 To altob-1
		 		
            origen = origen2 And &hffff
            destino=destino2 And &hffff
            
            pix=ram(origen)
		 		Blitter_pixel(destino,(pix Shr 4) And &h0f,dato,mask Or &hf0,mascara)
            blitter_accesos+=2
            
            origen = (origen + sxadv) And &hffff
            destino=(destino + dxadv) And &hffff
            
		    	For dd=anchob-2 To 0 Step -1
		         pix=(pix Shl 8) Or ram(origen)
		         Blitter_pixel(destino,(pix Shr 4) And &hff,dato,mask,mascara)
		         blitter_accesos+=2
		         origen = (origen + sxadv) And &hffff
               destino=(destino + dxadv) And &hffff
		    	Next
		    	
		    	Blitter_pixel(destino2,(pix Shl 4) And &hf0,dato,mask Or &h0f,mascara)
		    	blitter_accesos+=1
		    	origen2+=syadv
		    	
		    	If (dato And 2) Then 
		    		destino2=(destino2 And &hff00) Or ((destino2 + dyadv) and &hff)
		    	Else
		    		destino2+=dyadv
		    	EndIf
		 	Next
      End If
		 	
		 	
		 	
		 	'If blitter_reg(4)=&h01 And blitter_reg(5)=&h00 Then pantalla:slee+=1
		 	''If blitter_reg(4)=&h11 And blitter_reg(5)=&h1a Then pantalla:sleep
		 	'For f As Integer=0 To 7
		 	'	Locate 25+f,80
		 	'	Print Hex(&hca00+F),Hex(blitter_reg(f));"  "
		 	'	'Print #3,Right("00"+Hex(blitter_reg(f)),2);" ";
		 	'Next
		 	''Print #3,""
		 	'Locate 36,75
		 	''pantalla
		   'Print "o:";Hex(origen),"d:";Hex(destino);"   ";  
		 	''Sleep
		 	'If slee>1 Then pantalla:Sleep

End Sub
