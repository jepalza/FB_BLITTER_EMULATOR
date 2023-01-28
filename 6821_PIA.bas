    Dim Shared PIA_LOG As Integer=0 ' si es 1, se muestra un LOG
    Dim Shared nPIA As Integer ' PIA en activo
 
    Dim num As Integer=2 ' numero de PIAs a emular
    
    Dim Shared as Integer m_in_a(num)
    Dim Shared as Integer m_in_ca1(num)
    Dim Shared as Integer m_in_ca2(num)
    Dim Shared as Integer m_out_a(num)
    Dim Shared as Integer m_out_ca2(num)
    Dim Shared as Integer m_ddr_a(num)
    Dim Shared as Integer m_ctl_a(num)
    Dim Shared as Integer m_irq_a1(num)
    Dim Shared as Integer m_irq_a2(num)
    Dim Shared as Integer m_irq_a_state(num)

    Dim Shared as Integer m_in_b(num)
    Dim Shared as Integer m_in_cb1(num)
    Dim Shared as Integer m_in_cb2(num)
    Dim Shared as Integer m_out_b(num)
    Dim Shared as Integer m_out_cb2(num)
    Dim Shared as Integer m_ddr_b(num)
    Dim Shared as Integer m_ctl_b(num)
    Dim Shared as Integer m_irq_b1(num)
    Dim Shared as Integer m_irq_b2(num)
    Dim Shared as Integer m_irq_b_state(num)
    
    Dim Shared as Integer m_last_out_cb2_z(num)
    Dim Shared as Integer m_port_a_z_mask(num)


Declare Sub update_interrupts()

Declare Function m6821_Read (numPIA As Integer, Offset as Integer, Dato As integer) As Integer
Declare Sub      m6821_Write(numPIA As Integer, Offset as Integer, Dato As integer)
Declare Sub      set_a_input(Dato as Integer, z_mask as integer)

Declare Sub set_out_ca2(Dato as Integer)
Declare Sub set_out_cb2(Dato as Integer)

Declare Function get_in_a_value()  As Integer
Declare Function get_in_b_value()  As Integer
Declare Function get_out_a_value() As Integer
Declare Function get_out_b_value() As Integer

Declare Function port_a_r()    As Integer
Declare Function ddr_a_r()     As Integer
Declare Function port_b_r()    As Integer
Declare Function ddr_b_r()     As Integer
Declare Function control_a_r() As Integer
Declare Function control_b_r() As Integer

Declare Sub 	  ddr_a_w(Dato as integer)
Declare Sub 	  ddr_b_w(Dato as integer)
Declare Sub 	  port_b_w(Dato as Integer)
Declare Sub 	  port_a_w(Dato as integer)
Declare Sub 	  control_a_w(Dato as Integer)
Declare Sub 	  control_b_w(Dato as Integer)

Declare Function a_output() As Integer
Declare Function ca2_output() As Integer
Declare Function ca2_output_z() As Integer
Declare Function b_output() As Integer
Declare Function cb2_output  () As Integer
Declare Function cb2_output_z() As Integer 
    
    
#define PIA_IRQ1	(&h80)
#define PIA_IRQ2	(&h40)

'#define TRUE	(1)
'#define FALSE	(0)

#define IRQ1_ENABLED(c)			((((c) Shr 0) And &h01))
#define C1_LOW_TO_HIGH(c)		((((c) Shr 1) And &h01))
#define C1_HIGH_TO_LOW(c)		((((c) Shr 1) And &h01)=0)
#define OUTPUT_SELECTED(c)		((((c) Shr 2) And &h01))
#define IRQ2_ENABLED(c)			((((c) Shr 3) And &h01))
#define STROBE_E_RESET(c)		((((c) Shr 3) And &h01))
#define STROBE_C1_RESET(c)		((((c) Shr 3) And &h01)=0)
#define C2_SET(c)				   ((((c) Shr 3) And &h01))
#define C2_LOW_TO_HIGH(c)		((((c) Shr 4) And &h01))
#define C2_HIGH_TO_LOW(c)		((((c) Shr 4) And &h01)=0)
#define C2_SET_MODE(c)			((((c) Shr 4) And &h01))
#define C2_STROBE_MODE(c)		((((c) Shr 4) And &h01)=0)
#define C2_OUTPUT(c)			   ((((c) Shr 5) And &h01))
#define C2_INPUT(c)				((((c) Shr 5) And &h01)=0)


'-------------------------------------------------
'  update_interrupts
'-------------------------------------------------
Sub update_interrupts()

    ' start with IRQ A
	Dim new_state As Integer
	
	New_state = (m_irq_a1(nPIA) and IRQ1_ENABLED(m_ctl_a(nPIA))) Or (m_irq_a2(nPIA) And IRQ2_ENABLED(m_ctl_a(nPIA)))

	if (new_state <> m_irq_a_state(nPIA)) Then
		m_irq_a_state(nPIA) = new_state
		'm_irq_a_func(m_irq_a_state(nPIA))
	End If

	' then do IRQ B
	new_state = (m_irq_b1(nPIA) and IRQ1_ENABLED(m_ctl_b(nPIA))) Or (m_irq_b2(nPIA) And IRQ2_ENABLED(m_ctl_b(nPIA)))

	if (new_state <> m_irq_b_state(nPIA)) Then
		m_irq_b_state(nPIA) = new_state
		'm_irq_b_func(m_irq_b_state(nPIA))
	End If
	
End sub


'-------------------------------------------------
'  get_in_a_value
'-------------------------------------------------
Function get_in_a_value() As Integer

	Dim port_a_dato As Integer = 0 ' si no sirve el resto de codigo, tampoco esta linea
	Dim ret As Integer

	' update the input
	'if m_in_a(nPIA)_func=0 Then
		'port_a_dato = 255 'Int(Rnd(1)*255) 'm_in_a(nPIA)_func'(0)
	'Else
	'	if (m_in_a(nPIA)_pushed) Then
   '      port_a_dato = m_in_a(nPIA)
	'	Else
	'		' mark all pins disconnected
	'		m_port_a_z_mask(nPIA) = &hff
	'	End If
	'End If

	 ' - connected pins are always read
    ' - disconnected pins read the output buffer in output mode
    ' - disconnected pins are HI in input mode
	ret = ((255 Xor m_port_a_z_mask(nPIA)) and port_a_dato) Or _
	      ( m_port_a_z_mask(nPIA) and  m_ddr_a(nPIA) And m_out_a(nPIA)) Or _
	      ( m_port_a_z_mask(nPIA) And (255 Xor m_ddr_a(nPIA)))

	return ret
	
End Function


'-------------------------------------------------
'  get_in_b_value
'-------------------------------------------------
Function get_in_b_value() As Integer

	Dim ret As Integer
   Dim port_b_dato As Integer ' si el resto de codigo no vale, tampoco esta linea

	if (m_ddr_b(nPIA) = &hff) then
		' all output, just return buffer
		ret = m_out_b(nPIA)
	Else

		'' update the input
		'if (m_in_b(nPIA)_func=0) Then
        	'port_b_dato = 255 'Int(Rnd(1)*255) 'm_in_b(nPIA)_func'(0)
		'Else
		'	if (m_in_b(nPIA)_pushed) Then
		'		port_b_dato = m_in_b(nPIA)
		'	Else
		'		' undefined -- need to return something
		'		port_b_dato = &h00
		'	End If
		'End If

		' the DDR determines if the pin or the output buffer is read
		ret = (m_out_b(nPIA) and m_ddr_b(nPIA)) or (port_b_dato and (255 Xor m_ddr_b(nPIA)))
	End if

	return ret
End Function


'-------------------------------------------------
'  get_out_a_value
'-------------------------------------------------
Function get_out_a_value() As Integer
	Dim ret As Integer

	if (m_ddr_a(nPIA) = &hff) Then
		' all output
		ret = m_out_a(nPIA)
	Else
		' input pins don't change
		ret = (m_out_a(nPIA) and m_ddr_a(nPIA)) or (get_in_a_value() and (255 Xor m_ddr_a(nPIA)))
   End If
	return ret
End Function


'-------------------------------------------------
'  get_out_b_value
'-------------------------------------------------
Function get_out_b_value() As Integer
	' input pins are high-impedance - we just send them as zeros for backwards compatibility
	return m_out_b(nPIA) And m_ddr_b(nPIA)
End Function


'-------------------------------------------------
'  set_out_ca2
'-------------------------------------------------
Sub set_out_ca2(Dato as Integer)
	if (dato <> m_out_ca2(nPIA)) Then
		m_out_ca2(nPIA) = dato
		' send to output function
		'if (m_out_ca2(nPIA)_func=0) then
		'	m_out_ca2(nPIA)_func=0'(m_out_ca2(nPIA))
		'Else
		'	m_out_ca2(nPIA)_needs_pulled = TRUE
		'End If
	End If
End Sub


'-------------------------------------------------
'  set_out_cb2
'-------------------------------------------------
Sub set_out_cb2(Dato as Integer)

	Dim z As Integer = cb2_output_z()

	if ((dato <> m_out_cb2(nPIA)) Or (z <> m_last_out_cb2_z(nPIA))) Then
		m_out_cb2(nPIA) = dato
		m_last_out_cb2_z(nPIA) = z

		' send to output function
		'if (m_out_cb2(nPIA)_func=0) Then
		'	m_out_cb2(nPIA)_func=0'(m_out_cb2(nPIA))
		'else
		'	m_out_cb2(nPIA)_needs_pulled = TRUE
		'End If
	End If
End Sub


'-------------------------------------------------
'  port_a_r
'-------------------------------------------------
Function port_a_r() As Integer

	Dim ret As Integer = get_in_a_value()

	' IRQ flags implicitly cleared by a read
	m_irq_a1(nPIA) = FALSE
	m_irq_a2(nPIA) = FALSE
	update_interrupts()

	' CA2 is configured as output and in read strobe mode
	if(C2_OUTPUT(m_ctl_a(nPIA)) And C2_STROBE_MODE(m_ctl_a(nPIA))) Then
	
		' this will cause a transition low
		set_out_ca2(FALSE)

		' if the CA2 strobe is cleared by the E, reset it right away
		if(STROBE_E_RESET(m_ctl_a(nPIA))) Then set_out_ca2(TRUE)

	End If

	If PIA_LOG then Locate 16,1:Print "PIA ";nPIA;": port A read = "; ret

	return ret
End function


'-------------------------------------------------
'  ddr_a_r
'-------------------------------------------------
Function ddr_a_r() As Integer

	Dim ret As Integer = m_ddr_a(nPIA)

	If PIA_LOG then Locate 17,1:Print "PIA ";nPIA;": DDR A read = ";ret

	return ret
End Function


'-------------------------------------------------
'  port_b_r
'-------------------------------------------------
Function port_b_r() As Integer

	Dim ret As Integer = get_in_b_value()

	 ' This read will implicitly clear the IRQ B1 flag.  If CB2 is in write-strobe
    ' mode with CB1 restore, and a CB1 active transition set the flag,
    ' clearing it will cause CB2 to go high again.  Note that this is different
    ' from what happens with port A.
    
	if(m_irq_b1(nPIA) and C2_STROBE_MODE(m_ctl_b(nPIA)) And STROBE_C1_RESET(m_ctl_b(nPIA))) Then
   		set_out_cb2(TRUE)
	End If

	' IRQ flags implicitly cleared by a read
	m_irq_b1(nPIA) = FALSE
	m_irq_b2(nPIA) = FALSE
	update_interrupts()

	If PIA_LOG then Locate 18,1:Print "PIA ";nPIA;": port B read = ";ret

	return ret
End Function


'-------------------------------------------------
'  ddr_b_r
'-------------------------------------------------
Function ddr_b_r() As Integer

	Dim ret As Integer = m_ddr_b(nPIA)

	If PIA_LOG then Locate 19,1:Print "PIA ";nPIA;": DDR B read = ";ret

	return ret
End Function


'-------------------------------------------------
'  control_a_r
'-------------------------------------------------
Function control_a_r() As Integer

	Dim ret As Integer

	' update CA1 & CA2 if callback exists, these in turn may update IRQ's
	'if (m_in_ca1(nPIA)_func=0) Then ca1_w(m_in_ca1(nPIA)_func())
	'if (m_in_ca2(nPIA)_func=0) Then ca2_w(m_in_ca2(nPIA)_func())

	' read control register
	ret = m_ctl_a(nPIA)

	' set the IRQ flags if we have pending IRQs
	if(m_irq_a1(nPIA)) then	ret = ret Or PIA_IRQ1
	if(m_irq_a2(nPIA) And C2_INPUT(m_ctl_a(nPIA))) Then ret = ret Or PIA_IRQ2

	If PIA_LOG then Locate 20,1:Print "PIA ";nPIA;": control A read = ";ret

	return ret
End Function


'-------------------------------------------------
'  control_b_r
'-------------------------------------------------

Function control_b_r() As Integer

	Dim ret As Integer

	' update CB1 & CB2 if callback exists, these in turn may update IRQ's
	'if (m_in_cb1(nPIA)_func.isnull()=0) Then cb1_w(m_in_cb1(nPIA)_func())
	'if (m_in_cb2(nPIA)_func.isnull()=0) Then cb2_w(m_in_cb2(nPIA)_func())

	' read control register
	ret = m_ctl_b(nPIA)

	' set the IRQ flags if we have pending IRQs
	if(m_irq_b1(nPIA)) then	ret = ret Or PIA_IRQ1
	if(m_irq_b2(nPIA) And C2_INPUT(m_ctl_b(nPIA))) Then ret = ret Or PIA_IRQ2

	If PIA_LOG then Locate 21,1:Print "PIA ";nPIA;": control B read = ";ret

	return ret
End Function


'-------------------------------------------------
'  read
'-------------------------------------------------
'READ8_MEMBER( pia6821_read )
'{
'    return reg_r(offset)
'}

Function m6821_Read(numPIA As Integer, Offset as Integer, dato As integer) As Integer

	Dim ret As Integer
   nPIA=numPIA

	Select Case (offset And &h03)
		case &h00
			if (OUTPUT_SELECTED(m_ctl_a(nPIA))) then
				ret = port_a_r()
			Else
				ret = ddr_a_r()
         End If

		case &h01
			ret = control_a_r()

		case &h02
			if (OUTPUT_SELECTED(m_ctl_b(nPIA))) Then
				ret = port_b_r()
			Else
				ret = ddr_b_r()
         End If

		case &h03
			ret = control_b_r()
	End Select

	return ret
End Function


'-------------------------------------------------
'  pia6821_alt_r
'-------------------------------------------------
'READ8_MEMBER( pia6821_read_alt )
'{
'    return reg_r(((offset shl 1) & &h02) | ((offset shr 1) & &h01))
'}



'-------------------------------------------------
'  send_to_out_a_func
'-------------------------------------------------
'Sub pia6821_send_to_out_a_func(const char* message)
'{
'	' input pins are pulled high
'	Dato as integer = get_out_a_value()
'
'	Print (("PIA ";nPIA;": %s = %02X\n", tag(), message, dato))
'
'	if(!m_out_a(nPIA)_func.isnull())
'    {
'		m_out_a(nPIA)_func(0, dato)
'    }
'	else
'	{
'		if(m_out_a(nPIA)_needs_pulled)
'        {
'			Print Error("PIA ";nPIA;": Warning! No port A write handler. Previous value has been lost!\n", tag())
'        }
'
'		m_out_a(nPIA)_needs_pulled = TRUE
'	}
'End Sub


'-------------------------------------------------
'  send_to_out_b_func
'-------------------------------------------------
'Sub pia6821_send_to_out_b_func(const char* message)
'{
'	' input pins are high-impedance - we just send them as zeros for backwards compatibility
'	Dato as integer = get_out_b_value()
'
'	Print (("PIA ";nPIA;": %s = %02X\n", nPIA, message, dato))
'
'	if(!m_out_b(nPIA)_func.isnull())
'    {
'		m_out_b(nPIA)_func(0, dato)
'    }
'	else
'	{
'		if(m_out_b(nPIA)_needs_pulled)
'        {
'			Print Error("PIA ";nPIA;": Warning! No port B write handler. Previous value has been lost!\n", nPIA)
'        }
'
'		m_out_b(nPIA)_needs_pulled = TRUE
'	}
'End Sub


'-------------------------------------------------
'  port_a_w
'-------------------------------------------------
Sub port_a_w(Dato as integer)

	' buffer the output value
	m_out_a(nPIA) = dato

End Sub


'-------------------------------------------------
'  ddr_a_w
'-------------------------------------------------
Sub ddr_a_w(Dato as integer)

	If (dato = &h00) Then
		If PIA_LOG then Locate 31,1:print "PIA ";nPIA;": DDR A write (input mode)  ";dato
	End If
	If (dato = &hff) Then
		If PIA_LOG then Locate 32,1:Print "PIA ";nPIA;": DDR A write (output mode) ";dato
	End If
   If (dato <> &h00) And (dato <> &hff) Then
		If PIA_LOG then Locate 33,1:Print "PIA ";nPIA;": DDR A write (mixed mode)  ";dato
   End If

	If(m_ddr_a(nPIA) <> dato) Then
	'	' DDR changed, call the callback again
   	m_ddr_a(nPIA) = dato
	'	m_Print ged_port_a_not_connected = FALSE
	'	send_to_out_a_func("port A write due to DDR change")
	End If
End Sub


'-------------------------------------------------
'  port_b_w
'-------------------------------------------------
Sub port_b_w(Dato as Integer)

	' buffer the output value
	m_out_b(nPIA) = dato

	'send_to_out_b_func("port B write")

	' CB2 in write strobe mode
	If (C2_STROBE_MODE(m_ctl_b(nPIA))) then
		' this will cause a transition low
		set_out_cb2(FALSE)
		' if the CB2 strobe is cleared by the E, reset it right away
		if(STROBE_E_RESET(m_ctl_b(nPIA))) Then	set_out_cb2(TRUE)
	End If
End Sub


'-------------------------------------------------
'  ddr_b_w
'-------------------------------------------------
Sub ddr_b_w(Dato as integer)

	If (dato = &h00) Then
		If PIA_LOG then Locate 34,1:print "PIA ";nPIA;": DDR B write (input mode)  ";dato
	End If
	If (dato = &hff) Then
		If PIA_LOG then Locate 35,1:Print "PIA ";nPIA;": DDR B write (output mode) ";dato
	End If
   If (dato <> &h00) And (dato <> &hff) Then
		If PIA_LOG then Locate 36,1:Print "PIA ";nPIA;": DDR B write (mixed mode)  ";dato
   End If

	If(m_ddr_b(nPIA) <> dato) Then
	'	' DDR changed, call the callback again
		m_ddr_b(nPIA) = dato
	'	m_Print ged_port_b_not_connected = FALSE
	'	send_to_out_b_func("port B write due to DDR change")
	End If

End Sub


'-------------------------------------------------
'  control_a_w
'-------------------------------------------------
Sub control_a_w(Dato as Integer)
   Dim temp As Integer

	' bit 7 and 6 are read only
	dato = dato And &h3f

   If PIA_LOG then Locate 22,1:Print "------------------------"
	If PIA_LOG then Locate 23,1:Print "PIA ";nPIA;": control A write = ";dato

	' update the control register
	m_ctl_a(nPIA) = dato

	' CA2 is configured as output
	if(C2_OUTPUT(m_ctl_a(nPIA))) Then
		if(C2_SET_MODE(m_ctl_a(nPIA))) Then
			' set/reset mode - bit value determines the new output
			temp = C2_SET(m_ctl_a(nPIA))
		Else
			' strobe mode - output is always high unless strobed
			temp = TRUE
		End If
		set_out_ca2(temp)
	End If

	' update externals
	update_interrupts()
End Sub


'-------------------------------------------------
'  control_b_w
'-------------------------------------------------
Sub control_b_w(Dato as Integer)

	Dim temp As Integer

	' bit 7 and 6 are read only
	dato =dato And &h3f

	If PIA_LOG then Locate 24,1:Print "PIA ";nPIA;": control B write = ";dato

	' update the control register
	m_ctl_b(nPIA) = dato

	if (C2_SET_MODE(m_ctl_b(nPIA))) Then
		' set/reset mode - bit value determines the new output
		temp = C2_SET(m_ctl_b(nPIA))
	Else
		' strobe mode - output is always high unless strobed
		temp = TRUE
   End if

	set_out_cb2(temp)

	' update externals
	update_interrupts()
End Sub


'-------------------------------------------------
'  write
'-------------------------------------------------
'WRITE8_MEMBER( pia6821_write )
'{
'    reg_w(offset, dato)
'}

Sub m6821_Write (numPIA As Integer, Offset as Integer, Dato As integer)

   nPIA=numPIA
   
	Select Case (offset And &h03)
		case &h00
			if (OUTPUT_SELECTED(m_ctl_a(nPIA))) Then
				port_a_w(dato)
			Else
				ddr_a_w(dato)
         End If

		case &h01
			control_a_w(dato)

		case &h02
			if(OUTPUT_SELECTED(m_ctl_b(nPIA))) Then
				port_b_w(dato)
			Else
				ddr_b_w(dato)
			End If

		case &h03
			control_b_w(dato)
	End Select
	
End Sub


'-------------------------------------------------
'  write_alt
'-------------------------------------------------
'WRITE8_MEMBER( pia6821_write_alt )
'{
'    reg_w(((offset shl 1) and &h02) or ((offset shr 1) and &h01), dato)
'}


'-------------------------------------------------
'  porta_r
'-------------------------------------------------
'READ8_MEMBER( pia6821_porta_r )
'{
'	return m_in_a(nPIA)
'}


'-------------------------------------------------
'  set_a_input
'-------------------------------------------------
Sub set_a_input(Dato as Integer, z_mask as integer)

	'assert_always(m_in_a(nPIA)_func.isnull(), "pia6821_porta_w() called when in_a_func implemented")

	If PIA_LOG then locate 25,1:print "PIA ";nPIA;" set input port A = "; dato; " estoy parado en SET_A_INPUT de la PIA"
	Sleep

	m_in_a(nPIA) = dato
	m_port_a_z_mask(nPIA) = z_mask
	'm_in_a(nPIA)_pushed = TRUE
End Sub


'-------------------------------------------------
'  pia6821_porta_w
'-------------------------------------------------
'WRITE8_MEMBER( pia6821_porta_w )
'{
'	set_a_input(dato, 0)
'}


'-------------------------------------------------
'  a_output
'-------------------------------------------------
Function a_output() As Integer

	'm_out_a(nPIA)_needs_pulled = FALSE

	return get_out_a_value()
End Function


'-------------------------------------------------
'  ca1_r
'-------------------------------------------------
'READ_LINE_MEMBER( pia6821_ca1_r )
'{
'	return m_in_ca1(nPIA)
'}


'-------------------------------------------------
'  ca1_w
'-------------------------------------------------
'WRITE_LINE_MEMBER( pia6821_ca1_w )
'{
'	Print (("PIA ";nPIA;": set input CA1 = %d\n", nPIA, state))
'
'	' the new state has caused a transition
'	if((m_in_ca1(nPIA) <> state) and ((state and C1_LOW_TO_HIGH(m_ctl_a(nPIA))) Or (!state And C1_HIGH_TO_LOW(m_ctl_a(nPIA)))))
'	{
'		Print (("PIA ";nPIA;": CA1 triggering\n", nPIA))
'
'		' mark the IRQ
'		m_irq_a1(nPIA) = TRUE
'
'		' update externals
'		update_interrupts()
'
'		' CA2 is configured as output and in read strobe mode and cleared by a CA1 transition
'		if(C2_OUTPUT(m_ctl_a(nPIA)) and C2_STROBE_MODE(m_ctl_a(nPIA)) And STROBE_C1_RESET(m_ctl_a(nPIA)))
'        {
'			set_out_ca2(TRUE)
'        }
'	}
'
'	' set the new value for CA1
'	m_in_ca1(nPIA) = state
'	m_in_ca1(nPIA)_pushed = TRUE
'}


'-------------------------------------------------
'  ca2_r
'-------------------------------------------------
'READ_LINE_MEMBER( pia6821_ca2_r )
'{
'	return m_in_ca2(nPIA)
'}


'-------------------------------------------------
'  ca2_w
'-------------------------------------------------
'WRITE_LINE_MEMBER( pia6821_ca2_w )
'{
'	Print (("PIA ";nPIA;": set input CA2 = %d\n", nPIA, state))
'
'	' if input mode and the new state has caused a transition
'	if(C2_INPUT(m_ctl_a(nPIA)) and (m_in_ca2(nPIA) <> state) and ((state and C2_LOW_TO_HIGH(m_ctl_a(nPIA))) Or (!state And C2_HIGH_TO_LOW(m_ctl_a(nPIA)))))
'	{
'		Print (("PIA ";nPIA;": CA2 triggering\n", nPIA))
'
'		' mark the IRQ
'		m_irq_a2(nPIA) = TRUE
'
'		' update externals
'		update_interrupts()
'	}
'
'	' set the new value for CA2
'	m_in_ca2(nPIA) = state
'	m_in_ca2(nPIA)_pushed = TRUE
'}


'-------------------------------------------------
'  ca2_output
'-------------------------------------------------
Function ca2_output() As Integer

	'm_out_ca2(nPIA)_needs_pulled = FALSE

	return m_out_ca2(nPIA)
End Function


'-------------------------------------------------
'  ca2_output_z - version of ca2_output which
'  takes account of internal pullup resistor
'-------------------------------------------------
Function ca2_output_z() As Integer

	'm_out_ca2(nPIA)_needs_pulled = FALSE

	' If it's an output, output the bit, if it's an input, it's
	' pulled up
	return (m_out_ca2(nPIA) or C2_INPUT(m_ctl_a(nPIA)))
End Function


'-------------------------------------------------
'  portb_r
'-------------------------------------------------
'READ8_MEMBER( pia6821_portb_r )
'{
'	return m_in_b(nPIA)
'}


'-------------------------------------------------
'  portb_w
'-------------------------------------------------
'WRITE8_MEMBER( pia6821_portb_w )
'{
'	assert_always(m_in_b(nPIA)_func.isnull(), "pia_set_input_b() called when in_b_func implemented")
'
'	Print (("PIA ";nPIA;": set input port B = %02X\n", nPIA, dato))
'
'	m_in_b(nPIA) = dato
'	m_in_b(nPIA)_pushed = TRUE
'}


'-------------------------------------------------
'  b_output
'-------------------------------------------------
Function b_output() As Integer

	'm_out_b(nPIA)_needs_pulled = FALSE

	return get_out_b_value()
End Function


'-------------------------------------------------
'  cb1_r
'-------------------------------------------------
'READ_LINE_MEMBER( pia6821_cb1_r )
'{
'	return m_in_cb1(nPIA)
'}


'-------------------------------------------------
'  cb1_w
'-------------------------------------------------
'WRITE_LINE_MEMBER( pia6821_cb1_w )
'{
'	Print (("PIA ";nPIA;": set input CB1 = %d\n", nPIA, state))
'
'	' the new state has caused a transition
'	if((m_in_cb1(nPIA) <> state) and ((state and C1_LOW_TO_HIGH(m_ctl_b(nPIA))) Or (!state And C1_HIGH_TO_LOW(m_ctl_b(nPIA)))))
'	{
'		Print (("PIA ";nPIA;": CB1 triggering\n", nPIA))
'
'		' mark the IRQ
'		m_irq_b1(nPIA) = 1
'
'		' update externals
'		update_interrupts()
'
'		' If CB2 is configured as a write-strobe output which is reset by a CB1
'        ' transition, this reset will only happen when a read from port B implicitly
'        ' clears the IRQ B1 flag.  So we handle the CB2 reset there.  Note that this
'        ' is different from what happens with port A.
'	}
'
'	' set the new value for CB1
'	m_in_cb1(nPIA) = state
'	m_in_cb1(nPIA)_pushed = TRUE
'}


'-------------------------------------------------
'  cb2_r
'-------------------------------------------------
'READ_LINE_MEMBER( pia6821_cb2_r )
'{
'	return m_in_cb2(nPIA)
'}


'-------------------------------------------------
'  cb2_w
'-------------------------------------------------
'WRITE_LINE_MEMBER( pia6821_cb2_w )
'{
'	Print (("PIA ";nPIA;": set input CB2 = %d\n", nPIA, state))
'
'	' if input mode and the new state has caused a transition
'	if (C2_INPUT(m_ctl_b(nPIA)) And
'		(m_in_cb2(nPIA) <> state) And
'		((state and C2_LOW_TO_HIGH(m_ctl_b(nPIA))) or (!state And C2_HIGH_TO_LOW(m_ctl_b(nPIA)))))
'	{
'		Print "PIA ";nPIA;": CB2 triggering"
'
'		' mark the IRQ
'		m_irq_b2(nPIA) = 1
'
'		' update externals
'		update_interrupts()
'	}
'
'	' set the new value for CA2
'	m_in_cb2(nPIA) = state
'	m_in_cb2(nPIA)_pushed = TRUE
'}


'-------------------------------------------------
'  output_cb2
'-------------------------------------------------
Function cb2_output() As Integer
	'm_out_cb2(nPIA)_needs_pulled = FALSE
	return m_out_cb2(nPIA)
End Function


'-------------------------------------------------
'  cb2_output_z
'-------------------------------------------------
Function cb2_output_z() As Integer
	return C2_OUTPUT(m_ctl_b(nPIA))
End Function