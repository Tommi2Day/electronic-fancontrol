'Fan control with LED Display and remote control
'based on LM75 Temperature Sensor
'Thomas Dressler (C) oct. 2007
'free for private use according GPL2

'10LED Block
'LED10(red)    Portb.4  indicates level overflow +6(and more)
'LED9(red)     Portb.3                           +4
'LED8(red)     Portb.2                           +2
'LED7(green)   Portd.6  indicates normal value(below level)
'LED6(green)   Portd.5                           -2
'LED5(green)   Portd.4                           -4
'LED4(green)   Portd.3                           -6
'LED3(green)   Portd.2                           -8
'no LEDs above active means below Level-10 Grad
'LED2(green)   Portb.6  not used
'LED1(green)   Portb.5  blink (indicates working)

'Config section
$regfile = "2313def.dat"
$crystal = 4000000
$baud = 9600
$hwstack = 48
$swstack = 32
'$sim

'default definitions
Config Aci = Off                                            'very important, otherwise trouble with B1
Config Watchdog = 2048                                      '2s, will not work with urxc enabled
Config Timer0 = Timer , Prescale = 64                       'pwm timer
On Timer0 Timer_isr

Config Portb = Output
Config Portd = Output
Portb = 0                                                   'reset all pins
Portd = 0

'special definitions
Led Alias Portb.5                                           '1. LED
Fan Alias Portb.7                                           'Fan connector

Config Sda = Portb.0                                        'I2C-Pins for LM75
Config Scl = Portb.1


'constant settings, maybe to change
Const Basisadresse = 144                                    'lm75 base adresse
Const Readadresse = Basisadresse + 1
Const Minvalue = 64                                         'min stepvalue -1, Fan should not stop with this value
Const Stepvalue = 2
Const Maxvalue = 255 -1 -stepvalue
Const Deftemp = 35


Dim Highbyte As Byte                                        'lm75 response High
Dim Lowbyte As Byte                                         'lm75 respose Low (contains fraction)
Dim Value As Byte                                           'temperature from lm75 without fraction
Dim Nk As Bit                                               'indicates +1/2 Grad
Dim Slevel As Eram Byte                                     'Nonviolate Store Level parametern
Dim Level As Byte                                           'temperature switch value
Dim Pwm As Bit                                              'pwm phase
Dim Pwmlow As Byte                                          'pwm timer value
Dim Pwmhigh As Byte                                         'pwm timer value
Dim Key As Byte                                             'input from serial

Declare Sub Get_value()
Declare Sub Change_pwm()
Declare Sub Show_led()

'prepare start
Pwm = 0                                                     'pwm phase
Pwmhigh = 255                                               'start Fan with 100%high
Pwmlow = 0                                                  'and 0% low period
If Slevel > 50 Or Slevel < 10 Then                          'assume eeprom starts with 0xFF
   Slevel = Deftemp                                         'start with switch level 27 Grad
End If
Level = Slevel                                              'get last value for level from EEPROM

'start background processes
Enable Timer0
Enable Interrupts
Start Watchdog

'Init
Print "Fan control V1.5 Switch Level ->" ; Level ; " Grad"
I2cinit

Do
'main
   If Ischarwaiting() = 1 Then
     Key = Waitkey()
     Select Case Key                                        'remote adjust switch level
      Case 43 : Incr Level                                  '"+" key
      Case 45 : Decr Level                                  '"-" key
     End Select
     Slevel = Level                                         'store in eeprom
     Print ""
     Print "New Switch Level ->" ; Level ; " Grad"          'remote confirmation and information
   End If
   Toggle Led                                               ' blink

   Get_value                                                'get temperature
   Print Value ;                                            'remote status information
   If Nk = 1 Then
      Print ",5";
   End If
   Print " Grad";
   Change_pwm
   Print "->pwm:" ; Pwmhigh ; "::" ; Pwmlow;
   Show_led
   Reset Watchdog
   Wait 1
Loop
End                                                         'end program

'Calculate LED Block Bits
Sub Show_led()
Local W As Byte
Local B As Byte
Local I As Byte
B = 0
W = Level - 10                                              '5 led a 2 Grad in green,loop starts with level-10 grad
For I = 0 To 7                                              'only 8 Bit of 10 LED, upto Level+6Grad(red)
   If Value > W Then
      Set B.i                                               'set LED
   End If
   W = W + 2
Next I                                                      'remote status
Print " ->Led : " ; Bin(b)
Portb.4 = B.7
Portb.3 = B.6
Portb.2 = B.5
Portd.6 = B.4
Portd.5 = B.3
Portd.4 = B.2
Portd.3 = B.1
Portd.2 = B.0
End Sub

'change pwm duty
Sub Change_pwm
If Value > Level Then                                       'change high period
   If Pwmhigh < Maxvalue Then
      Pwmhigh = Pwmhigh + Stepvalue
   End If
Else
   If Pwmhigh > Minvalue Then
      Pwmhigh = Pwmhigh - Stepvalue
   End If
End If
Pwmlow = 255 - Pwmhigh                                      'recalculate low period as complement
End Sub

'read lm75
Sub Get_value()
   I2cstart
   I2cwbyte Readadresse                                     'pointer already set to register 0 (Temperature)
   I2crbyte Highbyte , Ack                                  'read 2 bytes
   I2crbyte Lowbyte , Nack
   I2cstop

   Value = Highbyte
   If Value > 127 Then                                      'negative value
      Value = Highbyte And 127                              'reset msb
      Value = 255 - Value                                   'build real value complement
   End If

   Lowbyte = Lowbyte And 128
   If Lowbyte > 0 Then                                      'add .5 Grad
       Set Nk
   Else
      Reset Nk
   End If
End Sub

'Timer interupt, alternate pwm Pin with precalculated Relation
'timer is counting upwards to 255, then raising the interrupt
'short periods needs higher preloaded values
'for this reason we have to set the opposite values for the timer to get the right phase durations
'or we have to use the LOAD command, witch causes every time a recalculation
Timer_isr:
If Pwm = 0 Then                                             'select by last phase
   Set Fan                                                  ' set Fan Pin High
   Timer0 = Pwmlow                                          'set Period Timer with the oposite value
   Pwm = 1                                                  'mark high phase
Else
   Reset Fan                                                'set Fan Pin Low
   Timer0 = Pwmhigh                                         'set Period Timer with the oposite value
   Pwm = 0                                                  'mark low phase
End If
Return