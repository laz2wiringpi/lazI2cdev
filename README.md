# lazI2cdev
lazarus I2c lib   fpopen  /dev/i2c-1 

 I2C_Read16 <BR>
 I2C_Read8<BR>
 I2C_Write16 <BR>
 I2C_Write8<BR>

TIc2Base 
----
constructor Create(ai2cadd: Cint) <BR>
property hdev: Cint read Fhdev write Fhdev;<BR>
property i2cadd: Cint read Fi2cadd;<BR>
function    connect : boolean ;<BR>
procedure   disconect ;<BR>
-------
units - class for IC's<BR>
--
<BR>
mpc4725 - DAC 12 bit<BR>
----
procedure MCP4725_Write12bit(value : longint  ; eprom : boolean = false );<BR>
<BR>
ADS1015 - ADC PGA  12 bit <BR>
----- 
property conversionDelay: integer  <BR>
property bitShift: smallint <BR> 
property samplepersecond: TADSsamplepersecond<BR>
property gain: tADSgain<BR>     

function ADSread_SingleEnded(channel: Cint): word;<BR>
function Getconfig(): word;<BR>
function configToStr(): string;<BR>
function getLastConversionResults(): word;<BR>
function  ADSreadDifferential(muxmode: TADSmuxmode): integer;<BR>
<BR>
PGA9685 
----
 
 

