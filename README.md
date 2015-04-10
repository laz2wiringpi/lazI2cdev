# lazI2cdev
lazarus I2c lib   fpopen  /dev/i2c-1 

 

 
function I2C_Read16(fh: Cint; reg: byte): word;
function I2C_Write16(fh: Cint; reg: byte; Data: word): boolean; 

overview 

TIc2Base 

constructor Create(ai2cadd: Cint) 
property hdev: Cint read Fhdev write Fhdev;
property i2cadd: Cint read Fi2cadd;
function    connect : boolean ;
procedure   disconect ;

unit - class for IC's

mpc4725 - DAC 12 bit
  procedure MCP4725_Write12bit(value : longint  ; eprom : boolean = false );
ADS1015 - ADC PGA  12 bit 
 
  property conversionDelay: integer  
  property bitShift: smallint  
  property samplepersecond: TADSsamplepersecond
  property gain: tADSgain     

  function ADSread_SingleEnded(channel: Cint): word;
  function Getconfig(): word;
  function configToStr(): string;
  function getLastConversionResults(): word;
  function  ADSreadDifferential(muxmode: TADSmuxmode): integer;

 
 

