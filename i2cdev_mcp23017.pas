unit i2cdev_mcp23017;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,i2cdev_base;
   {

/***************************************************
  This is a library for the MCP23017 i2c port expander

  These displays use I2C to communicate, 2 pins are required to
  interface
  Adafruit invests time and resources providing this open source code,
  please support Adafruit and open-source hardware by purchasing
  products from Adafruit!

  Written by Limor Fried/Ladyada for Adafruit Industries.
  BSD license, all text above must be included in any redistribution
 ****************************************************/



}

type

{ TMCP23017 }

TMCP23017  = class(TIc2Base)

private

  _pinmodeA : byte ;
  _pinmodeB : byte ;


public
//  Procedure  begin(uint8_t addr);
 // Procedure begin(Procedure);

     constructor Create();  override;
    constructor Create(ai2cadd: byte);  override ;

     procedure init(aMCP23017_IODIRA: byte=$FF; aMCP23017_IODIRB : byte=$FF);

  Procedure pinMode( PORTA : boolean  ; iodir : byte );
  Procedure digitalWrite(PORTA : boolean  ;  data : byte);
  Procedure pullUp(  PORTA : boolean  ;  gppu: byte);
  Function  digitalRead(PORTA : boolean  ) : byte ;

  //  read both a abd b
 // Procedure writeGPIOAB(data :word);
//  Function  readGPIOAB() : word;



 end;


Const
 MCP23017_ADDRESS = $20; // $20

// registers
 MCP23017_IODIRA = $00;
 MCP23017_IPOLA = $02;
 MCP23017_GPINTENA = $04;
 MCP23017_DEFVALA = $06;
 MCP23017_INTCONA = $08;
 MCP23017_IOCONA = $0A;
 MCP23017_GPPUA = $0C;
 MCP23017_INTFA = $0E;
 MCP23017_INTCAPA = $10;
 MCP23017_GPIOA = $12;
 MCP23017_OLATA = $14;


 MCP23017_IODIRB = $01;
 MCP23017_IPOLB = $03;
 MCP23017_GPINTENB = $05;
 MCP23017_DEFVALB = $07;
 MCP23017_INTCONB = $09;
 MCP23017_IOCONB = $0B;
 MCP23017_GPPUB = $0D;
 MCP23017_INTFB = $0F;
 MCP23017_INTCAPB = $11;
 MCP23017_GPIOB = $13;
 MCP23017_OLATB = $15;




implementation


////////////////////////////////////////////////////////////////////////////////
  constructor TMCP23017.Create();
begin
     create (MCP23017_ADDRESS);


end;



Constructor TMCP23017.Create(ai2cadd: Byte);
begin
  inherited Create(ai2cadd);



end;

     procedure   TMCP23017.init(aMCP23017_IODIRA : byte = $FF ; aMCP23017_IODIRB : byte = $FF);

  // set defaults!
     begin
 connect;


  I2C_Write8( self.hdev , MCP23017_IODIRA,aMCP23017_IODIRA) ;    // all   on port A

  I2C_Write8( self.hdev , MCP23017_IODIRB ,aMCP23017_IODIRB) ;   // all   on port B


 end;



    procedure TMCP23017.pinMode( PORTA : boolean  ;  iodir: byte);
    var
     iodiraddr : byte ;

  begin
    connect ;

   if PORTA then
   iodiraddr  := MCP23017_IODIRA

   else
     iodiraddr := MCP23017_IODIRB;
   {

  // read the current IODIR
  WIRE.beginTransmission(MCP23017_ADDRESS | i2caddr);
  wiresend(iodiraddr);
  WIRE.endTransmission();

  WIRE.requestFrom(MCP23017_ADDRESS | i2caddr, 1);
  iodir = wirerecv();

  // set the pin and direction
  if (d == INPUT) {
    iodir |= 1 << p;
  } else {
    iodir &= ~(1 << p);
  }
      }


  // write the new IODIR
  I2C_Write8( self.hdev , iodiraddr ,iodir )



  end;

    procedure TMCP23017.digitalWrite( PORTA : boolean  ;  data : byte);
      var

     gpioaddr : byte ;

  begin
    connect ;

   if PORTA then
   gpioaddr  := MCP23017_GPIOA

   else
     gpioaddr := MCP23017_GPIOB ;





  {


  // read the current GPIO output latches  // TODO
  WIRE.beginTransmission(MCP23017_ADDRESS | i2caddr);
  wiresend(olataddr);
  WIRE.endTransmission();

  WIRE.requestFrom(MCP23017_ADDRESS | i2caddr, 1);
   gpio = wirerecv();

  // set the pin and direction
  if (d == HIGH) {
    gpio |= 1 << p;
  } else {
    gpio &= ~(1 << p);
  }
   }
  // write the new GPIO
    I2C_Write8( self.hdev , gpioaddr ,data )  ;

  end;

    procedure TMCP23017.pullUp(PORTA : boolean  ;  gppu: byte);
  var

     gppuaddr : byte ;

  begin
    connect ;

   if PORTA then
   gppuaddr  := MCP23017_GPPUA

   else
     gppuaddr := MCP23017_GPPUB ;

  I2C_Write8( self.hdev , gppuaddr ,gppu )




  end;

    function TMCP23017.digitalRead(PORTA : boolean   ): byte;



    var

     gpioaddr : byte ;

  begin
    connect ;

   if PORTA then
   gpioaddr  := MCP23017_GPIOA

   else
     gpioaddr := MCP23017_GPIOB ;

 result :=  I2C_Read8 ( self.hdev , gpioaddr )

  end;
    {
    procedure TMCP23017.writeGPIOAB(data: word);
  begin
   {
       WIRE.beginTransmission(MCP23017_ADDRESS | i2caddr);
  wiresend(MCP23017_GPIOA);
  wiresend(ba & 0xFF);
  wiresend(ba >> 8);
  WIRE.endTransmission();
  }
  end;

    function TMCP23017.readGPIOAB(): word;
  begin
  {
  uint16_t ba = 0;
  uint8_t a;

  // read the current GPIO output latches
  WIRE.beginTransmission(MCP23017_ADDRESS | i2caddr);
  wiresend(MCP23017_GPIOA);
  WIRE.endTransmission();

  WIRE.requestFrom(MCP23017_ADDRESS | i2caddr, 2);
  a = wirerecv();
  ba = wirerecv();
  ba <<= 8;
  ba |= a;

  return ba;
  }
  end;
   }





end.

