unit i2cdev_lcd;

{$mode objfpc}{$H+}

interface

{
/***************************************************


The shield uses I2C to communicate, 2 pins are required to
interface
Adafruit invests time and resources providing this open source code,
please support Adafruit and open-source hardware by purchasing
products from Adafruit!

Written by Limor Fried/Ladyada for Adafruit Industries.
BSD license, all text above must be included in any redistribution
****************************************************/
}
uses
  Classes, SysUtils, i2cdev_mcp23017;

const

  // commands
  LCD_CLEARDISPLAY = $01;
  LCD_RETURNHOME = $02;
   //--------------------------------------------------------
  LCD_ENTRYMODESET = $04;

  LCD_ENTRYMODESET_INCREMENT_BIT = 1;
  LCD_ENTRYMODESET_SHIFT_BIT = 0;

  //--------------------------------------------------------
  LCD_DISPLAYCONTROL = $08;
    // flags for display on/off control  //LCD_DISPLAYCONTROL

  LCD_DISPLAYON_BIT = 2;
  LCD_CURSORON_BIT = 1;
  LCD_BLINKON_BIT = 0;

  //-------------------------------------------------------
  LCD_CURSOR_DISPLAY_SHIFT = $10;

    // flags for display/cursor shift
  LCD_DISPLAY_MOVE_BIT = 3;  // S
  LCD_MOVE_RIGHT = 2;



  // ---------------------------------------------------------
  LCD_FUNCTIONSET = $20;

  LCD_SETCGRAMADDR = $40;

  LCD_SETDDRAMADDR = $80;

  // flags for display entry mode
  //LCD_SHIFT_DIRECTION_BIT = 1;
  //LCD_ENTRYSHIFTINCREMENT_BIT = 0;







  // flags for function set
  LCD_8BITMODE = $10;

  LCD_2LINE = $08;

  LCD_5x10DOTS = $04;


type

  { TLCD }

  TLCD = class(TMCP23017)

  public

    constructor Create(); override;
    constructor Create(ai2cadd: byte); override;
    procedure initMCP();
    procedure initLCD();
    procedure cleardisplay();
    procedure SendString(cmdstr: string);

    procedure SendChar(cmdbyte: char);

       procedure setCursor(col, row: byte);


    procedure home();

    procedure noDisplay();
    procedure display();
    procedure noBlink();
    procedure blink();
    procedure noCursor();
    procedure cursor();
    procedure scrollDisplayLeft();
    procedure scrollDisplayRight();
    procedure leftToRight();
    procedure rightToLeft();
    procedure autoscroll();
    procedure noAutoscroll();


    //   Procedure createChar(uint8_t, uint8_t[]);
    //  Procedure setCursor( col , row : Byte );



  private


    _rs_pin_bit: byte; // LOW: command.  HIGH: character.
    _rw_pin_bit: byte; // LOW: write to LCD.  HIGH: read from LCD.
    _enable_pin_bit: byte;  // activated by a HIGH pulse.
   // _data_pins: array  [0..7] of byte;
  //  _button_pins: array  [0..5] of byte;
    _displayfunction: byte;
    _displaycontrol: byte;
    _displaymode: byte;

    _initialized: byte;

    _numlines: byte;
    _currline: byte;

    procedure Command(cmdbyte: byte);



  end;



implementation

uses i2cdev_base;

constructor TLCD.Create();
begin
  Create($20);
end;

{ TLCD }
 {
// When the display powers up, it is configured as follows:
//
// 1. Display clear
// 2. Function set:
//    DL = 1; 8-bit interface data
//    N = 0; 1-line display
//    F = 0; 5x8 dot character font
// 3. Display on/off control:
//    D = 0; Display off
//    C = 0; Cursor off
//    B = 0; Blinking off
// 4. Entry mode set:
//    I/D = 1; Increment by 1
//    S = 0; No shift
//
// Note, however, that resetting the Arduino doesn't reset the LCD, so we
// can't assume that its in that state when a sketch starts (and the
// RGBLCDShield constructor is called).
 }
constructor TLCD.Create(ai2cadd: byte);
begin
  inherited Create(ai2cadd);


  //_displayfunction := LCD_4BITMODE or LCD_1LINE or LCD_5x8DOTS;

  // the I/O expander pinout
  _rs_pin_bit := 7;
  _rw_pin_bit := 6;
  _enable_pin_bit := 5;


     {
  _button_pins[0] := 8;
  _button_pins[1] := 9;
  _button_pins[2] := 10;
  _button_pins[3] := 11;
  _button_pins[4] := 12;

    }

  // we can't begin() yet :(

end;

procedure TLCD.Command(cmdbyte: byte);
begin

  I2C_Write8(hdev, MCP23017_GPIOA, 32);   // all   on port B
  I2C_Write8(hdev, MCP23017_GPIOB, cmdbyte);   // all   on port B
  sleep(5);
  I2C_Write8(hdev, MCP23017_GPIOA, 0);   // all   on port B
  sleep(5);
end;

procedure TLCD.SendChar(cmdbyte: char);
begin
  connect ;




  I2C_Write8(hdev, MCP23017_GPIOA, 160);   // all   on port B
  I2C_Write8(hdev, MCP23017_GPIOB, Ord(cmdbyte));   // all   on port B

  I2C_Write8(hdev, MCP23017_GPIOA, 128);   // all   on port B
  sleep(2);
end;

procedure TLCD.home();
begin
  Command(LCD_RETURNHOME);  // set cursor position to zero
  delayMicroseconds(2000);  // this command takes a long time!
end;

procedure TLCD.noDisplay();
begin
  BitOff_8(_displaycontrol, LCD_DISPLAYON_bit);

  Command(LCD_DISPLAYCONTROL or _displaycontrol);

end;

procedure TLCD.display();
begin
  BitON_8(_displaycontrol, LCD_DISPLAYON_bit);

  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;

procedure TLCD.noBlink();
begin
  BitOFF_8(_displaycontrol, LCD_BLINKON_BIT);

  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;

procedure TLCD.blink();
begin
  BitON_8(_displaycontrol, LCD_BLINKON_BIT);

  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;

procedure TLCD.noCursor();
begin
  BitOFF_8(_displaycontrol, LCD_CURSORON_BIT);

  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;

procedure TLCD.cursor();
begin
  BitON_8(_displaycontrol, LCD_CURSORON_BIT);

  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;
    //will not work ?
procedure TLCD.scrollDisplayLeft();
var
  _displayshift : byte;

begin
 BitOn_8(_displayshift,LCD_DISPLAY_MOVE_BIT) ;
// These commands scroll the display without changing the RAM

 command(LCD_CURSOR_DISPLAY_SHIFT OR _displayshift   );     //will not work ?

end;
   //will not work ?
procedure TLCD.scrollDisplayRight();

  var
  _displayshift : byte;

begin
 BitOn_8(_displayshift,LCD_DISPLAY_MOVE_BIT) ;
  BitOn_8(_displayshift,LCD_MOVE_RIGHT) ;

    command(LCD_CURSOR_DISPLAY_SHIFT OR _displayshift );     //will not work ?
end;

procedure TLCD.leftToRight();
begin
    // This is for text that flows Left to Right

   biton_8( _displaymode,LCD_ENTRYMODESET );
    command(LCD_ENTRYMODESET OR _displaymode);
end;

procedure TLCD.rightToLeft();
begin
  // This is for text that flows Right to Left

 bitoff_8( _displaymode,LCD_ENTRYMODESET);
  command(LCD_ENTRYMODESET OR _displaymode);


end;

procedure TLCD.autoscroll();
begin
 // This will 'right justify' text from the cursor
//  biton_8( _displaymode,LCD_ENTRYSHIFTINCREMENT_BIT );
  command(LCD_ENTRYMODESET OR _displaymode);




end;

procedure TLCD.noAutoscroll();
begin
 // This will 'left justify' text from the cursor

  // bitoff_8( _displaymode,LCD_ENTRYSHIFTINCREMENT_BIT );
  command(LCD_ENTRYMODESET OR _displaymode);
end;


 procedure  TLCD.setCursor(  col,   row : byte );
 var
 data : Integer ;
begin
// int row_offsets[] = { 0x00, 0x40, 0x14, 0x54 };
 //if ( row > _numlines ) {
 //  row = _numlines-1;    // we count rows starting w/0
  if row  > 0 then
   data := col  +  40
   else
     data := col;




 command(LCD_SETDDRAMADDR OR data);

 end;

   {
// Allows us to fill the first 8 CGRAM locations
// with custom characters
void Adafruit_RGBLCDShield::createChar(uint8_t location, uint8_t charmap[])
 location &= 0x7; // we only have 8 locations 0-7
 command(LCD_SETCGRAMADDR | (location << 3));
 for (int i=0; i<8; i++) {
   write(charmap[i]);

 command(LCD_SETDDRAMADDR);  // unfortunately resets the location to 0,0
 } }
procedure TLCD.SendString(cmdstr: string);
var
  cnt: integer;

begin
  if length(cmdstr) > 0 then
    for cnt := 0 to length(cmdstr) - 1 do
      SendChar(cmdstr[cnt]);

end;

procedure TLCD.initLCD();

begin
  // set up the pins

  connect;

  _displayfunction := LCD_FUNCTIONSET OR
  LCD_8BITMODE or
  LCD_2LINE
  //or LCD_5x10DOTS  // only one line
  ;
  BitOn_8(_displaycontrol, LCD_BLINKON_BIT);
  BitOn_8(_displaycontrol, LCD_CURSORON_BIT);
  BitOn_8(_displaycontrol, LCD_DISPLAYON_BIT);

 // BitOn_8 (  _displaymode    ,    );



  Command(_displayfunction);
  Command(LCD_DISPLAYCONTROL or _displaycontrol);
  Command(LCD_ENTRYMODESET or _displaymode );
  cleardisplay();


 // SendString('THIS IS TEST!');
  disconect;

end;

procedure TLCD.initMCP();
begin
  // set up the pins
  connect;

  // PORT A CNTRL AND SITCHES

  I2C_Write8(hdev, MCP23017_IODIRA, $1F);    // all OUTPUT   on port A
  I2C_Write8(hdev, MCP23017_GPPUA, $1F); // ,$1F) ;   //  -1 5

  // PORT A DATA
  I2C_Write8(hdev, MCP23017_IODIRB, 0);   // all   on port B

  // UTRN ON PULLUP for inputs



  InitLcd();

end;

procedure TLCD.cleardisplay();

begin

  Command(LCD_CLEARDISPLAY);
  delayMicroseconds(2000);

end;


{







void Adafruit_RGBLCDShield::pulseEnable(void) {
 _digitalWrite(_enable_pin, LOW);
 delayMicroseconds(1);
 _digitalWrite(_enable_pin, HIGH);
 delayMicroseconds(1);    // enable pulse must be >450ns
 _digitalWrite(_enable_pin, LOW);
 delayMicroseconds(100);   // commands need > 37us to settle
}

}

end.

