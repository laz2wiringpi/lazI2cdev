unit i2cdev_lcd;
{$MODE objfpc}{$H+}

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
  // --------------------------------------------------------
  LCD_ENTRYMODESET = $04;

  LCD_ENTRYMODESET_INCREMENT_BIT = 1;
  LCD_ENTRYMODESET_SHIFT_BIT = 0;

  // --------------------------------------------------------
  LCD_DISPLAYCONTROL = $08;
  // flags for display on/off control  //LCD_DISPLAYCONTROL

  LCD_DISPLAYON_BIT = 2;
  LCD_CURSORON_BIT = 1;
  LCD_BLINKON_BIT = 0;

  // -------------------------------------------------------
  LCD_CURSOR_DISPLAY_SHIFT = $10;

  // flags for display/cursor shift
  LCD_DISPLAY_MOVE_BIT = 3; // S
  LCD_MOVE_RIGHT = 2;

  // ---------------------------------------------------------
  LCD_FUNCTIONSET = $20;

  LCD_SETCGRAMADDR = $40;

  LCD_SETDDRAMADDR = $80;

  // flags for display entry mode
  // LCD_SHIFT_DIRECTION_BIT = 1;
  // LCD_ENTRYSHIFTINCREMENT_BIT = 0;

  // flags for function set
  LCD_8BITMODE = $10;

  LCD_2LINE = $08;

  LCD_5x10DOTS = $04;

type

  TLcdbytechar = array [1..8] of byte;

  { TLCD }


type

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
    procedure createChar(location: byte; charmap: TLcdbytechar);
    function ReadChar(): char;

    // Procedure createChar(uint8_t, uint8_t[]);
    // Procedure setCursor( col , row : Byte );

    procedure ReadcustomChar(location: byte; var charmap: TLcdbytechar);

  private

    _rs_pin_bit: byte; // LOW: command.  HIGH: character.
    _rw_pin_bit: byte; // LOW: write to LCD.  HIGH: read from LCD.
    _enable_pin_bit: byte; // activated by a HIGH pulse.
    // _data_pins: array  [0..7] of byte;
    // _button_pins: array  [0..5] of byte;

    _DisplayFunction: byte;
    _DisplayControl: byte;
    _DisplayMode: byte;
    _Entrymode : byte;
    _DisplayMoveShift : byte;

  //  _initialized: byte;

  //  _numlines: byte;
  //  _currline: byte;

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


  // _displayfunction := LCD_4BITMODE or LCD_1LINE or LCD_5x8DOTS;

  // the I/O expander pinout
  _rs_pin_bit := 7;
  _rw_pin_bit := 6;
  _enable_pin_bit := 5;



  _DisplayFunction := 0;
  _DisplayFunction  :=    LCD_8BITMODE or LCD_2LINE
  // or LCD_5x10DOTS  // only one line
    ;
  _DisplayControl := 0;

  BitON_8(_DisplayControl , LCD_BLINKON_BIT);
  BitON_8(_DisplayControl, LCD_CURSORON_BIT);
  BitON_8(_DisplayControl, LCD_DISPLAYON_BIT);

   _Entrymode := 0;

   BitON_8(_Entrymode, LCD_ENTRYMODESET_INCREMENT_BIT );
  // BitON_8(_Entrymode, LCD_ENTRYMODESET_SHIFT_BIT  );


  _DisplayMoveShift := 0;
  //   BitON_8(_DisplayMoveShift, LCD_DISPLAY_MOVE_BIT   );
   //   BitON_8(_DisplayMoveShift, LCD_MOVE_RIGHT   );
end;

procedure TLCD.Command(cmdbyte: byte);
var
 controlbyte : byte   ;

begin
  connect;
      pinMode(false, 0 );  // ALL WRITE
          controlbyte := 0;
  BitOFF_8 ( controlbyte,_rs_pin_bit);
  BitOFF_8 ( controlbyte,_rw_pin_bit   );
  BitOn_8 ( controlbyte,_enable_pin_bit );


  I2C_Write8(hdev, MCP23017_GPIOA, controlbyte); // all   on port B
  I2C_Write8(hdev, MCP23017_GPIOB, cmdbyte); // all   on port B
 // sleep(5);

     BitOff_8 ( controlbyte,_enable_pin_bit );
  I2C_Write8(hdev, MCP23017_GPIOA, controlbyte); // all   on port B
    delayMicroseconds(5000);
end;

procedure TLCD.SendChar(cmdbyte: char);
var
 controlbyte : byte   ;

begin
  connect;
      pinMode(false, 0 );  // ALL WRITE
          controlbyte := 0;
  BitON_8 ( controlbyte,_rs_pin_bit);
  BitOFF_8 ( controlbyte,_rw_pin_bit   );
  BitOn_8 ( controlbyte,_enable_pin_bit );

  I2C_Write8(hdev, MCP23017_GPIOA, controlbyte); // all   on port B
  I2C_Write8(hdev, MCP23017_GPIOB, Ord(cmdbyte)); // all   on port B
   BitOff_8 ( controlbyte,_enable_pin_bit );
  I2C_Write8(hdev, MCP23017_GPIOA, controlbyte); // all   on port B
  sleep(2);
end;

function  TLCD.ReadChar() : char;
var
 controlbyte : byte   ;

begin
  connect;
    pinMode(false, $FF );  // ALL READ
    controlbyte := 0;
  BitOn_8 ( controlbyte,_rs_pin_bit);
  BitON_8 ( controlbyte,_rw_pin_bit   );
  BitOn_8 ( controlbyte,_enable_pin_bit );




  I2C_Write8(hdev, MCP23017_GPIOA, controlbyte); // all   on port B
    sleep(1) ;


        sleep(1) ;
  result :=  char( I2C_Read8 (hdev, MCP23017_GPIOB)); // all   on port B
      BitOFF_8 ( controlbyte,_enable_pin_bit );
         I2C_Write8(hdev, MCP23017_GPIOA, controlbyte); // all   on port B

  sleep(1);
end;

procedure TLCD.home();
begin
  connect;
  Command(LCD_RETURNHOME); // set cursor position to zero
  delayMicroseconds(2000); // this command takes a long time!
end;

procedure TLCD.noDisplay();
begin
  connect;
  BitOff_8(_displaycontrol, LCD_DISPLAYON_BIT);

  Command(LCD_DISPLAYCONTROL or _displaycontrol);

end;

procedure TLCD.display();
begin
  connect;
  BitON_8(_displaycontrol, LCD_DISPLAYON_BIT);

  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;

procedure TLCD.noBlink();
begin
  connect;
  BitOff_8(_displaycontrol, LCD_BLINKON_BIT);

  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;

procedure TLCD.blink();
begin
  connect;
  BitON_8(_displaycontrol, LCD_BLINKON_BIT);

  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;

procedure TLCD.noCursor();
begin
  connect;
  BitOff_8(_displaycontrol, LCD_CURSORON_BIT);

  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;

procedure TLCD.cursor();
begin
  connect;
  BitON_8(_displaycontrol, LCD_CURSORON_BIT);

  Command(LCD_DISPLAYCONTROL or _displaycontrol);
end;

// will not work ?
procedure TLCD.scrollDisplayLeft();


begin
  BitON_8(_DisplayMoveShift, LCD_DISPLAY_MOVE_BIT);
  // These commands scroll the display without changing the RAM

  Command(LCD_CURSOR_DISPLAY_SHIFT OR _DisplayMoveShift); // will not work ?

end;

// will not work ?
procedure TLCD.scrollDisplayRight();


begin
  connect;
  BitON_8(_DisplayMoveShift, LCD_DISPLAY_MOVE_BIT);
  BitON_8(_DisplayMoveShift, LCD_MOVE_RIGHT);

  Command(LCD_CURSOR_DISPLAY_SHIFT OR _DisplayMoveShift); // will not work ?
end;

procedure TLCD.leftToRight();
begin
  // This is for text that flows Left to Right
     connect;
  BitON_8(_displaymode, LCD_ENTRYMODESET_SHIFT_BIT);
  Command(LCD_ENTRYMODESET OR _Entrymode);
end;

procedure TLCD.rightToLeft();
begin
  // This is for text that flows Right to Left
    connect;
  BitOff_8(_displaymode, LCD_ENTRYMODESET_SHIFT_BIT );
  Command(LCD_ENTRYMODESET OR _Entrymode);

end;

procedure TLCD.autoscroll();
begin
  // This will 'right justify' text from the cursor
  BitON_8(_displaymode, LCD_ENTRYMODESET_INCREMENT_BIT);
  Command(LCD_ENTRYMODESET OR _Entrymode);

end;

procedure TLCD.noAutoscroll();
begin
  // This will 'left justify' text from the cursor
   connect;
  BitOff_8(_displaymode, LCD_ENTRYMODESET_INCREMENT_BIT);
  Command(LCD_ENTRYMODESET OR _Entrymode);
end;

procedure TLCD.setCursor(col, row: byte);
var
  data: Integer;
begin
  connect;
  // int row_offsets[] = { 0x00, 0x40, 0x14, 0x54 };
  // if ( row > _numlines ) {
  // row = _numlines-1;    // we count rows starting w/0
  if row > 0 then
    data := col + $40
  else
    data := col;

  Command(LCD_SETDDRAMADDR OR data);

end;


  // Allows us to fill the first 8 CGRAM locations
  // with custom characters
   procedure TLCD.createChar(  location : byte ;   charmap : TLcdbytechar   )  ;
   var
     i : Integer  ;
     devcharmap : TLcdbytechar  ;
     alocation : byte;
   begin
    alocation  :=   ( location * 8  )    ; // we only have 8 locations 0-7


  ReadcustomChar ( location, devcharmap  ) ;


    if NOT  (
    (charmap[1]  =  devcharmap[1] ) and   (charmap[2]  =  devcharmap[2] )
    AND  (charmap[3]  =  devcharmap[3] )  AND  (charmap[4]  =  devcharmap[4] )
    AND  (charmap[5]  =  devcharmap[5] )  AND  (charmap[6]  =  devcharmap[6] )
    AND  (charmap[7]  =  devcharmap[7] )  AND  (charmap[8]  =  devcharmap[8] )

    )

    then
    begin

    command(LCD_SETCGRAMADDR or  alocation);

     for i := 1 to 8 do

     SendChar  (  char( charmap[i] ) )  ;


    end;

  command(LCD_SETDDRAMADDR);  // unfortunately resets the location to 0,0
 end;

     procedure TLCD.ReadcustomChar(   location : byte ; var    charmap : TLcdbytechar   )  ;
   var
     i : Integer  ;
       alocation : byte;

   begin
    alocation  :=   ( location * 8  )    ; // we only have 8 locations 0-7
  command(LCD_SETCGRAMADDR or  alocation);

  for i := 1 to 8 do
    charmap[i]  :=  byte( readChar  ())  ;


  // command(LCD_SETDDRAMADDR);  // unfortunately resets the location to 0,0
 end;

  procedure TLCD.SendString(cmdstr: string);

var
  cnt: Integer;

begin

  if length(cmdstr) > 0 then
    for cnt := 0 to length(cmdstr) - 1 do
      SendChar(cmdstr[cnt]);

end;

procedure TLCD.initLCD();

begin
  // set up the pins

  connect;


  Command(LCD_FUNCTIONSET or _DisplayFunction );

  Command(LCD_DISPLAYCONTROL or _DisplayControl );
  Command(LCD_ENTRYMODESET or _EntryMode);
  Command(LCD_CURSOR_DISPLAY_SHIFT or _DisplayMoveShift);

  cleardisplay();

  // SendString('THIS IS TEST!');


end;

procedure TLCD.initMCP();
begin
  // set up the pins
  connect;

  // PORT A CNTRL AND SITCHES

  I2C_Write8(hdev, MCP23017_IODIRA, $1F); // all OUTPUT   on port A
  I2C_Write8(hdev, MCP23017_GPPUA, $1F); // ,$1F) ;   //  -1 5

  // PORT A DATA
  I2C_Write8(hdev, MCP23017_IODIRB, 0); // all   on port B

  // UTRN ON PULLUP for inputs

  initLCD();

end;

procedure TLCD.cleardisplay();

begin
  connect ;

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


