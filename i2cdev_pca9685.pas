unit i2cdev_PCA9685;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, baseUnix, i2cdev_base, contnrs;

{
/***************************************************
  This is a library for our Adafruit 16-channel PWM & Servo driver

  Pick one up today in the adafruit shop!
  ------> http://www.adafruit.com/products/815

  These displays use I2C to communicate, 2 pins are required to
  interface. For Arduino UNOs, thats SCL -> Analog 5, SDA -> Analog 4

  Adafruit invests time and resources providing this open source code,
  please support Adafruit and open-source hardware by purchasing
  products from Adafruit!

  Written by Limor Fried/Ladyada for Adafruit Industries.
  BSD license, all text above must be included in any redistribution
 ****************************************************/
 }

type

  { TPCA9685Pin }

  TPCA9685 = class;



  { TPCA9685 }



  TPCA9685Pin = class(TObject)




  private
    Fled_off: word;
    Fled_on: word;

    // ACTUAL VALUE IN REG
    Fled_offL: byte;
    Fled_onL: byte;
    Fled_offH: byte;
    Fled_onH: byte;

    Old_led_offL: byte;
    Old_led_onL: byte;
    Old_led_offH: byte;
    Old_led_onH: byte;



    procedure Setled_off(const AValue: word);
    procedure Setled_on(const AValue: word);

  public

    pinnum: integer;
    pPCA9685: TPCA9685;

    constructor Create();

    procedure updatepin;
    property led_on: word read Fled_on write Setled_on;
    property led_off: word read Fled_off write Setled_off;


  end;


  { TPCA9685Pins }

  TPCA9685Pins = class(TObjectList)
  private
    function GetItem(Index: integer): TPCA9685Pin;
    procedure SetItem(Index: integer; AValue: TPCA9685Pin);
  public


    property Items[Index: integer]: TPCA9685Pin read GetItem write SetItem; default;
  end;



  TPCA9685 = class(TIc2Base)



  private




  public

    pins: TPCA9685Pins;


    function getMODE1(): byte;



    constructor Create(); override;
    destructor Destroy; override;


    //  procedure begin();
    procedure reset();
    procedure setPWMFreq(freq: extended);
    procedure setPWM(num: Cint; ton, toff: word; update: boolean = True);
    procedure setPin(num: Cint; val: word; invert: boolean = False);


    function getLEDALLCALLADD(): byte;

    function getMODE2(): byte;
    function getPWMFreqbit(): byte;
    function getSUBADD1(): byte;
    function getSUBADD2(): byte;
    function getSUBADD3(): byte;




    // mode 1 propery

  private

  const
    PCA9685_ADDRESS = $40;

    //# Registers
    PCA9685_SUBADR1 = $02;
    PCA9685_SUBADR2 = $03;
    PCA9685_SUBADR3 = $04;

    PCA9685_MODE1 = $00;
    PCA9685_MODE2 = $01;


    PCA9685_SUBADD1 = $02;
    PCA9685_SUBADD2 = $03;
    PCA9685_SUBADD3 = $04;

    PCA9685_LEDALLCALLADR = $05;

    PCA9685_PRESCALE = $FE;

    LED0_ON_L = $06;
    LED0_ON_H = $07;
    LED0_OFF_L = $08;
    LED0_OFF_H = $09;

    ALLLED_ON_L = $FA;
    ALLLED_ON_H = $FB;
    ALLLED_OFF_L = $FC;
    ALLLED_OFF_H = $FD;



  end;



implementation

{ TPCA9685Pins }

function TPCA9685Pins.GetItem(Index: integer): TPCA9685Pin;
begin
  Result := TPCA9685Pin(inherited Items[Index]);
end;

procedure TPCA9685Pins.SetItem(Index: integer; AValue: TPCA9685Pin);
begin
  inherited  Items[Index] := AValue;
end;

{ TPCA9685Pin }




procedure TPCA9685Pin.Setled_off(const AValue: word);
begin
  if Fled_off = AValue then
    exit;
  Fled_off := AValue;
  Fled_offH := hi(Fled_off);
  Fled_offL := lo(Fled_off);

end;

procedure TPCA9685Pin.Setled_on(const AValue: word);
begin
  if Fled_on = AValue then
    exit;
  Fled_on := AValue;
  Fled_onH := hi(Fled_on);
  Fled_onL := lo(Fled_on);
end;

constructor TPCA9685Pin.Create();
begin

  inherited;
  Old_led_offL := $FF;
  Old_led_onL := $FF;
  Old_led_offH := $FF;
  Old_led_onH := $FF;

end;



procedure TPCA9685Pin.updatepin;
begin
  pPCA9685.connect;

  if Old_led_onL <> Fled_onL then
  begin
    I2C_Write8(pPCA9685.hdev, (pinnum * 4) + 6 + 0, Fled_onL);
  end;


  if Old_led_onH <> Fled_onH then
  begin
    I2C_Write8(pPCA9685.hdev, (pinnum * 4) + 6 + 1, Fled_onH);
  end;



  if Old_led_offL <> Fled_offL then
  begin
    I2C_Write8(pPCA9685.hdev, (pinnum * 4) + 6 + 2, Fled_offL);
  end;

  if Old_led_offH <> Fled_offH then
  begin
    I2C_Write8(pPCA9685.hdev, (pinnum * 4) + 6 + 3, Fled_offH);
  end;

  Old_led_onH := Fled_onH;
  Old_led_onL := Fled_onL;
  Old_led_offL := Fled_offL;
  Old_led_offH := Fled_offH;

end;

// uses math;
{ TPCA9685 }

constructor TPCA9685.Create();
var
  cnt: integer;
  tmp: TPCA9685Pin;

begin
  inherited Create(PCA9685_ADDRESS);
  pins := TPCA9685Pins.Create(False);
  for cnt := 0 to 15 do
  begin

    tmp := TPCA9685Pin.Create;
    tmp.pinnum := cnt;
    tmp.pPCA9685 := self;
    pins.Add(tmp);

  end;

end;

destructor TPCA9685.Destroy;
var
  cnt: integer;
begin
  for cnt := 0 to 15 do
    pins[cnt].Free;
  inherited Destroy;
end;

procedure TPCA9685.reset();
begin
  connect;
  I2C_Write16(hdev, 0, 6);
  disconect;
end;


function TPCA9685.getPWMFreqbit(): byte;
begin
  Result := I2C_Read8(hdev, PCA9685_PRESCALE);
end;




function TPCA9685.getMODE1(): byte;
begin
  connect;
  Result := I2C_Read8(hdev, PCA9685_MODE1);

end;


function TPCA9685.getMODE2(): byte;
begin
  Result := I2C_Read8(hdev, PCA9685_MODE2);
end;

function TPCA9685.getLEDALLCALLADD(): byte;
begin
  Result := I2C_Read8(hdev, PCA9685_LEDALLCALLADR);
end;

function TPCA9685.getSUBADD1(): byte;
begin
  Result := I2C_Read8(hdev, PCA9685_SUBADD1);
end;

function TPCA9685.getSUBADD2(): byte;
begin
  Result := I2C_Read8(hdev, PCA9685_SUBADD2);
end;

function TPCA9685.getSUBADD3(): byte;
begin
  Result := I2C_Read8(hdev, PCA9685_SUBADD3);
end;




procedure TPCA9685.setPWMFreq(freq: extended);
var
  prescale, oldmode, newmode: byte;

begin
  connect;

  // uint8_t prescale = round(((float)25000000 / (float)(freq * (long)4096))) - 1;
  prescale := round((25000000 / (freq * 4096))) - 1;
   {
  //Serial.print("Attempting to set freq ");
  //Serial.println(freq);
  freq *= 0.9;  // Correct for overshoot in the frequency setting (see issue #11).
  float prescaleval = 25000000;
  prescaleval /= 4096;
  prescaleval /= freq;
  prescaleval -= 1;

  uint8_t prescale = floor(prescaleval + 0.5);
    }
  // set the bit to on fpr sleep
  oldmode := I2C_Read8(hdev, PCA9685_MODE1);
  newmode := oldmode;

   BitOn_8(newmode, 4);  // sleep  bit 5 - 1 base

  I2C_Write8(hdev, PCA9685_MODE1, newmode); // go to sleep
  sleep(5);
  I2C_Write8(hdev, PCA9685_PRESCALE, prescale); // set the prescaler
  oldmode := 1;
   BitOFF_8(oldmode, 4);  // sleep
  //oldmode := BitON_8(oldmode , 5);  // autoinc
  BitON_8(oldmode, 7);  // reset  bit 7

  I2C_Write8(hdev, PCA9685_MODE1, oldmode);
  sleep(5);

  // I2C_Write8(hdev,PCA9685_MODE1, oldmode | 0xa1);  //  This sets the MODE1 register to turn on auto increment.
  // This is why the beginTransmission below was not working.
  //  Serial.print("Mode now 0x"); Serial.println(read8(PCA9685_MODE1), HEX);
  //  newmode := I2C_Read8(hdev ,PCA9685_MODE1);
end;

procedure TPCA9685.setPWM(num: Cint; ton, toff: word; update: boolean = True);
var
  reg: byte;
begin
  connect;


  pins.Items[num].led_on := ton;
  pins[num].led_off := toff;
  if update then
    pins[num].updatepin;
  // reg := LED0_ON_L + (4 * num);
  // 8 8,8 8,8  - I2C_Write16_2(fh,reg,word ,word) ; //  - 5 bytes
  //I2C_Write16_2(hdev, reg, ton, toff);

end;

procedure TPCA9685.setPin(num: Cint; val: word; invert: boolean);
begin
  // Clamp value between 0 and 4095 inclusive.
  // val = min(val, 4095);
  if val > 4095 then
    val := 4095;


  if (invert) then
  begin
    if (val = 0) then
    begin
      // Special value for signal fully on.
      setPWM(num, 4096, 0);
    end
    else if (val = 4095) then
    begin
      // Special value for signal fully off.
      setPWM(num, 0, 4096);
    end
    else
    begin
      setPWM(num, 0, 4095 - val);
    end;
  end
  else
  begin
    if (val = 4095) then
    begin
      // Special value for signal fully on.
      setPWM(num, 4096, 0);
    end
    else if (val = 0) then
    begin
      // Special value for signal fully off.
      setPWM(num, 0, 4096);
    end
    else
    begin
      setPWM(num, 0, val);
    end;
  end;
end;


end.

