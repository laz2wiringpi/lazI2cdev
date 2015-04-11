unit i2cdev_PCA9685;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,baseUnix,i2cdev_base;

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

{ TPCA9685 }

TPCA9685 = class(TIc2Base)


private

public

    constructor Create(); override;

//  procedure begin();
  procedure  reset();
  procedure setPWMFreq(freq : Extended  );
  procedure setPWM(  num :Cint ;   ton  ,  toff : Word );
  procedure setPin(  num :Cint ;   val : Word  ;   invert : Boolean  = false);

Private

 CONST
   PCA9685_ADDRESS = $40;

//# Registers
 PCA9685_SUBADR1  = $02;
 PCA9685_SUBADR2  = $03;
 PCA9685_SUBADR3  = $04;

 PCA9685_MODE1  = $00;
 PCA9685_PRESCALE  = $FE;

 LED0_ON_L  = $06;
 LED0_ON_H  = $07;
 LED0_OFF_L  = $08;
 LED0_OFF_H  = $09;

 ALLLED_ON_L  = $FA;
 ALLLED_ON_H  = $FB;
 ALLLED_OFF_L  = $FC;
 ALLLED_OFF_H  = $FD;



 end;



implementation
// uses math;
{ TPCA9685 }

constructor TPCA9685.Create();
begin
  inherited Create(PCA9685_ADDRESS);

end;

procedure TPCA9685.reset();
begin
  connect ;
 I2C_Write8(hdev ,PCA9685_MODE1, 0);
end;

procedure TPCA9685.setPWMFreq(freq: Extended);
var
 prescale, oldmode,newmode :byte;

begin
  connect ;

 // uint8_t prescale = round(((float)25000000 / (float)(freq * (long)4096))) - 1;
   prescale  :=  round( ( 25000000 /  ( freq  *  4096)) ) - 1;
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
  oldmode := I2C_Read8(hdev ,PCA9685_MODE1);

  newmode := BitOn_8(oldmode , 5);  // sleep  bit 5 - 1 base

  I2C_Write8(hdev,PCA9685_MODE1, newmode); // go to sleep
  sleep(5);
  I2C_Write8(hdev,PCA9685_PRESCALE, prescale); // set the prescaler
  I2C_Write8(hdev,PCA9685_MODE1, oldmode);
   sleep(5);

 // I2C_Write8(hdev,PCA9685_MODE1, oldmode | 0xa1);  //  This sets the MODE1 register to turn on auto increment.
                                          // This is why the beginTransmission below was not working.
  //  Serial.print("Mode now 0x"); Serial.println(read8(PCA9685_MODE1), HEX);

end;

procedure TPCA9685.setPWM(num: Cint; ton, toff: Word);
var
  reg : byte;
begin
connect ;
    // need a new one reg word word ;


     reg := LED0_ON_L + (4 * num);
   // 8 8,8 8,8  - I2C_Write16_2(fh,reg,word ,word) ; //  - 5 bytes
     I2C_Write16_2(hdev,reg,ton,toff);

 // WIRE.write(LED0_ON_L+ (4*num));
 // WIRE.write(on);
 // WIRE.write(on>>8);
 // WIRE.write(off);
 // WIRE.write(off>>8);
 // WIRE.endTransmission();

end;
procedure TPCA9685.setPin(num: Cint; val: Word; invert: Boolean);
begin
  // Clamp value between 0 and 4095 inclusive.
 // val = min(val, 4095);
  if val > 4095 then
  val := 4095 ;


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
      setPWM(num, 0, 4095-val);
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
