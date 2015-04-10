unit i2cdev_base;

{$mode objfpc}{$H+}

interface


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils
  , baseUnix;

type

  { TIc2Base }

  TIc2Base = class

  private
    Fhdev: Cint;
    Fi2cadd: Cint;
  public



    constructor Create(); virtual; abstract;
    constructor Create(ai2cadd: Cint); virtual; overload;
    destructor Destroy(); override;
    property hdev: Cint read Fhdev write Fhdev;
    property i2cadd: Cint read Fi2cadd;

    function connect: boolean;

    procedure disconect;



  end;

function I2C_Open(iDevAddr: Cint): Cint;
procedure I2C_Close(fh: Cint);

function I2C_Read8(fh: Cint; reg: byte): byte;
function I2C_Read16(fh: Cint; reg: byte): word;

function I2C_Write8(fh: Cint; reg: byte; Data: byte): boolean;
function I2C_Write16(fh: Cint; reg: byte; Data: word): boolean;

---
function I2C_Write16_2(fh: Cint; reg: byte; Data1,data2: word): boolean;
--- 8bit functions 
function BitOn_8(const val: byte; const TheBit: Byte): byte;
function BitOff_8(const val: byte; const TheBit: Byte): byte;
function IsBitSet_8(const val: byte; const TheBit: Byte): Boolean;
 


implementation

procedure I2C_Close(fh: Cint);
begin
  fpclose(fh);
end;

function I2C_Open(iDevAddr: Cint): Cint;
const
  I2C_SLAVE = 1795;
var
  devPath: string = '/dev/i2c-1';
begin

  Result := fpopen(devPath, O_RDWR);
  if Result < 1 then
  begin
    raise Exception.Create('I2C_Open ERROR ');

  end;
  fpIOCtl(Result, I2C_SLAVE, pointer(iDevAddr));

end;

function I2C_Write8(fh: Cint; reg: byte; Data: byte): boolean;
var
  buf: packed array [0..1] of byte;

begin

  buf[0] := reg;
  buf[1] := Data;


  if fpwrite(fh, buf, 2) <> 2 then
  begin
    raise Exception.Create('I2C_Write8 ERROR');

  end;

  Result := True;

end;

function I2C_Write16(fh: Cint; reg: byte; Data: word): boolean;

var
  buf: packed array [0..2] of byte;

begin

  buf[0] := reg;
  buf[1] := hi(Data);
  buf[2] := lo(Data);

  if fpwrite(fh, buf, 3) <> 3 then
  begin
    raise Exception.Create('I2C_Write16 ERROR');

  end;

  Result := True;

end;
function I2C_Write16_2(fh: Cint; reg: byte; Data1,data2: word): boolean;

var
  buf: packed array [0..4] of byte;

begin

  buf[0] := reg;
  buf[1] := hi(Data1);
  buf[2] := lo(Data1);
  buf[3] := hi(Data2);
  buf[4] := lo(Data1);
  if fpwrite(fh, buf, 5) <> 5 then
  begin
    raise Exception.Create('I2C_Write16 ERROR');

  end;

  Result := True;

end;

function I2C_Read8(fh: Cint; reg: byte): byte;
var
  buf: byte;

begin

  if fpwrite(fh, reg, 1) <> 1 then
  begin
    raise Exception.Create('I2C_Read8 REG  ERROR');

  end;

  if fpread(fh, buf, 1) <> 1 then
  begin
    raise Exception.Create('I2C_Read8  read   ERROR');

  end;

  Result := buf;

end;

function I2C_Read16(fh: Cint; reg: byte): word;

var
  buf: packed array [0..1] of byte = (0, 0);

begin

  if fpwrite(fh, reg, 1) <> 1 then
  begin
    raise Exception.Create('I2C_Read16 REG  ERROR');

  end;

  if fpread(fh, buf, 2) <> 2 then
  begin
    raise Exception.Create('I2C_Read16  read   ERROR');

  end;

  Result := (buf[0] shl 8) or buf[1];

end;

{ TIc2Base }



constructor TIc2Base.Create(ai2cadd: Cint);
begin

  Fi2cadd := ai2cadd;
  fhdev := 0;

end;

destructor TIc2Base.Destroy();
begin
  disconect;
  inherited;

end;

function TIc2Base.connect: boolean;
begin
  if (Fhdev = 0) and (Fi2cadd <> 0) then
  begin
    Fhdev := I2C_Open(Fi2cadd);

  end;
  Result := not (Fhdev = 0);
end;

procedure TIc2Base.disconect;
begin
  if Fhdev <> 0 then
    I2C_Close(Fhdev);
  Fhdev := 0;
end;
// ------------------------------BIT FUNCTIONS 8 BIT 
function BitOn_8(const val: byte; const TheBit: Byte): byte;
begin
  Result := val or (1 shl TheBit);
end;
function BitOff_8(const val: byte; const TheBit: Byte): byte;
begin
  Result := val and ((1 shl TheBit) xor $FFFF);
end;
function IsBitSet_8(const val: byte; const TheBit: Byte): Boolean;
begin
  Result := (val and (1 shl TheBit)) <> 0;
end;


end.

