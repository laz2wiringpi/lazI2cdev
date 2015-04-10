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
    destructor Destroy();  override;
    property hdev: Cint read Fhdev write Fhdev;
    property i2cadd: Cint read Fi2cadd;

    function    connect : boolean ;

    procedure   disconect ;



  end;

procedure I2C_Close(fh: Cint);
function I2C_Open(iDevAddr: Cint): Cint;
function I2C_Read16(fh: Cint; reg: byte): word;
function I2C_Write16(fh: Cint; reg: byte; Data: word): boolean;


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
  fpIOCtl(Result, I2C_SLAVE, pointer(iDevAddr));

end;

function I2C_Write16(fh: Cint; reg: byte; Data: word): boolean;

var
  buf: packed array [0..2] of byte;

begin

  buf[0] := reg;
  buf[1] := hi(Data);
  buf[2] := lo(Data);

  fpwrite(fh, buf, 3);

  Result := True;

end;

function I2C_Read16(fh: Cint; reg: byte): word;

var
  buf: packed array [0..1] of byte = (0,0);

begin

  fpwrite(fh, reg, 1);


  fpread(fh, buf, 2);

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
 disconect ;
 Inherited  ;


end;

function TIc2Base.connect : boolean ;
begin
   if  ( Fhdev = 0) and (Fi2cadd <> 0 )  then
     begin
      Fhdev :=  I2C_Open (Fi2cadd) ;


     end;
   result := not (Fhdev = 0);
end;

procedure TIc2Base.disconect;
begin
    if Fhdev <> 0 then
    I2C_Close(Fhdev);
  Fhdev := 0;
end;


end.

