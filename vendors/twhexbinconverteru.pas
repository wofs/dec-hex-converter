unit TwHexBinConverterU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TConvertType = (ctToHex, ctToDec);

  { TwHexBinConverter }

  TwHexBinConverter = class
  private
    function ConvertHexToDec(aHex:string):string;
    function ConvertDecToHex(aDec:string):string;
  public
    function Convert(aInput:string; aType:TConvertType):string;
    function HexClear(aInput:string):string;
  end;
implementation

{ TwHexBinConverter }

function TwHexBinConverter.ConvertHexToDec(aHex: string): string;
var i, r : integer;
begin
  Val('$'+Trim(aHex),r, i);

  if i<>0 then
    Result := 'Invalid input'
  else
    Result := IntToStr(r);
end;

function TwHexBinConverter.ConvertDecToHex(aDec: string): string;
var
  iTemp: Longint;
begin
  TryStrToInt(aDec,iTemp);
  Result:= UpperCase(IntToHex(iTemp,0));
end;

function TwHexBinConverter.Convert(aInput: string; aType: TConvertType): string;
begin
  case aType of
      ctToHex:
        Result:= '0x'+ConvertDecToHex(aInput);
      ctToDec:
        Result:= ConvertHexToDec(HexClear(aInput));
    end;
end;

function TwHexBinConverter.HexClear(aInput:string):string;
var
  aPos: SizeInt;
begin
  aPos:= Pos('x',LowerCase(aInput));

  if aPos>0 then
    Result:= Copy(aInput, aPos+1, Length(aInput))
  else
    Result:= aInput;
end;

end.

