unit resizeProfiles;

{
     Gère les profils de résolution et les valeurs correspondantes
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, globalizationDatas;

type
  PResizeProfile = ^TResizeProfile;

  TResizeProfile = record
    Name: string;
    width, height: integer;
  end;

  { TResizeProfileList }
  TResizeProfileList = class(TList)
  private
    function Get(Index: integer): PResizeProfile;

    function Add(Value: PResizeProfile): integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddProfile(_name: string; _width, _height: integer);
    property Items[Index: integer]: PResizeProfile read Get; default;
  end;

implementation

{ TResizeProfileList }
function TResizeProfileList.Get(Index: integer): PResizeProfile;
begin
  Result := PResizeProfile(inherited Get(Index));
end;

destructor TResizeProfileList.Destroy;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    FreeMem(Items[i]);
  inherited;
end;

procedure TResizeProfileList.AddProfile(_name: string; _width, _height: integer);
var
  profile: PResizeProfile;
begin
  New(profile);
  profile^.Name           := _name;
  profile^.width          := _width;
  profile^.height         := _height;
  Add(profile);
end;

constructor TResizeProfileList.Create;
begin
  inherited;

end;

function TResizeProfileList.Add(Value: PResizeProfile): integer;
begin
  Result := inherited Add(Value);
end;

end.
