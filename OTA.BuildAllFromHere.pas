unit OTA.BuildAllFromHere;

interface

uses
  System.SysUtils, ToolsAPI;

type
  TBuildAllFromHere = class(TInterfacedObject, TFunc<IOTAKeyboardBinding>)
  protected
    function Invoke: IOTAKeyboardBinding;
  end;

implementation

uses
  Winapi.Windows, System.Classes, Vcl.Menus,
  OTA.KeyboardBinding;

function TBuildAllFromHere.Invoke: IOTAKeyboardBinding;
begin
  Result := TOTA_KeyboardBinding.Create('BuildAllFromHere', '', [ShortCut(VK_OEM_3, [ssCtrl])]) as IOTAKeyboardBinding;
end;

end.
