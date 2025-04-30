unit OTA.KeyboardBinding;

interface

uses
  System.Classes, ToolsAPI;

type
  TOTA_KeyboardBinding = class(TNotifierObject, IOTAKeyboardBinding)
  private
    FKeyProc: TKeyBindingProc;
    FMenuItemName: string;
    FDisplayName: string;
    FShortCuts: TArray<TShortCut>;
    procedure DefaultKeyProc(const Context: IOTAKeyContext; KeyCode: TShortcut; var
        BindingResult: TKeyBindingResult);
  protected
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  public
    constructor Create(aMenuItemName: string; aDisplayName: string = '';
        aShortCuts: TArray<TShortCut> = []; aKeyProc: TKeyBindingProc = nil);
  end;

implementation

uses
  Winapi.Windows, Vcl.Menus;

procedure TOTA_KeyboardBinding.BindKeyboard(
  const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding(FShortCuts, FKeyProc, nil, kfImplicitShift or kfImplicitModifier or kfImplicitKeypad, '', FMenuItemName)
end;

procedure TOTA_KeyboardBinding.DefaultKeyProc(const Context: IOTAKeyContext;
    KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
begin
  BindingResult := TKeyBindingResult.krUnhandled;
end;

constructor TOTA_KeyboardBinding.Create(aMenuItemName: string; aDisplayName:
    string = ''; aShortCuts: TArray<TShortCut> = []; aKeyProc: TKeyBindingProc
    = nil);
begin
  inherited Create;
  FMenuItemName := aMenuItemName;
  FShortCuts := Copy(aShortCuts, Low(aShortCuts), Length(aShortCuts));
  if Assigned(aKeyProc) then
    FKeyProc := aKeyProc
  else
    FKeyProc := DefaultKeyProc;
end;

function TOTA_KeyboardBinding.GetBindingType: TBindingType;
begin
  Result := TBindingType.btPartial;
end;

function TOTA_KeyboardBinding.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

function TOTA_KeyboardBinding.GetName: string;
begin
  Result := FMenuItemName;
end;

end.
