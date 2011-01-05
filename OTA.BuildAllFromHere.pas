unit OTA.BuildAllFromHere;

interface

uses
  ToolsAPI, Menus;

type
  TBuildAllFromHere = class(TNotifierObject, INTAProjectMenuCreatorNotifier)
  private
    function ExecuteCommand(const aID: string; const aMenu: TMenu): Boolean;
        overload;
    function ExecuteCommand(const aID: string; const aItem: TMenuItem): Boolean;
        overload;
    procedure OnMenuClick(Sender: TObject);
  protected
    function AddMenu(const Ident: string): TMenuItem;
    function CanHandle(const Ident: string): Boolean;
  end;

implementation

uses SysUtils;

function TBuildAllFromHere.AddMenu(const Ident: string): TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := 'Build or Build All From Here';
  Result.ShortCut := TextToShortCut('Ctrl+`');
  Result.OnClick := OnMenuClick;
end;

function TBuildAllFromHere.CanHandle(const Ident: string): Boolean;
begin
  Result := SameText(Ident, sProjectContainer);
end;

function TBuildAllFromHere.ExecuteCommand(const aID: string; const aMenu:
    TMenu): Boolean;
var i: integer;
begin
  Result := False;
  for i := 0 to aMenu.Items.Count - 1 do begin
    Result := ExecuteCommand(aID, aMenu.Items[i]);
    if Result then
      Break;
  end;
end;

function TBuildAllFromHere.ExecuteCommand(const aID: string; const aItem:
    TMenuItem): Boolean;
var i: integer;
begin
  Result := False;
  if not aItem.Enabled then Exit;

  Result := SameText(aItem.Caption, aID);
  if Result then
    aItem.Click
  else begin
    {$region 'Recursive Search'}
    for i := 0 to aItem.Count - 1 do begin
      Result := ExecuteCommand(aID, aItem.Items[i]);
      if Result then
        Break;
    end;
    {$endregion}
  end;
end;

procedure TBuildAllFromHere.OnMenuClick(Sender: TObject);
var M: TMenu;
begin
  M := TMenuItem(Sender).GetParentMenu;
  if not ExecuteCommand('Build All From &Here', M) then
    ExecuteCommand('&Build', M);
end;

end.
