unit OTA.RemoveDefaultResource;

interface

uses Menus, ToolsAPI;

type
  TRemoveDefaultResource = class(TNotifierObject, INTAProjectMenuCreatorNotifier)
  private
    procedure OnMenuClick(Sender: TObject);
  protected
    function AddMenu(const Ident: string): TMenuItem;
    function CanHandle(const Ident: string): Boolean;
    function RemoveDefaultResource(const Project: IOTAProject): Boolean;
  end;

implementation

uses SysUtils, OTA.IDE;

function TRemoveDefaultResource.AddMenu(const Ident: string): TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := 'Remove Default Resource ($R *.res}';
  Result.OnClick := OnMenuClick;
end;

function TRemoveDefaultResource.CanHandle(const Ident: string): Boolean;
begin
  Result := SameText(Ident, {$if CompilerVersion < 21}sProjectContainer{$else}sProjectGroupContainer{$ifend});
end;

procedure TRemoveDefaultResource.OnMenuClick(Sender: TObject);
var i, iCount: integer;
    G: IOTAProjectGroup;
    M: IOTAMessageServices;
begin
  G := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
  M := BorlandIDEServices as IOTAMessageServices;
  iCount := 0;
  for i := 0 to G.ProjectCount - 1 do begin
    if RemoveDefaultResource(G.Projects[i]) then begin
      if iCount = 0 then
        M.ClearMessageGroup(nil);
      M.AddTitleMessage(Format('Remove default resource entry for project %s', [ExtractFileName(G.Projects[i].ProjectOptions.TargetName)]));
      Inc(iCount);
    end;
  end;
  if iCount > 0 then begin
    M.AddTitleMessage(Format('%d project(s) default resource entry removed', [iCount]));
    M.ShowMessageView(nil);
  end;
end;

function TRemoveDefaultResource.RemoveDefaultResource(const Project: IOTAProject): Boolean;
var o: IOTAEditBuffer;
    p: IOTAEditPosition;
    iError: integer;
    sText: string;
    iRow, iCol: integer;
begin
  Result := False;
  if Project.CurrentEditor.QueryInterface(IOTAEditBuffer, o) = S_OK then begin
    p := o.EditPosition;
    iRow := p.Row;
    iCol := p.Column;
    sText := '{$R *.res}';
    if p.Search(sText, False, False, True, sdForward, iError) then begin
      if p.Column = Length(sText) + 1 then begin
        p.BackspaceDelete(11);
        p.MoveRelative(iRow, iCol);
        Result := True;
      end;
    end;
  end;
end;

end.
