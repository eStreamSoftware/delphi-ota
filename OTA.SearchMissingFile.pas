unit OTA.SearchMissingFile;

interface

uses
  ToolsAPI, Vcl.Menus;

type
  TSearchMissingFile = class(TNotifierObject, INTAProjectMenuCreatorNotifier)
  private
    FText: string;
    procedure OnMenuClick(Sender: TObject);
  protected
    function AddMenu(const Ident: string): TMenuItem;
    function CanHandle(const Ident: string): Boolean;
  public
    procedure AfterConstruction; override;
  end;

implementation

uses System.SysUtils, Vcl.Dialogs;

function TSearchMissingFile.AddMenu(const Ident: string): TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := 'Search Missing File';
  Result.OnClick := OnMenuClick;
end;

procedure TSearchMissingFile.AfterConstruction;
begin
  inherited;
  FText := 'Library.rc';  //by default search Library.rc
end;

function TSearchMissingFile.CanHandle(const Ident: string): Boolean;
begin
  Result := SameText(Ident, {$if CompilerVersion < 21}sProjectContainer{$else}sProjectGroupContainer{$ifend});
end;

procedure TSearchMissingFile.OnMenuClick(Sender: TObject);
var i, j, lCount: integer;
    bFound: boolean;
    G: IOTAProjectGroup;
    M: IOTAMessageGroup;
begin
  if InputQuery('Search Missing File', 'Please enter file name', FText) then begin
    lCount := 0;
    M := (BorlandIDEServices as IOTAMessageServices).AddMessageGroup(Format('Missing "%s" Projects', [FText]));
    (BorlandIDEServices as IOTAMessageServices).ClearMessageGroup(M);
    G := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
    for i := 0 to G.ProjectCount - 1 do begin
      if not SameText(ExtractFileExt(G.Projects[i].ProjectOptions.TargetName), '.BPL') then Continue;
      bFound := False;
      for j := 0 to G.Projects[i].GetModuleCount - 1 do begin
        bFound := SameText(G.Projects[i].GetModule(j).Name, FText);
        if bFound then
          Break;
      end;
      if not bFound then begin
        (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(ExtractFileName(G.Projects[i].ProjectOptions.TargetName), M);
        Inc(lCount);
      end;
    end;
    ShowMessageFmt('%d projects found.', [lCount]);
    (BorlandIDEServices as IOTAMessageServices).ShowMessageView(M);
  end;
end;

end.
