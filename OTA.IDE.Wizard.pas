unit OTA.IDE.Wizard deprecated;

interface

procedure Register;

implementation

uses
  Winapi.Windows, System.Classes, System.SysUtils, Vcl.Dialogs, Vcl.Forms,
  DesignEditors, DesignIntf, ToolsApi;

type
  TResourceDataModule = class(TDataModule);

  TResourceDataModuleCreatorWizard = class(TNotifierObject, IOTAWizard,
      IOTARepositoryWizard, IOTAFormWizard)
  public
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: THandle;
  end;

  TResourceDataModuleCreator = class(TInterfacedObject, IOTACreator,
      IOTAModuleCreator)
  public
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  TResourceDataModuleSourceFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: string;
  public
    function GetSource: string;
    function GetAge: TDateTime;
    constructor Create(const Source: string);
  end;

  TResourceForm = class(TForm);

  TResourceFormCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  public
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  TResourceFormCreatorWizard = class(TNotifierObject, IOTAWizard,
      IOTARepositoryWizard, IOTAFormWizard)
  public
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: THandle;
  end;

  TResourceFormSourceFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: string;
  public
    function GetSource: string;
    function GetAge: TDateTime;
    constructor Create(const Source: string);
  end;

  TResourceFrame = class(TFrame);

  TFrameCustomModule = class(TCustomModule)
  public
    function Nestable: Boolean; override;
  end;

  TResourceFrameCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  public
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  TResourceFrameCreatorWizard = class(TNotifierObject, IOTAWizard,
      IOTARepositoryWizard, IOTAFormWizard)
  public
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: THandle;
  end;

  TResourceFrameSourceFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: string;
  public
    function GetSource: string;
    function GetAge: TDateTime;
    constructor Create(const Source: string);
  end;

procedure Register;
begin
  RegisterCustomModule(TResourceForm, TCustomModule);
  RegisterPackageWizard(TResourceFormCreatorWizard.Create);

//  RegisterCustomModule(TResourceFrame, TFrameCustomModule);
//  RegisterPackageWizard(TResourceFrameCreatorWizard.Create);

  RegisterCustomModule(TResourceDataModule, TCustomModule);
  RegisterPackageWizard(TResourceDataModuleCreatorWizard.Create);
end;

procedure TResourceDataModuleCreatorWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(TResourceDataModuleCreator.Create);
end;

function TResourceDataModuleCreatorWizard.GetAuthor: string;
begin
  Result := 'E.Stream.Software';
end;

function TResourceDataModuleCreatorWizard.GetComment: string;
begin
  Result := 'EStream Resource Data Module Creator';
end;

function TResourceDataModuleCreatorWizard.GetGlyph: THandle;
begin
  Result := 0;
end;

function TResourceDataModuleCreatorWizard.GetIDString: string;
begin
  Result := 'E.Stream.ResourceDataModuleCreatorWizard';
end;

function TResourceDataModuleCreatorWizard.GetName: string;
begin
  Result := 'E Stream Resource DataModule';
end;

function TResourceDataModuleCreatorWizard.GetPage: string;
begin
  Result := 'New';
end;

function TResourceDataModuleCreatorWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TResourceDataModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  // Nothing
end;

function TResourceDataModuleCreator.GetAncestorName: string;
begin
  Result := 'ResourceDataModule';
end;

function TResourceDataModuleCreator.GetCreatorType: string;
begin
  // Return sUnit or sText as appropriate
  Result := sForm;
end;

function TResourceDataModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TResourceDataModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TResourceDataModuleCreator.GetFormName: string;
begin
  Result := '';
end;

function TResourceDataModuleCreator.GetImplFileName: string;
begin
  Result := '';
end;

function TResourceDataModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TResourceDataModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TResourceDataModuleCreator.GetOwner: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  NewModule: IOTAModule;
begin
  // You may prefer to return the project group's ActiveProject instead
  Result := nil;
  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
  Module := ModuleServices.CurrentModule;

  if Module <> nil then
  begin
    if Module.QueryInterface(IOTAProject, NewModule) = S_OK then
      Result := NewModule
    else if Module.OwnerModuleCount > 0 then
    begin
      NewModule := Module.OwnerModules[0];
      if NewModule <> nil then
        if NewModule.QueryInterface(IOTAProject, Result) <> S_OK then
          Result := nil;
    end;
  end;
end;

function TResourceDataModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TResourceDataModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TResourceDataModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TResourceDataModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TResourceDataModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TResourceDataModuleCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

constructor TResourceDataModuleSourceFile.Create(const Source: string);
begin
  FSource := Source;
end;

function TResourceDataModuleSourceFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TResourceDataModuleSourceFile.GetSource: string;
begin
  Result := FSource;
end;

procedure TResourceFormCreator.FormCreated(const FormEditor: 
    IOTAFormEditor);
begin
  // Nothing
end;

function TResourceFormCreator.GetAncestorName: string;
begin
  Result := 'ResourceForm';
end;

function TResourceFormCreator.GetCreatorType: string;
begin
  // Return sUnit or sText as appropriate
  Result := sForm;
end;

function TResourceFormCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TResourceFormCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TResourceFormCreator.GetFormName: string;
begin
  Result := '';
end;

function TResourceFormCreator.GetImplFileName: string;
begin
  Result := '';
end;

function TResourceFormCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TResourceFormCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TResourceFormCreator.GetOwner: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  NewModule: IOTAModule;
begin
  // You may prefer to return the project group's ActiveProject instead
  Result := nil;
  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
  Module := ModuleServices.CurrentModule;

  if Module <> nil then
  begin
    if Module.QueryInterface(IOTAProject, NewModule) = S_OK then
      Result := NewModule
    else if Module.OwnerModuleCount > 0 then
    begin
      NewModule := Module.OwnerModules[0];
      if NewModule <> nil then
        if NewModule.QueryInterface(IOTAProject, Result) <> S_OK then
          Result := nil;
    end;
  end;
end;

function TResourceFormCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TResourceFormCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TResourceFormCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TResourceFormCreator.NewFormFile(const FormIdent, 
    AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TResourceFormCreator.NewImplSource(const ModuleIdent, FormIdent,
    AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TResourceFormCreator.NewIntfSource(const ModuleIdent, FormIdent,
    AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

procedure TResourceFormCreatorWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(TResourceFormCreator.Create);
end;

function TResourceFormCreatorWizard.GetAuthor: string;
begin
  Result := 'E.Stream.Software';
end;

function TResourceFormCreatorWizard.GetComment: string;
begin
  Result := 'EStream Resource Form Creator';
end;

function TResourceFormCreatorWizard.GetGlyph: THandle;
begin
  Result := 0;
end;

function TResourceFormCreatorWizard.GetIDString: string;
begin
  Result := 'E.Stream.ResourceFormCreatorWizard';
end;

function TResourceFormCreatorWizard.GetName: string;
begin
  Result := 'E Stream Resource Form';
end;

function TResourceFormCreatorWizard.GetPage: string;
begin
  Result := 'New';
end;

function TResourceFormCreatorWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

constructor TResourceFormSourceFile.Create(const Source: string);
begin
  FSource := Source;
end;

function TResourceFormSourceFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TResourceFormSourceFile.GetSource: string;
begin
  Result := FSource;
end;

procedure TResourceFrameCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  // Nothing
end;

function TResourceFrameCreator.GetAncestorName: string;
begin
  Result := 'ResourceFrame';
end;

function TResourceFrameCreator.GetCreatorType: string;
begin
  // Return sUnit or sText as appropriate
  Result := sForm;
end;

function TResourceFrameCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TResourceFrameCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TResourceFrameCreator.GetFormName: string;
begin
  Result := '';
end;

function TResourceFrameCreator.GetImplFileName: string;
begin
  Result := '';
end;

function TResourceFrameCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TResourceFrameCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TResourceFrameCreator.GetOwner: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  NewModule: IOTAModule;
begin
  // You may prefer to return the project group's ActiveProject instead
  Result := nil;
  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
  Module := ModuleServices.CurrentModule;

  if Module <> nil then
  begin
    if Module.QueryInterface(IOTAProject, NewModule) = S_OK then
      Result := NewModule
    else if Module.OwnerModuleCount > 0 then
    begin
      NewModule := Module.OwnerModules[0];
      if NewModule <> nil then
        if NewModule.QueryInterface(IOTAProject, Result) <> S_OK then
          Result := nil;
    end;
  end;
end;

function TResourceFrameCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TResourceFrameCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TResourceFrameCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TResourceFrameCreator.NewFormFile(const FormIdent, AncestorIdent: 
    string): IOTAFile;
begin
  Result := nil;
end;

function TResourceFrameCreator.NewImplSource(const ModuleIdent, FormIdent, 
    AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TResourceFrameCreator.NewIntfSource(const ModuleIdent, FormIdent, 
    AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

procedure TResourceFrameCreatorWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(TResourceFrameCreator.Create);
end;

function TResourceFrameCreatorWizard.GetAuthor: string;
begin
  Result := 'E.Stream.Software';
end;

function TResourceFrameCreatorWizard.GetComment: string;
begin
  Result := 'EStream Resource Frame Creator';
end;

function TResourceFrameCreatorWizard.GetGlyph: THandle;
begin
  Result := 0;
end;

function TResourceFrameCreatorWizard.GetIDString: string;
begin
  Result := 'E.Stream.ResourceFrameCreatorWizard';
end;

function TResourceFrameCreatorWizard.GetName: string;
begin
  Result := 'E Stream Resource Frame';
end;

function TResourceFrameCreatorWizard.GetPage: string;
begin
  Result := 'New';
end;

function TResourceFrameCreatorWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

constructor TResourceFrameSourceFile.Create(const Source: string);
begin
  FSource := Source;
end;

function TResourceFrameSourceFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TResourceFrameSourceFile.GetSource: string;
begin
  Result := FSource;
end;

{ TFrameCustomModule }

function TFrameCustomModule.Nestable: Boolean;
begin
  Result := True;
end;

end.
