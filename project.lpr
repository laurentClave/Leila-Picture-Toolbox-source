program project;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}
    cthreads,
  {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms,
  mainFormUnit,
  helpers,
  resample,
  globalizationDatas,
  resizeProfiles, ResizeThread, SysUtils,
  Translations, gettext;

{$R *.res}

begin
  Application.Title := 'LEILA Picture Toolbox';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, FForm);
  Application.Run;
end.
