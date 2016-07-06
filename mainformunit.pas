unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Grids, Buttons, Menus, helpers, Exif,
  resizeProfiles, globalizationDatas, typinfo, ResizeThread;

type
  TGridCracker = class(TStringGrid);
  {required to access protected method InvalidateRow}

  { TMainForm }

  TMainForm = class(TForm)
    btn_browseFolder: TButton;
    btn_run: TBitBtn;
    cbx_folders_by_exif_data: TComboBox;
    cbx_rename_by_exif_data: TComboBox;
    cbx_outputExt: TComboBox;
    cb_resizeProfiles: TComboBox;
    check_recursive: TCheckBox;
    check_detectRotation: TCheckBox;
    check_saveRatio: TCheckBox;
    ed_height: TEdit;
    ed_picturesFolder: TEdit;
    ed_renameCustom: TEdit;
    ed_newFolder: TEdit;
    ed_width: TEdit;
    grp_rename: TGroupBox;
    grp_exif: TGroupBox;
    lbl_version: TLabel;
    LogoImage: TImage;
    img_preview: TImage;
    btn_leftPic: TImage;
    lbl_waitPics: TLabel;
    lbl_height: TLabel;
    lbl_hpx: TLabel;
    lbl_pictureInfos: TLabel;
    btn_rightPic: TImage;
    grid_picturesFiles: TStringGrid;
    grp_folder: TGroupBox;
    lbl_width: TLabel;
    lbl_wpx: TLabel;
    loadingProgressBar: TProgressBar;
    pgeCtrl_main: TPageControl;
    pnl_thumb: TPanel;
    rd_by_exif_data: TRadioButton;
    rd_custom_folder: TRadioButton;
    rd_noRename: TRadioButton;
    rd_other_ext: TRadioButton;
    rd_same_ext: TRadioButton;
    rd_sameFolder: TRadioButton;
    rd_renameCustom: TRadioButton;
    rd_renameExif: TRadioButton;
    grp_format: TTabSheet;
    tab_destination: TTabSheet;
    tab_resize: TTabSheet;
    tab_picturesList: TTabSheet;

    procedure check_recursiveChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn_leftPicClick(Sender: TObject);
    procedure cb_resizeProfilesChange(Sender: TObject);
    procedure btn_rightPicClick(Sender: TObject);
    procedure btn_browseFolderClick(Sender: TObject);
    procedure filesStrGridSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure btn_runClick(Sender: TObject);
    procedure grid_picturesFilesCheckboxToggled(sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
    procedure grid_picturesFilesDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure isNumberKeyPress(Sender: TObject; var Key: char);


  private
    { private declarations }
    imagesAry: array of string;
    exifProperties: TStringList;
    FImageIndex: integer;
    resizeProfiles: TResizeProfileList;
    imagesDirectory: string;
    resizeThread: TResizeThread;
    globalization: TGlobalization;

    procedure loadImagesFromDirectory(const PathName: string);
    procedure initLoadingBarToMaxValue( const maxValue : integer );
    procedure refreshLoadingBarToPosition( const newPosition : integer );
    procedure refreshGrid;
    procedure refreshTabCount;
    procedure nextPicture;
    procedure lastPicture;
    procedure refreshPictureViewer;
    procedure ResizeComplete;
    procedure ResizeProgress(const imageIndex: integer);
    procedure ResizeError(const message: string);

  public
    { public declarations }


  end;

var
  FForm: TMainForm;


implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  prc: integer;

begin
  globalization := TGlobalization.Create;
  globalization.setFormLocales(Self);

  exifProperties := TStringList.Create;
  exifProperties.Add(globalization.getLocale('EXIF_MODEL'));
  exifProperties.Add(globalization.getLocale('EXIF_DATE'));
  exifProperties.Add(globalization.getLocale('EXIF_RESOLUTION'));

  FForm.Caption := globalization.getLocale('FORM_CAPTION');

  cbx_rename_by_exif_data.Items                            := exifProperties;
  cbx_rename_by_exif_data.ItemIndex                        := 0;
  cbx_folders_by_exif_data.Items                           := exifProperties;
  cbx_folders_by_exif_data.ItemIndex                       := 0;

  lbl_pictureInfos.Caption                                 := '';

  resizeProfiles := TResizeProfileList.Create;

  with resizeProfiles do begin

    AddProfile( 'RES_THUMB',     320,    200);
    AddProfile( 'RES_LOW',       640,    480);
    AddProfile( 'RES_VERY_LOW',  800,    600);
    AddProfile( 'RES_MAIL',      1024,   780);
    AddProfile( 'RES_MEDIUM',    1280,   768);
    AddProfile( 'RES_HD',        1600,   1200);
    AddProfile( 'RES_4K',        3840,   2160);

  end;

  // Loading resizing profiles
  for prc := 0 to resizeProfiles.Count - 1 do
  begin
    cb_resizeProfiles.Items.Add(globalization.getLocale(resizeProfiles[prc]^.Name));
  end;
end;

procedure TMainForm.check_recursiveChange(Sender: TObject);
begin
     loadImagesFromDirectory(ed_picturesFolder.Text);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  exifProperties.Free;
  resizeProfiles.Free;
  globalization.Free;
  if Assigned(resizeThread) then resizeThread.Free;
end;

{
    Run resizing
}
procedure TMainForm.btn_runClick(Sender: TObject);
begin

  if Assigned(resizeThread) then begin

    ResizeComplete;

  end else begin

    pgeCtrl_main.Enabled                       := false;

    Application.ProcessMessages;

    loadingProgressBar.Max                     := Length(ImagesAry);
    loadingProgressBar.Visible                 := true;

    resizeThread                               := TResizeThread.Create;
    resizeThread.ImagesDirectory               := ed_picturesFolder.Text;
    resizeThread.ImagesAry                     := ImagesAry;
    resizeThread.ResizeWidth                   := StrToInt(ed_width.Text);
    resizeThread.ResizeHeight                  := StrToInt(ed_height.Text);
    resizeThread.PreserveRatio                 := check_saveRatio.Checked;
    resizeThread.DetectRotation                := check_detectRotation.Checked;
    resizeThread.CustomFileName                := ed_renameCustom.Text;
    resizeThread.CustomFolderName              := ed_newFolder.Text;
    resizeThread.OnComplete                    := @ResizeComplete;
    resizeThread.OnProgress                    := @ResizeProgress;
    resizeThread.DateErrorFolder               := globalization.getLocale('DATE_ERROR_FOLDER');

    if rd_other_ext.Checked then resizeThread.outputExt           := cbx_outputExt.Text;

    if rd_noRename.Checked then resizeThread.RenameType           := TNameType.SAME
    else if rd_renameCustom.Checked then resizeThread.RenameType  := TNameType.CUSTOM
    else if rd_renameExif.Checked then resizeThread.RenameType    := TNameType.EXIF_DATA;

    if rd_sameFolder.Checked then resizeThread.FolderType         := TNameType.SAME
    else if rd_custom_folder.Checked then resizeThread.FolderType := TNameType.CUSTOM
    else if rd_by_exif_data.Checked then begin

      resizeThread.FolderType  := TNameType.EXIF_DATA;

      if cbx_folders_by_exif_data.Text = globalization.getLocale('EXIF_MODEL') then begin
        resizeThread.FolderExifDataType:=TExifDataType.MODEL;
      end else if cbx_folders_by_exif_data.Text = globalization.getLocale('EXIF_DATE') then begin
        resizeThread.FolderExifDataType:=TExifDataType.DATE;
      end else if cbx_folders_by_exif_data.Text = globalization.getLocale('EXIF_RESOLUTION') then begin
        resizeThread.FolderExifDataType:=TExifDataType.RESOLUTION;
      end;

    end;

    resizeThread.Start;

    btn_run.Caption := globalization.getLocale('STOP_BTN');

  end;
end;

procedure TMainForm.refreshTabCount;
begin
     tab_picturesList.Caption := globalization.getLocale('PICTURES_LIST_TAB') + ' (' + IntToStr( Length(imagesAry) ) + ')';
end;

procedure TMainForm.grid_picturesFilesCheckboxToggled(sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
var
  checkedImages: TStringList;
  grid: TStringGrid;
  i: integer;

begin
  checkedImages := TStringList.Create;
  grid := sender as TStringGrid;

  for i:=0 to grid.RowCount-1 do
      if grid.Rows[i].Strings[0] = '1' then checkedImages.Add(grid.Rows[i].Strings[4]);

  Setlength(imagesAry, checkedImages.count);
  for i:=0 to checkedImages.count -1 do imagesAry[i] := checkedImages.Strings[i];

  checkedImages.Free;

  refreshTabCount;
end;

procedure TMainForm.grid_picturesFilesDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  grid: TStringGrid;
begin
  if gdFixed in aState then Exit;

  grid := Sender as TStringGrid;

  if (grid.Row = aRow) and (aCol > 0) then
  begin
    with Grid.Canvas.Brush do
    begin
      Color := $F0CAA6;
      Style := bsSolid;
    end;
    grid.Canvas.FillRect(aRect);
    grid.Canvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, grid.Cells[acol, arow]);
    Grid.Canvas.Brush := grid.Brush;
  end;
end;

procedure TMainForm.ResizeProgress(const imageIndex: integer);
begin
  loadingProgressBar.Position := imageIndex + 1;
  lbl_pictureInfos.Caption    := globalization.getLocale('RESIZING_RUNNING') + ' ' + intToStr( imageIndex + 1 ) + '/' + intToStr( Length( imagesAry ) );
  FForm.Caption               := globalization.getLocale('FORM_CAPTION') + ' : ' + lbl_pictureInfos.Caption;
  img_preview.Picture.LoadFromFile( imagesAry[imageIndex] );
end;

procedure TMainForm.ResizeError(const message: string);
begin
  ShowMessage(message);
end;

procedure TMainForm.ResizeComplete;
begin
  resizeThread.Terminate;
  resizeThread := nil;

  FImageIndex                := 0;
  loadingProgressBar.Visible := false;
  lbl_pictureInfos.Caption   := globalization.GetLocale('RESIZING_TERMINATED');
  btn_run.Caption            := globalization.getLocale('RUN_BTN');
  FForm.Caption              := globalization.getLocale('FORM_CAPTION');

  pgeCtrl_main.Enabled       := true;
end;

procedure TMainForm.isNumberKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9']) then
    Key := #0;
end;

{
    Browse folder
}
procedure TMainForm.btn_browseFolderClick(Sender: TObject);
begin
  if SelectDirectory(globalization.getLocale('CHOOSE_DIR'),
    ExtractFilePath(ParamStr(0)), imagesDirectory) then
  begin
    ed_picturesFolder.Text := IncludeTrailingPathDelimiter( imagesDirectory );
    loadImagesFromDirectory(imagesDirectory);
  end;
end;

{
 Progress bar
}
procedure TMainForm.initLoadingBarToMaxValue(const maxValue: integer);
begin
    loadingProgressBar.Position := 0;
    loadingProgressBar.Min := 0;
    loadingProgressBar.Step := 1;
    loadingProgressBar.Max := maxValue;
    Application.ProcessMessages;
end;

procedure TMainForm.refreshLoadingBarToPosition( const newPosition : integer );
begin
    loadingProgressBar.Position := newPosition;
    Application.ProcessMessages;
end;

{
    Last Picture
}
procedure TMainForm.lastPicture;
begin
  if FImageIndex > 0 then
  begin
    Dec(FImageIndex);
    refreshPictureViewer;
  end;
end;

procedure TMainForm.btn_leftPicClick(Sender: TObject);
begin
  lastPicture;
end;

{
    Next picture
}
procedure TMainForm.btn_rightPicClick(Sender: TObject);
begin
  nextPicture;
end;

procedure TMainForm.nextPicture;
begin
  if FImageIndex < Length( imagesAry ) - 1 then
  begin
    Inc(FImageIndex);
    refreshPictureViewer;
  end;
end;

{
    Resolution change profile
}
procedure TMainForm.cb_resizeProfilesChange(Sender: TObject);
begin
  ed_width.Caption := IntToStr(resizeProfiles.Items[cb_resizeProfiles.ItemIndex]^.Width);
  ed_height.Caption := IntToStr(resizeProfiles.Items[cb_resizeProfiles.ItemIndex]^.Height);
end;

{
    Loading picture on grid row clic
}
procedure TMainForm.filesStrGridSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
begin

  if (pgeCtrl_main.TabIndex = 0) and (aCol = 1) then
  begin
    FImageIndex := aRow - 1;
    refreshPictureViewer();
  end;

  with TGridCracker(Sender as TStringGrid) do
  begin
    InvalidateRow(Row);
    InvalidateRow(aRow);
  end;

end;

procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i:integer;
begin
  if DirectoryExists(FileNames[0]) then
  begin
     loadImagesFromDirectory(FileNames[0]);
  end else begin
    Setlength(imagesAry, Length(FileNames));
    for i:=0 to High(FileNames) do
        imagesAry[i] := FileNames[i];
  end;

  if Length(imagesAry) > 0 then begin
     ed_picturesFolder.Text := FileNames[0];
  end;

end;

{
     Loading images in a folder
}
procedure TMainForm.loadImagesFromDirectory(const PathName: string);
var
  i: integer;
  imagesList: TStringList;
begin
  Assert(Assigned(imagesAry));

  imagesList := TStringList.Create;

  try
       FileSearch(PathName, '.jpg;.png;.gif;.bmp;.ico;.tga;.tif;', check_recursive.Checked, imagesList);
       SetLength(imagesAry, imagesList.Count);

       for i:=0 to imagesList.count -1 do imagesAry[i] := imagesList.Strings[i];
  finally
       imagesList.Free;
  end;

  refreshGrid;

  if Length( imagesAry ) = 0 then
    ShowMessage(globalization.getLocale('DIR_IS_EMPTY'));

end;

procedure TMainForm.refreshGrid;
var
  i:integer;
begin

  lbl_waitPics.Caption := '';
  refreshTabCount;

  grid_picturesFiles.RowCount := Length(imagesAry) + 1;

  for i := 0 to High(imagesAry) do
  begin

    with grid_picturesFiles do
    begin
      Cells[0, i + 1] := '1';                                                 // 1 checked, 0 unchecked
      Cells[1, i + 1] := ExtractFileName(imagesAry[i]);                       // Nom
      Cells[2, i + 1] := FormatByteSize(FileSize(imagesAry[i]));              // Taille
      Cells[3, i + 1] := dateTimeToStr(TimeModificationFichier(imagesAry[i]));// Date
      Cells[4, i + 1] := imagesAry[i];                                        // Chemin vers le fichier
    end;

  end;

  FImageIndex := 0;

  pgeCtrl_main.Enabled := Length(imagesAry) > 0;
  btn_run.Enabled := pgeCtrl_main.Enabled;

  refreshPictureViewer;

end;

{
    Refreshed the current image in the preview window
}
procedure TMainForm.refreshPictureViewer;
var
  exif: TExif;
begin
  if assigned(imagesAry) then
  begin

    img_preview.visible := false;
    initLoadingBarToMaxValue( 5 );

    loadingProgressBar.Visible := true;

    lbl_pictureInfos.Caption := globalization.getLocale('LOADING') + ' ' + intToStr( FImageIndex + 1 ) + '/' + intToStr( Length(imagesAry) );
    refreshLoadingBarToPosition(1);

    img_preview.Picture.LoadFromFile( imagesAry[FImageIndex] );
    refreshLoadingBarToPosition(2);

    lbl_pictureInfos.Caption := format(globalization.getLocale('PIC_INFO_SIZES'), [img_preview.Picture.Width, img_preview.Picture.Height]);

    exif := TExif.Create;
    refreshLoadingBarToPosition(3);

    exif.ReadFromFile( imagesAry[ FImageIndex ] );

    refreshLoadingBarToPosition(4);

    lbl_pictureInfos.Caption := lbl_pictureInfos.Caption + sLineBreak + format(globalization.getLocale('PIC_INFO_EXIF'), [exif.Model, exif.DateTime]);
    refreshLoadingBarToPosition(5);

    btn_leftPic.Visible := (FImageIndex > 0);
    btn_rightPic.Visible := (FImageIndex < Length(imagesAry) - 1);

    loadingProgressBar.Visible := false;
    img_preview.visible := true;
    exif.Free;

  end;
end;

end.
