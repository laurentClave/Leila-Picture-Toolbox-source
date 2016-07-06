unit globalizationDatas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, TypInfo, Variants, Regexpr, Grids, Dialogs, Translations, gettext;

Resourcestring
  {All}
  rs_appName                = 'Leila Picture Toolbox';
  rs_tabPictureList         = 'Images list';
  rs_tabResize              = 'Resizing';
  rs_tabDestination         = 'Destination';
  rs_tabFileFormat          = 'Format';

  rs_resizeProfile          = 'Profile';
  rs_resizeWidth            = 'Width';
  rs_resizeHeight           = 'Height';
  rs_resizeSaveRatio        = 'Save ratio';
  rs_resizeDetectRotation   = 'Detect rotation';

  rs_titleName              = 'Name';
  rs_titleDate              = 'Date';
  rs_titleSize              = 'Size';

  rs_destFileName           = 'FileName';
  rs_destRdNoRename         = 'No rename';
  rs_destRdCustomRename     = 'Custom';
  rs_destRdExifRename       = 'Rename by exif';
  rs_destRenameExemple      = 'Resized_';
  rs_destFolderName         = 'Folder';
  rs_destRdSameFolder       = 'Same folder';
  rs_destRdSubFolder        = 'Sub folder';
  rs_destRdExifFolder       = 'Folder by exif';
  rs_destSubFolderExemple   = 'Resized pictures';

  rs_profileThumb           = 'Thumb';
  rs_profileVeryLow         = 'Very Low';
  rs_profileLow             = 'Low';
  rs_profileMail            = 'Mail';
  rs_profileMedium          = 'Medium';
  rs_profileHD              = 'HD';
  rs_profile4K              = '4K';

  rs_destExifModel          = 'Model';
  rs_destExifDate           = 'Date';
  rs_destExifResolution     = 'Resolution';

  rs_formatSame             = 'Same format';
  rs_formatOther            = 'Other format';

  rs_inputFolder            = 'Folder';
  rs_inputBrowse            = 'Browse';
  rs_inputRecursive         = 'Recursive';

  rs_waitingPics            = 'Waiting pictures';

  rs_runBtn                 = 'Start';
  rs_stopBtn                = 'Stop';
  rs_dateErrorFolder        = 'Date error';
  rs_resizingRunning        = 'Resizing running';
  rs_resizingTerminated     = 'Resizing complete';
  rs_chooseDirectory        = 'Select directory';
  rs_dirIsEmpty             = 'Directory is empty';
  rs_loading                = 'Loading...';
  rs_picInfoSizes           = 'Height : %d px - Width : %d px';
  rs_picInfoExif            = 'Model : %s - Date : %s';

type

  { TGlobalization }

  TGlobalization = class

  private
    locales: TStringList;
    function isLocaleTag(var tag: string): boolean;

  public

    constructor Create; overload;
    destructor Free; overload;

    procedure setFormLocales(form: TForm);
    function getLocale(tag: string): string;
    function getTag(locale: string): string;
  end;

implementation


{
    Indique si une chaîne est définie dans la traduction
}
function TGlobalization.isLocaleTag(var tag: string): boolean;
var
  RegEx: TRegExpr;
  tagTmp: string;
begin
  tagTmp := '';
  RegEx := TRegExpr.Create;
  try
    Regex.Expression := '(\[)([A-Za-z0-9_ ]+)(\])';
    if Regex.Exec(tag) then
    begin
      tagTmp := Regex.Match[2];
      tag := getLocale(tagTmp);
      Result := True;
    end
    else
      Result := False;
  finally
    RegEx.Free;
  end;
end;

constructor TGlobalization.Create;
var
  PODirectory, Lang, FallbackLang : String;
begin
  PODirectory:= IncludeTrailingPathDelimiter('lang');
  Lang := '';
  FallbackLang := '';
  GetLanguageIDs(Lang, FallbackLang);
  Translations.TranslateUnitResourceStrings('globalizationDatas', PODirectory + 'locale_%s.po', Lang, FallbackLang);

  locales := TStringList.Create;

  with locales do
  begin

    Values['FORM_CAPTION']                  := rs_appName;

    // Tabs
    Values['PICTURES_LIST_TAB']             := rs_tabPictureList;
    Values['RESIZE_TAB']                    := rs_tabResize;
    Values['DESTINATION_TAB']               := rs_tabDestination;
    Values['FORMAT_TAB']                    := rs_tabFileFormat;

    // Tab picture list
    Values['TITLE_NAME']                    := rs_titleName;
    Values['TITLE_SIZE']                    := rs_titleSize;
    Values['TITLE_DATE']                    := rs_titleDate;

    // Tab resize
    Values['RESIZE_PROFILE']                := rs_resizeProfile;
    Values['RESIZE_WIDTH']                  := rs_resizeWidth;
    Values['RESIZE_HEIGHT']                 := rs_resizeHeight;
    Values['RESIZE_SAVE_RATIO']             := rs_resizeSaveRatio;
    Values['RESIZE_DETECT_ROTATION']        := rs_resizeDetectRotation;
    // Tab resize -> Profiles
    Values['RES_THUMB']                     := rs_profileThumb;
    Values['RES_VERY_LOW']                  := rs_profileVeryLow;
    Values['RES_LOW']                       := rs_profileLow;
    Values['RES_MAIL']                      := rs_profileMail;
    Values['RES_MEDIUM']                    := rs_profileMedium;
    Values['RES_HD']                        := rs_profileHD;
    Values['RES_4K']                        := rs_profile4K;

    // Tab destination
    Values['DEST_GRP_FILENAME']             := rs_destFileName;
    Values['DEST_RD_NO_RENAME']             := rs_destRdNoRename;
    Values['DEST_RD_CUSTOM_RENAME']         := rs_destRdCustomRename;
    Values['DEST_RD_EXIF_RENAME']           := rs_destRdExifRename;
    Values['DEST_RENAME_EXEMPLE']           := rs_destRenameExemple;
    Values['DEST_GRP_FOLDER']               := rs_destFolderName;
    Values['DEST_RD_SAME_FOLDER']           := rs_destRdSameFolder;
    Values['DEST_RD_SUB_FOLDER']            := rs_destRdSubFolder;
    Values['DEST_RD_EXIF_FOLDER']           := rs_destRdExifFolder;
    Values['DEST_SUB_FOLDER_EXEMPLE']       := rs_destSubFolderExemple;

    Values['EXIF_MODEL']                    := rs_destExifModel;
    Values['EXIF_DATE']                     := rs_destExifDate;
    Values['EXIF_RESOLUTION']               := rs_destExifResolution;

    // Tab format
    Values['FORMAT_RD_SAME']                := rs_formatSame;
    Values['FORMAT_RD_OTHER']               := rs_formatOther;

    // Input
    Values['INPUT_FOLDER']                  := rs_inputFolder;
    Values['INPUT_BROWSE']                  := rs_inputBrowse;
    Values['INPUT_RECURSIVE']               := rs_inputRecursive;

    // Other
    Values['WAITING_PICS']                  := rs_waitingPics;
    Values['DATE_ERROR_FOLDER']             := rs_dateErrorFolder;
    Values['RESIZING_RUNNING']              := rs_resizingRunning;
    Values['RESIZING_TERMINATED']           := rs_resizingTerminated;
    Values['CHOOSE_DIR']                    := rs_chooseDirectory;
    Values['DIR_IS_EMPTY']                  := rs_dirIsEmpty;
    Values['LOADING']                       := rs_loading;
    Values['PIC_INFO_SIZES']                := rs_picInfoSizes;
    Values['PIC_INFO_EXIF']                 := rs_picInfoExif;
    Values['RUN_BTN']                       := rs_runBtn;
    Values['STOP_BTN']                      := rs_stopBtn;

  end;
end;

destructor TGlobalization.Free;
begin
  if Assigned(locales) then locales.Free;
end;

{
    Change les valeurs visuelles d'un composant par les valeurs traduites
    Fonctionne avec les propriétés Caption, Text et Title d'un TGridColumns d'un TStringGrid
}
procedure TGlobalization.setFormLocales(form: TForm);
var
  i, c, d: integer;
  PropInfo: PPropInfo;
  Caption, localizedCaption: string;
  comp: TComponent;
  Columns: TGridColumns;
  props: array[0..2] of string = ('Caption', 'Text', 'TStringGrid');
begin
  // Boucle sur chaque composant du formulaire
  for i := form.ComponentCount - 1 downto 0 do
  begin
    comp := form.Components[i];
    for c := length(props) - 1 downto 0 do
    begin
      PropInfo := GetPropInfo(comp.ClassInfo, props[c]);
      // Le composant est une colonne de tableau
      if comp.ClassType.ClassName = 'TStringGrid' then
      begin
        try
          Columns := (comp as TStringGrid).Columns;
          // On boucle sur toute les colonnes
          for d := Columns.Count - 1 downto 0 do
          begin
            Caption := Columns[d].title.Caption;
            if isLocaletag(Caption) then
            begin
              Columns[d].title.Caption := Caption;
            end;
          end;
        except
          on e: Exception do
          begin
            ShowMessage('setFormLocale error : ' + e.message +
              ' Composant : ' + comp.ClassType.ClassName);
          end;
        end;

      end
      // Le composant est un composant classique comprenant un Caption ou un Text
      else if PropInfo <> nil then
      begin
        // On extrait la valeur de la propriété
        Caption := varToStr(GetPropValue(comp, props[c]));
        if isLocaletag(Caption) then
        begin
          // On la traduit
          localizedCaption := Caption;
          // et on la valorise dans la propriété
          SetStrProp(comp, PropInfo, localizedCaption);
        end;

      end;

    end;
  end;
end;

{
 Renvoi la chaîne avec la traduction définite
}
function TGlobalization.getLocale(tag: string): string;
begin
  if tag = '' then Result := ''
  else if locales.IndexOfName(tag) > -1 then
    Result := locales.Values[ tag ]
  else
    Result := '[noLocale=' + tag + ']';
end;

{
 Renvoi le tag de la chaîne traduite
}
function TGlobalization.getTag(locale: string): string;
var
  Index: Integer;
begin
  if locale = '' then
     Result := ''
  else if locales.Find(locale, Index) then
       Result := intToStr(Index);//locales[Index];
end;



end.
