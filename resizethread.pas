unit ResizeThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Exif, BGRABitmap, BGRABitmapTypes, Helpers, Typinfo, Math;

type
TNameType = (SAME, CUSTOM, EXIF_DATA);
TExifDataType = (MODEL, DATE, RESOLUTION);
TpictureFormat = (PNG, JPG, BMP, TIFF, GIF);

TResizeParameters = record
   sourceImageFilePath, outputFolderName, outputFileName: string;
   pictureFormat: TpictureFormat;
   usedExifTypeIndex, width, height: integer;
   preserveRatio, detectRotation, overiteFile: boolean;
end;

TStartProcedure    = procedure of Object;
TCompleteProcedure = procedure of Object;
TErrorProcedure    = procedure(const message: string) of Object;
TProgressProcedure = procedure(const index: integer) of Object;

TResizeThread = class(TThread)

  private
    FImageIndex: Integer;
    FImagesCount: Integer;

    procedure DoProgress;
    procedure Error(const message: string);
    procedure Complete;
    procedure SetCountTo(const Value: Integer);
    procedure ThreadDone(Sender: TObject);
    procedure resizePicture(const parameters: TResizeParameters);

  protected
    procedure Execute; override;

  public
    ImagesDirectory, CustomFileName, CustomFolderName,
    DateErrorFolder, outputExt                              : string;
    ImagesAry                                               : array of string;
    ResizeWidth, ResizeHeight                               : integer;
    PreserveRatio, DetectRotation                           : boolean;
    RenameType, FolderType                                  : TNameType;
    RenameExifDataType, FolderExifDataType                  : TExifDataType;
    OnComplete                                              : TCompleteProcedure;
    OnProgress                                              : TProgressProcedure;
    OnError                                                 : TErrorProcedure;

    constructor Create;
    destructor Destroy; override;

    property CountTo: Integer read FImagesCount write SetCountTo;

end;

implementation

constructor TResizeThread.Create;
begin
  FreeOnTerminate := true;
  FImageIndex := 0;
  inherited Create(false);
end;

destructor TResizeThread.Destroy;
begin
  inherited Destroy;
end;

procedure TResizeThread.SetCountTo(const Value: Integer) ;
begin
 FImagesCount := Value;
end;

procedure TResizeThread.Execute;
var
  resizeParameters: TResizeParameters;
  imageFilePath : string;
  exif: Texif;

  function exifDateTime(strDateTime: string) : string;
  var
    Fmt : TFormatSettings;
    strDay, strMonth, strYear, monthFullName: string;
    date: TDateTime;
  begin
    if strDateTime = '' then begin

      Result := DateErrorFolder;

    end else begin

      fmt.ShortDateFormat := 'yyyy:mm:dd';
      fmt.DateSeparator   := ':';
      fmt.LongTimeFormat  := 'hh:nn:ss';
      fmt.TimeSeparator   := ':';

      date                := strtodatetime(strDateTime, Fmt);
      strDay              := formatdatetime('dd', date );
      strMonth            := formatdatetime('mm', date );
      strYear             := formatdatetime('yyyy', date );

      monthFullName       := ShortMonthNames[ strToInt(strMonth) ];
      Result              := strDay + ' ' + monthFullName + ' ' + strYear;

    end;
  end;
begin

  exif                    := TExif.Create;
  FImagesCount            := Length( ImagesAry );

  try

    while not(Terminated) and (FImageIndex < FImagesCount) do
    begin

      Synchronize(@DoProgress);

      imageFilePath := ImagesAry[ FImageIndex ];

      exif.ReadFromFile( imageFilePath );

      resizeParameters.sourceImageFilePath               := imageFilePath;
      resizeParameters.width                             := ResizeWidth;
      resizeParameters.height                            := ResizeHeight;
      resizeParameters.preserveRatio                     := PreserveRatio;
      resizeParameters.detectRotation                    := DetectRotation;

      {FILE}
      if RenameType = TNameType.SAME  then begin

         resizeParameters.outputFileName := ExtractFileName( imageFilePath );
         resizeParameters.overiteFile := true;

      end else if RenameType = TNameType.CUSTOM then begin

          resizeParameters.outputFileName := CustomFileName + IntToStr( FImageIndex ) + ExtractFileExt(imageFilePath);
          resizeParameters.overiteFile := false;

      end else if RenameType = TNameType.EXIF_DATA  then begin

        if RenameExifDataType = TExifDataType.MODEL then resizeParameters.outputFileName := exif.Model
        else if RenameExifDataType = TExifDataType.DATE then resizeParameters.outputFileName := exifDateTime(exif.DateTime)
        else resizeParameters.outputFileName := IntToStr(exif.XResolution) + 'x' + IntToStr(exif.YResolution);

        if resizeParameters.outputFileName = '' then resizeParameters.outputFileName := ExtractFileName( imageFilePath )
        else resizeParameters.outputFileName := resizeParameters.outputFileName + '_' + IntToStr( FImageIndex ) + ExtractFileExt(imageFilePath);

        resizeParameters.overiteFile := false;

      end;

      {FOLDER}
      if FolderType = TNameType.SAME then begin

         resizeParameters.outputFolderName := '';

      end else if FolderType = TNameType.CUSTOM then begin

          resizeParameters.outputFolderName := CustomFolderName;

      end else if FolderType = TNameType.EXIF_DATA then begin

        if FolderExifDataType = TExifDataType.MODEL then resizeParameters.outputFolderName := exif.Model
        else if FolderExifDataType = TExifDataType.DATE then resizeParameters.outputFolderName := exifDateTime(exif.DateTime)
        else resizeParameters.outputFolderName := IntToStr(exif.XResolution) + 'x' + IntToStr(exif.YResolution);

      end;

      if outputExt <> '' then begin
         resizeParameters.outputFileName := StringReplace(resizeParameters.outputFileName, ExtractFileExt(resizeParameters.outputFileName), outputExt, [rfReplaceAll, rfIgnoreCase]);
      end;

      resizePicture( resizeParameters );

      inc(FImageIndex);

  end;
  except
    on E : Exception do begin
      Error(E.Message);
    end;
  end;

  exif.Free;

  Synchronize(@Complete);

end;

procedure TResizeThread.resizePicture(const parameters: TResizeParameters);
var
  sourceTex : TBGRABitmap;
  destinationFile, destinationFolder, fileName: string;
  widthRatio, heightRatio, ratio : double;
  destWidth, destHeight : integer;

begin

  sourceTex := TBGRABitmap.Create(parameters.sourceImageFilePath);

      destWidth                := parameters.width;
      destHeight               := parameters.height;

  if parameters.detectRotation and (destHeight > destWidth) then begin

      destWidth                := parameters.height;
      destHeight               := parameters.width;

  end;

  if parameters.preserveRatio then begin

     widthRatio                := destWidth  / sourceTex.Width;
     heightRatio               := destHeight  / sourceTex.Height;
     ratio                     := min( widthRatio, heightRatio );
     destWidth                 := round(sourceTex.Width * ratio);
     destHeight                := round(sourceTex.Height * ratio);

  end;

  BGRAReplace(sourceTex, sourceTex.Resample(destWidth, destHeight));

  if parameters.outputFolderName <> '' then begin

    destinationFolder := ConcatPaths([ExtractFilePath(parameters.sourceImageFilePath), parameters.outputFolderName]);
    if not DirectoryExists(destinationFolder) then ForceDirectories(destinationFolder);

  end else begin

    destinationFolder := ExtractFilePath(parameters.sourceImageFilePath);

  end;

  if not parameters.overiteFile then begin

     fileName := GetNextFileName(destinationFolder, parameters.outputFileName);

  end else begin

    fileName := parameters.outputFileName;

  end;

  destinationFile := ConcatPaths([destinationFolder, fileName]);

  sourceTex.SaveToFile(destinationFile);
  sourceTex.Free;

end;

procedure TResizeThread.DoProgress;
begin
     if Assigned(OnProgress) then OnProgress(FImageIndex);
end;

procedure TResizeThread.Error(const message: string);
begin
     if Assigned(OnError) then OnError(message);
end;

procedure TResizeThread.Complete;
begin
     if Assigned(OnComplete) then OnComplete;
end;

procedure TResizeThread.ThreadDone(Sender: TObject);
begin
  if Assigned(OnComplete) then OnComplete;
end;

end.

