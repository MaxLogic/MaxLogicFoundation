unit MaxLogic.BalloonDefaultImageList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList;

type
  TImageListForBalloonForm = class(TForm)
    ImageList1: TImageList;
  private
    class var fInstance: TImageListForBalloonForm;
  private
    fOrgOnFormChange: TNotifyEvent;
    procedure init;
    procedure myOnFormChange(sender: Tobject);
    procedure processForm(f: TForm);
  public
    destructor Destroy; override;
    class function Instance: TImageListForBalloonForm; static;
  public const
    imgInfo = 0; // never change
    imgWarning = 1;
  end;

  // just a marker to be added to the forms that we did already processed, this will help us to not check all the components each time again and again
  TIsAdjustedMarker = class(Tcomponent)

  end;

implementation

{$R *.dfm}

{ TImageListForBalloonForm }

destructor TImageListForBalloonForm.Destroy;
begin
  try
    if assigned(screen) then
      screen.OnActiveFormChange := fOrgOnFormChange;
  except
    // do nothing
  end;
  inherited;
end;

procedure TImageListForBalloonForm.init;
begin
  fOrgOnFormChange := screen.OnActiveFormChange;
  screen.OnActiveFormChange := myOnFormChange;

  if screen.ActiveForm <> nil then
    processForm(screen.ActiveForm);
end;

class function TImageListForBalloonForm.Instance: TImageListForBalloonForm;
begin
  if fInstance = nil then
  begin
    fInstance := TImageListForBalloonForm.create(application);
    fInstance.init;
  end;

  result := fInstance;
end;

procedure TImageListForBalloonForm.myOnFormChange(sender: Tobject);
begin
  try
    if sender <> nil then
      if sender is TForm then
        processForm(sender as TForm);
  finally
    if assigned(fOrgOnFormChange) then
      fOrgOnFormChange(sender);
  end;

end;

procedure TImageListForBalloonForm.processForm(f: TForm);
var
  x: Integer;
  c: Tcomponent;
  b: TBalloonHint;
begin

  // find balloon hints or the marker
  for x := f.ComponentCount - 1 to 0 do
  begin
    c := f.Components[x];
    if c is TIsAdjustedMarker then
      exit; // we have been here already

    if c is TBalloonHint then
    begin
      b := c as TBalloonHint;
      if b.Images = nil then
      begin
        b.Images := self.ImageList1;
        b.ImageIndex := 0;
      end;
    end;
  end;

  // set the marker so we do not scan this form again
  // Note: it will be destroyed with the form, so there is no need to hold a reference to it
  TIsAdjustedMarker.create(f);
end;

end.
