{$I OVC.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFNDEF Win32}
{$G+} {286 Instructions}
{$N+} {Numeric Coprocessor}

{$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

{*********************************************************}
{*                   OVCSFPE.PAS 2.15                    *}
{*     Copyright (c) 1995-97 TurboPower Software Co      *}
{*                 All rights reserved.                  *}
{*********************************************************}


unit OvcSfPe;
  {-Simple field property editor}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Buttons, Classes, Controls, DsgnIntf, Graphics, Forms, StdCtrls, SysUtils,
  OvcConst, OvcData, OvcHelp;

type
  TOvcfrmSimpleMask = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    cbxMaskCharacter: TComboBox;
    lblPictureChars: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure cbxMaskCharacterChange(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  protected
    { Private declarations }
    Mask : Char;
  end;

type
  {property editor for picture mask}
  TSimpleMaskProperty = class(TCharProperty)
  public
    function GetAttributes: TPropertyAttributes;
      override;
    function AllEqual: Boolean;
      override;
    procedure Edit;
      override;
  end;


implementation


uses
  OvcSf, OvcAe, OvcTCSim;

{$R *.DFM}

procedure TOvcfrmSimpleMask.FormCreate(Sender: TObject);
var
  I : Word;
begin
  {load mask character strings}
  for I := stsmFirst to stsmLast do
    cbxMaskCharacter.Items.Add(GetOrphStr(I));
end;

procedure TOvcfrmSimpleMask.cbxMaskCharacterChange(Sender: TObject);
begin
  {return the mask character}
  with cbxMaskCharacter do
    Mask := Items[ItemIndex][1];
end;


{*** TSimpleMaskProperty ***}

type
  TLocalSF = class(TOvcCustomSimpleField);

function TSimpleMaskProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paMultiSelect]
end;

function TSimpleMaskProperty.AllEqual: Boolean;
begin
  Result := True;
end;

procedure TSimpleMaskProperty.Edit;
var
  SfPE : TOvcfrmSimpleMask;
  I, J : Integer;
  C    : TComponent;
begin
  SfPE := TOvcfrmSimpleMask.Create(Application);
  try
    C := TComponent(GetComponent(0));
    if C is TOvcCustomSimpleField then
      SfPE.Mask := TLocalSF(C).PictureMask
    else if C is TOvcSimpleArrayEditor then
      SfPE.Mask := TOvcSimpleArrayEditor(C).PictureMask
    else if C is TOvcTCSimpleField then
      SfPE.Mask := TOvcTCSimpleField(C).PictureMask;

    J := -1;

    {if only one field is selected select the combo box item}
    {that corresponds to the current mask character}
    if PropCount = 1 then begin
      with SfPE.cbxMaskCharacter do begin
        for I := 0 to Items.Count-1 do begin
          if Items[I][1] = SfPE.Mask then begin
            J := I;
            Break;
          end;
        end;
        ItemIndex := J;
      end;
    end;

    {show the form}
    SfPE.ShowModal;

    if SfPe.ModalResult = idOK then begin
      {update all selected components with new mask}
      for I := 1 to PropCount do begin
        C := TComponent(GetComponent(I-1));
        if C is TOvcCustomSimpleField then
          TLocalSF(C).PictureMask := SfPE.Mask
        else if C is TOvcSimpleArrayEditor then
          TOvcSimpleArrayEditor(C).PictureMask := SfPE.Mask
        else if C is TOvcTCSimpleField then
          TOvcTCSimpleField(C).PictureMask := SfPE.Mask;
      end;
      Modified;
    end;
  finally
    SfPE.Free;
  end;
end;

procedure TOvcfrmSimpleMask.btnHelpClick(Sender: TObject);
begin
  ShowHelpContext(hcSimpleFieldMask);
end;

end.
