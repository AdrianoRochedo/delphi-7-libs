{*****************************************}
{   TeeChart-Pro 4.0  TChartListBox class }
{   Copyright (c) 1995-98 David Berneda   }
{       Component Registration Unit       }
{*****************************************}
{$I teedefs.inc}
unit TeeLisB;

interface

Uses WinTypes,WinProcs,Messages,StdCtrls,Chart,Classes,Graphics,Teengine,
     Forms,Menus,Controls;

type TChartListBox=class;

     TDblClickSeriesEvent=procedure(Sender:TChartListBox; Index:Integer) of object;

     PListBoxSection=^TListBoxSection;
     TListBoxSection=record
       Width   : Integer;
       Visible : Boolean;
     end;

     TListBoxSections=class(TList)
     private
       function GetSection(Index:Integer):TListBoxSection;
       Procedure SetSection(Index:Integer; Const Value:TListBoxSection);
     public
       Procedure AddSection(AWidth:Integer);
       Function SectionLeft(Index:Integer):Integer;
       property Section[Index:Integer]:TListBoxSection read GetSection
                          write SetSection; {$IFNDEF D1}default;{$ENDIF}
     end;

     TChartListBox=class(TCustomListBox)
     private
       FActiveSeries     : TCheckBox;
       FAllowDelete      : Boolean;
       FAskDelete        : Boolean;
       FChart            : TCustomChart;
       FHitTest          : TPoint;
       FSections         : TListBoxSections;
       FOnEditSeries     : TDblClickSeriesEvent;
       FOtherItems       : TStrings;
       FOtherItemsChange : TNotifyEvent;
       FRefresh          : TNotifyEvent;
       FSeriesBitmap     : TBitmap;
       ComingFromDoubleClick:Boolean;
       procedure LBSeriesClick(Sender: TObject);
       procedure LBSeriesDragDrop(Sender, Source: TObject; X,Y: Integer);
       procedure LBSeriesDrawItem(Control: TWinControl; Index: Integer;
                                           Rect: TRect; State: TOwnerDrawState);
       procedure LBSeriesDragOver( Sender, Source: TObject; X,
                                   Y: Integer; State: TDragState; var Accept: Boolean);
       procedure LBSeriesKeyUp(Sender: TObject; var Key: Word;
                                       Shift: TShiftState);
       procedure LBSeriesMouseDown( Sender: TObject;
                                    Button: TMouseButton; Shift: TShiftState;
                                    X, Y: Integer);
       procedure DoDblClick(Sender: TObject);
       procedure DoRefresh;
       procedure SetChart(Value:TCustomChart);
       procedure SetSectionVisible(Index:Integer; Value:Boolean);
     protected
       procedure Notification(AComponent: TComponent;
                              Operation: TOperation); override;
       procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
       procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
     public
       Constructor Create(AOwner:TComponent); override;
       Destructor Destroy; override;

       property ActiveCheckBox:TCheckBox read FActiveSeries;
       Function AddSeriesGallery:TChartSeries;
       procedure ChangeTypeSeries(Sender: TObject);
       procedure ClearItems;
       Procedure CloneSeries;
       Procedure DeleteSeries;
       procedure FillSeries(OldSeries:TChartSeries);
       Procedure MoveCurrentDown;
       Procedure MoveCurrentUp;
       property OtherItems:TStrings read FOtherItems write FOtherItems;
       Function PointInSection(Const P:TPoint; ASection:Longint):Boolean;
       Procedure RenameSeries;
       Function SeriesAtMousePos(Var p:TPoint):Longint;
       procedure SwapSeries(tmp1,tmp2:Longint);
       Function SelectedSeries:TChartSeries;
       property Sections:TListBoxSections read FSections;
       Function GetShowActive:Boolean;
       Procedure SetShowActive(Value:Boolean);
       Function GetShowIcon:Boolean;
       Procedure SetShowIcon(Value:Boolean);
       Function GetShowColor:Boolean;
       Procedure SetShowColor(Value:Boolean);
       Function GetShowTitle:Boolean;
       Procedure SetShowTitle(Value:Boolean);
     published
       property AllowDeleteSeries : Boolean read FAllowDelete
                                  write FAllowDelete default True;
       property AskDelete:Boolean read FAskDelete write FAskDelete
                                  default True;
       property Chart:TCustomChart read FChart write SetChart;
       property OnOtherItemsChange:TNotifyEvent read FOtherItemsChange
                                              write FOtherItemsChange;
       property OnDblClickSeries:TDblClickSeriesEvent read FOnEditSeries
                                                  write FOnEditSeries;
       property OnRefresh:TNotifyEvent read FRefresh write FRefresh;
       property ShowActiveCheck:Boolean read GetShowActive
                                        write SetShowActive default True;
       property ShowSeriesColor:Boolean read GetShowColor
                                           write SetShowColor default True;
       property ShowSeriesIcon:Boolean read GetShowIcon
                                           write SetShowIcon default True;
       property ShowSeriesTitle:Boolean read GetShowTitle
                                           write SetShowTitle default True;

       property Align;
       property BorderStyle;
       property Color;
       property Ctl3D;
       property Enabled;
       property ExtendedSelect;
       property Font;
       {$IFDEF D3}
       property ImeMode;
       property ImeName;
       {$ENDIF}
       property MultiSelect;
       property ParentColor;
       property ParentCtl3D;
       property ParentFont;
       property ParentShowHint;
       property PopupMenu;
       property ShowHint;
       property TabOrder;
       property TabStop;
       property Visible;
       property OnEnter;
       property OnExit;
       property OnKeyDown;
       property OnKeyPress;
     end;

procedure MoveList(Source,Dest:TListBox);
procedure MoveListAll(Source,Dest:TListBox);

implementation

{$IFDEF D1}
{$R TEEBMPS.R16}
{$ELSE}
{$R TEEBMPS.RES}
{$ENDIF}

Uses PenDlg,TeeGally, Dialogs, TeeConst, SysUtils,TeeProcs;

{ TListBoxSections }
Function TListBoxSections.SectionLeft(Index:Integer):Integer;
var t:Integer;
begin
  result:=0;
  for t:=1 to Index do
  With Section[t-1] do
  if Visible then Inc(result,Width);
end;

Procedure TListBoxSections.AddSection(AWidth:Integer);
Var P:^TListBoxSection;
begin
  New(P);
  P^.Visible:=True;
  P^.Width:=AWidth;
  Add(P);
end;

function TListBoxSections.GetSection(Index:Integer):TListBoxSection;
begin
  result:=TListBoxSection(inherited Items[Index]^);
end;

Procedure TListBoxSections.SetSection(Index:Integer; Const Value:TListBoxSection);
begin
  While Count<Index do Add(nil);
  TListBoxSection(inherited Items[Index]^):=Value;
end;


{ TChartListBox }
Constructor TChartListBox.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  ComingFromDoubleClick:=False;

  FSeriesBitmap:=TBitmap.Create;
  FActiveSeries:=TCheckBox.Create(nil);
  With FActiveSeries do
  begin
    ParentColor := True;
    Caption:='';
    Height:=12;
    Width:=Height;
    Visible:=False;
  end;
  FSections:=TListBoxSections.Create;
  FSections.AddSection(26);
  FSections.AddSection(16);
  FSections.AddSection(26);
  FSections.AddSection(216);

  OnDragDrop:=LBSeriesDragDrop;
  OnDrawItem:=LBSeriesDrawItem;
  OnDragOver:=LBSeriesDragOver;
  OnMouseDown:=LBSeriesMouseDown;
  OnClick:=LBSeriesClick;
  OnDblClick:=DoDblClick;
  OnKeyUp:=LBSeriesKeyUp;
  Style:=lbOwnerDrawFixed;
  ItemHeight:=27;
  Sorted:=False;
  MultiSelect:=True;
  FAskDelete:=True;
  FAllowDelete:=True;
end;

Destructor TChartListBox.Destroy;
var t:Integer;
begin
  {$IFNDEF D1}
  FActiveSeries.Free;
  {$ENDIF}
  FSeriesBitmap.Free;
  for t:=0 to FSections.Count-1 do
      Dispose(PListBoxSection(FSections.Items[t]));
  FSections.Free;
  inherited Destroy;
end;

procedure TChartListBox.LBSeriesDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var tmp1 : Longint;
    tmp2 : Longint;
begin
  With TCustomListBox(Sender) do
  if ItemIndex<>-1 then
  begin
    tmp1:=ItemIndex;
    tmp2:=ItemAtPos(Point(X,Y),True);
    if (tmp2<>-1) and (tmp1<>tmp2) then SwapSeries(tmp1,tmp2);
  end;
end;

procedure TChartListBox.DoRefresh;
begin
  if Assigned(FRefresh) then FRefresh(Self);
end;

procedure TChartListBox.SetChart(Value:TCustomChart);
begin
  FChart:=Value;
  if Assigned(FChart) then
  begin
    {$IFNDEF D1}
    FChart.FreeNotification(Self);
    {$ENDIF}
    FillSeries(nil);
  end
  else ClearItems;
end;

procedure TChartListBox.ClearItems;
begin
  if not (csDestroying in ComponentState) then
  begin
    Items.Clear;
    if Assigned(FOtherItems) then FOtherItems.Clear;
  end;
end;

procedure TChartListBox.SwapSeries(tmp1,tmp2:Longint);
begin
  if Assigned(FChart) then
  begin
    Items.Exchange(tmp1,tmp2);
    if Assigned(FOtherItems) then FOtherItems.Exchange(tmp1,tmp2);
    FChart.ExchangeSeries(tmp1,tmp2);
    GetParentForm(Self).ActiveControl:=Self;
    Selected[tmp2]:=True;
    Repaint;
    DoRefresh;
  end;
end;

procedure TChartListBox.LBSeriesClick(Sender: TObject);
begin
  DoRefresh;
end;

procedure TChartListBox.LBSeriesDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
Const BrushColors : Array[Boolean] of TColor=(clWindow,clHighLight);
      FontColors  : Array[Boolean] of TColor=(clWindowText,clHighlightText);
var tmp       : Integer;
    tmpSeries : TChartSeries;
    tmpR      : TRect;
    CBRect    : TRect;
    tmpCanvas : TCanvas;
    OldCanvas : TCanvas;
begin
  if FActiveSeries.Parent=nil then
  begin
    FActiveSeries.Left   :=Left;
    FActiveSeries.Top    :=Top;
    FActiveSeries.Parent :=Parent;
    FActiveSeries.Visible:=True;
    FActiveSeries.SendToBack;
  end;

  tmpCanvas:=Canvas;
  With tmpCanvas do
  begin
    if odSelected in State then Brush.Color:=clHighLight
                           else Brush.Color:=Self.Color;
    FillRect(Rect);

    Brush.Color:=Self.Color;
    Brush.Style:=bsSolid;
    tmpR.Top    :=Rect.Top;
    tmpR.Bottom :=Rect.Bottom;
    tmpR.Left   :=Rect.Left;
    tmpR.Right  :=Sections.SectionLeft(3)-4;
    FillRect(tmpR);

    tmpSeries:=FChart[Index];

    if ShowSeriesIcon then
    begin
      tmpSeries.GetBitmapEditor(FSeriesBitmap);
      {$IFDEF D3}
      FSeriesBitmap.Transparent:=True;
      {$ENDIF}
      Draw(Sections.SectionLeft(0),Rect.Top+2,FSeriesBitmap);
    end;

    if ShowSeriesColor and (not tmpSeries.ColorEachPoint) then
    begin
      tmp:=Sections.SectionLeft(2)-2;
      tmpR:=Classes.Rect(tmp,Rect.Top,tmp+Sections.Section[2].Width,Rect.Bottom);
      InflateRect(tmpR,-4,-4);

      Brush.Style:=bsSolid;
      Brush.Color:=FChart.Legend.Color;
      With FChart do
      begin
        OldCanvas:=Canvas.ReferenceCanvas;
        Canvas.ReferenceCanvas:=tmpCanvas;
        try
          Series[Index].DrawLegend(-1,tmpR);
        finally
          Canvas.ReferenceCanvas:=OldCanvas;
        end;
      end;
    end;

    if ShowActiveCheck then
    begin
      tmp:=Sections.SectionLeft(1);
      CBRect:=Classes.Rect(tmp,Rect.Top+6,tmp+12,Rect.Top+18);

      {$IFDEF D1}
      Brush.Style:=bsClear;
      SetBkMode(Handle,Transparent);
      Pen.Color:=clBlack;
      Pen.Style:=psSolid;
      With CBRect do Rectangle(Left,Top,Right,Bottom);
      if tmpSeries.Active then
      With CBRect do
      begin
        MoveTo(Left,Top);
        LineTo(Right,Bottom);
        MoveTo(Left,Bottom-1);
        LineTo(Right,Top-1);
      end;
      {$ELSE}
      With FActiveSeries do
      begin
        Checked:=tmpSeries.Active;
        PaintTo(tmpCanvas.Handle,CBRect.Left,CBRect.Top);
      end;
      {$ENDIF}
    end;

    if ShowSeriesTitle then
    begin
      if odSelected in State then Font.Color:=clHighlightText
                             else Font.Color:=Self.Font.Color;
      Brush.Style:=bsClear;
      TextOut(Sections.SectionLeft(3),Rect.Top+((ItemHeight-TextHeight('W')) div 2),Items[Index]);
    end;
  end;
end;

Function TChartListBox.SelectedSeries:TChartSeries;
begin
  if (ItemIndex<>-1) and (ItemIndex<FChart.SeriesCount) then
     result:=TChartSeries(Items.Objects[ItemIndex])
  else
     result:=nil;
end;

procedure TChartListBox.LBSeriesDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=Sender=Source;
end;

Function TChartListBox.SeriesAtMousePos(Var p:TPoint):Longint;
begin
  GetCursorPos(p);
  p:=ScreenToClient(p);
  result:=ItemAtPos(p,True);
end;

Function TChartListBox.PointInSection(Const P:TPoint; ASection:Longint):Boolean;
Var tmpPos : Longint;
begin
  if Sections.Section[ASection].Visible then
  begin
    tmpPos:=Sections.SectionLeft(ASection);
    result:=(p.x>tmpPos) and (p.x<tmpPos+Sections.Section[ASection].Width);
  end
  else result:=False;
end;

procedure TChartListBox.FillSeries(OldSeries:TChartSeries);
var t                : Longint;
    tmpSelectedIndex : Longint;
    tmpSeries        : TChartSeries;
    tmpSt            : String;
Begin
  ClearItems;
  if Assigned(FChart) then
  With FChart do
  Begin
    tmpSelectedIndex:=-1;
    for t:=0 to SeriesCount-1 do
    begin
      tmpSeries:=Series[t];
      if tmpSeries.Title<>'' then tmpSt:=tmpSeries.Title
                             else tmpSt:=tmpSeries.Name;
      Items.AddObject(tmpSt,tmpSeries);
      if Assigned(FOtherItems) then
         FOtherItems.AddObject(tmpSt,tmpSeries);
      if Assigned(OldSeries) and (tmpSeries=OldSeries) then
         tmpSelectedIndex:=t;
    end;
    if tmpSelectedIndex=-1 then
       if SeriesCount>0 then tmpSelectedIndex:=0;
    if tmpSelectedIndex<>-1 then
       Selected[tmpSelectedIndex]:=True
    else
    if Assigned(FOtherItemsChange) then FOtherItemsChange(Self);
    DoRefresh;
  end;
end;

procedure TChartListBox.ChangeTypeSeries(Sender: TObject);
var tmpSeries : TChartSeries;
    NewClass  : TChartSeriesClass;
    t         : Longint;
    FirstTime : Boolean;
begin
  if SelCount>0 then
  begin
    FirstTime:=True;
    NewClass:=nil;
    for t:=0 to Items.Count-1 do
    if Selected[t] then
    begin
      tmpSeries:=FChart[t];
      if FirstTime then
      begin
        ChangeSeriesTypeGallery(tmpSeries.Owner,tmpSeries);
        NewClass:=TChartSeriesClass(tmpSeries.ClassType);
        FirstTime:=False;
      end
      else ChangeSeriesType(tmpSeries,NewClass);
    end;
    FillSeries(tmpSeries);
  end;
end;

procedure TChartListBox.DoDblClick(Sender: TObject);
var p   : TPoint;
    tmp : Longint;
begin
  ComingFromDoubleClick:=True;
  tmp:=SeriesAtMousePos(p);
  if (tmp<>-1) and Selected[tmp] then
  begin
    if PointInSection(p,0) then ChangeTypeSeries(Self)
    else
    if PointInSection(p,2) then
    begin
      With FChart[tmp] do
      if not ColorEachPoint then
      begin
        SeriesColor:=EditColor(Self,SeriesColor);
        Self.Repaint;
      end;
    end
    else
    if PointInSection(p,3) then
       if Assigned(FOnEditSeries) then FOnEditSeries(Self,tmp);
  end;
end;

procedure TChartListBox.LBSeriesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tmp : Longint;
    p   : TPoint;
begin
  if (ItemIndex<>-1) then
  begin
    tmp:=SeriesAtMousePos(p);
    if (tmp<>-1) and (PointInSection(p,1)) then
    begin
      with FChart[tmp] do Active:=not Active;
      Repaint;
    end
    else
    if PointInSection(p,3) and (not ComingFromDoubleClick) and
       (not (ssShift in Shift)) and
       (not (ssCtrl in Shift)) and
       ( ( ssLeft in Shift) ) then
            BeginDrag(False);
    ComingFromDoubleClick:=False;
    if Assigned(FOtherItemsChange) then FOtherItemsChange(Self);
  end;
end;

Procedure TChartListBox.MoveCurrentUp;
begin
  if ItemIndex>0 then SwapSeries(ItemIndex,ItemIndex-1);
end;

Procedure TChartListBox.MoveCurrentDown;
begin
  if (ItemIndex<>-1) and (ItemIndex<Items.Count-1) then
     SwapSeries(ItemIndex,ItemIndex+1);
end;

Procedure TChartListBox.DeleteSeries;
var GoDelete       : Boolean;
    NewFocusSeries : TChartSeries;
    t              : Longint;
    i              : Longint;
    tmpSt          : String;
begin
  if SelCount>0 then
  begin
    if FAskDelete then
    begin
      if SelCount=1 then tmpSt:=SelectedSeries.Name
                    else tmpSt:=TeeMsg_SelectedSeries;
      GoDelete:=MessageDlg( Format(TeeMsg_SureToDeleteSeries,[tmpSt]),
                     mtConfirmation,[mbYes,mbNo],0)=mrYes;
    end
    else GoDelete:=True;
    if GoDelete then
    begin
      NewFocusSeries:=nil;
      t:=0;
      for i:=0 to Items.Count-1 do
      begin
        if Selected[i] then
        begin
         FChart[t].Free;
         if t>(FChart.SeriesCount-1) then t:=FChart.SeriesCount-1;
         if (t>=0) and (t<FChart.SeriesCount) then
            NewFocusSeries:=FChart[t]
         else
            NewFocusSeries:=nil;
        end
        else Inc(t);
      end;
      FillSeries(NewFocusSeries);
    end;
  end;
end;

Procedure TChartListBox.CloneSeries;
begin
  if ItemIndex<>-1 then FillSeries(CloneChartSeries(SelectedSeries));
end;

procedure TChartListBox.LBSeriesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_DELETE) and (FAllowDelete) then DeleteSeries;
end;

Procedure TChartListBox.RenameSeries;
var tmp       : Longint;
    tmpSeries : TChartSeries;
    tmpSt     : String;
begin
  tmp:=ItemIndex;
  if tmp<>-1 then
  begin
    tmpSeries:=SelectedSeries;
    tmpSt:=tmpSeries.Title;
    if tmpSt='' then tmpSt:=tmpSeries.Name;
    if InputQuery( TeeMsg_ChangeSeriesTitle,
                   TeeMsg_NewSeriesTitle,tmpSt) then
    if tmpSt<>'' then
    begin
      if tmpSt<>tmpSeries.Name then tmpSeries.Title:=tmpSt;
      Items[tmp]:=tmpSt;
      if Assigned(FOtherItems) then FOtherItems[tmp]:=tmpSt;
    end;
    Selected[tmp]:=True;
  end;
end;

Function TChartListBox.AddSeriesGallery:TChartSeries;
begin
  result:=nil;
  if Assigned(FChart) then
     result:=CreateNewSeriesGallery(FChart.Owner,SelectedSeries,FChart,True,True);
  if Assigned(result) then FillSeries(result);
end;

procedure TChartListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  {$IFDEF D1}
  if not (csDestroying in ComponentState) then
  {$ENDIF}
  if Operation=opRemove then
     if Assigned(FChart) and (AComponent=FChart) then
        Chart:=nil;
end;

procedure TChartListBox.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  inherited;
  FHitTest := SmallPointToPoint(Msg.Pos);
end;

procedure TChartListBox.WMSetCursor(var Msg: TWMSetCursor);
var I: Integer;
begin
  if csDesigning in ComponentState then Exit;
  FHitTest := ScreenToClient(FHitTest);
  with Msg do
  if HitTest = HTCLIENT then
      for I := 0 to FSections.Count - 2 do  { don't count last section }
      begin
        if PointInSection(FHitTest,I) then
        begin
          if (I=0) or (I=2) then
          begin
            SetCursor(Screen.Cursors[crTeeHand]);
            exit;
          end
          else break;
        end;
      end;
  inherited;
end;

Function TChartListBox.GetShowActive:Boolean;
begin
  result:=Sections.Section[1].Visible;
end;

procedure TChartListBox.SetSectionVisible(Index:Integer; Value:Boolean);
var tmp:TListBoxSection;
begin
  tmp:=Sections.Section[Index];
  tmp.Visible:=Value;
  Sections.Section[Index]:=tmp;
  Repaint;
end;

procedure TChartListBox.SetShowActive(Value:Boolean);
begin
  SetSectionVisible(1,Value);
end;

Function TChartListBox.GetShowIcon:Boolean;
begin
  result:=Sections.Section[0].Visible;
end;

procedure TChartListBox.SetShowIcon(Value:Boolean);
begin
  SetSectionVisible(0,Value);
end;

Function TChartListBox.GetShowColor:Boolean;
begin
  result:=Sections.Section[2].Visible;
end;

procedure TChartListBox.SetShowColor(Value:Boolean);
begin
  SetSectionVisible(2,Value);
end;

Function TChartListBox.GetShowTitle:Boolean;
begin
  result:=Sections.Section[3].Visible;
end;

procedure TChartListBox.SetShowTitle(Value:Boolean);
begin
  SetSectionVisible(3,Value);
end;

{ Helper Listbox methods... }
procedure MoveList(Source,Dest:TListBox);
var t:Integer;
begin
  with Source do
  begin
    for t:=0 to Items.Count-1 do
        if Selected[t] then Dest.Items.AddObject(Items[t],Items.Objects[t]);
    t:=0;
    While t<Items.Count do
    begin
      if Selected[t] then Items.Delete(t)
                     else Inc(t);
    end;
  end;
end;

procedure MoveListAll(Source,Dest:TListBox);
var t:Integer;
begin
  With Source do
  for t:=0 to Items.Count-1 do Dest.Items.AddObject(Items[t],Items.Objects[t]);
  Source.Clear;
end;

end.
