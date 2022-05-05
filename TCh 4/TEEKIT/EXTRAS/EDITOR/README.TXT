==========================================================
               TeeChart-Pro 3.0
      Copyright 1995-1997 by David Berneda. 
             All Rights Reserved.

teeMach, SL
Gran Via 533, Barcelona 08011, Catalonia, Spain.
==========================================================

TeeChart Editor Dialog Examples
=============================================


1 -- Customizing the Chart Editor 
2 -- Avoiding automatic Chart redrawing



1 -- Customizing the Chart Editor 
=========================================

The Chart Editor contains a public property to control
which Tabs and Buttons are accessible by the end user.

You can change the Editor options by using this code:

Uses EditChar,IEditCha;

Var OldEditChart:TOnCreateEditChart;

Procedure MyEditChart(Sender:TChartEditForm);
begin
  if Assigned(OldEditChart) then OldEditChart(Sender);
  Sender.EditorOptions:=[ ceChange ];
end;

Procedure MyChartEditor( Owner:TForm; AChart : TCustomChart );
begin
  OldEditChart:=InternalOnCreateEditChart;
  InternalOnCreateEditChart:=MyEditChart;
  EditChart(Owner,AChart);
end;


Then calling this procedure to edit your Charts:

MyChartEditor( Self, Chart1 );


In this example, the Editor will allow only to "Change"
Series, but not adding or deleting or changing the Series
datasource properties.

The different options for "EditorOptions" are:

ceAdd              Allow user to Add new Series
ceDelete           Allow user to Delete Series
ceChange           Allow user to Change a Series type
ceClone            Allow user to Clone Series
ceDataSource       Allow user to change the Series DataSource

---------------------------------------------------------------------

2 -- Avoiding automatic Chart redrawing
==============================================

The Chart Editor, by default, automatically redraws the entire Chart
whenever a property is changed.
This is because the editor changes the Chart properties in the
same way the Delphi Object Inspector does, thus forcing a 
Chart redraw to reflect any property changes.

You can avoid this automatic redraw in the Editor by
redrawing the Chart only one time after the Editor is closed.

To do so, you should create a new Chart component:

type TMyChart = class( TChart )
     end;

And override the Chart "Invalidate" method:

type TMyChart = class( TChart )
     public
       Editing:Boolean;
       Procedure Invalidate; override;
     end;

Procedure TMyChart.Invalidate; 
begin
  if not Editing then inherited Invalidate;
end;


To show the Editor Dialog for this new Chart, you 
should call this procedure:

Procedure EditMyChart( AChart : TMyChart );
begin
  AChart.Editing:=True;
  EditChart( nil, AChart );
  AChart.Editing:=False;
  AChart.Repaint;
end;

To make it work at design-time, you should create a unit containing
and registering this component editor:

type
  TMyChartCompEditor=class(TChartCompEditor)
  public
    procedure Edit; override;
  end;

procedure TMyChartCompEditor.Edit; 
begin  
  EditMyChart(TMyChart(Component));
end;

Procedure Register;
begin
  RegisterComponents( 'MyCharts', [TMyChart] );
  RegisterComponentEditor(TMyChart,TMyChartCompEditor);
end;


---------------------------------------------------------------------
