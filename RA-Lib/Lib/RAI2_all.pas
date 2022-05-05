{***********************************************************
                R&A Library
                   RAI2
       Copyright (C) 1998-2000 R&A

       component   : none
       description : registering all RAI2 Adapters

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAI2_all;

interface

implementation

uses RAI2,
  RAI2_System, RAI2_Windows, RAI2_SysUtils,
  RAI2_Graphics, RAI2_Classes, RAI2_Controls,
  RAI2_StdCtrls, RAI2_ComCtrls, RAI2_ExtCtrls, RAI2_Forms,
  RAI2_Menus, RAI2_Grids, RAI2_Dialogs,
  RAI2_Db, RAI2_DBTables, RAI2_DbCtrls, RAI2_DbGrids,
  RAI2_Quickrpt,
  RAI2_RAUtils, RAI2_RAEditor, RAI2_RARegAuto,
  RAI2Fm
  ;

initialization
  RAI2_System.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_Windows.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_SysUtils.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_Classes.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_Graphics.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_Controls.RegisterRAI2Adapter(GlobalRAI2Adapter);

  RAI2_StdCtrls.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_ComCtrls.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_Extctrls.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_Forms.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_Dialogs.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_Menus.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_Grids.RegisterRAI2Adapter(GlobalRAI2Adapter);

  RAI2_Db.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_DbTables.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_DbCtrls.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_DbGrids.RegisterRAI2Adapter(GlobalRAI2Adapter);

  RAI2_Quickrpt.RegisterRAI2Adapter(GlobalRAI2Adapter);

  RAI2_RAUtils.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_RAEditor.RegisterRAI2Adapter(GlobalRAI2Adapter);
  RAI2_RARegAuto.RegisterRAI2Adapter(GlobalRAI2Adapter);

  RAI2Fm.RegisterRAI2Adapter(GlobalRAI2Adapter);
end.
