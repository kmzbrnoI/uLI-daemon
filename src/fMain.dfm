object F_Main: TF_Main
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSizeToolWin
  Caption = 'uLI-daemon'
  ClientHeight = 178
  ClientWidth = 413
  Color = clBtnFace
  Constraints.MinHeight = 100
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SB_Main: TStatusBar
    Left = 0
    Top = 159
    Width = 413
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 200
      end>
    ParentShowHint = False
    ShowHint = True
    OnDblClick = SB_MainDblClick
  end
  object AL_Main: TActionList
    Left = 16
    Top = 16
    object A_Debug: TAction
      Caption = 'Debug'
      ShortCut = 115
      OnExecute = A_DebugExecute
    end
    object A_Errors: TAction
      Caption = 'Errors'
      ShortCut = 16453
      OnExecute = A_ErrorsExecute
    end
    object A_ResetCounter: TAction
      Caption = 'A_ResetCounter'
      ShortCut = 16466
      OnExecute = A_ResetCounterExecute
    end
  end
end
