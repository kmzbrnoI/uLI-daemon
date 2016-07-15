object F_Main: TF_Main
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'uLI-daemon'
  ClientHeight = 320
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
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
    Top = 301
    Width = 635
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
    object A_ServerConnect: TAction
      Caption = 'A_ServerConnect'
    end
    object A_ServerDisconnect: TAction
      Caption = 'A_ServerDisconnect'
      Enabled = False
    end
  end
end
