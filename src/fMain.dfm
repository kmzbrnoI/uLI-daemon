object F_Main: TF_Main
  Left = 0
  Top = 0
  Caption = 'F_Main'
  ClientHeight = 320
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 39
    Width = 129
    Height = 253
    ItemHeight = 13
    TabOrder = 1
  end
  object Button2: TButton
    Left = 143
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 143
    Top = 39
    Width = 484
    Height = 253
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object Button3: TButton
    Left = 384
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 4
  end
  object Button4: TButton
    Left = 472
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Set status'
    TabOrder = 5
    OnClick = Button4Click
  end
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
        Width = 50
      end
      item
        Width = 50
      end>
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
