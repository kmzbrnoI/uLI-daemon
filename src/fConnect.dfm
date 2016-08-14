object F_Connect: TF_Connect
  Left = 0
  Top = 0
  Align = alClient
  BorderStyle = bsNone
  Caption = 'F_Connect'
  ClientHeight = 203
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GB_Connect: TGroupBox
    Left = 40
    Top = 40
    Width = 273
    Height = 121
    Caption = ' Vyberte COM port '
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 16
      Width = 121
      Height = 13
      AutoSize = False
      Caption = 'Porty s uLI-master:'
    end
    object LB_Ports: TListBox
      Left = 16
      Top = 35
      Width = 121
      Height = 70
      ItemHeight = 13
      TabOrder = 0
    end
    object CHB_Remember: TCheckBox
      Left = 156
      Top = 15
      Width = 106
      Height = 17
      Caption = 'Zapamatovat port'
      TabOrder = 1
    end
    object B_Connect: TButton
      Left = 167
      Top = 80
      Width = 75
      Height = 25
      Caption = 'P'#345'ipojit'
      Default = True
      TabOrder = 2
      OnClick = B_ConnectClick
    end
    object B_Update: TButton
      Left = 167
      Top = 38
      Width = 74
      Height = 21
      Caption = 'Aktualizovat'
      TabOrder = 3
      OnClick = B_UpdateClick
    end
  end
end
