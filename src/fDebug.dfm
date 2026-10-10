object F_Debug: TF_Debug
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Debug'
  ClientHeight = 474
  ClientWidth = 616
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnDestroy = FormDestroy
  TextHeight = 13
  object Label1: TLabel
    Left = 424
    Top = 453
    Width = 66
    Height = 13
    Caption = 'D'#233'lka zpr'#225'vy:'
  end
  object L_len: TLabel
    Left = 545
    Top = 453
    Width = 25
    Height = 13
    Alignment = taRightJustify
    Caption = 'L_len'
  end
  object Label2: TLabel
    Left = 576
    Top = 453
    Width = 28
    Height = 13
    Caption = 'znak'#367
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 43
    Height = 13
    Caption = 'Loglevel:'
  end
  object LV_Log: TListView
    Left = 8
    Top = 31
    Width = 600
    Height = 272
    Columns = <
      item
        Caption = #268'as'
        Width = 80
      end
      item
        Caption = 'Zpr'#225'va'
        Width = 480
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 4
    ViewStyle = vsReport
    OnChange = LV_LogChange
    OnCustomDrawItem = LV_LogCustomDrawItem
  end
  object M_Data: TMemo
    Left = 8
    Top = 309
    Width = 600
    Height = 140
    ReadOnly = True
    TabOrder = 5
    OnChange = M_DataChange
  end
  object B_ClearLog: TButton
    Left = 533
    Top = 8
    Width = 75
    Height = 17
    Caption = 'Smazat log'
    TabOrder = 3
    OnClick = B_ClearLogClick
  end
  object CHB_KeepAlive: TCheckBox
    Left = 214
    Top = 8
    Width = 113
    Height = 17
    Caption = 'Logovat keep-alive'
    TabOrder = 1
    OnClick = CHB_KeepAliveClick
  end
  object CHB_PingLogging: TCheckBox
    Left = 333
    Top = 8
    Width = 82
    Height = 17
    Caption = 'Logovat ping'
    TabOrder = 2
    OnClick = CHB_KeepAliveClick
  end
  object CB_Loglevel: TComboBox
    Left = 57
    Top = 4
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 2
    TabOrder = 0
    Text = 'varov'#225'n'#237
    OnChange = CB_LoglevelChange
    Items.Strings = (
      'nic'
      'chyby'
      'varov'#225'n'#237
      'informace'
      'data'
      'debug')
  end
end
