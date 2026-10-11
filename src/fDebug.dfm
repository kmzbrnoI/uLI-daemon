object F_Debug: TF_Debug
  Left = 0
  Top = 0
  Caption = 'Debug'
  ClientHeight = 474
  ClientWidth = 705
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object LV_Log: TListView
    Left = 0
    Top = 41
    Width = 705
    Height = 433
    Align = alClient
    Columns = <
      item
        Caption = #268'as'
        Width = 80
      end
      item
        Caption = 'Zdroj'
      end
      item
        Caption = 'Level'
        Width = 60
      end
      item
        Caption = 'Zpr'#225'va'
        Width = 470
      end>
    MultiSelect = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnCustomDrawItem = LV_LogCustomDrawItem
  end
  object P_Header: TPanel
    Left = 0
    Top = 0
    Width = 705
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label3: TLabel
      Left = 96
      Top = 14
      Width = 43
      Height = 13
      Caption = 'Loglevel:'
    end
    object B_ClearLog: TButton
      Left = 7
      Top = 12
      Width = 75
      Height = 17
      Caption = 'Smazat log'
      TabOrder = 0
      OnClick = B_ClearLogClick
    end
    object CHB_PingLogging: TCheckBox
      Left = 421
      Top = 14
      Width = 82
      Height = 17
      Caption = 'Logovat ping'
      TabOrder = 3
      OnClick = CHB_KeepAliveClick
    end
    object CHB_KeepAlive: TCheckBox
      Left = 302
      Top = 14
      Width = 113
      Height = 17
      Caption = 'Logovat keep-alive'
      TabOrder = 2
      OnClick = CHB_KeepAliveClick
    end
    object CB_Loglevel: TComboBox
      Left = 145
      Top = 10
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 1
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
end
