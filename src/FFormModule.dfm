object FormModule: TFormModule
  Left = 887
  Top = 124
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Konfigurace modulu'
  ClientHeight = 393
  ClientWidth = 545
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object gb_1: TGroupBox
    Left = 8
    Top = 8
    Width = 290
    Height = 185
    Caption = ' Stav vstup'#367' '
    TabOrder = 0
  end
  object gb_2: TGroupBox
    Left = 7
    Top = 200
    Width = 290
    Height = 185
    Caption = ' Stav v'#253'stup'#367' '
    TabOrder = 1
  end
  object gb_3: TGroupBox
    Left = 305
    Top = 7
    Width = 233
    Height = 346
    Caption = 'Nastaven'#237
    TabOrder = 2
    object L_nazev_desky: TLabel
      Left = 8
      Top = 176
      Width = 94
      Height = 13
      Caption = 'N'#225'zev MTB desky :'
    end
    object Label1: TLabel
      Left = 13
      Top = 228
      Width = 50
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Verze FW:'
    end
    object L_FWVersion: TLabel
      Left = 13
      Top = 241
      Width = 58
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '[FWVersion]'
    end
    object rg_delay: TRadioGroup
      Left = 120
      Top = 16
      Width = 97
      Height = 89
      Caption = 'Zpo'#382'd'#283'n'#237' vstupu'
      ItemIndex = 0
      Items.Strings = (
        '300 ms'
        '600 ms'
        '900 ms'
        '1200 ms')
      TabOrder = 0
    end
    object cbIR0: TCheckBox
      Left = 16
      Top = 24
      Width = 65
      Height = 17
      Caption = 'IR 0-3'
      TabOrder = 1
    end
    object cbIR1: TCheckBox
      Left = 16
      Top = 40
      Width = 65
      Height = 17
      Caption = 'IR 4-7'
      TabOrder = 2
    end
    object cbIR2: TCheckBox
      Left = 16
      Top = 56
      Width = 65
      Height = 17
      Caption = 'IR 8-11'
      TabOrder = 3
    end
    object cbIR3: TCheckBox
      Left = 16
      Top = 72
      Width = 65
      Height = 17
      Caption = 'IR 12-15'
      TabOrder = 4
    end
    object cbSCOM0: TCheckBox
      Left = 16
      Top = 104
      Width = 75
      Height = 17
      Caption = 'S-COM 0,1'
      TabOrder = 5
    end
    object cbSCOM1: TCheckBox
      Left = 16
      Top = 120
      Width = 75
      Height = 17
      Caption = 'S-COM 2,3'
      TabOrder = 6
    end
    object cbSCOM2: TCheckBox
      Left = 16
      Top = 136
      Width = 75
      Height = 17
      Caption = 'S-COM 4,5'
      TabOrder = 7
    end
    object cbSCOM3: TCheckBox
      Left = 16
      Top = 152
      Width = 75
      Height = 17
      Caption = 'S-COM 6,7'
      TabOrder = 8
    end
    object E_nazev_desky: TEdit
      Left = 8
      Top = 192
      Width = 217
      Height = 21
      TabOrder = 9
    end
  end
  object B_Save: TButton
    Left = 456
    Top = 360
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 3
    OnClick = B_SaveClick
  end
  object B_Cancel: TButton
    Left = 376
    Top = 360
    Width = 75
    Height = 25
    Caption = 'Zru'#353'it'
    TabOrder = 4
    OnClick = B_CancelClick
  end
end
