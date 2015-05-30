object F_About: TF_About
  Left = 765
  Top = 599
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'About MTB Driver'
  ClientHeight = 222
  ClientWidth = 441
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object L_About1: TLabel
    Left = 0
    Top = 0
    Width = 441
    Height = 25
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'Knihovnu vytvoril Michal Petrilak a Jan Horacek '#169' 2008 - 2013'
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clRed
    Font.Height = -16
    Font.Name = 'Calibri'
    Font.Style = []
    ParentFont = False
  end
  object L_About2: TLabel
    Left = 0
    Top = 25
    Width = 441
    Height = 13
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'MTB Driver je urcen jako prostrednik mezi programem'
  end
  object L_About3: TLabel
    Left = 0
    Top = 38
    Width = 441
    Height = 21
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = ' na ovladani kolejiste a MTB-USB modulem'
  end
  object ST_about4: TStaticText
    Left = 0
    Top = 59
    Width = 441
    Height = 20
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'Ve'#353'ker'#225' pr'#225'va k programu vyhrazena'
    TabOrder = 0
  end
  object ST_about5: TStaticText
    Left = 0
    Top = 79
    Width = 441
    Height = 13
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'Klub model'#225#345#367' '#382'eleznic Brno I '
    TabOrder = 1
  end
  object ST_about6: TStaticText
    Left = 0
    Top = 92
    Width = 441
    Height = 17
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'http://www.kmz-brno.cz'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    TabOrder = 2
  end
  object GB_Versions: TGroupBox
    Left = 0
    Top = 109
    Width = 441
    Height = 108
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alTop
    Caption = ' Verze '
    TabOrder = 3
    object Label1: TLabel
      Left = 13
      Top = 20
      Width = 76
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Verze knihovny:'
    end
    object Label2: TLabel
      Left = 13
      Top = 37
      Width = 65
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Verze driveru:'
    end
    object Label3: TLabel
      Left = 13
      Top = 55
      Width = 110
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Verze FW v MTB-USB:'
    end
    object L_VersionLib: TLabel
      Left = 161
      Top = 20
      Width = 67
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      Caption = '[L_VersionLib]'
    end
    object L_VersionDrv: TLabel
      Left = 160
      Top = 37
      Width = 70
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      Caption = '[L_VersionDrv]'
    end
    object L_VersionUSB: TLabel
      Left = 155
      Top = 55
      Width = 75
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Alignment = taRightJustify
      Caption = '[L_VersionUSB]'
    end
  end
end
