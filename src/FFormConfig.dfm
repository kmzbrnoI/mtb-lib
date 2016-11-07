object FormConfig: TFormConfig
  Left = 1120
  Top = 261
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'MTB konfigurace'
  ClientHeight = 388
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MM_Main
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PC_Main: TPageControl
    Left = 0
    Top = 0
    Width = 529
    Height = 388
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    ActivePage = TS_Device
    Align = alClient
    TabOrder = 0
    object TS_Device: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'P'#345'ipojen'#237
      ImageIndex = 1
      object l_2: TLabel
        Left = 13
        Top = 62
        Width = 87
        Height = 13
        Caption = 'Rychlost sbernice:'
      end
      object Label3: TLabel
        Left = 13
        Top = 17
        Width = 64
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = #344#237'd'#237'c'#237' deska:'
      end
      object Label4: TLabel
        Left = 155
        Top = 85
        Width = 24
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'baud'
      end
      object L_LogLevel: TLabel
        Left = 13
        Top = 217
        Width = 43
        Height = 13
        Caption = 'Loglevel:'
      end
      object cb_mtbName: TComboBox
        Left = 13
        Top = 35
        Width = 137
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cb_mtbNameChange
      end
      object cb_speed: TComboBox
        Left = 13
        Top = 78
        Width = 137
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 1
        Text = '38 400'
        OnChange = cb_speedChange
        Items.Strings = (
          '38 400'
          '57 600'
          '115 200')
      end
      object b_ScanBrd: TButton
        Left = 156
        Top = 35
        Width = 51
        Height = 22
        Caption = 'Vyhledat'
        TabOrder = 2
        OnClick = b_ScanBrdClick
      end
      object RG_TimerInterval: TRadioGroup
        Left = 13
        Top = 105
        Width = 137
        Height = 107
        Align = alCustom
        Caption = ' Scan timer interval '
        Items.Strings = (
          '50 ms'
          '100 ms'
          '200 ms'
          '250 ms')
        TabOrder = 3
        OnClick = RG_TimerIntervalClick
      end
      object CB_LogLevel: TComboBox
        Left = 13
        Top = 235
        Width = 137
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        OnChange = CB_LogLevelChange
        Items.Strings = (
          'Nic'
          'Chyby'
          'Zm'#283'ny stav'#367
          'P'#345#237'kazy'
          'Raw data'
          'Debug')
      end
    end
    object TS_Stav: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Stav p'#345'ipojen'#237
      object Label1: TLabel
        Left = 15
        Top = 13
        Width = 43
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Za'#345#237'zen'#237':'
      end
      object Label2: TLabel
        Left = 15
        Top = 31
        Width = 62
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Komunikace:'
      end
      object L_Openned: TLabel
        Left = 248
        Top = 15
        Width = 45
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Alignment = taRightJustify
        Caption = 'uzav'#345'eno'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -9
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object L_Started: TLabel
        Left = 244
        Top = 33
        Width = 49
        Height = 13
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Alignment = taRightJustify
        Caption = 'zastavena'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -9
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
    end
    object TS_Modules: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'MTB moduly'
      ImageIndex = 2
      object l_3: TLabel
        Left = 10
        Top = 12
        Width = 68
        Height = 13
        Caption = 'Po'#269'et modul'#367':'
      end
      object l_modcount: TLabel
        Left = 112
        Top = 12
        Width = 6
        Height = 13
        Alignment = taRightJustify
        Caption = '0'
      end
      object lv_modules: TListView
        Left = 0
        Top = 44
        Width = 521
        Height = 316
        Align = alBottom
        Columns = <
          item
            Caption = 'N'#225'zev'
            Width = 280
          end
          item
            Caption = 'Adresa'
          end
          item
            Caption = 'Typ'
            Width = 70
          end
          item
            Caption = 'Nastaven'#237
            Width = 65
          end>
        GridLines = True
        ReadOnly = True
        RowSelect = True
        PopupMenu = pm_mod
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lv_modulesDblClick
      end
      object B_DeleteNonExist: TButton
        Left = 288
        Top = 9
        Width = 223
        Height = 25
        Caption = 'Vymazat konfiguraci neexistuj'#237'c'#237'ch modul'#367
        TabOrder = 1
        OnClick = B_DeleteNonExistClick
      end
    end
    object TS_Log: TTabSheet
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Log'
      ImageIndex = 3
      object LV_Log: TListView
        Left = 0
        Top = 0
        Width = 521
        Height = 360
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Align = alClient
        Columns = <
          item
            Caption = #268'as'
            Width = 80
          end
          item
            Caption = 'LogLevel'
            Width = 60
          end
          item
            Caption = 'Zpr'#225'va'
            Width = 350
          end>
        GridLines = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnCustomDrawItem = LV_LogCustomDrawItem
      end
    end
  end
  object pm_mod: TPopupMenu
    OnPopup = pm_modPopup
    Left = 272
    Top = 296
    object pm_mod_nastaveni: TMenuItem
      Caption = 'Diagnostika'
      OnClick = pm_mod_nastaveniClick
    end
  end
  object MM_Main: TMainMenu
    Left = 336
    Top = 296
    object MI_Window: TMenuItem
      Caption = 'Okno'
      object PM_Close: TMenuItem
        Caption = 'Zav'#345#237't'
        OnClick = PM_CloseClick
      end
    end
    object PM_Help: TMenuItem
      Caption = 'N'#225'pov'#283'da'
      object PM_About: TMenuItem
        Caption = 'O knihovn'#283
        OnClick = PM_AboutClick
      end
    end
  end
end
