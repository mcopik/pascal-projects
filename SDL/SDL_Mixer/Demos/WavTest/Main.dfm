object Form1: TForm1
  Left = 295
  Top = 196
  Width = 432
  Height = 309
  HorzScrollBar.Range = 419
  VertScrollBar.Range = 273
  ActiveControl = Button4
  AutoScroll = False
  Caption = 'SDL Test Mixer Program'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 14
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 396
    Top = 124
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = SpeedButton1Click
  end
  object Button2: TButton
    Left = 344
    Top = 156
    Width = 75
    Height = 25
    Caption = 'Quit SDL'
    Enabled = False
    TabOrder = 0
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 14
    Top = 156
    Width = 85
    Height = 25
    Caption = 'Play Wav File'
    TabOrder = 1
    OnClick = Button4Click
  end
  object gbOptions: TGroupBox
    Left = 12
    Top = 8
    Width = 405
    Height = 105
    Caption = '[ Options ]'
    TabOrder = 2
    object Label2: TLabel
      Left = 4
      Top = 20
      Width = 53
      Height = 13
      Caption = 'Audio Rate'
    end
    object Label3: TLabel
      Left = 124
      Top = 20
      Width = 74
      Height = 13
      Caption = 'Audio Channels'
    end
    object Label4: TLabel
      Left = 268
      Top = 20
      Width = 62
      Height = 13
      Caption = 'Audio Format'
    end
    object Label1: TLabel
      Left = 4
      Top = 48
      Width = 51
      Height = 13
      Caption = 'Buffer Size'
    end
    object cbLoops: TCheckBox
      Left = 28
      Top = 68
      Width = 45
      Height = 17
      Caption = 'Loop'
      TabOrder = 0
    end
    object seAudioRate: TSpinEdit
      Left = 60
      Top = 16
      Width = 57
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 100
    end
    object seAudioChannels: TSpinEdit
      Left = 204
      Top = 16
      Width = 57
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 8
    end
    object seAudioFormat: TSpinEdit
      Left = 336
      Top = 16
      Width = 57
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 8
    end
    object seAudioBuffers: TSpinEdit
      Left = 60
      Top = 44
      Width = 57
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 4
      Value = 100
    end
  end
  object Edit1: TEdit
    Left = 12
    Top = 124
    Width = 373
    Height = 21
    ReadOnly = True
    TabOrder = 3
    Text = 'C:\'
  end
  object Memo: TMemo
    Left = 12
    Top = 188
    Width = 404
    Height = 85
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object OpenDialog: TOpenDialog
    Filter = 'Wav Files|*.wav'
    Title = 'Open'
    Left = 376
    Top = 64
  end
end
