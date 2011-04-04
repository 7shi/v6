﻿Imports System.IO
Imports PDP11Lib

Partial Public Class MainPage
    Inherits UserControl

    Private aout As AOut
    Private parg As ProcArg
    Private fs As New SLFileSystem("Tests")

    Private Class ProcArg
        Public Args As String()
        Public Verbose As Boolean
    End Class

    Public Sub New()
        InitializeComponent()
        ReadFile("hello1", New ProcArg With {.Verbose = True})
        addTest(True, "hello1")
        addTest(True, "hello2")
        addTest(True, "hello3")
        addTest(True, "hello4")
        addTest(True, "args", "test", "arg")
        addTest(False, "nm", "args")
    End Sub

    Public Sub Clear()
        txtDis.Text = ""
        txtSrc.Text = ""
        txtBin.Text = ""
        txtTrace.Text = ""
        txtOut.Text = ""
    End Sub

    Private Sub addTest(verbose As Boolean, t$, ParamArray args$())
        Dim parg = New ProcArg With {.Args = args, .Verbose = verbose}
        Dim button = New Button With {.Content = t, .Tag = parg}
        AddHandler button.Click, AddressOf btnTest_Click
        menuStack.Children.Add(button)
    End Sub

    Private Sub ReadFile(path$, parg As ProcArg)
        Using s = fs.Open(path)
            ReadStream(s.Stream, GetFileName(path), parg)
        End Using
        Using s = fs.Open(path + ".c")
            txtSrc.Text = ReadText(s.Stream)
        End Using
    End Sub

    Private Sub ReadStream(s As Stream, path$, parg As ProcArg)
        Dim data(CInt(s.Length - 1)) As Byte
        s.Read(data, 0, data.Length)
        aout = New AOut(data, path)
        Me.parg = parg
        Run()
        btnSave.IsEnabled = True
    End Sub

    Private Sub btnOpen_Click(sender As Object, e As RoutedEventArgs)
        Dim ofd = New OpenFileDialog()
        If ofd.ShowDialog() <> True Then Return

        Clear()
        Try
            Dim fi = ofd.File
            If fi.Length >= 64 * 1024 Then
                Throw New Exception("ファイルが大き過ぎます。上限は64KBです。")
            End If
            Using fs = ofd.File.OpenRead()
                ReadStream(fs, ofd.File.Name, Nothing)
            End Using
        Catch ex As Exception
            txtDis.Text = ex.Message + Environment.NewLine +
                "読み込みに失敗しました。" + Environment.NewLine
        End Try
    End Sub

    Private Sub btnSave_Click(sender As Object, e As RoutedEventArgs)
        Dim sfd = New SaveFileDialog()
        If sfd.ShowDialog() <> True Then Return

        Using fs = sfd.OpenFile()
            fs.Write(aout.Data, 0, aout.Data.Length)
        End Using
    End Sub

    Private Sub btnTest_Click(sender As Object, e As RoutedEventArgs)
        Dim button = CType(sender, Button)
        If button Is Nothing Then Return

        Dim cur = Cursor
        Cursor = Cursors.Wait
        ReadFile(button.Content.ToString(), CType(button.Tag, ProcArg))
        Cursor = cur
    End Sub

    Private Sub comboBox1_SelectionChanged(sender As Object, e As SelectionChangedEventArgs)
        Run()
    End Sub

    Private Sub Run()
        If aout Is Nothing Then Return

        Dim cur = Cursor
        Cursor = Cursors.Wait
        aout.UseOct = comboBox1.SelectedIndex = 1
        Dim vm = New VM(aout, fs, parg.Verbose)
        vm.Run(parg.Args)
        txtDis.Text = aout.GetDisassemble()
        txtBin.Text = aout.GetDump()
        txtTrace.Text = vm.Trace
        txtTrace.SelectionStart = txtTrace.Text.Length
        txtOut.Text = vm.Output
        txtOut.SelectionStart = txtOut.Text.Length
        Cursor = cur
    End Sub

    Public Shared Function GetFileName$(path$)
        Dim p = path.LastIndexOf(CChar("/"))
        Return If(p < 0, path, path.Substring(p + 1))
    End Function
End Class
