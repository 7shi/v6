Partial Public Class OpCode
    Public Property Length As UShort

    Private val%, reg%
    Private src, dst As Operand
    Private disasm As Func(Of BinData, Integer, String)
    Private exec As Action(Of VM)

    Public Sub New(value%)
        val = value
        SetInst()
    End Sub

    Public Function Disassemble$(bd As BinData, pos%)
        Return If(disasm Is Nothing, Nothing, disasm(bd, pos))
    End Function

    Public Function Run(vm As VM) As Boolean
        If exec Is Nothing Then
            Return False
        Else
            exec(vm)
            Return True
        End If
    End Function

    Private Sub SetMne(mne$)
        disasm = Function(bd, pos) mne
        Length = 2
    End Sub

    Private Sub SetDst(mne$, size As UShort)
        dst = Operands(val And 63)
        If dst.IsValid Then
            disasm = Function(bd, pos) mne + " " + dst.ToString(bd, pos + 2, size)
            Length = 2US + dst.Length
        End If
    End Sub

    Private Sub SetSrcDst(mne$, size As UShort)
        dst = Operands(val And 63)
        src = Operands((val >> 6) And 63).Check(dst)
        If src.IsValid AndAlso dst.IsValid Then
            disasm = Function(bd, pos)
                         Return mne + " " + src.ToString(bd, pos + 2, size) +
                             ", " + dst.ToString(bd, pos + 2 + src.Length, size)
                     End Function
            Length = 2US + src.Length + dst.Length
        End If
    End Sub

    Private Sub SetRegDst(mne$, size As UShort)
        reg = (val >> 6) And 7
        dst = Operands(val And 63)
        If dst.IsValid Then
            disasm = Function(bd, pos)
                         Return mne + " " + GetRegString(bd, reg, pos + 2) +
                             ", " + dst.ToString(bd, pos + 2, size)
                     End Function
            Length = 2US + dst.Length
        End If
    End Sub

    Private Sub SetSrcReg(mne$, size As UShort)
        reg = (val >> 6) And 7
        src = Operands(val And 63)
        If src.IsValid Then
            disasm = Function(bd, pos)
                         Return mne + " " + src.ToString(bd, pos + 2, size) +
                             "," + GetRegString(bd, reg, pos + 2)
                     End Function
            Length = 2US + src.Length
        End If
    End Sub

    Private Sub SetReg(mne$)
        reg = val And 7
        disasm = Function(bd, pos) mne + " " + GetRegString(bd, reg, pos + 2)
        Length = 2
    End Sub

    Private Sub SetNum(mne$)
        disasm = Function(bd, pos) mne + " " + bd.Enc(CByte(bd(pos) And &O77))
        Length = 2
    End Sub

    Private Sub SetRegOffset(mne$)
        reg = (val >> 6) And 7
        disasm = Function(bd, pos)
                     Return mne + " " + GetRegString(bd, reg, pos + 2) +
                         ", " + bd.Enc(CUShort(pos + 2 - (val And &O77) * 2))
                 End Function
        Length = 2
    End Sub

    Private Sub SetOffset(mne$)
        disasm = Function(bd, pos) mne + " " + bd.Enc(bd.GetOffset(pos + 2))
        Length = 2
    End Sub
End Class
