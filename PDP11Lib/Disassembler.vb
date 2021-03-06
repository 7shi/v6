﻿Public Module Disassembler
    Public ReadOnly RegNames As String() =
        {"r0", "r1", "r2", "r3", "r4", "r5", "sp", "pc"}

    Public ReadOnly SectionNames As String() = {Nothing, ".text", ".data", ".bss"}

    Public ReadOnly SysNames As String() =
        {"indir", "exit", "fork", "read", "write", "open", "close", "wait",
         "creat", "link", "unlink", "exec", "chdir", "time", "mknod", "chmod",
         "chown", "break", "stat", "seek", "getpid", "mount", "umount", "setuid",
         "getuid", "stime", "ptrace", Nothing, "fstat", Nothing, "smdate", "stty",
         "gtty", Nothing, "nice", "sleep", "sync", "kill", "switch", Nothing,
         Nothing, "dup", "pipe", "times", "prof", "tiu", "setgid", "getgid", "signal"}

    Public ReadOnly SigNames As String() =
        {Nothing, "SIGHUP", "SIGINT", "SIGQIT", "SIGINS", "SIGTRC", "SIGIOT", "SIGEMT",
         "SIGFPT", "SIGKIL", "SIGBUS", "SIGSEG", "SIGSYS", "SIGPIPE"}

    Public ReadOnly SysArgs As Integer() =
        {1, 0, 0, 2, 2, 2, 0, 0, 2, 2, 1, 2, 1, 0, 3, 2,
         2, 1, 2, 2, 0, 3, 1, 0, 0, 0, 3, 0, 1, 0, 1, 1,
         1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 4, 0, 0, 0, 2}

    Public ReadOnly OpCodes(65535) As OpCode

    Sub New()
        For i = 0 To 65535
            OpCodes(i) = New OpCode(i)
        Next
    End Sub

    Public Function Disassemble$(bd As BinData, pos%, op As OpCode)
        Dim vm = TryCast(bd, VM)
        Dim st = If(vm IsNot Nothing, New VMState(vm), Nothing)
        Dim ret = op.Disassemble(bd, pos)
        If st IsNot Nothing Then st.Restore()
        Return ret
    End Function

    Public Function ConvShort(v As UShort) As Short
        Return CShort(If(v < &H8000, v, v - &H10000))
    End Function

    Public Function ConvSByte(v As Byte) As SByte
        Return CSByte(If(v < &H80, v, v - &H100))
    End Function

    Public Function GetRegString$(bd As BinData, r%, pc%)
        Return RegNames(r) + bd.GetReg(r, CUShort(pc And &HFFFF))
    End Function
End Module
