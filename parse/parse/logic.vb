Public Class logic

    Public Property verbose As Boolean = False

    Public Property partref As Dictionary(Of String, String)
    Public Property partdict As Dictionary(Of String, Dictionary(Of String, String))

    Public Structure state_transition
        Public prev As Dictionary(Of String, Boolean)
        Public curr As Dictionary(Of String, Boolean)
    End Structure


    Sub New(ByRef _partref As Dictionary(Of String, String), ByRef _partdict As Dictionary(Of String, Dictionary(Of String, String)))
        partref = _partref
        partdict = _partdict
    End Sub

    Sub add_state(ByRef states As List(Of state_transition), ByVal inp As List(Of String), ByVal ip As Integer, ByVal ic As Integer)
        states.Add(New state_transition With {.curr = New Dictionary(Of String, Boolean),
                                                    .prev = New Dictionary(Of String, Boolean)()})
        For j As Integer = 0 To inp.Count - 1
            states.Last.prev.Add(inp(j), (ip >> j) Mod 2)
            states.Last.curr.Add(inp(j), (ic >> j) Mod 2)
        Next
    End Sub

    '                                           input state                         output state
    Function check_add(ByRef all_net As Dictionary(Of String, Boolean),
                       ByVal inpa As List(Of String), ByVal inpb As List(Of String), ByVal inpc As String,
                       ByVal outy As List(Of String), ByVal outc As String, Optional ByVal detail As Boolean = False) As Boolean

        Dim y As Integer
        Dim ret As Boolean = True

        Console.WriteLine("Checking add function ... ")

        For c As Integer = 0 To 1
            For b As Integer = 0 To inpb.Count - 1
                For a As Integer = 0 To inpa.Count - 1
                    For i As Integer = 0 To inpa.Count - 1
                        all_net.Item(inpa(i)) = (a >> i) Mod 2
                    Next
                    For i As Integer = 0 To inpb.Count - 1
                        all_net.Item(inpb(i)) = (b >> i) Mod 2
                    Next
                    all_net.Item(inpc) = c Mod 2

                    While compute(all_net)
                    End While

                    y = 0
                    For i As Integer = 0 To outy.Count - 1
                        y += all_net.Item(outy(i)) And (&H1 << i)
                    Next
                    y += all_net.Item(outc) And (&H1 << outy.Count)

                    If a + b + c <> y Then
                        Console.WriteLine("error")
                        ret = False
                    End If

                    If detail Then
                        Console.WriteLine(a & " " & b & " " & c & " = " & y & ":" & a + b + c)
                    End If
                Next
            Next
        Next
        Return ret
    End Function


    Function prop_delay(ByRef all_net As Dictionary(Of String, Boolean), ByVal inp As List(Of String), ByVal outp As List(Of String), Optional ByVal detail As Boolean = False) As Integer
        Dim inp_c As Integer = inp.Count - 1
        Dim states As New List(Of state_transition)

        Console.WriteLine("Computing possible all states and transitions ... ")

        Dim ip As Integer = 0
        Dim ic As Integer = 1
        Dim n As Integer = 2 ^ inp.Count - 1
        While (ip < n - 1)

            add_state(states, inp, ip, ic)

            If (ic = n) Then
                ip += 1
            End If

            add_state(states, inp, ic, ip)

            If (ic = n) Then
                ic = ip + 1
            Else
                ic += 1
            End If

        End While
        add_state(states, inp, ip, ic)
        add_state(states, inp, ic, 0)
        Console.WriteLine(vbTab & states.Count & " possible transitions" & vbCrLf)

        Console.WriteLine("Simulating all states ...")
        Dim count As Integer
        Dim maxcount As Integer = 0
        Dim iter As Integer = 1
        Dim max As Integer = (2 ^ (2 * inp.Count) - 1) / 100
        Dim nCurrent As Integer = 1


        'for all config
        For Each s In states
            iter += 1
            'put the system in prev state
            For Each k In s.prev
                all_net.Item(k.Key) = k.Value
            Next


            While compute(all_net)
            End While

            'generate  input value
            For Each k In s.curr
                all_net.Item(k.Key) = k.Value
            Next

            count = 0
            While compute(all_net)
                count += 1
            End While


            If count > maxcount Then
                maxcount = count
            End If

            If detail Then
                Console.WriteLine(dic_to_string(s.prev) & "->" & dic_to_string(s.curr) & " : " & count)
            Else
                If iter >= max Then
                    iter = 1
                    Console.Write("{0}Progression : {1}%", vbCr & vbTab, nCurrent)
                    nCurrent += 1
                End If
            End If
        Next
        Console.WriteLine(vbCrLf & vbTab & "Done" & vbCrLf)

        Return maxcount
    End Function

    Function dic_to_string(ByVal nets As Dictionary(Of String, Boolean)) As String
        Dim str As String = ""
        For Each n In nets
            If n.Value Then
                str &= "1"
            Else
                str &= "0"
            End If
        Next
        Return str
    End Function

    Function compute(ByRef all_net As Dictionary(Of String, Boolean)) As Boolean
        Dim pout As Boolean
        Dim nout As Boolean
        Dim has_chaged As Boolean = False
        Dim new_out As New Dictionary(Of String, Boolean)

        For Each p In partdict
            With p.Value
                Select Case partref.Item(p.Key)
                    Case "74ALS00N"
                        pout = all_net.Item(.Item("O"))
                        nout = Not (all_net.Item(.Item("I0")) And all_net.Item(.Item("I1")))
                        new_out.Add((.Item("O")), nout)
                    Case "74LS151N"
                        pout = all_net.Item(.Item("Y"))
                        Dim adr As Integer = (all_net.Item(.Item("C")) And &H4) Or (all_net.Item(.Item("B")) And &H2) Or (all_net.Item(.Item("B")) And &H1)
                        nout = all_net.Item(.Item("D" & adr)) And (Not all_net.Item(.Item("G")))
                        new_out.Add(.Item("Y"), nout)
                        'new_out.Add(.Item("W"))
                    Case "74LS86N"
                        pout = all_net.Item(.Item("O"))
                        nout = (all_net.Item(.Item("I0")) Xor all_net.Item(.Item("I1")))
                        new_out.Add((.Item("O")), nout)
                    Case Else
                        pout = False
                        nout = False
                End Select
            End With
            If nout <> pout Then
                If verbose Then
                    Console.WriteLine(p.Key)
                End If

                has_chaged = True
            End If
        Next

        If has_chaged = True Then
            For Each n In new_out
                all_net.Item(n.Key) = n.Value
            Next
        End If
        If verbose Then
            Console.WriteLine("")
        End If
        Return has_chaged
    End Function


End Class
