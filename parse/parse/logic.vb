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

    Function prop_delay(ByRef all_net As Dictionary(Of String, Boolean), ByVal inp As List(Of String), Optional ByVal detail As Boolean = False) As Integer
        Dim inp_c As Integer = inp.Count - 1
        Dim states As New List(Of state_transition)

        'previous state -> new state
        For ic As Integer = 0 To 2 ^ (inp.Count) - 1
            For ip As Integer = 0 To 2 ^ (inp.Count) - 1
                If ic <> ip Then
                    states.Add(New state_transition With {.curr = New Dictionary(Of String, Boolean),
                                                     .prev = New Dictionary(Of String, Boolean)()})
                    For j As Integer = 0 To inp.Count - 1
                        states.Last.prev.Add(inp(j), (ip >> j) Mod 2)
                        states.Last.curr.Add(inp(j), (ic >> j) Mod 2)
                    Next
                End If
            Next
        Next

        Dim count As Integer
        Dim maxcount As Integer = 0

        'for all config
        For Each s In states
            'put the system in prev state
            For Each n In s.prev
                all_net.Item(n.Key) = n.Value
            Next

            While compute(all_net)
            End While

            For Each n In s.curr
                all_net.Item(n.Key) = n.Value
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
            End If
        Next

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
