(ns spork.sim.impure.observable)

;This is a simple implementation of the oberserver pattern.
;An observer serves as an interface for registering "listeners" that respond to 
;any number of external stimuli by triggering subscribed procedures...
;This is an explicit implementation of native .Net language features.  When we 
;port to .Net or clojure, we "probably" won;t even need this class.

;Basically, the observer keeps an internal list of procedures to call.  We;ll do
;this with object references So, an observer keeps a list of Callables. A 
;callable is just a delegate<;T>, basically it takes an argument and performs 
;some sideffect with it. We will use observers to flexibly automate tasks like 
;logging events, keep track of statistics, recording graphs and trends, etc.

;As a result, the main control flow of the program will populate the event 
;stream, which can be listened to [observed] by any number of registered 
;observers.  The observers will, by convention, perform side effects that 
;DO NOT affect the control flow of the simulation.  They are primarily 
;responsible for recording state, updating the user interface, writing to files, 
;storing statistics for later retrieval. Basically, they accessorize the engine, 
;and provide an easily extendable mechanism to add lots and lots of ad-hoc 
;functionality to the basic simulation without screwing with the dedicated 
;internals of the simulation.

;Note-> we CAN use observers to create side-effects that control the flow of the 
;simulation, but I propose avoiding using them in this manner, to avoid some 
;nasty debugging issues.  The fact that we can use them for this purpose is 
;nice to have around, but it should be done with care and only when necessary.

;Note -> we can also use observers to model functional composition, by passing 
;state references through an observer chain, where each oberver partially 
;mutates the referenced state. While useful, this is again discouraged....
;mutation can inhibit reasoning about the program state, and could create funky 
;bugs that are hard to spot.  Still, if necessary, it could be a powerful tool.

;Note -> there is currently no order of execution in notifying the observers 
;(actually, the order they register is the order of execution.  The net effect
;of this is that observations should be cummutative i.e., the effect of an
;observation should be independent of order of execution. This is another 
;argument against using observers as mutators of state, or signal chains, since 
;the designer would need to be extra vigilant about the order of execution.
;Note -> all of these concerns are moot in F# and clojure.
Option Explicit

Public name As String
Public clients As Dictionary
Public subscriptions As Dictionary

Private emptydict As Dictionary

Public subscribef As IFn

Implements IObservable
Implements ITriggerable

Private Sub Class_Initialize()
Set clients = New Dictionary
Set subscriptions = New Dictionary
Set emptydict = New Dictionary
name = "Anonymous"
Set subscribef = Nothing
End Sub


'All an observer needs to do is provide and interface that allows interested parties (ICallable) to
'register themselves with the observer.

Public Sub Register(clientname As String, client As ITriggerable, Optional msgid As Long)
'bookkeeping...we only want to allow 2 classes of registration: universal and specific.
'universal register with msgid = 0, specific is any nonzero.
'if a msgid is nonzero, we make sure it's not pre-registered as universal.
'if it is, we remove it from universal.
Dim key
Dim ptr As Dictionary

If subscribef Is Nothing Then

    If clients.exists(clientname) Then
        Set ptr = clients(clientname)
    Else
        Set ptr = New Dictionary
        clients.add clientname, ptr
    End If
    ptr.add msgid, msgid
    
    If msgid <> 0 And ptr.exists(0) Then
        unRegister clientname, 0
    ElseIf msgid = 0 And ptr.count > 1 Then
        For Each key In ptr
            If CLng(key) <> 0 Then unRegister clientname, CLng(key)
        Next key
    End If
    
    If subscriptions.exists(msgid) Then
        Set ptr = subscriptions(msgid)
    Else
        Set ptr = New Dictionary
        subscriptions.add msgid, ptr
    End If
    ptr.add clientname, client
    
    Set ptr = Nothing
Else
    Call subscribef.apply(list(Me, clientname, client, msgid))
End If

End Sub

Public Sub unRegister(clientname As String, Optional msgid As Long)
Dim ptr As Dictionary

If subscriptions.exists(msgid) Then
    Set ptr = subscriptions(msgid)
    If ptr.exists(clientname) Then
        ptr.Remove clientname
        If ptr.count = 0 Then subscriptions.Remove msgid
        
        Set ptr = clients(clientname)
        ptr.Remove (msgid)
        If ptr.count = 0 Then
            Set ptr = Nothing
            clients.Remove (clientname)
        End If
    End If
End If
Set ptr = Nothing

End Sub
Public Sub clearClient(clientname As String)
Dim evt
Dim ptr As Dictionary

Static subs As Dictionary
If clients.exists(clientname) Then
    Set ptr = clients(clientname)
    If ptr.count > 0 Then
        For Each evt In ptr
            Set subs = subscriptions(CLng(evt))
            subs.Remove clientname
        Next evt
        clients.Remove clientname
    End If
End If
Set ptr = Nothing

End Sub
'TOM Change 30 June 2011 -> state is now passed as a genericpacket.
'Public Sub notify(msgid As Long, Optional state As Dictionary)
Public Sub notify(msgid As Long, Optional state As GenericPacket)
Dim clientname
Dim client As ITriggerable
Dim ptr As Dictionary

If clients.count > 0 Then
    'recursively notify indiscriminate listeners....things listening for 0
    If subscriptions.exists(msgid) Then
        Set ptr = subscriptions(msgid)
        For Each clientname In ptr
            Set client = ptr(clientname)
            client.trigger msgid, state
        Next clientname
    End If
    
    'If msgid <> 0 Then notify 0, state 'notify universal listeners....
    If subscriptions.exists(0) Then
        Set ptr = subscriptions(0)
        For Each clientname In ptr
            Set client = ptr(clientname)
            client.trigger msgid, state
        Next clientname
    End If
'Else
    'Debug.Print "no clients registered for "; msgid & " under observer " & name
End If

Set ptr = Nothing

End Sub

Public Function getClients(msgid As Long) As Dictionary

If subscriptions.exists(msgid) Then
    Set getClients = subscriptions(msgid)
Else
    Set getClients = emptydict
End If

End Function

Public Function getSubscriptions(clientname As String) As Dictionary

If clients.exists(clientname) Then
    Set getSubscriptions = clients(clientname)
Else
    Set getSubscriptions = New Dictionary
End If
    
End Function

Private Sub Class_Terminate()

Set clients = Nothing
Set subscriptions = Nothing

'Set ptr = Nothing
Set emptydict = Nothing

End Sub



Private Function IObservable_getRegistered() As Collection
Set IObservable_getRegistered = list(clients, subscriptions)
End Function

'observers can be triggered, so observers can subscribe to eachother.
'their trigger simply points to the notify method.
'TOM Change 30 june 2011
'Private Sub ITriggerable_trigger(msgid As Long, Optional data As Scripting.IDictionary)
Private Sub ITriggerable_trigger(msgid As Long, Optional data As GenericPacket)
notify msgid, data
End Sub

'IObservable implementations mirror earlier ones.
Private Sub IObservable_clearClient(clientname As String)
clearClient clientname
End Sub

Private Function IObservable_getClients(msgid As Long) As Scripting.IDictionary
Set IObservable_getClients = getClients(msgid)
End Function

Private Function IObservable_getSubscriptions(clientname As String) As Scripting.IDictionary
Set IObservable_getSubscriptions = getSubscriptions(clientname)
End Function

Private Sub IObservable_notify(msgid As Long, Optional state As GenericPacket)
notify msgid, state
End Sub

Private Sub IObservable_Register(clientname As String, client As ITriggerable, Optional msgid As Long)
Register clientname, client, msgid
End Sub

Private Sub IObservable_unRegister(clientname As String, Optional msgid As Long)
unRegister clientname, msgid
End Sub

