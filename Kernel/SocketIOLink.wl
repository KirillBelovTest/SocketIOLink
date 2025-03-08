(* ::Package:: *)

BeginPackage["KirillBelov`SocketIOLink`", {
    "KirillBelov`Objects`", 
    "KirillBelov`CSockets`TCP`", 
    "KirillBelov`CSockets`Handler`", 
    "KirillBelov`LTP`", 
    "JLink`"
}];


SocketIOConnect::usage = 
"SocketIOConnect[url] connecting to the specific endpoint and returns IOSocketObject.
SocketIOConnect[url, headers] connecting using specific extra HTTP headers."; 


IOSocketListen::usage = 
"IOSocketListen[conn, event, func] listen specific event type and call func as handler."; 


IOSocketEmit::usage = 
"IOSocketEmit[conn, event, obj] emit obj with specific event type."; 


SocketIOObject::usage = 
"SocketIOObject[] representation of SocketIO connection."; 


Begin["`Private`"]; 


CreateType[SocketIOObject, {
    "ListenSocket", 
    "Listener", 
    "JavaIOSocket", 
    "JavaIOListener", 
    "JavaIOAck", 
    "ForwardPort", 
    "JLTPClient"
}]; 


Options[SocketIOConnect] = {
    "HTTPHeaders" -> <||>, 
    "ForwardPort" -> Automatic, 
    "EventHandler" -> socketIOEventHandler
}; 


SocketIOConnect[url_String, OptionsPattern[]] := 
Block[{
    jsocket, connect, jlistener, jack, jltpClient, listener, port,
    httpHeaders = OptionValue["HTTPHeaders"], 
    forwardPort = OptionValue["ForwardPort"], 
    eventHandler = OptionValue["EventHandler"]
}, 
    If[forwardPort === Automatic, 
        port = RandomInteger[{20000, 60000}], 
        port = forwardPort
    ]; 

    listener = createLTPListener[port, eventHandler]; 

    jsocket = createSocketIOJavaObject[url, httpHeaders]; 
    jsocket@connect[]; 

    jltpClient = JavaNew[LTPClientClass, port];

    jlistener = JavaNew[LTPForwardListenerClass, jltpClient]; 
    jack = JavaNew[LTPForwardAckClass, jltpClient]; 

    SocketIOObject[
        "Listener" -> listener, 
        "JSocket" -> jsocket, 
        "JListener" -> jlistener, 
        "JAck" -> jack, 
        "ForwardPort" -> port, 
        "JLTPClient" -> jltpClient
    ]
]; 


SocketIOObject /: SocketIOListen[connection_SocketIOObject, event_String] := 
Block[{
	jsocket = connection["JSocket"], 
	on, 
	jlistener = connection["JListener"]
}, 
	jsocket@on[event, jlistener]; 
]; 


SocketIOObject /: SocketEmit[connection_SocketIOObject, event_String] := 
Block[{
	jsocket = connection["JSocket"], 
	on, 
	jlistener = connection["JListener"]
}, 
	jsocket@on[event, jlistener]; 
]; 


createSocketIOJavaObject[url_String, httpHeaders: _Association?AssociationQ: <||>] := 
Block[{socket, headers, options, add, put, extraHeaders}, 
    IOClass; 

    options = JavaNew[IOOptionsClass]; 

    headers = JavaNew[HashMapClass]; 

    KeyValueMap[Function[{k, v}, 
        Block[{
            vlist = JavaNew[ArrayListClass], 
            kobj = MakeJavaObject[k]
        }, 
            vlist@add[MakeJavaObject[v]]; 
            headers@put[kobj, vlist]; 
        ]
    ], httpHeaders]; 

    options@extraHeaders = headers; 

    socket = io`socket`client`IO`socket[url, options]; 

    (*Return*)
    socket
]; 


createLTPListener[port_Integer?Positive, eventHandler_] := 
With[{ltp = LTPHandler[]}, 
    Block[{server, handler, listener}, 
        ltp["MessageHandler"] = eventHandler; 
        ltp["Deserializer"] = ByteArrayToString; 
        ltp["Responsible"] = False; 

        handler = CSocketHandler[]; 
        
        handler["Accumulator", "LTP"] = LTPPacketQ -> LTPPacketLength; 
        handler["Handler", "LTP"] = LTPPacketQ -> ltp; 

        server = CSocketOpen[port]; 
        listener = SocketListen[server, handler]; 
        
        (*Return*)
        listener
    ]
]; 


socketIOEventHandler[packet_] := 
Echo[packet]; 


IOClass := IOClass = 
LoadJavaClass["io.socket.client.IO"]; 


IOOptionsClass := IOOptionsClass = 
LoadJavaClass["io.socket.client.IO$Options"]; 


JSONObjectClass := JSONObjectClass = 
LoadJavaClass["org.json.JSONObject"]; 


LTPClientClass := LTPClientClass = 
LoadJavaClass["kirillbelov.ltp.LTPClient"];


LTPForwardAckClass := LTPForwardAckClass = 
LoadJavaClass["kirillbelov.socketiolink.LTPForwardAck"]; 


LTPForwardListenerClass := LTPForwardListenerClass = 
LoadJavaClass["kirillbelov.socketiolink.LTPForwardListener"]; 


HashMapClass := HashMapClass = 
LoadJavaClass["java.util.HashMap"]; 


ArrayListClass := ArrayListClass = 
LoadJavaClass["java.util.ArrayList"]; 


$directory = 
DirectoryName[$InputFileName, 2]; 


Map[AddToClassPath] @ 
Map[Last] @ 
GroupBy[StringRiffle[StringSplit[#, "-"][[;; -2]], "-"]&] @ 
FileNames["*.jar", {FileNameJoin[{$directory, "Java"}]}]; 


End[];


EndPackage[];