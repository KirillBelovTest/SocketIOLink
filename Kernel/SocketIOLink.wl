(* ::Package:: *)

BeginPackage["KirillBelov`SocketIOLink`", {
    "KirillBelov`Objects`", 
    "KirillBelov`CSockets`", 
    "KirillBelov`LTP`", 
    "JLink`"
}];


SocketIOConnect::usage = 
"SocketIOConnect[url, headers] connecting to the specific endpoint and returns IOSocketObject."; 


IOSocketListen::usage = 
"IOSocketListen[conn, event, func] listen specific event type and call func as handler."; 


IOSocketEmit::usage = 
"IOSocketEmit[conn, event, obj] emit obj with specific event type."; 


SocketIOObject::usage = 
"SocketIOObject[] representation of SocketIO connection."; 


Begin["`Private`"]; 


CreateType[SocketIOObject, {
    "Socket", "Listener", "Port", "JAsk", "JListener"
}]; 


Options[SocketIOConnect] = {
    "HTTPHeaders" -> <||>, 
    "ForwardPort" -> Automatic
}; 


SocketIOConnect[url_String, OptionsPattern[]] := 
Block[{
    socket, connect, 
    httpHeaders = OptionValue["HTTPHeaders"], 
    frowardPort = OptionValue["ForwardPort"]
}, 
    socket = createJavaIOSocket[url, httpHeaders]; 
    socket@connect[]; 

    port = RandomInteger[20000, 60000]; 

    server = CSocketOpen[port]; 

    ltp = LTPHandler[]; 

    handler = CSocketHandler[]; 

    SocketIOObject[<|
        "Socket" -> socket, 
        "Listener" -> listener
    |>]
]; 


SocketIOListen[SocketIOObject[assoc_Association?AssociationQ], event_String] := 
{}; 


createJavaIOSocket[url_String, httpHeaders: _Association?AssociationQ: <||>] := 
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





IOClass := IOClass = 
LoadJavaClass["io.socket.client.IO"]; 


IOOptionsClass := IOOptionsClass = 
LoadJavaClass["io.socket.client.IO$Options"]; 


JSONObjectClass := JSONObjectClass = 
LoadJavaClass["org.json.JSONObject"]; 


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