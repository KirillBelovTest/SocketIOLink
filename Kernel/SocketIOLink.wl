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


SocketIOListen::usage = 
"SocketIOListen[conn, event] listen specific event type."; 


SocketIOEmit::usage = 
"SocketIOEmit[conn, event, obj] emit obj with specific event type."; 


SocketIOObject::usage = 
"SocketIOObject[] representation of SocketIO connection."; 


Begin["`Private`"]; 


CreateType[SocketIOObject, {
    "Endpoint", 
    "HTTPHeaders", 
    "Handler", 
    
    "ListenPort", 
    "ListenSocket", 
    "ListenHandler",  
    "Listener", 

    "JavaIOSocket", 
    "JavaIOListener", 
    "JavaIOAck", 
    "JavaLTPClient"
}]; 


Options[SocketIOConnect] = {
    "HTTPHeaders" -> <||>, 
    "Handler" -> handlerFunc
}; 


SocketIOConnect[endpoint_String, OptionsPattern[]] := 
With[{
    socketObj = SocketIOObject[], 
    listenHandler = CSocketHandler[], 
    ltpHandler = LTPHandler[]
}, 
    Block[{
        httpHeaders = OptionValue["HTTPHeaders"], 
        handler = OptionValue["Handler"], 
        listenPort = RandomInteger[{20000, 60000}], 
        listenSocket, 
        listener, 
        
        javaIOSocket, 
        javaIOOptions, 
        javaIOHeaders, 
        javaIOListener, 
        javaIOAck, 
        javaLTPClient, 
        
        connect, 
        extraHeaders, 
        add, 
        put
    }, 
        socketObj["Endpoint"] = endpoint; 
        socketObj["HTTPHeaders"] = httpHeaders; 
        socketObj["Handler"] = handler; 

        socketObj["ListenPort"] = listenPort; 

        listenSocket = CSocketOpen[listenPort]; 
        socketObj["ListenSocket"] = listenSocket; 

        listener = SocketListen[listenSocket, listenHandler]; 
        socketObj["Listener"] = listener; 
        socketObj["ListenerHandler"] = listenHandler; 

        listenHandler["Accumulator", "LTP"] = LTPPacketQ -> LTPPacketLength; 
        listenHandler["Handler", "LTP"] = LTPPacketQ -> ltpHandler; 

        ltpHandler["Responsible"] = False; 
        ltpHandler["Deserializer"] = ByteArrayToString; 
        ltpHandler["Handler"] := socketObj["Handler"]; 

        IOClass; 

        javaIOOptions = JavaNew[IOOptionsClass];

        javaIOHeaders = JavaNew[HashMapClass]; 

        KeyValueMap[Function[{key, value}, 
            Block[{
                valueList = JavaNew[ArrayListClass], 
                keyObject = MakeJavaObject[key]
            }, 
                valueList@add[MakeJavaObject[value]]; 
                javaIOHeaders@put[keyObject, valueList]; 
            ]
        ], httpHeaders]; 

        javaIOOptions@extraHeaders = javaIOHeaders; 

        javaIOSocket = io`socket`client`IO`socket[endpoint, javaIOOptions]; 
        
        socketObj["JavaIOSocket"] = javaIOSocket; 

        javaIOSocket@connect[]; 

        javaLTPClient = JavaNew[LTPClientClass, listenPort]; 
        socketObj["JavaLTPClient"] = javaLTPClient; 

        javaIOListener = JavaNew[LTPForwardListenerClass, javaLTPClient]; 
        socketObj["JavaIOListener"] = javaIOListener; 

        javaIOAck = JavaNew[LTPForwardAckClass, javaLTPClient]; 
        socketObj["JavaIOAck"] = javaIOAck; 

        socketObj
    ]
]; 


SocketIOListen[socketObj_SocketIOObject, eventName_String] := 
Block[{on, javaIOSocket = socketObj["JavaIOSocket"], javaIOListener}, 
    javaIOSocket@on[eventName, javaIOListener]
]; 


handlerFunc[packet_] := 
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