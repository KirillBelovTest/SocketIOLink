(* ::Package:: *)

BeginPackage["KirillBelov`SocketIOLink`", {
    "KirillBelov`Objects`", 
    "KirillBelov`CSockets`TCP`", 
    "KirillBelov`CSockets`Handler`", 
    "KirillBelov`LTP`", 
    "JLink`"
}];


SocketIOConnect::usage = 
"SocketIOConnect[url] connecting to the specific endpoint and returns SocketIOConnection.
SocketIOConnect[url, headers] connecting using specific extra HTTP headers."; 


SocketIOListen::usage = 
"SocketIOListen[conn, event, handler] listen specific event type."; 


SocketIOListenAll::usage = 
"SocketIOListenAll[conn, handler] listen all events."; 


SocketIOEmit::usage = 
"SocketIOEmit[conn, event, data, handler] emit data with specific event type."; 


SocketIOConnection::usage = 
"SocketIOConnection[] representation of SocketIO connection."; 


Begin["`Private`"]; 


CreateType[SocketIOConnection, {
    "Icon" -> $connectionIcon, 
    "PublicFields" -> {"Endpoint"}, 
    "Connected" -> False, 

    "Endpoint", 
    "HTTPHeaders", 
    "Handler", 

    "EventHandlers",
    "AckHandlers",
    
    "ListenPort", 
    "ListenSocket", 
    "ListenHandler",  
    "Listener", 

    "JavaIOSocket", 
    "JavaIOListeners", 
    "JavaIOAcks", 
    "JavaLTPClient"
}]; 


Options[SocketIOConnect] = {
    "HTTPHeaders" -> <||>, 
    "Handler" -> handlerFunc
}; 


SocketIOConnect[endpoint_String, OptionsPattern[]] := 
With[{
    connection = SocketIOConnection[], 
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
        javaLTPClient, 
        
        connect, 
        extraHeaders, 
        add, 
        put
    }, 
        connection["Endpoint"] = endpoint; 
        connection["HTTPHeaders"] = httpHeaders; 
        connection["Handler"] = handler; 

        connection["EventHandlers"] = <||>;
        connection["AckHandlers"] = <||>;

        connection["ListenPort"] = listenPort; 

        listenSocket = CSocketOpen[listenPort]; 
        connection["ListenSocket"] = listenSocket; 

        listener = SocketListen[listenSocket, listenHandler]; 
        connection["Listener"] = listener; 
        connection["ListenHandler"] = listenHandler; 

        listenHandler["Accumulator", "LTP"] = LTPPacketQ -> LTPPacketLength; 
        listenHandler["Handler", "LTP"] = LTPPacketQ -> ltpHandler; 

        ltpHandler["Responsible"] = False; 
        ltpHandler["Deserializer"] = ImportByteArray[#, "RawJSON"]&; 
        ltpHandler["Handler"] := connection["Handler"][connection, #]&; 

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
        
        connection["JavaIOSocket"] = javaIOSocket; 

        connection["Connected"] = connection["JavaIOSocket"] @ connected[];

        javaIOSocket@connect[]; 

        javaLTPClient = JavaNew[LTPClientClass, listenPort]; 
        connection["JavaLTPClient"] = javaLTPClient; 

        connection["JavaIOListeners"] = <||>; 

        connection["JavaIOAcks"] = <||>; 

        (*Return*)
        connection
    ]
]; 


SocketIOListen[connection_SocketIOConnection, eventName_String, eventHandler_: Identity] := 
Block[{
    on, 
    javaIOListener, 
    javaLTPClient = connection["JavaLTPClient"],
    javaIOSocket = connection["JavaIOSocket"]
}, 
    connection["EventHandlers", eventName] = eventHandler;

    If[!KeyExistsQ[connection["JavaIOListeners"], eventName], 
        javaIOListener = JavaNew[LTPForwardListenerClass, eventName, javaLTPClient];
        connection["JavaIOListeners", eventName] = javaIOListener;
        javaIOSocket@on[eventName, javaIOListener]
    ];

    (*Return*)
    connection
]; 


SocketIOListenAll[connection_SocketIOConnection, eventHandler_: Identity] := 
Block[{
    onAnyIncoming, 
    javaIOListener, 
    javaLTPClient = connection["JavaLTPClient"],
    javaIOSocket = connection["JavaIOSocket"]
}, 
    connection["EventHandlers", "*"] = eventHandler;

    If[!KeyExistsQ[connection["JavaIOListeners"], "*"], 
        javaIOListener = JavaNew[LTPForwardListenerClass, "*", javaLTPClient];
        connection["JavaIOListeners", "*"] = javaIOListener;
        javaIOSocket@onAnyIncoming[javaIOListener]
    ]; 

    (*Return*)
    connection
];


SocketIOEmit[connection_SocketIOConnection, eventName_String, assoc_?AssociationQ, ackHandler_: Identity] := 
Block[{
    emit, javaIOAck, 
    javaIOSocket = connection["JavaIOSocket"], 
    javaLTPClient = connection["JavaLTPClient"], 
    obj = MakeJavaObject[{toJavaJSON[assoc]}]
}, 
    connection["AckHandlers", eventName] = ackHandler;
    
    If[!KeyExistsQ[connection["JavaIOAcks"], eventName],
        javaIOAck = JavaNew[LTPForwardAckClass, eventName, javaLTPClient];
        connection["JavaIOAcks", eventName] = javaIOAck, 
    (*Else*)
        javaIOAck = connection["JavaIOAcks", eventName]
    ]; 
    
    javaIOSocket@emit[eventName, obj, javaIOAck]; 

    (*Return*)
    connection
]; 


SocketIOConnection::errevnt = 
"Unknown event type: `1`";


handlerFunc[connection_SocketIOConnection, data_?AssociationQ] := 
With[{
    eventName = data["name"], 
    eventType = data["type"],
    ackHandlers = connection["AckHandlers"], 
    eventHandlers = connection["EventHandlers"], 
    arg = Append[data, "connection" -> connection]
}, 
    Module[{res}, 
        Echo[data, "handlerFunc"];
        res = Which[
            eventType === "ack" && KeyExistsQ[ackHandlers, eventName], Echo[ackHandlers[eventName][arg], "handlerFunc Ack"], 
            eventType === "event" && KeyExistsQ[eventHandlers, eventName], eventHandlers[eventName][arg], 
            True, Message[SocketIOConnection::errevnt, arg]
        ];
        Echo[data, "handlerFunc Success"];
        res
    ]
]; 


toJavaJSON[assoc_?AssociationQ] := 
Block[{json = JavaNew[JSONObjectClass], put}, 
    KeyValueMap[json@put[#1, toJavaJSON[#2]]&, assoc]; 
    json
]; 


toJavaJSON[value: _String | _?NumericQ] := 
MakeJavaObject[value]; 


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


$connectionIcon = 
Import[FileNameJoin[{$directory, "Images", "socket-io.png"}]]; 


Map[AddToClassPath] @ 
Map[Last] @ 
GroupBy[StringRiffle[StringSplit[#, "-"][[;; -2]], "-"]&] @ 
FileNames["*.jar", {FileNameJoin[{$directory, "Java"}]}]; 


End[];


EndPackage[];
