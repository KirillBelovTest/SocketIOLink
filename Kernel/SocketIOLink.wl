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
"SocketIOListen[conn, listener, event] listen specific event type."; 


SocketIOEmit::usage = 
"SocketIOEmit[conn, ack, event, obj] emit obj with specific event type."; 


SocketIOObject::usage = 
"SocketIOObject[] representation of SocketIO connection."; 


Begin["`Private`"]; 


CreateType[SocketIOObject, {
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
        javaLTPClient, 
        
        connect, 
        extraHeaders, 
        add, 
        put
    }, 
        socketObj["Endpoint"] = endpoint; 
        socketObj["HTTPHeaders"] = httpHeaders; 
        socketObj["Handler"] = handler; 

        socketObj["EventHandlers"] = <||>;
        socketObj["AckHandlers"] = <||>;

        socketObj["ListenPort"] = listenPort; 

        listenSocket = CSocketOpen[listenPort]; 
        socketObj["ListenSocket"] = listenSocket; 

        listener = SocketListen[listenSocket, listenHandler]; 
        socketObj["Listener"] = listener; 
        socketObj["ListenHandler"] = listenHandler; 

        listenHandler["Accumulator", "LTP"] = LTPPacketQ -> LTPPacketLength; 
        listenHandler["Handler", "LTP"] = LTPPacketQ -> ltpHandler; 

        ltpHandler["Responsible"] = False; 
        ltpHandler["Deserializer"] = ImportByteArray[#, "RawJSON"]&; 
        ltpHandler["Handler"] := socketObj["Handler"][socketObj, #]&; 

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

        socketObj["JavaIOListeners"] = <||>; 

        socketObj["JavaIOAcks"] = <||>; 

        (*Return*)
        socketObj
    ]
]; 


SocketIOListen[socketObj_SocketIOObject, eventName_String, eventHandler_: Echo] := 
Block[{
    on, 
    javaIOListener, 
    javaLTPClient = socketObj["JavaLTPClient"],
    javaIOSocket = socketObj["JavaIOSocket"]
}, 
    socketObj["EventHandlers", eventName] = eventHandler;

    If[!KeyExistsQ[socketObj["JavaIOListeners"], eventName], 
        javaIOListener = JavaNew[LTPForwardListenerClass, eventName, javaLTPClient];
        socketObj["JavaIOListeners", eventName] = javaIOListener;
        javaIOSocket@on[eventName, javaIOListener]
    ];
]; 


SocketIOEmit[socketObj_SocketIOObject, eventName_String, assoc_?AssociationQ, ackHandler_: Echo] := 
Block[{
    emit, javaIOAck, 
    javaIOSocket = socketObj["JavaIOSocket"], 
    javaLTPClient = socketObj["JavaLTPClient"], 
    obj = MakeJavaObject[{toJavaJSON[assoc]}]
}, 
    socketObj["AckHandlers", eventName] = ackHandler;
    
    If[!KeyExistsQ[socketObj["JavaIOAcks"], eventName],
        javaIOAck = JavaNew[LTPForwardAckClass, eventName, javaLTPClient];
        socketObj["JavaIOAcks", eventName] = javaIOAck, 
    (*Else*)
        javaIOAck = socketObj["JavaIOAcks", eventName]
    ]; 
    
    javaIOSocket@emit[eventName, obj, javaIOAck]
]; 


SocketIOObject::errevnt = 
"Unknown event type: `1`";


handlerFunc[socketObj_SocketIOObject, data_] := 
With[{
    eventName = data["name"], 
    eventType = data["type"],
    ackHandlers = socketObj["AckHandlers"], 
    eventHandlers = socketObj["EventHandlers"]
},  
    Which[
        eventType === "ack" && KeyExistsQ[ackHandlers, eventName], ackHandlers[eventName][data], 
        eventType === "event" && KeyExistsQ[eventHandlers, eventName], eventHandlers[eventName][data], 
        Else, Message[SocketIOObject::errevnt, data]
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


Map[AddToClassPath] @ 
Map[Last] @ 
GroupBy[StringRiffle[StringSplit[#, "-"][[;; -2]], "-"]&] @ 
FileNames["*.jar", {FileNameJoin[{$directory, "Java"}]}]; 


End[];


EndPackage[];
