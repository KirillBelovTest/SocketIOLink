(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["KirillBelov`SocketIOLink`", {
    "KirillBelov`Objects`"
}];


(* ::Text:: *)
(*Declare your public symbols here:*)


IOSocketConnect::usage = 
"SocketIOConnect[url, headers] connecting to the specific endpoint and returns IOSocketObject."; 


IOSocketListen::usage = 
"IOSocketListen[conn, event, func] lsiten specific event type and call func as handler."; 


IOSocketEmit::usage = 
"IOSocketEmit[conn, event, obj] emit obj with specific event type."; 


IOSocketObject::usage = 
"IOSocketObject[] representation of SocketIO connection."; 


Begin["`Private`"]; 





$directory = 
DirectoryName[$InputFileName, 2]; 


IOClass := IOClass = 
LoadJavaClass["io.socket.client.IO"]; 


JSONObjectClass := JSONObjectClass = 
LoadJavaClass["org.json.JSONObject"]; 


TCPForwardAckClass := TCPForwardAckClass = 
LoadJavaClass["kirillbelov.socketiolink.TCPForwardAckClass"]; 


TCPForwardListenerClass := TCPForwardListenerClass = 
LoadJavaClass["kirillbelov.socketiolink.TCPForwardListenerClass"]; 


IOOptionsClass := IOOptionsClass = 
LoadJavaClass["io.socket.client.IO$Options", StaticsVisible -> True]; 


JavaHashMapClass := JavaHashMapClass = 
LoadJavaClass["java.util.HashMap", StaticsVisible -> True]; 


JavaArrayListClass := JavaArrayListClass = 
LoadJavaClass["java.util.ArrayList", StaticsVisible -> True]; 


End[];


EndPackage[];