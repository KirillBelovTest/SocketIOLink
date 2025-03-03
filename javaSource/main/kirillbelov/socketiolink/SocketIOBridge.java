package kirillbelov.socketiolink;

import io.socket.client.IO;
import io.socket.client.Socket;
import io.socket.emitter.Emitter;
import java.util.*;

public class SocketIOBridge {

    public static void main(String[] args) {
        SocketIOBridge bridge = new SocketIOBridge("https://access.ccdb.waexservices.com", "fWomFCcS", "ccdb.sid=s%3AhK2JuUnzRHmfseIwpElthdsN5EFu5VAp.LRbVlMgbh8%2BaK8t34E2BiHDoV7mbiNO%2Ft9jOkf6PFPo; Path=/; Expires=Thu, 27 Feb 2025 00:56:09 GMT; HttpOnly; SameSite=Strict");
        bridge.connect();
    }

    private Socket socket;

    public Socket getSocket(){
        return socket;
    }

    public SocketIOBridge(String url, String csrfToken, String cookies) {
        try {
            IO.Options options = new IO.Options();

			Map<String, List<String>> headers = new HashMap<>();
            headers.put("csrf_token", Arrays.asList(csrfToken));

			headers.put("Cookie", Arrays.asList(cookies));
            options.extraHeaders = headers;
            
            socket = IO.socket(url, options);
        } catch(Exception e) {
            e.printStackTrace();
        }
    }

    public void connect() {
        socket.connect();
        System.out.println("connected to socket");
    }

    public void disconnect() {
        socket.disconnect();
    }

    public void subscribe(String event, Emitter.Listener listener) {
        socket.on(event, listener);
    }

    public void unsubscribe(String event, Emitter.Listener listener) {
        socket.off(event, listener);
    }

    public void sendMessage(String event, Object message) {
        socket.emit(event, message);
    }
}