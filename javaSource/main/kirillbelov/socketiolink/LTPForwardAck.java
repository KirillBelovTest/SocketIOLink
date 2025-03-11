package kirillbelov.socketiolink;

import io.socket.client.Ack;
import kirillbelov.ltp.LTPClient;

import org.json.JSONArray;

import java.io.IOException;

public class LTPForwardAck implements Ack {
    private LTPClient ltpClient; 

    public LTPClient getLTPClient(){
        return ltpClient;
    }

    public void setLTPClient(LTPClient ltpClient){
        this.ltpClient = ltpClient;
    }
    
    public LTPForwardAck(LTPClient ltpClient){
        this.ltpClient = ltpClient; 
    }

    @Override
    public void call(Object... args){
        try{
            JSONArray jsonArray = new JSONArray(); 
            
            for (Object o : args) {
                jsonArray.put(o);
            }

            System.out.println("LTPForwardAck.call: " + jsonArray.toString()); 
            
            ltpClient.send(jsonArray.toString());
        } 
        catch (IOException e){

        }
    }
}
