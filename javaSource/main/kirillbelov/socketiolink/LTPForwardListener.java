package kirillbelov.socketiolink;

import io.socket.emitter.Emitter;
import kirillbelov.ltp.LTPClient;

import java.io.IOException;
import java.util.Arrays;

import org.json.JSONArray;

public class LTPForwardListener implements Emitter.Listener {
    private LTPClient ltpClient; 

    public LTPClient getLTPClient(){
        return ltpClient; 
    }

    public void setLTPClient(LTPClient ltpClient){
        this.ltpClient = ltpClient; 
    }
    
    public LTPForwardListener(LTPClient ltpClient){
        super(); 
        this.ltpClient = ltpClient; 
    }
    
    @Override
    public void call(Object... args){
        try {
            JSONArray jsonArray = new JSONArray(); 
            
            for (Object o : args) {
                jsonArray.put(o);
            }

            System.out.println("LTPForwardListener.call: " + jsonArray.toString()); 
            
            ltpClient.send(jsonArray.toString());
        } catch (IOException e) {
            
        }
    }
}