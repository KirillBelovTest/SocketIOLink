package kirillbelov.socketiolink;

import io.socket.emitter.Emitter;
import kirillbelov.ltp.LTPClient;

import java.io.IOException;
import java.util.Arrays;

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
            System.out.println("LTPForwardListener.call: " + Arrays.toString(args)); 
            ltpClient.sendMessage(Arrays.toString(args));
        } catch (IOException e) {
            
        }
    }
}