package kirillbelov.socketiolink;

import io.socket.client.Ack;
import kirillbelov.ltp.LTPClient;

import java.util.Arrays;
import java.io.IOException;

import java.util.Arrays;

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
        System.out.println("LTPForwardAck.call: " + Arrays.toString(args));
        
        ltpClient.sendMessage(Arrays.toString(args));
    }
}
