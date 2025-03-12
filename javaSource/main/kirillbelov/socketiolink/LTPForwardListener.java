package kirillbelov.socketiolink;

import io.socket.emitter.Emitter;
import kirillbelov.ltp.LTPClient;
import java.io.IOException;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class LTPForwardListener implements Emitter.Listener {
    private String name;
    private LTPClient ltpClient; 

    public String getName(){
        return name; 
    }

    public LTPClient getLTPClient(){
        return ltpClient; 
    }

    public void setLTPClient(LTPClient ltpClient){
        this.ltpClient = ltpClient; 
    }

    public void setName(String name){
        this.name = name; 
    }
    
    public LTPForwardListener(String name, LTPClient ltpClient){
        super(); 
        this.name = name;
        this.ltpClient = ltpClient; 
    }
    
    @Override
    public void call(Object... args){
        try {
            JSONArray jsonArray = new JSONArray(); 
            
            for (Object o : args) {
                jsonArray.put(o);
            }

            JSONObject jsonObject = new JSONObject();

            jsonObject.put("type", "event");
            jsonObject.put("name", name);
            jsonObject.put("data", jsonArray);

            System.out.println("LTPForwardListener.call: " + jsonObject.toString()); 
            
            ltpClient.send(jsonObject.toString());
        } catch (IOException e) {
            e.printStackTrace();
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }
}