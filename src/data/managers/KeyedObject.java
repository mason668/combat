package data.managers;

public class KeyedObject {
	private double myKey;
	private Object myObject;
	
	public KeyedObject (double key, Object object){
		if (object == null) return;
		myKey = key;
		myObject = object;
	}
	
	public double getKey(){
		return myKey;
	}
	public Object getObject(){
		return myObject;
	}

}
