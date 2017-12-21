package data.managers;

import data.csd.Sensor;

public class SensorList extends ObjectList{

	public Sensor getSensor (String name){
		Object o = this.get(name);
		if ( !( o instanceof Sensor)) return null;
		return (Sensor) o;
	}
	
	public void add (Sensor item){
		if ( !( item instanceof Sensor)) return;
		String name = item.getName();
		super.add(name, item);
	}
	
	public Sensor getFirst(){
		Object o = super.getFirst();
		if (!(o instanceof Sensor)) return null;
		return (Sensor)o;
	}
	
}
