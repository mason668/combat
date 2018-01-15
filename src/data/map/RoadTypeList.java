package data.map;

import data.managers.ObjectList;

public class RoadTypeList extends ObjectList {

	public RoadType getRoadType (String name){
		Object o = this.get(name);
		if ( !( o instanceof RoadType)) return null;
		return (RoadType) o;
	}
	
	public void add (RoadType item){
		String name = item.getName();
		super.add(name, item);
	}
	
}
