package data.map;

import data.managers.ObjectList;

public class RiverTypeList extends ObjectList {

	public RiverType getRiverType (String name){
		Object o = this.get(name);
		if ( !( o instanceof RiverType)) return null;
		return (RiverType) o;
	}
	
	public void add (RiverType item){
		String name = item.getName();
		super.add(name, item);
	}
	
}
