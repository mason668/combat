package data.map;

import data.managers.ObjectList;

public class AreaTypeList extends ObjectList {

	public AreaType getAreaType (String name){
		Object o = this.get(name);
		if ( !( o instanceof AreaType)) return null;
		return (AreaType) o;
	}
	
	public void add (AreaType item){
		String name = item.getName();
		super.add(name, item);
	}
	
}
