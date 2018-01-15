package data.map;

import data.managers.ObjectList;

public class WallTypeList extends ObjectList {

	public WallType getWallType (String name){
		Object o = this.get(name);
		if ( !( o instanceof WallType)) return null;
		return (WallType) o;
	}
	
	public void add (WallType item){
		String name = item.getName();
		super.add(name, item);
	}
	
}
