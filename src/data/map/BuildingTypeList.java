package data.map;

import data.managers.ObjectList;

public class BuildingTypeList extends ObjectList {

	public BuildingType getBuildingType (String name){
		Object o = this.get(name);
		if ( !( o instanceof BuildingType)) return null;
		return (BuildingType) o;
	}
	
	public void add (BuildingType item){
		String name = item.getName();
		super.add(name, item);
	}
	
}
