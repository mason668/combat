package data.map;

import data.managers.ObjectList;

public class FeatureTypeList extends ObjectList {

	public FeatureType getFeatureType (String name){
		Object o = this.get(name);
		if ( !( o instanceof FeatureType)) return null;
		return (FeatureType) o;
	}
	
	public void add (FeatureType item){
		String name = item.getName();
		super.add(name, item);
	}
	
}
