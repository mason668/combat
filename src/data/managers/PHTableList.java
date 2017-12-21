package data.managers;

import data.csd.PHTable;
import utils.Logger;

public class PHTableList extends ObjectList{

	public PHTable get (String name){
		Object o = super.get(name);
		if ( !( o instanceof PHTable)) return null;
		return (PHTable) o;
	}
	
	public void add (PHTable p){
		if ( !( p instanceof PHTable)) return;
		String name = p.getName();
		Logger.log(Logger.FINEST, "adding phtable " + name);
		super.add(name, p);
	}
}
