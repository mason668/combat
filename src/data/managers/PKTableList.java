package data.managers;

import data.csd.PKTable;
import utils.Logger;

public class PKTableList extends ObjectList{

	public PKTable get (String name){
		Object o = this.get(name);
		if ( !( o instanceof PKTable)) return null;
		return (PKTable) o;
	}
	
	public void add (PKTable p){
		if ( !( p instanceof PKTable)) return;
		String name = p.getName();
		Logger.log(Logger.FINEST, "adding pktable " + name);
		super.add(name, p);
	}
}
