package data.managers;

import java.util.HashMap;

import sim.Constants;
import sim.forces.Force;

public class ForceList extends ObjectList {
	
	public Force getForce (String name){
		Object o = this.get(name);
		if ( !( o instanceof Force)) return null;
		return (Force) o;
	}
	
	public void add (Force item){
		if ( !( item instanceof Force)) return;
		String name = item.getName();
		super.add(name, item);
	}
	
}
