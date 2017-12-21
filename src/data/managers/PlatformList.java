package data.managers;

import data.csd.Platform;
import data.csd.Weapon;
import utils.Logger;

public class PlatformList extends ObjectList{
	
	public static void main(String[] args){
		PlatformList me = new PlatformList();
		Platform p1 = new Platform("one");
		Platform p2 = new Platform("two");
		me.add(p1.getName(), p1);
		me.add(p2.getName(),p2);
		me.add(p2.getName(),p2);
		
		Platform p3 = (Platform) me.get("three");
		Platform p4 = (Platform) me.get("one");
		Platform p5 = me.getPlatform("one");
	}
	
	public Platform getPlatform (String name){
		Object o = this.get(name);
		if ( !( o instanceof Platform)) return null;
		return (Platform) o;
	}
	
	public void add (Platform item){
		if ( !( item instanceof Platform)) return;
		String name = item.getName();
		//Logger.log(Logger.FINEST, "adding platform " + name); //TODO remove
		super.add(name, item);
	}
}
