package data.managers;

import data.csd.Weapon;

public class WeaponList extends ObjectList{

	public Weapon getWeapon (String name){
		Object o = this.get(name);
		if ( !( o instanceof Weapon)) return null;
		return (Weapon) o;
	}
	
	public void add (Weapon item){
		if ( !( item instanceof Weapon)) return;
		String name = item.getName();
		super.add(name, item);
	}
}
