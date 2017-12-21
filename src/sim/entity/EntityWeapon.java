package sim.entity;

import data.csd.Weapon;
import sim.Constants;
import data.csd.PlatformWeapon;

public class EntityWeapon {
	
	public EntityWeapon(PlatformWeapon w){
		weaponSlot = w;
		weapon = w.getWeapon();
	}
	private Weapon weapon;
	public Weapon getWeapon(){return weapon;}
	
	private PlatformWeapon weaponSlot;
	public PlatformWeapon getWeaponSlot(){return weaponSlot;}
	
	private double reloadTime= 0.0;
	public double getReloadTime(){return reloadTime;}
	public void setReloadTime(double d){reloadTime = d;}
	
	private int currentAmmo = 100;
	public int getCurrentAmmo(){return currentAmmo;}
	public void setCurrentAmmo(int i){currentAmmo = i;}
	
	private EntityWeapon alternateWeapon = null;
	public EntityWeapon getAlternateWeapon(){return alternateWeapon;}
	public void setAlternateWeapon(EntityWeapon weapon){alternateWeapon = weapon;}
	
	private double firedLast = -Constants.NEVER;
	public double getFiredLast(){return firedLast;}
	public void setFiredLast(double time){
		firedLast = time;
	}

}
