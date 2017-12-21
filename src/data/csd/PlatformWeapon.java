package data.csd;

import data.managers.Identifiable;

public class PlatformWeapon implements Identifiable{
	
	public PlatformWeapon(Weapon w){
		weapon = w;
	}
	@Override
	public String getName() {
		// TODO Auto-generated method stub
		return null;
	}
	@Override
	public void setName(String name) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public int getNumber() {
		// TODO Auto-generated method stub
		return 0;
	}
	@Override
	public void setNumber(int number) {
		// TODO Auto-generated method stub
		
	}
	

	
	// what real weapon occupies this slot
	private Weapon weapon = null; // globsysc.kweapnc
	public Weapon getWeapon(){return weapon;}
	
	// time (mins) needed to setup the weapon after moving
	private double setupTime = 0.0; // globsysc.cwpn_time_setup
	public double getSetupTime(){return setupTime;}
	public void setSetupTime(double d){setupTime = d;}

	// time (mins) needed to packup the weapon before moving
	private double packupTime = 0.0; // globsysc.cwpn_time_pack
	public double getPackupTime(){return packupTime;}
	public void setPackupTime(double d){packupTime = d;}

	// the basic ammunition load for weapon
	private int readyAmmunition = 100; // globsysc.kbloadc
	private int storedAmmunition = 100; // globsysc.ksloadc
	private double ammoTransferTime = 1.0; // globsysc.wpnxfertimec
	// pointer to the actual weapon type allocated to this slot
	// time (mins) to upload one round from a resupply system
	private double uploadTime = 0.16; // globsysc.kupldtimc
	private int squadWeapon = 0; // globsysc.squadwpnc
	public void setSquadWeapon(int i){
		if (i<0) return;
		if (i> 100) return;
		squadWeapon = i;
	}
	public int getSquadWeapon(){return squadWeapon;}

	// flag to indicate this slot only holds ammunition and can not be used to engage targets
	private boolean ammoOnly = false; // globsysc.kammo_only
	public boolean getAmmoOnly(){return ammoOnly;}
	public void setAmmoOnly(boolean b){ammoOnly = b;}
	//TODO
	/*
	 * globsysc
	BYTE		KALTWPNC   ( NUMRELWPNS, NUMCSDTYPES )

     *  	KALTWPNC,  		! For each weapon/ord a system may
					! have, an alternate weapon to use if
					! the first weapon defined by "KWEAPON"
					! is expended. Weapon values are system
					! relative in that they range from 1
					! to NUMRELWPNS.
	 */

}
