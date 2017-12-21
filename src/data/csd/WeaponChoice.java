package data.csd;

public class WeaponChoice {
	
	private Platform platform;
	private double range;
	private PlatformWeapon closeWeapon;
	private PlatformWeapon longWeapon;
	
	public WeaponChoice (Platform p, double d, PlatformWeapon w1, PlatformWeapon w2){
		platform = p;
		range = d;
		closeWeapon = w1;
		longWeapon = w2;
	}
	
	public Platform getPlatform(){return platform;}
	public double getRange(){return range;}
	public PlatformWeapon getCloseWeapon(){return closeWeapon;}
	public PlatformWeapon getLongWeapon(){return longWeapon;}
	

}
