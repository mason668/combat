package data.csd;

/**
 * Define class to represent flyer data.
 * See glbflyerc.f
 *
 */
public class Flyer {
	
	private String name = "test_flyer";
	public String getName(){return name.substring(0);}
	
	/*
	 * Define altitude data in km
	 */
	private double altitudeMin = 0.010; // cflyaltud
	private double altitudeMax = 10.000;
	private double altitudeDefault = 0.500;
	public double getMinimumAltitude(){return altitudeMin;}
	public double getMaximumAltitude(){return altitudeMax;}

	/*
	 * Define speed data in kph
	 */
	private double speedMin = 0.0; // cflyveloc
	private double speedMax = 500.0;
	private double speedDefault = 100.0;
	
	/*
	 * Define height of sensor mast in m
	 */
	private double mastHeight = 0.0; // cheimast
	
	/*
	 * pop time n sec //TODO not used
	 */
	private double popTime = 1.0; // cpoptim

	/*
	 * Not sure if flyertype is used
	 */
	private FlyerType flyerType = FlyerType.NORMAL; // kflyclassc
	
	public enum FlyerType {NORMAL, FOGM, SPECIAL, SPECIAL_SPECIAL}; // kflyclassc
	
	public String toText(){
		String newline = "\n";
		String s = "";
		s = s + "flyer " + this.name + newline;
		s = s + "min_altitude " + this.altitudeMin + newline;
		s = s + "max_altitude " + this.altitudeMax + newline;
		s = s + "altitude " + this.altitudeDefault + newline;
		s = s + "min_speed " + this.speedMin + newline;
		s = s + "max_speed " + this.speedMax + newline;
		s = s + "speed " + this.speedDefault + newline;
		s = s + "mast " + this.mastHeight + newline;
		return s;
	}

	public static void main(String[] args){
		Flyer me = new Flyer();
		System.out.println(me.toText());
	}
	

}
