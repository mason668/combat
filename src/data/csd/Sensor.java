package data.csd;

import data.managers.Identifiable;

public class Sensor {
	public final static int BAND_OPTICAL1 = 0;
	public final static int BAND_OPTICAL2 = 1;
	public final static int BAND_THERMAL1 = 2;
	public final static int BAND_THERMAL2 = 3;
	public final static int BAND_MMR = 4;
	public final static int MAX_SENSOR_BANDS = 4;
	public final static String[] SENSOR_BANDS = {
			"optical 1", "optical 2", "thermal 1", "thermal 2", "mmr",
	};
	
	public static void main(String[] args){
		System.out.println("sensor");
	}

	public Sensor(String name){
		myName = name.substring(0);
	}
	
	private int myBand = 0;
	public int getBand(){return myBand;}
	public void setBand(int band){
		if (band < 0) return;
		if (band > MAX_SENSOR_BANDS) return;
		myBand = band;
	}
	
	private String myName = "sensor";
	public String getName(){return myName.substring(0);}
	public void setName(String name){myName = name.substring(0);}
	
	private boolean moverOnly = false;
	public void setMoverOnly(boolean b){moverOnly = b;}
	public boolean onlyDetectMovers(){ return moverOnly;}
	
	private double minSpeed = 1.0;
	public double getMinSpeed(){
		return minSpeed;
	}
	public void setMinSpeed(double speed){
		if (speed >= 0.0) minSpeed = speed;
	}
	
	private double maxVisibility = 5.0;
	public double getMaxVisibility(){
		return maxVisibility;
	}
	public void setMaxVisibility(double range){
		if (range >= 0.0) maxVisibility = range;
	}
	
	private double fieldOfView = Math.PI / 6.0; // 30 degrees
	public double getFov(){return fieldOfView;}
	
}
