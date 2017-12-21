package data;

import data.csd.PHTable;
import data.csd.PKTable;
import data.csd.Platform;
import data.csd.Weapon;
import data.managers.PHTableList;
import data.managers.PKTableList;
import data.managers.PlatformList;
import data.managers.SensorList;
import data.managers.WeaponList;
import interpreterOLD.Interpreter;
import utils.Logger;
import utils.Parser;

public class CSData {
	
	/**
	 * Chemical Susceptibility data
	 * globchmc
	 */
	private double selfRecognitionDose = 0.5; // cdosagl1
	public double getSelfRecognitionDose(){return selfRecognitionDose;}
	public void setSelfRecognitionDose(double d){selfRecognitionDose = d;}
	
	private double incapacityMean = 3.33; // cd2mean
	public double getIncapacityMean(){return incapacityMean;}
	public void setIncapacityMean(double d){incapacityMean = d;}
	
	private double incapacitySigma = 0.3; // cd2sigm
	public double getIncapacitySigma(){return incapacitySigma;}
	public void setIncapacitySigma(double d){incapacitySigma = d;}
	
	private double deathMean = 4.25; // cd3mean
	public double getDeathMean(){return deathMean;}
	public void setDeathMean(double d){deathMean = d;}
	
	private double deathSigma = 0.31; // cd3sigm
	public double getDeathSigma(){return deathSigma;}
	public void setDeathSigma(double d){deathSigma = d;}
	
	private double alarmConcentration = 0.74; // cdosagl4
	public double getAlarmConcentration(){return alarmConcentration;}
	public void setAlaramConcentration(double d){alarmConcentration = d;}
	
	private double selfRecognitionTime = 3.0; // crespnl1 (secs)
	public double getSelfRecognitionTime(){return selfRecognitionTime;}
	public void setSelfRecognitionTime(double d){selfRecognitionTime = d;}
	
	private double maskingTime = 7.5; // ctimemask (secs)
	public double getMaskingTime(){return maskingTime;}
	public void setMaskingTime(double d){maskingTime = d;}
	
	private double crewAlarmTime = 2.0; // ctimealarm (secs)
	public double getCrewAlarmTime(){return crewAlarmTime;}
	public void setCrewAlarmTime(double d){crewAlarmTime = d;}
	
	private double incapacitationResponseTime = 15.0; // crespnl2 (secs)
	public double getIncapacitationResponseTime(){return incapacitationResponseTime;}
	public void setIncapacitationResponseTime(double d){incapacitationResponseTime = d;}
	
	private double expirationResponseTime = 30.0; // crespnl3 (secs)
	public double getExpirationResponseTime(){return expirationResponseTime;}
	public void setExpirationResponseTime(double d){expirationResponseTime = d;}

	private double detectorAlarmTime = 3.0; // crespnl4 (secs)
	public double getDetectorAlarmTime(){return detectorAlarmTime;}
	public void setDetectorAlarmTime(double d){detectorAlarmTime = d;}
	
	/**
	 * Object managers
	 */
	
	private PHTableList phTableList = new PHTableList();
	public PHTableList getPHTableList(){return phTableList;}
	
	private PKTableList pkTableList = new PKTableList();
	public PKTableList getPKTableList() {return pkTableList;}
	
	private PlatformList platformList = new PlatformList();
	public PlatformList getPlatformList(){ return platformList;}
	
	private SensorList sensorList = new SensorList();
	public SensorList getSensorList(){return sensorList;}
	
	private WeaponList weaponList = new WeaponList();
	public WeaponList getWeaponList(){return weaponList;}
	
	public static void main(String args[]){
		CSData me = new CSData();
		me.test();
	}

	public void test(){
		Logger.setLevel(Logger.FINE);
		Interpreter interpreter = new Interpreter();
		interpreter.setData(this);
		interpreter.interpret(Parser.convert("load data/csdata.txt"));
		log();
		
	}
	
	public void log(){
		Logger.log(Logger.INFO, "testing CSData");
		
		Logger.log(Logger.INFO, "platform table contains:");
//		Logger.log(Logger.INFO,platformList.list());

		Logger.log(Logger.INFO, "weapon table contains:");
//		Logger.log(Logger.INFO,weaponList.list());

		Logger.log(Logger.INFO, "ph table contains:");
//		Logger.log(Logger.INFO,phTableList.list());

		Logger.log(Logger.INFO, "pk table contains:");
//		Logger.log(Logger.INFO,pkTableList.list());
		
	}
	
	public void maketestData(){
		getPlatformList().add(new Platform ("platform_01"));
		getPlatformList().add(new Platform ("platform_02"));
		getPlatformList().add(new Platform ("platform_03"));
		getPlatformList().add(new Platform ("platform_04"));
		getPlatformList().add(new Platform ("platform_05"));
		getPlatformList().add(new Platform ("platform_06"));
		getPlatformList().add(new Platform ("platform_07"));
		getPlatformList().add(new Platform ("platform_08"));
		getPlatformList().add(new Platform ("platform_09"));
		getPlatformList().add(new Platform ("platform_10"));
		getPlatformList().add(new Platform ("platform_11"));
		getPlatformList().add(new Platform ("platform_12"));
		getPlatformList().add(new Platform ("platform_13"));
		getPlatformList().add(new Platform ("platform_14"));
		getPlatformList().add(new Platform ("platform_15"));
		getPlatformList().add(new Platform ("platform_16"));
		getPlatformList().add(new Platform ("platform_17"));
		getPlatformList().add(new Platform ("platform_18"));
		getPlatformList().add(new Platform ("platform_19"));
		getPlatformList().add(new Platform ("platform_20"));

		getPHTableList().add(new PHTable ("ph0001"));
		getPHTableList().add(new PHTable ("ph0002"));
		getPHTableList().add(new PHTable ("ph0003"));
		
		getPKTableList().add(new PKTable ("pk0001"));

		getWeaponList().add(new Weapon ("weapon_01"));
		getWeaponList().add(new Weapon ("weapon_02"));
		getWeaponList().add(new Weapon ("weapon_03"));
		getWeaponList().add(new Weapon ("weapon_04"));
		
		Weapon w = getWeaponList().getWeapon("weapon_01");
		PHTable ph = getPHTableList().get("ph0001");
		w.setPlatformPH("platform_01", ph);
	}

}
