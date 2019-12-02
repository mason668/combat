package utils;

import java.util.StringTokenizer;
import java.util.Vector;

import data.map.Coordinate;

public class Parser {
	public static Vector<String> convert (String s){
		Vector<String> v = new Vector<String>();
		StringTokenizer t = new StringTokenizer(s);
		while (t.hasMoreTokens()){
			String e = t.nextToken();
			v.addElement(e);
		}
		return v;
	}
	public static Vector<String> convert (String[] s){ //TODO deal with "--" prefix
		Vector<String> v = new Vector<String>();
		for (int i=0;i<s.length;i++){
			v.addElement(s[i]);
		}
		return v;
	}

	/**
	 * Convert the supplied value in secs to a formatted time 
	 * @param t
	 * @return
	 */
	private static final int secsPerMin = 60;
	private static final int secsPerHour = 3600;
	private static final int secsPerDay = 24 * 3600;
	public static String formatTime(double d, boolean decimal){
		String clock = "dd:hh:mm:ss.t";
		long days = (int)(d / secsPerDay);
		double remainder = d - days * secsPerDay;
		long hours = (int) (remainder / secsPerHour);
		remainder = remainder - hours * secsPerHour;
		long minutes = (int) (remainder / secsPerMin);
		remainder = remainder - minutes * secsPerMin;
		long seconds = (int) (remainder);
		remainder = remainder - seconds;
		remainder = remainder * 1000;
		long mils = (int) remainder;

		clock = pad(Long.toString(days),2,"0");
		clock = clock + ":" + pad(Long.toString(hours),2,"0");
		clock = clock + ":" + pad(Long.toString(minutes),2,"0");
		clock = clock + ":" + pad(Long.toString(seconds),2,"0");
		if (decimal) {
			clock = clock + "." + mils;
		}
		return clock;
	}
	
	public static String formatTime(double d){
		String clock = "dd:hh:mm:ss.t";
		long days = (int)(d / secsPerDay);
		double remainder = d - days * secsPerDay;
		long hours = (int) (remainder / secsPerHour);
		remainder = remainder - hours * secsPerHour;
		long minutes = (int) (remainder / secsPerMin);
		remainder = remainder - minutes * secsPerMin;
		long seconds = (int) (remainder);
		remainder = remainder - seconds;
		remainder = remainder * 1000;
		long mils = (int) remainder;

		clock = pad(Long.toString(days),2,"0");
		clock = clock + ":" + pad(Long.toString(hours),2,"0");
		clock = clock + ":" + pad(Long.toString(minutes),2,"0");
		clock = clock + ":" + pad(Long.toString(seconds),2,"0");
		return clock;
	}

	/**
	 * 
	 * @param string
	 * @return
	 */
	public static double parseTime(double currentTime, String string){
		double d = -1.0;
		try{
			d = Double.parseDouble(string);
		} catch (Exception e){
			d = Parser.parseFormattedTime(currentTime, string);
		}
		return d;
	}

	public static void main(String[] args){
		double d;
		String time;
		time = "3:30";
		Logger.say("input "+ time);
		d = parseFormattedTime (time);
		Logger.say("time  " + d);
	}

	/**
	 * 
	 * @param string
	 * @return
	 */
	public static double parseFormattedTime(double currentTime, String string){
		// initialise return value
		double d = -1.0;
		if (string.length()<=0) return d;

		//process offset prefix
		String prefix = string.substring(0,1);
		if (prefix.compareTo("+")==0){
			string = string.substring(1);
		} else if (prefix.compareTo("-")==0){
			string = string.substring(1);
		} else prefix = "";

		d = parseFormattedTime(string);
		
		// apply offset if required
		if (prefix.compareTo("+")==0){
			d = currentTime + d;
		} else if (prefix.compareTo("-")==0){
			d = currentTime - d;
		} 
		
		// return value
		return d;
	}
	
	/**
	 * 
	 * @param string
	 * @return
	 */
	public static double parseFormattedTime(String string){
		//TODO this needs to deal with an actual formatted time - not just assume string length
		// initialise return value
		double d = 0.0;
		
		Vector<String> vector = new Vector<String>();
		StringTokenizer tokenizer = new StringTokenizer(string,":");
		while (tokenizer.hasMoreTokens()){
			String word = tokenizer.nextToken();
			vector.addElement(word);
		}

		// initialise intermediate data
		int days = 0;
		int hours = 0;
		int mins = 0;
		double secs = 0;
		
		if (vector.size()== 4){
			try{
				days = Integer.parseInt(vector.remove(0));
			} catch (Exception e){} // TODO should we return an error?
		}
		if (vector.size()== 3){
			try{
				hours = Integer.parseInt(vector.remove(0));
			} catch (Exception e){} // TODO should we return an error?
		}
		if (vector.size()== 2){
			try{
				mins = Integer.parseInt(vector.remove(0));
			} catch (Exception e){} // TODO should we return an error?
		}
		if (vector.size()== 1){
			try{
				secs = Double.parseDouble(vector.remove(0));
			} catch (Exception e){} // TODO should we return an error?
		}


		// combine the elements into a single value
		d = secs + mins*60.0 + hours * 60*60 + days * 24*60*60;
		
		// return value
		return d;
	}

	public static Coordinate parseCoordinate (Coordinate reference, 
			String xstring, String ystring){
		//TODO should really use ints and UTM
		Coordinate result = null;
		if (xstring == null) return result;
		if (ystring == null) return result;
		//TODO process relative coord
		double x = -1;
		double y = -1;
		try{
			x = Double.parseDouble(xstring);
			y = Double.parseDouble(ystring);
			result = new Coordinate(x,y);
		} catch (Exception e){
		}
		return result;
	}
	
	// pad a string (prefix) with a specified character
	public static String pad(String s, long length, String c){
		if (s.length()>=length) return s;
		String r = s.substring(0);
		long add = length - s.length();
		for (int i=0;i<add;i++){
			r = c.substring(0) + r;
		}
		return r;
	}
	
	public static double rad2deg(double radians){
		double degrees = radians * 180 / Math.PI;
		return degrees;
	}
	
	public static double degreesFromNorth(double cartesianDegrees){
		return 90-cartesianDegrees;
	}


}
