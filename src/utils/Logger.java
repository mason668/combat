package utils;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.Vector;

import view.TraceListener;

public class Logger {


	private static String logFileName = "log.txt";
	private static BufferedWriter logFile;
	private static boolean ok = false;
	
	public static final int COURSEST = 1;
	public static final int COURSER = 2;
	public static final int COURSE = 3;
	public static final int FINE = 4;
	public static final int FINER = 5;
	public static final int FINEST = 6;
	public static final int INFO = 1;
	public static final int WARNING = 2;
	public static final int ERROR = 3;
	public static final int SEVERE = 4;
	private static int logLevel = FINEST;
	private static boolean echo = true;
	
	/**
	 * Code to test the class
	 * @param args
	 */
	public static void main(String args[]){
		Logger.say("writing test message to default file");
		Logger.log("test message");
		Logger.say("testing an alternate file");
		Logger.setLogFile("test_file.txt");
		Logger.log("test message");
		Logger.close();
	}

	public Logger(){
		setLogFile(logFileName);
	}

	/**
	 * Change the name of the logfile
	 * @param name
	 */
	public static void setLogFile (String name){
		if ( logFile != null){
			close();
			logFile = null;
		}
    	try {
    		logFile = new BufferedWriter(new FileWriter(name));
    		logFileName = name;
    		ok = true;
    	} catch (Exception e) {
    		System.err.println("error opening log file " + name);
    	}
	}
	
	/**
	 * Close the log file.
	 */
	public static void close(){
		if ( logFile == null){ return;}
    	try {
    		logFile.close();    		
    	} catch (Exception e) {}
	}

	/**
	 * Write a message to standard output
	 * @param message
	 */
	public static void say(String message){
		System.out.println(message);
	}
	
	/**
	 * Write a message to error output
	 * @param message
	 */
	public static void err(int level, String message){
		String error;
		//TODO do we have all levels covered
		if ( level == Logger.ERROR){
			error = "Error: ";
		} else if (level == Logger.WARNING){
			error = "Warning: ";
		} else {
			error = "";
		}
		log(error + message);
	}
	public static void err(Object object, int level, String message){
		err(level, object.getClass().getName() + ":" + message);
	}
	
	/**
	 * Write a message to the log file.
	 * @param message
	 */
	public static void log(String message){
		if (echo) say(message);
		if ( !ok ) {
			setLogFile(logFileName);
		}
		if ( logFile != null){
			try {
			  logFile.write(message+"\r\n");
			  logFile.flush();
			} catch (Exception e) {}
		}
	}
	
	public static void log(int level, String message){
		if (level > logLevel) return;
		log(message);
	}
	
	public static void log(Object object, String message){
		if (object != null){
			log(object.getClass().getName() + ": " + message);
		}
	}
	
	public static void setLevel(int level){
		if (level < COURSEST) return;
		if (level > FINEST) return;
		logLevel = level;
	}
	
	public static void setEcho (boolean b){
		echo = b;
	}
	
}
