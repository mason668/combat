package utils;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.Vector;

public class Tracer {


	private static String traceFileName = "trace.txt";
	private static BufferedWriter traceFile;
	private static boolean ok = false;
	
	public static final int DIRECTFIRE = 0;
	public static final int MOVEMENT = 1;
	public static final int DETECTION = 2;
	
	//TODO not implemented in actual models
	/*
	 * Constants to define the level of detail in the trace reports.
	 */
	public static final int COURSEST = 0;
	public static final int COURSER = 1;
	public static final int COURSE = 2;
	public static final int FINE = 3;
	public static final int FINER = 4;
	public static final int FINEST = 5;
	
	private static int level = FINEST;
	
	public static void setTraceLevel(int i){
		if ( i<0) return;
		if (i> FINEST) return;
		level = i;
	}
	
	private static boolean echo = false;
	private static boolean traceDirectFire = false;
	private static boolean traceMovement = false;
	
	/**
	 * Code to test the class
	 * @param args
	 */
	public static void main(String args[]){
		Tracer.say("writing test message to default file");
		Tracer.write("test message");
		Tracer.say("testing an alternate file");
		Tracer.setTraceFile("test_file.txt");
		Tracer.write("test message");
		Tracer.close();
	}

	public Tracer(){
		setTraceFile(traceFileName);
	}

	/**
	 * Change the name of the tracefile
	 * @param name
	 */
	public static void setTraceFile (String name){
		if ( traceFile != null){
			close();
			traceFile = null;
		}
    	try {
    		traceFile = new BufferedWriter(new FileWriter(name));
    		traceFileName = name;
    		ok = true;
    	} catch (Exception e) {
    		System.err.println("error opening trace file " + name);
    	}
	}
	
	/**
	 * Close the file.
	 */
	public static void close(){
		if ( traceFile == null){ return;}
    	try {
    		traceFile.close();    		
    	} catch (Exception e) {}
	}

	/**
	 * Write a message to standard output
	 * @param message
	 */
	private static void say(String message){
		System.out.println(message);
	}
	
	/**
	 * Write a message to the trace file.
	 * <p>This should be replaced.
	 * @param message
	 */
	public static void write(int mode, int level, String message){
		if ( mode == DIRECTFIRE){
			if (!traceDirectFire) return;
		}
		if ( mode == MOVEMENT){
			if (!traceMovement) return;
		}
		if (level > Tracer.level) return;
		if (level > 0){
			String prefix="";
			for (int i=0;i<level;i++){
				prefix = "    " + prefix;
			}
			message = prefix + message;
		}
		write (message);
	}
	
	public static void write(String message){
		if (echo) say(message);
		if ( !ok ) {
			setTraceFile(traceFileName);
		}
		if ( traceFile != null){
			try {
			  traceFile.write(message+"\r\n");
			  traceFile.flush();
			  for (TraceListener listener: listeners){
				  listener.write(message);
			  }
			} catch (Exception e) {}
		}
	}

	public static void write(int level, String message){
		if (level > Tracer.level) return;
		if (level > 0){
			String prefix="";
			for (int i=0;i<level;i++){
				prefix = "    " + prefix;
			}
			message = prefix + message;
		}
		write (message);
	}
	
	public static void setEcho (boolean b){
		echo = b;
	}
	
	public static void setDirectFire(boolean b){
		traceDirectFire = b;
	}
	public static boolean getMovement(){
		return traceMovement;
	}
	public static void setMovement(boolean b){
		traceMovement = b;
	}
	
	private static Vector<TraceListener> listeners = new Vector<TraceListener>();
	public static void addListener(TraceListener listener){
		listeners.add(listener);
	}
}
