package data.csd;

import data.managers.Identifiable;

public class PHTable extends ProbabilityTable {
	
	/**
	 * Make this class executable for testing.
	 * @param args
	 */
	public static void main(String[] args){
		PHTable me = new PHTable("test");
//		me.test();
//		me.interpret(Parser.convert("load data/ph0001.txt"));
//		me.interpret(Parser.convert(args));
		me.test();
	}

	private static int numRows = 16;
	private static int numCols = 5;
	
	public static final int SSDF = 0;
	public static final int SSDH = 1;
	public static final int SSEF = 2;
	public static final int SSEH = 3;
	public static final int SMDF = 4;
	public static final int SMDH = 5;
	public static final int SMEF = 6;
	public static final int SMEH = 7;
	public static final int MSDF = 8;
	public static final int MSDH = 9;
	public static final int MSEF = 10;
	public static final int MSEH = 11;
	public static final int MMDF = 12;
	public static final int MMDH = 13;
	public static final int MMEF = 14;
	public static final int MMEH = 15;

	private static final String postureNames[] = {
			"ssdf",
			"ssdh",
			"ssef",
			"sseh",
			"smdf",
			"smdh",
			"smef",
			"smeh",
			"msdf",
			"msdh",
			"msef",
			"mseh",
			"mmdf",
			"mmdh",
			"mmef",
			"mmeh",
		};

	public PHTable (String s){
		super(s, numRows, numCols);
		for (int i=0; i< numRows;i++){
			setPosture(i,postureNames[i]);
		}
	}
	
	public static String getPostureName(int i){
		if (i<0) return null;
		if (i>= numRows) return null;
		return postureNames[i];
	}
	
	public static String[] getPostureNames(){
		return postureNames; //TODO probably should copy
	}

}
