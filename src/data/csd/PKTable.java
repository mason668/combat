package data.csd;

import data.managers.Identifiable;
import utils.Parser;

public class PKTable extends ProbabilityTable {
	
	/**
	 * Make this class executable for testing.
	 * @param args
	 */
	public static void main(String[] args){
		PKTable me = new PKTable("test");
//		me.test();
//		me.interpret(Parser.convert("load data/pk0001.txt"));
//		me.interpret(Parser.convert(args));
		me.test();
	}

	private static int numRows = 12;
	private static int numCols = 5;

	public PKTable (String s){
		super(s, numRows, numCols);
		for (int i=0; i< numRows;i++){
			setPosture(i,postureNames[i]);
		}
	}

	public static final int KDF = 0;
	public static final int KDH = 1;
	public static final int KEF = 2;
	public static final int KEH = 3;
	public static final int MDF = 4;
	public static final int MDH = 5;
	public static final int MEF = 6;
	public static final int MEH = 7;
	public static final int FDF = 8;
	public static final int FDH = 9;
	public static final int FEF = 10;
	public static final int FEH = 11;

	private static final String postureNames[] = {
			"kdf",
			"kdh",
			"kef",
			"keh",
			"mdf",
			"mdh",
			"mef",
			"meh",
			"fdf",
			"fdh",
			"fef",
			"feh",
		};
	
	private static final int phPosture[] = {
			0, 1, 2, 3,
			0, 1, 2, 3,
			0, 1, 2, 3,
			0, 1, 2, 3,
	};

	public static String getPostureName(int i){
		if (i<0) return null;
		if (i>= numRows) return null;
		return postureNames[i];
	}
	
	public static String[] getPostureNames(){
		return postureNames; //TODO probably should copy
	}
	
	public static int getpostureFromPh(int i){
		if (i<0) return -1;
		if (i>= numRows) return -1;
		return phPosture[i];
	}

}
