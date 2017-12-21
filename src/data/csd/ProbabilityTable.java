package data.csd;

public class ProbabilityTable {
	
	/**
	 * Make this class executable for testing.
	 * @param args
	 */
	public static void main(String[] args){
		ProbabilityTable me = new ProbabilityTable();
		me.makeTable();
		me.test();
	}

	/**
	 * A public test method
	 */
	public void test(){
		System.out.println("testing " + this.getClass().getCanonicalName());
		System.out.println(this.toLog());
		double maxRange = this.ranges[this.cols-1];
		for (int i=0;i<5;i++){
			double range = maxRange * Math.random();
			int row = (int)(Math.random() * this.rows) +1 ;
			System.out.println("row " + row + " range " 
					+ range + " = " + this.getProb(row-1, range));
			System.out.println("row " + row + " range " 
					+ range + " = " + this.getProb("row " + row, range));
		}
	}

	/** 
	 * Define attributes
	 */
	private int cols = 5; // number of columns in table
	private int rows = 5; // number of rows in table
	private double[][] data; // array to hold actual probability data
	private double[] ranges; // ranges that correspond to each column
	private String[] postures; // posture codes that correspond to each row
	private boolean valid = false; // flag to denote the table is valid
	private String name = "test_table";

	/**
	 * Constructors
	 */
	
	public ProbabilityTable(){		
		this.makeTable();
	}
	public ProbabilityTable (int r, int c){
		this.rows = r;
		this.cols = c;
		this.makeTable();
	}
	public ProbabilityTable (String s, int r, int c){
		this.name = s.substring(0);
		this.rows = r;
		this.cols = c;
		this.makeTable();
	}
	
	/**
	 * Public methods
	 */
	
	/**
	 * Set (change) the number of rows and rebuild table
	 * @param r number of rows
	 */
	public void setRows(int r){
		if (r <= 0) return;
		rows = r;
		this.makeTable();
	}
	/**
	 * Set (change) the number of cols and rebuild table
	 * @param c number of columns
	 */
	public void setCols(int c){
		if (c <=0) return;
		cols = c;
		this.makeTable();
	}
	/**
	 * Set (change) the number of rows and columns and rebuild table
	 * @param r number of rows
	 * @param c number of columns
	 */
	public void setRowsAndColumns(int r, int c){
		if (r <= 0) return;
		if (c <=0) return;
		rows = r;
		cols = c;
		this.makeTable();
	}
	
	/**
	 * Get the number of columns in the table
	 * @return number of columns
	 */
	public int getCols(){return cols;}
	
	/**
	 * Get the number of rows in the table
	 * @return number of rows
	 */
	public int getRows(){ return rows;}

	/**
	 * Get the probability at a certain row and column
	 * @param row row number starting with zero
	 * @param col column number starting with zero
	 * @return probability data
	 */
	public double getData(int row, int col){
		if (row <0) return 0.0;
		if (col <0) return 0.0;
		if (row >= rows) return 0.0;
		if (col >= cols) return 0.0;
		return data[row][col];
	}
	
	/**
	 * Set the probability data at a certain row and column
	 * @param row row number starting with zero
	 * @param col column number starting with zero
	 * @param prob
	 */
	public void setData(int row, int col, double prob){
		if (row <0) return;
		if (col <0) return;
		if (row >= rows) return;
		if (col >= cols) return;
		if (prob < 0.0) return;
		data[row][col] = prob;
	}
	public void setData(String posture, int col, double prob){
		int row = this.getRow(posture);
		this.setData(row, col, prob);
	}
	
	public double getRange(int col){
		if ( col < 0) return -1;
		if ( col >= cols) return -1;
		return ranges[col];
	}
	
	public String getName(){
		return name.substring(0);
	}
	
	public String toLog(){
		String s = "table: " + this.name + "\n"; 
		s = s + "         ";
		for (int col = 0; col < cols; col++){
			s = s + ranges[col] + "    ";
		}
		s = s + "\n";
		for (int row = 0; row<rows;row++){
			s = s + postures[row] + "    ";
			for (int col = 0; col < cols; col++){
				s = s + data[row][col] + "    ";
			}
			s = s + "\n";
		}
		return s;
	}
	
	public void setPosture (int row, String s){
		if ( row <0) return;
		if (row >= rows) return;
		this.postures[row] = s.substring(0);
	}
	
	public String getPosture(int row){
		if ( row <0) return null;
		if (row >= rows) return null;
		return this.postures[row].substring(0);
	}
	
	public double getProb(String posture, double rangeActual){
		int row = getRow(posture);
		if (row >= 0) return getProb(row, rangeActual);
		return 0.0;
	}
	
	private int getRow(String posture){
		int row = -1;
		for (int i=0; i< rows;i++){
			if (posture.compareTo(this.postures[i]) == 0) row = i;
		}
		return row;
	}
	
	/**
	 * Given a row number and a range, linearly interpolate a probability value
	 * corresponding to the range.
	 * 
	 * @param row
	 * @param rangeActual
	 * @return
	 */
	public double getProb(int row, double rangeActual){
		// if not a valid table just return zero 
		if (! this.valid) return 0.0; 
		// if row is invalid return zero
		if ( row < 0) return 0.0; 
		if ( row >= rows) return 0.0;
		// if range outside bounds, return zero
		if (rangeActual > ranges[cols-1]) return 0.0; 
		if (rangeActual < ranges[0]) return 0.0;
		// calculate which column is to the right of the actual range
		int col = 0;
		while (rangeActual > ranges[col]){
			col++;
		}
		// if we are lucky and the range is exactly on a data point, don't bother
		// to do any calculations, just return the matching probability value
		if (rangeActual == ranges[col]) {
			return data[row][col];
		}
		/* now calculate the probability
		 * probActual = 
		 * 		probMin + (
		 * 			(probMax – probMin) x (
		 * 				(rangeActual – rangeMin) / (rangeMax – rangeMin)
		 * 			)
		 * 		)
		 */
		double rangeMin = ranges[col-1];
		double rangeMax = ranges[col];
		double diffRange = rangeMax - rangeMin;
		double diffActual = rangeActual - rangeMin;
		double probMin = data[row][col-1];
		double probMax = data[row][col];
		double diffProb = probMax - probMin;
		double ratio = diffActual / diffRange;
		double probActual = probMin + (ratio * diffProb);
		return probActual;
	}
	
	/**
	 * Private methods
	 */

	protected void makeTable(){
		if (rows <=0) return;
		if (cols <=0) return;
		data = new double[rows][cols];
		ranges = new double[cols];
		postures = new String[rows];
		for (int row=0; row<rows; row++ ){
			postures[row] = "row " + (row+1);
			double prob = Math.random();
			for (int col = 0; col < cols; col++){
				data[row][col] = prob;
				prob = prob * 0.75;
			}
		}
		double range = 1.0;
		ranges [0] = 0.0;
		for (int col = 1; col < cols; col++){
			ranges[col] = range;
			range = range * 2.0;
		}
		this.valid = true; // TODO actually test validity
	}

}
