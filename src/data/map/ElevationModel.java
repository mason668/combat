package data.map;

import utils.Tracer;

/**
 * This class defines an elevation model.
 * The model is represented by a 2-dimensional grid of cells containing the elevation of
 * the terrain at that point. Cells can be referenced using their x (width) and y (height) 
 * value in the grid.
 */
public class ElevationModel {
	
	public static void main(String[] args){
		ElevationModel me = new ElevationModel(args);
		me.test();
	}
	
	/**
	 * store the elevation in metres as double.
	 */
	private double[][] array;
	
	/**
	 * define the width and height (x and y) of the map in cells
	 */
	private int mapWidth, mapHeight;
	
	/**
	 * Construct a new ElevationModel of the specified width and height in cells.
	 * Each cell has an associated elevation.
	 * @param width The width of the ElevationModel in cells.
	 * @param height The height of the ElevationModel in cells
	 */
	public ElevationModel(int width, int height){
		width = Math.max(width, 1);
		height = Math.max(height,  1);
		mapWidth = width;
		mapHeight = height;
		makeArray();
	}
	
	/**
	 * Construct a new square ElevationModel with the width and height 
	 * set to the specified size in cells.
	 * @param size The width and height of the ElevationModel in cells.
	 */
	public ElevationModel(int size){
		size = Math.max(size, 1);
		mapWidth = size;
		mapHeight = size;
		makeArray();
	}
	
	/**
	 * Construct a user defined ElevationModel
	 * @param args List of arguments defining the shape of the model.
	 */
	private ElevationModel(String[] args){
		// set default values
		mapWidth = 5;
		mapHeight = 5;
		double elevation = 0.0;

		// process the arguments
		for (int i = 0; i<args.length;i++){
			if (args[i].compareToIgnoreCase("--flat")==0){
				if (i < (args.length-1)){
					i++;
					try{
						elevation = Double.parseDouble(args[i]); 
					} catch (Exception e){}
				}
			} else if (args[i].compareToIgnoreCase("--height")==0){
				if (i < (args.length-1)){
					i++;
					try{
						mapHeight = Integer.parseInt(args[i]); 
					} catch (Exception e){}
				}
			} else if (args[i].compareToIgnoreCase("--width")==0){
				if (i < (args.length-1)){
					i++;
					try{
						mapWidth = Integer.parseInt(args[i]); 
					} catch (Exception e){}
				}
			}
		}
		// create a model
		makeArray();
		flattenM(elevation);
	}

	/**
	 * Construct the actual array based on width and height settings.
	 */
	private void makeArray(){
		array = new double[mapWidth][mapHeight];
		flattenM(0.0);
	}
	
	/**
	 * Get the width of the ElevationModel in cells.
	 * @return The width.
	 */
	public int getWidth(){
		return mapWidth;
	}
	
	/**
	 * Get the height of the ElevationModel in cells.
	 * @return The height.
	 */
	public int getHeight(){
		return mapHeight;
	}

	/**
	 * Set all of the cells in the ElevationModel to the specified elevation in kilometres.
	 * @param elevation The value to set the elevation.
	 */
	public void flattenKM(double elevation){
		for (int x = 0;x<mapWidth;x++){
			for (int y=0; y<mapHeight;y++){
				array[x][y] = elevation;
			}
		}
	}

	/**
	 * Set all of the cells in the ElevationModel to the specified elevation in metres.
	 * @param elevation The value to set the elevation.
	 */
	public void flattenM(double elevation){
		for (int x = 0;x<mapWidth;x++){
			for (int y=0; y<mapHeight;y++){
				array[x][y] = elevation;
			}
		}
	}
	
	/**
	 * Get the elevation (in metres) at the specified cell.
	 * @param x The cell X coordinate.
	 * @param y The cell Y coordinate.
	 * @return The elevation in metres (double).
	 */
	public double getElevationM(int x, int y){
		if (x <0) return 0.0;
		if (x > mapWidth-1) return 0.0;
		if (y <0) return 0.0;
		if (y > mapHeight-1) return 0.0;
		return array[x][y];
	}
	
	/**
	 * Get the elevation (in kilometres) at the specified cell.
	 * @param x The cell X coordinate.
	 * @param y The cell Y coordinate.
	 * @return The elevation in kilometres (double).
	 */
	public double getElevationKM(int x, int y){
		return getElevationM(x,y) * 0.001;
	}

	/**
	 * Get the elevation (in pentimetres) at the specified cell.
	 * @param x The cell X coordinate.
	 * @param y The cell Y coordinate.
	 * @return The elevation in pentimetres (double).
	 */
	public double getElevationPM(int x, int y){
		return getElevationM(x,y) * 5.0;
	}
	
	/**
	 * Set the elevation (in metres) at the specified cell. 
	 * @param x The cell x coordinate.
	 * @param y The cell y coordinate.
	 * @param elevation The elevation in metres.
	 */
	public void setElevationM(int x, int y, double elevation){
		if (x <0) return;
		if (x > mapWidth-1);
		if (y <0) return;
		if (y > mapHeight-1);
		array[x][y] = elevation;
	}

	/**
	 * Set the elevation (in kilometres) at the specified cell. 
	 * @param x The cell x coordinate.
	 * @param y The cell y coordinate.
	 * @param elevation The elevation in kilometres.
	 */
	public void setElevationKM(int x, int y, double elevation){
		setElevationM(x,y,elevation * 1000.0);
	}

	/**
	 * Set the elevation (in pentimetres) at the specified cell. 
	 * @param x The cell x coordinate.
	 * @param y The cell y coordinate.
	 * @param elevation The elevation in pentimetres.
	 */
	public void setElevationPM(int x, int y, double elevation){
		setElevationM(x,y,elevation / 5.0);
	}
	
	/**
	 * An internal test routine. Displays the size of the map and
	 * lists its elevation data.
	 */
	private void test(){
		Tracer.setEcho(true);
		Tracer.write(this.getClass().getCanonicalName());
		Tracer.write("size "+ mapWidth + " by " + mapHeight);
		Tracer.write("elevation in  m at (0,0) is " + this.getElevationM(0, 0));
		Tracer.write("elevation in km at (0,0) is " + this.getElevationKM(0, 0));
		Tracer.write("elevation in pm at (0,0) is " + this.getElevationPM(0, 0));
		for (int x = 0;x<mapWidth;x++){
			for (int y=0; y<mapHeight;y++){
				Tracer.write("cell (" + x + "," + y + ") " 
					+ String.format("%.5f", array[x][y]));
			}
		}
	}

}
