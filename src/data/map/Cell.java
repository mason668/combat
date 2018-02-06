package data.map;

public class Cell {
	
	private double elevation = 0.0;
	public double getElevation(){return elevation;}
	public void setElevation(double d){elevation = d;}
	
	private boolean buildingPresent = false;
	private boolean fencePresent = false;
	private boolean roadPresent = false;
	private boolean riverPresent = false;
	private boolean vegetationPresent = false;
	private boolean urbanPresent = false;
	private boolean stringPresent = false;
	private boolean areaPresent = false;
	private boolean obstaclePresent = false;
	private boolean minefieldPresent = false;
	private boolean lanePresent = false;

	/*
	C		Bits 0-15	Elevation (pentimeters)			C
	C		Bit  16 	Building present			C
	C		Bit  17		Fence present				C
	C		Bit  18 	Road present				C
	C		Bit  19 	River present				C
	C		Bit  20 	Vegetation present			C
	C		Bit  21		Urban area present			C
	C		Bit  22		Generic String present			C
	C		Bit  23		Generic Area present			C
	C		Bit  24		Obstacle present			C
	C		Bit  25		Minefield present			C
	C		Bit  26		Breach Lane present			C
	C		Bits 27-31	Not used				C

		INTEGER*4	FEATS_PER_CELL
		PARAMETER	(FEATS_PER_CELL = 20)
		INTEGER*4	CELL_FEATURES
		INTEGER*4	MAX_CELLS
		PARAMETER (MAX_CELLS = (MAPGRIDX/10)*(MAPGRIDX/10))
	*/

}
