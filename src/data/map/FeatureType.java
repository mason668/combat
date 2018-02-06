package data.map;

import java.awt.Color;

import sim.Constants;

/**
 * A class to define feature types such as roads, rivers and vegetation.
 */
public class FeatureType {

	// Each FeatureType needs a name
	protected String myName = "generic_feature";
	// The colour to draw the feature
	private Color myColor = Color.GRAY;
	protected double[] speedFactor = new double[Constants.MOVER_TYPES]; // ktreedgrad, kcitydgrad, kroadgrad 
	
	
	/**
	 * Get the name of the FeatureType
	 * @return The FeatureType name as a string.
	 */
	public String getName(){ // trrnlib.trrn_get_feature_name
		return myName.substring(0);
	}
	
	/**
	 * Set the name of the FeatureType.
	 * @param name The name to give the FeatureType.
	 */
	public void setName(String name){myName = name.substring(0);}

	
	/**
	 * Get the colour 
	 * @return
	 */
	public Color getColor(){
		return myColor;
	}
	public void setColor (Color c){
		myColor = c;
	}
	public void setColor (int red, int green, int blue){
		if (red <0) return;
		if (red > 255) return;
		if (green <0) return;
		if (green > 255) return;
		if (blue <0) return;
		if (blue > 255) return;
		myColor = new Color(red, green, blue);
	}
	private double plos = 1.0; // globtrrn.treeplos, cityplos
	// roads and rivers have implied plos of 1
	public double getPLOS(){  // trrnlib.trrn_get_feature_los, trrn_get_build_plos
		return plos;
		/* TODO are buildings different?
		 * 	SUBROUTINE TRRN_GET_BUILD_PLOS ( IBUILD, PLOS)

	INCLUDE 	'glbparam.f'
	INCLUDE 	'globtrrn.f'
	include		'globrpt.f'

	REAL*4		PLOS

	ITYPE = 0
	IFLOORS = 0
	IF (IBUILD .LE. 0 ) RETURN

	ITYPE = KBILDTYPE(IBUILD)
	PLOS = BILDOPENING(ITYPE)

	RETURN
	END


		 */
	}
	public void setPLOS(double p) {
		if (p < 0.0) return;
		if (p > 1.0) return;
		plos = p;
	}

	public double getSpeedReduction (int moverType){ // trrnlib.trrn_get_feature_move
		if (moverType >= Constants.MOVER_TYPES) return 0.0;
		if (moverType <0) return 0.0;
		return speedFactor[moverType];
	}
	public void setSpeedReduction(int moverType, double d){
		if (moverType >= Constants.MOVER_TYPES) return;
		if (moverType <0) return;
		if (d < 0.0) return;
		if (d > 1.0) return;
		speedFactor[moverType] = d;
	}

	// INTEGER*2 KRIVERPATTERN(NUMRIVERTYPES)	!


}
