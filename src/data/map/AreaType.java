package data.map;

import java.awt.Color;

public class AreaType {
	
	public AreaType (String name){
		areaName = name.substring(0); //TODO should remove spaces
	}
	
	private String areaName = "generic_area";
	
	public double getSpeedReduction (int moverType){ // TODO validate mover type
		return 1.0;
	}
	public String getName(){
		return areaName.substring(0);
	}
	
	private boolean isWater = false;
	
	public boolean isWater(){
		return isWater;
	}
	
	private Color myColor = Color.GREEN;
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

}
