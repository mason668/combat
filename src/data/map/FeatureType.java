package data.map;

import java.awt.Color;

public class FeatureType {
	
	private Color myColor = Color.GRAY;
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
