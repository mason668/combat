package view.map;

import data.map.Coordinate;

public interface MapListener {
	public void moveMouse(Coordinate c);
	public void clickMap(Coordinate c);

}
