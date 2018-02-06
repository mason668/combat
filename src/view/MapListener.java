package view;

import data.map.Coordinate;

public interface MapListener {
	public void showCoordinate(Coordinate c);
	public void selectCoordinate(Coordinate c);

}
