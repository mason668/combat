package view.map;

import java.awt.Color;

import javax.swing.JPanel;
import javax.swing.JTextArea;

import data.map.AreaFeature;
import data.map.Coordinate;
import data.map.Map;

public class MapInfo extends JPanel implements MapListener{
	
	private JTextArea txtCoordinates = new JTextArea("00000.00000 00000.00000");
	private JTextArea txtMessage = new JTextArea("no location selected");
	private Map myMap;
	private boolean mapValid = false;
	
	public MapInfo(){
		super();
		txtCoordinates.setBackground(Color.GRAY);
		this.setBackground(Color.WHITE);
		this.add(txtCoordinates);
		this.add(txtMessage);
	}
	
	public void setMap(Map map){
		myMap = map;
		mapValid = true; // TODO is there more validation required?
	}

	@Override
	public void moveMouse(Coordinate c) {
		if (!mapValid) return;
		if (c == null) return;
		txtCoordinates.setText(String.format("%.5f", c.getX()) 
				+ " " + String.format("%.5f", c.getY()) );
	}

	@Override
	public void clickMap(Coordinate c) {
		if (!mapValid) return;
		String message = "Pressed " + String.format("%.5f", c.getX()) 
				+ " " + String.format("%.5f", c.getY());
		AreaFeature feature = myMap.whichAreaFeature(c);
		if (feature != null){
			message = feature.getFeatureType().getName();
		} else {
			message = " Open" ;
		}
		txtMessage.setText(message.substring(0));
	}

}
