package view;

import java.awt.Color;

import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.border.Border;

import data.map.AreaFeature;
import data.map.Coordinate;
import data.map.Map;

public class MapInfo extends JPanel implements MapListener{
	
	private JTextArea txtCoordinates = new JTextArea("00000.00000 00000.00000");
	private JTextArea txtMessage = new JTextArea("no location selected");
	private Map myMap;
	
	public MapInfo(Map map){
		super();
		myMap = map;
		txtCoordinates.setBackground(Color.GRAY);
		this.setBackground(Color.WHITE);
		this.add(txtCoordinates);
		this.add(txtMessage);
	}

	@Override
	public void showCoordinate(Coordinate c) {
		if (c == null) return;
		txtCoordinates.setText(String.format("%.5f", c.getX()) 
				+ " " + String.format("%.5f", c.getY()) );
	}

	@Override
	public void selectCoordinate(Coordinate c) {
		String message = "Pressed " + String.format("%.5f", c.getX()) 
				+ " " + String.format("%.5f", c.getY());
		AreaFeature feature = myMap.whichAreaFeature(c);
		if (feature != null){
			message = feature.getFeatureType().getName();
		} else {
			message = " open" ;
		}
		txtMessage.setText(message.substring(0));
	}

}
