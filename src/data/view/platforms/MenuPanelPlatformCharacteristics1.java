package data.view.platforms;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Set;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;

import data.csd.Platform;
import data.managers.PlatformList;
import data.view.CSDataModel;
import data.view.MenuPanel;

public class MenuPanelPlatformCharacteristics1 extends MenuPanel {
	
	private static final long serialVersionUID = 1L;
	private PlatformList myPlatformList;

	/**
	 * A default main routine to allow testing
	 * @param args
	 */
	public static void main(String args[]){
		PlatformList list = new PlatformList();
		list.add(new Platform ("test1"));
		list.add(new Platform ("test2"));
		list.add(new Platform ("test3"));
		list.add(new Platform ("test4"));
		MenuPanelPlatformCharacteristics1 me = new MenuPanelPlatformCharacteristics1(null, list);
		JFrame frame = new JFrame("test " + me.getClass().getCanonicalName());
		frame.add(me);
		frame.addWindowListener(
				new WindowAdapter(){
					public void windowClosing(WindowEvent event){
						System.exit(0);;
					}
				}
		);
		frame.setSize(800, 600);
		frame.setVisible(true);
		frame.validate();
	}

	public MenuPanelPlatformCharacteristics1(ActionListener actionListener, PlatformList list) {
		super(actionListener);
		myPlatformList = list;
		this.setPreferredSize(new Dimension(500,500));
		this.setLayout(new BorderLayout());
		this.add(new JLabel ("Platform Characteristics"),BorderLayout.PAGE_START);
		JTable table = new JTable(new MyTableModel());
		JScrollPane scrollPane = new JScrollPane(table, 
				JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, 
				JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		table.setFillsViewportHeight(true);
		this.add(scrollPane, BorderLayout.CENTER);
//		this.add(table.getTableHeader());
//		this.add(table);
//		this.add(new JLabel(" "));
//		this.add(new JLabel(" "));
		this.add(makeButton("Back", actionListener),BorderLayout.PAGE_END);
	}
	
	private class MyTableModel extends CSDataModel  {
		
		private static final long serialVersionUID = 1L;

		public MyTableModel(){
			int size = myPlatformList.getSize();
			super.setSize(size, 9);
			Set<String> keySet = myPlatformList.keySet();
			columnNames[0] = "Platform Name";
			columnNames[1] = "Max Road Speed";
			columnNames[2] = "Max Visibility";
			columnNames[3] = "Max Weapon Range";
			columnNames[4] = "Sensor height";
			columnNames[5] = "Crew Size";
			columnNames[6] = "Element Spacing";
			columnNames[7] = "Chemical Transmission Factor";
			columnNames[8] = "Host Capacity";
			int i=0;
			for (String key : keySet){
				Platform platform = myPlatformList.getPlatform(key);
				if (platform != null){
					dataTable[i][0] = key.substring(0);
					dataTable[i][1] = (Double) platform.getMaxVelocity();
					dataTable[i][2] = platform.getMaxVisibility();
					dataTable[i][3] = platform.getPrimaryRange();
					dataTable[i][4] = platform.getDetectionSize();
					dataTable[i][5] = platform.getCrewSize();
					dataTable[i][6] = platform.getElementSpacing();
					dataTable[i][7] = "chem";
					dataTable[i][8] = "host";
					i++;
				}
			}
		}

		public boolean isCellEditable(int row, int col) {
			//Note that the data/cell address is constant,
			//no matter where the cell appears onscreen.
			if (col < 2) {
			return false;
			} else {
			return true;
			}
		}

		/*
		public void setValueAt(Object value, int row, int col) {
	        data[row][col] = value;
	        fireTableCellUpdated(row, col);
	    }
	    */
	}



}
