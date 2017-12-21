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

public class MenuPanelPlatformDetectionData extends MenuPanel {
	
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
		MenuPanelPlatformDetectionData me = new MenuPanelPlatformDetectionData(null, list);
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

	public MenuPanelPlatformDetectionData(ActionListener actionListener, PlatformList list) {
		super(actionListener);
		myPlatformList = list;
		this.setPreferredSize(new Dimension(500,500));
		this.setLayout(new BorderLayout());
		this.add(new JLabel ("Platform Detection Data"),BorderLayout.PAGE_START);
		JTable table = new JTable(new MyTableModel());
		JScrollPane scrollPane = new JScrollPane(table, 
				JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, 
				JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		table.setFillsViewportHeight(true);
		this.add(scrollPane, BorderLayout.CENTER);
		this.add(makeButton("Back", actionListener),BorderLayout.PAGE_END);
	}
	
	private class MyTableModel extends CSDataModel  {
		
		private static final long serialVersionUID = 1L;

		public MyTableModel(){
			int size = myPlatformList.getSize();
			super.setSize(size, 9);
			Set<String> keySet = myPlatformList.keySet();
			columnNames[0] = "Platform Name";
			columnNames[1] = "Length";
			columnNames[2] = "Width";
			columnNames[3] = "Height";
			columnNames[4] = "Exposed";
			columnNames[5] = "Crawling";
			columnNames[6] = "Partial";
			columnNames[7] = "Full";
			columnNames[8] = "Pit";
			int i=0;
			for (String key : keySet){
				Platform platform = myPlatformList.getPlatform(key);
				if (platform != null){
					dataTable[i][0] = key.substring(0);
					dataTable[i][1] = "Unknown";
					dataTable[i][2] = "Unknown";
					dataTable[i][3] = "Unknown";
					dataTable[i][4] = "Unknown";
					dataTable[i][5] = "Unknown";
					dataTable[i][6] = "Unknown";
					dataTable[i][7] = "Unknown";
					dataTable[i][8] = "Unknown";
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
