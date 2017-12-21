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

public class MenuPanelPlatformCharacteristics2 extends MenuPanel {
	
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
		MenuPanelPlatformCharacteristics2 me = new MenuPanelPlatformCharacteristics2(null, list);
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

	public MenuPanelPlatformCharacteristics2(ActionListener actionListener, PlatformList list) {
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
			super.setSize(size, 11);
			Set<String> keySet = myPlatformList.keySet();
			columnNames[0] = "Platform Name";
			columnNames[1] = "Laser Designator";
			columnNames[2] = "Firer Category";
			columnNames[3] = "Flyer Type";
			columnNames[4] = "RADAR Type";
			columnNames[5] = "Surveilance Type";
			columnNames[6] = "Swimmer Type";
			columnNames[7] = "Crawl Speed";
			columnNames[8] = "Fording Depth";
			columnNames[9] = "Command Weapon";
			columnNames[10] = "Area Weapon";
			int i=0;
			for (String key : keySet){
				Platform platform = myPlatformList.getPlatform(key);
				if (platform != null){
					dataTable[i][0] = key.substring(0);
					dataTable[i][1] = "unknown";
					dataTable[i][2] = "unknown";
					dataTable[i][3] = "unknown";
					dataTable[i][4] = "unknown";
					dataTable[i][5] = "unknown";
					dataTable[i][6] = "unknown";
					dataTable[i][7] = "unknown";
					dataTable[i][8] = "unknown";
					dataTable[i][9] = "unknown";
					dataTable[i][10] = "unknown";
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
