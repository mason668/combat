package data.view;

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
import data.csd.Weapon;
import data.managers.PlatformList;

public class MenuPanelWeaponDataSets extends MenuPanel {
	
	private static final long serialVersionUID = 1L;
	private Weapon myWeapon;
	private PlatformList myPlatformList;

	/**
	 * A default main routine to allow testing
	 * @param args
	 */
	public static void main(String args[]){
		Weapon weapon = new Weapon("weapon01");
		PlatformList list = new PlatformList();
		list.add(new Platform("platform1"));
		list.add(new Platform("platform2"));
		list.add(new Platform("platform3"));
		list.add(new Platform("platform4"));

//		weapon.setPlatformPH("platform1", "ph001");
//		weapon.setPlatformPK("platform1", "ph001");
//		weapon.setPassengerPH("platform1", "pk001");
//		weapon.setPassengerPK("platform1", "pk001");
//		weapon.setPlatformPH("platform3", "ph003");
//		weapon.setPlatformPK("platform3", "ph003");
//		weapon.setPassengerPH("platform3", "pk003");
//		weapon.setPassengerPK("platform3", "pk003");
		MenuPanelWeaponDataSets me = new MenuPanelWeaponDataSets(null, weapon, list);
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

	public MenuPanelWeaponDataSets(ActionListener actionListener, Weapon weapon, 
			PlatformList platformList) {
		super(actionListener);
		myWeapon = weapon;
		myPlatformList = platformList;
		this.setPreferredSize(new Dimension(500,500));
		this.setLayout(new BorderLayout());
		this.add(new JLabel ("Ph PK Set Assignment for weapon " + myWeapon.getName())
				,BorderLayout.PAGE_START);
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
			super.setSize(size, 5);
			Set<String> keySet = myPlatformList.keySet();
			columnNames[0] = "Platform Name";
			columnNames[1] = "Platform PH Table";
			columnNames[2] = "Platform PK Table";
			columnNames[3] = "Passenger PH Table";
			columnNames[4] = "Passenger PK Table";
			int i=0;
			for (String key : keySet){
				Platform platform = myPlatformList.getPlatform(key);
				if (platform != null){
					dataTable[i][0] = key.substring(0);
					dataTable[i][1] = "Unknown";
					dataTable[i][2] = "Unknown";
					dataTable[i][3] = "Unknown";
					dataTable[i][4] = "Unknown";
					{ 
						//TODO phtable could be null - hence no name
						String tableName = myWeapon.getPlatformPH(key).getName();
						if ( tableName != null){
							dataTable[i][1] = tableName;
						} else {
							dataTable[i][1] = "";
						}
					}
					{ 
						String tableName = myWeapon.getPlatformPK(key).getName();
						if ( tableName != null){
							dataTable[i][2] = tableName;
						} else {
							dataTable[i][2] = "";
						}
					}
					{ 
						String tableName = myWeapon.getPassengerPH(key).getName();
						if ( tableName != null){
							dataTable[i][3] = tableName;
						} else {
							dataTable[i][3] = "";
						}
					}
					{ 
						String tableName = myWeapon.getPassengerPK(key).getName();
						if ( tableName != null){
							dataTable[i][4] = tableName;
						} else {
							dataTable[i][4] = "";
						}
					}
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
