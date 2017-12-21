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

import data.csd.Weapon;
import data.managers.WeaponList;

public class MenuPanelWeaponCharacteristics1 extends MenuPanel {
	
	private static final long serialVersionUID = 1L;
	private WeaponList myWeaponList;

	/**
	 * A default main routine to allow testing
	 * @param args
	 */
	public static void main(String args[]){
		WeaponList list = new WeaponList();
		list.add(new Weapon ("test1"));
		list.add(new Weapon ("test2"));
		list.add(new Weapon ("test3"));
		list.add(new Weapon ("test4"));
		MenuPanelWeaponCharacteristics1 me = new MenuPanelWeaponCharacteristics1(null, list);
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

	public MenuPanelWeaponCharacteristics1(ActionListener actionListener, WeaponList list) {
		super(actionListener);
		myWeaponList = list;
		this.setPreferredSize(new Dimension(500,500));
		this.setLayout(new BorderLayout());
		this.add(new JLabel ("Weapon Characteristics"),BorderLayout.PAGE_START);
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
			int size = myWeaponList.getSize();
			super.setSize(size, 8);
			Set<String> keySet = myWeaponList.keySet();
			columnNames[0] = "Weapon Name";
			columnNames[1] = "Lay Time";
			columnNames[2] = "Aim Time";
			columnNames[3] = "Reload Time";
			columnNames[4] = "Rounds per Trigger Pull";
			columnNames[5] = "Trigger Pulls per Reload";
			columnNames[6] = "Round Speed";
			columnNames[7] = "Min SSKP";
			int i=0;
			for (String key : keySet){
				Weapon weapon = myWeaponList.getWeapon(key);
				if (weapon != null){
					dataTable[i][0] = key.substring(0);
					dataTable[i][1] = "Unknown";
					dataTable[i][2] = "Unknown";
					dataTable[i][3] = "Unknown";
					dataTable[i][4] = "Unknown";
					dataTable[i][5] = "Unknown";
					dataTable[i][6] = "Unknown";
					dataTable[i][7] = "Unknown";
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
