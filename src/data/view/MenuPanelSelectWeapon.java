package data.view;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Set;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;

import data.csd.Weapon;
import data.managers.WeaponList;

public class MenuPanelSelectWeapon extends MenuPanel implements MouseListener{
	
	private static final long serialVersionUID = 1L;
	private JTable table;
	private DataViewer myFrame;
	WeaponList myWeaponList;
	String nextPanel;

	public MenuPanelSelectWeapon(DataViewer dv, WeaponList list, String panelName) {
		super(dv);
		myFrame = dv;
		myWeaponList = list;
		nextPanel = panelName;
		this.setPreferredSize(new Dimension(500,500));
		this.setLayout(new BorderLayout());
		this.add(new JLabel ("Weapons"),BorderLayout.PAGE_START);
		table = new JTable(new MyTableModel());
		JScrollPane scrollPane = new JScrollPane(table, 
				JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, 
				JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		table.setFillsViewportHeight(true);
		this.add(scrollPane, BorderLayout.CENTER);
//		this.add(table.getTableHeader());
//		this.add(table);
//		this.add(new JLabel(" "));
//		this.add(new JLabel(" "));
		this.add(makeButton("Back", dv),BorderLayout.PAGE_END);
		table.addMouseListener(this);
	}
	
	private class MyTableModel extends CSDataModel {
		private static final long serialVersionUID = 1L;
		
		public MyTableModel(){
			int rows = myWeaponList.getSize();
			int cols = 1;
			super.setSize(rows, cols);
			columnNames[0] = "Weapon Name";
			Set<String> keySet = myWeaponList.keySet();
			int i=0;
			for (String key : keySet){
				Weapon weapon = myWeaponList.getWeapon(key);
				if (weapon != null){
					dataTable[i][0] = key.substring(0);
					i++;
				}
			}
		}

		public boolean isCellEditable(int row, int col) {
			//Note that the data/cell address is constant,
			//no matter where the cell appears onscreen.
			if (col < 1) {
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

	/**
	 * MouseListener events
	 */
	@Override
	public void mouseReleased(MouseEvent arg0) {
		int j = table.getSelectedRow();
		String weaponName = (String) table.getModel().getValueAt(j, 0);
		System.out.println("mouse released " + j + " :" + weaponName + ":" + nextPanel);
		myFrame.changePanel(nextPanel, weaponName);
	}
	/**
	 * Unimplemented MouseListener events
	 */
	@Override
	public void mouseClicked(MouseEvent arg0) {}
	@Override
	public void mouseEntered(MouseEvent arg0) {}
	@Override
	public void mouseExited(MouseEvent arg0) {}
	@Override
	public void mousePressed(MouseEvent arg0) {}


}
