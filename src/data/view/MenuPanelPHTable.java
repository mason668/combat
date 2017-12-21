package data.view;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;

import data.csd.PHTable;

public class MenuPanelPHTable extends MenuPanel {
	
	private static final long serialVersionUID = 1L;
	private JTable table;
	private PHTable myPHTable;

	/**
	 * A default main routine to allow testing
	 * @param args
	 */
	public static void main(String args[]){
		PHTable phTable  = new PHTable("test_table");
		MenuPanelPHTable me = new MenuPanelPHTable(null, phTable);
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

	public MenuPanelPHTable(ActionListener actionListener, PHTable phTable) {
		myPHTable = phTable;
		if (myPHTable == null){} //TODO
		this.setPreferredSize(new Dimension(500,500));
		this.setLayout(new BorderLayout());
		this.add(new JLabel ("PH Table "+ myPHTable.getName()),BorderLayout.PAGE_START);
		table = new JTable(new MyTableModel());
		JScrollPane scrollPane = new JScrollPane(table, 
				JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, 
				JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		table.setFillsViewportHeight(true);
		this.add(scrollPane, BorderLayout.CENTER);
		this.add(makeButton("Back", actionListener),BorderLayout.PAGE_END);
	}
	
	private class MyTableModel extends CSDataModel {
		
		private static final long serialVersionUID = 1L;

		public MyTableModel(){
//			System.out.println(myPHTable.toLog());
			int rows = myPHTable.getRows();
			int cols = myPHTable.getCols()+1;
			super.setSize(rows, cols);
			for (int row = 0; row<rows;row++){
				for (int col=0;col<cols;col++){
					dataTable[row][col] = 0.0;  
					columnNames[col] = "col " + col;
				}
				dataTable[row][0] = "row " + row;
			}
			for (int row = 0; row<rows;row++){
				dataTable[row][0] = myPHTable.getPosture(row);
				for (int col=1;col<cols;col++){
					dataTable[row][col] = myPHTable.getData(row, col-1);  
				}
			}
			columnNames[0] = "Posture";
			for (int col=1;col<cols;col++){
				columnNames[col] = "Range " + myPHTable.getRange(col-1);
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
	}

}
