package data.view;

import javax.swing.table.AbstractTableModel;

public class CSDataModel extends AbstractTableModel {

	private static final long serialVersionUID = 1L;
	protected String[] columnNames;
	protected Object[][] dataTable;
	
	public void setSize(int rows, int cols){
		dataTable = new Object[rows][cols];
		columnNames = new String[cols];
	}

	@Override
	public int getColumnCount() {
		return columnNames.length;
	}

	@Override
	public int getRowCount() {
		return dataTable.length;
	}

	@Override
	public Object getValueAt(int row, int col) {
		return dataTable[row][col];
	}
	
	@Override
	public Class getColumnClass(int col) {
		return getValueAt(0, col).getClass();
	}

	@Override
	public String getColumnName(int col) {
        return columnNames[col];
    }
}
