package models.directfire.view;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Set;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import data.CSData;
import data.csd.PHTable;
import data.view.MenuPanel;

public class MenuPanelModelDirectFirePH extends MenuPanel{
	
	private static final long serialVersionUID = 1L;
	
	private JComboBox<String> phListBox;
	private JComboBox<String> postureListBox;
	private JTextField range = new JTextField("1.0",20);
	private JTextField result = new JTextField("0.0",20);
	private String[] phListArray;
	private String[] postureListArray;
	
	private CSData csdata;

	public MenuPanelModelDirectFirePH(ActionListener actionListener, CSData csd){
		super(actionListener);
		csdata = csd;
		populateData();

		setLines(7);
		this.add(new JLabel("Test PH Table"));
		{
			JPanel p1 = new JPanel();
			p1.add(new JLabel("Select PH Table) "));
			p1.add(phListBox);
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(new JLabel("Select posture "));
			p1.add(postureListBox);
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(new JLabel("Enter range "));
			p1.add(range);
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			JButton b1 = new JButton("Calculate");
			b1.addActionListener(new Handler());
			p1.add(b1);
			p1.add(new JLabel("Result: "));
			p1.add(result);
			this.add(p1);
		}

		this.add(new JLabel(""));
		this.add(makeButton("Back",actionListener));
	}
	
	private void populateData(){
		if (csdata == null) return;
		Set<String> set = csdata.getPHTableList().keySet();
		phListArray = set.toArray(new String[set.size()]);
		phListBox = new JComboBox<String>(phListArray);
		postureListArray = PHTable.getPostureNames();
		postureListBox = new JComboBox<String>(postureListArray);
	}
	
	private class Handler implements ActionListener {

		@Override
		public void actionPerformed(ActionEvent event) {
			PHTable phTable = csdata.getPHTableList().get(phListArray[phListBox.getSelectedIndex()]);
			String posture = postureListArray[postureListBox.getSelectedIndex()];
			try{
				double r = Double.parseDouble(range.getText());
				double ph = phTable.getProb(posture, r);
				result.setText(((Double) ph).toString());
			} catch (Exception e){}
		}
	}
}
