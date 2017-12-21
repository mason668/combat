package view;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.InputVerifier;
import javax.swing.JComponent;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 * Attempt to make a self updating data field - needs an object, not a primitive - ie can do string, but not double
 */
public class DataField extends JPanel {
	private static final long serialVersionUID = 1L;
	private JTextField myField = new JTextField();
	private JLabel myLabel = new JLabel("label");
	private JFormattedTextField myNumber = new JFormattedTextField();

	public DataField(String label, String value, ActionListener listener){
		init(label, value);
		myField.addActionListener(listener);
		myNumber.addActionListener(listener);
	}

	private void init (String label, String value){
		this.setPreferredSize(new Dimension(200,25));
		this.setLayout(new GridLayout(0,2));
		myField.setPreferredSize(new Dimension(20,20));
		myField.setMinimumSize(new Dimension(40,20));
		myField.setText(value.substring(0));
//		myField.setInputVerifier(new VerifyDouble());
		myLabel.setText(label);

		myNumber.setPreferredSize(new Dimension(20,20));
		myNumber.setMinimumSize(new Dimension(40,20));
		myNumber.setValue(new Double(0));
		myNumber.addPropertyChangeListener("value", new PropertyChangeListener(){

			@Override
			public void propertyChange(PropertyChangeEvent e) {
	        	System.out.println("change listener");
	        	Object source = e.getSource();
	        	if (source != myNumber) {
        	    	myNumber.setValue(new Double (0));
	        		return;
	        	}
	        	if (!(source instanceof Double)) {
        	    	myNumber.setValue(new Double (0));
	        		return;
	        	}
    	        try {
    	            double d = (Double) myNumber.getValue();
        	    	myNumber.setValue(new Double (d));
    	        } catch (NumberFormatException exception) {
    	        	System.out.println("caught");
        	    	myNumber.setValue(new Double (0));
    	        }
			}});
		
		this.add(myLabel);
		this.add(myNumber);
		this.setBackground(Color.gray);
	}
	
	/*
	private class VerifyDouble extends InputVerifier{

		@Override
		public boolean verify(JComponent input) {
        	System.out.println("verify");
			String text = ((JTextField) input).getText();
	        try {
	            double value = Double.parseDouble(text);
	            return true;
	        } catch (NumberFormatException e) {
	        	System.out.println("caught");
	            return false;
	        }
		}
		
	}
	*/
}
