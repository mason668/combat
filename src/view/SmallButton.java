package view;

import java.awt.Font;

import javax.swing.JButton;

public class SmallButton extends JButton {
	
	public SmallButton(){
		super();
		setFont();
	}
	
	public SmallButton(String label){
		super(label);
		setFont();
	}
	
	protected void setFont(){
		this.setFont(new Font("Arial", Font.PLAIN, 8));
	}

}
