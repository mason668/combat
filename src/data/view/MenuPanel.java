package data.view;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

public class MenuPanel extends JPanel{
	
	private static final long serialVersionUID = 1L;
	
	public MenuPanel(){}
	
	public MenuPanel(ActionListener actionListener){
//		this.setPreferredSize(new Dimension(500,500));
//		this.setBackground(new Color(100,100,100));
	}

	protected JButton makeButton(String label, ActionListener actionListener){
		JButton b1 = new JButton(label);
		if (actionListener != null){
			b1.addActionListener(actionListener);
		} else {
			// implement a default listener - mainly for testing
			b1.addActionListener(new ActionListener(){
				@Override
				public void actionPerformed(ActionEvent arg0) {
					JOptionPane.showMessageDialog(null, "no ActionListner has been implemented");
				}
			});
		}
		b1.setAlignmentX(Component.CENTER_ALIGNMENT);
		b1.setPreferredSize(new Dimension(200,30));
		return b1;
	}
	
	protected void setLines (int lines){
		this.setLayout(new GridLayout(lines,1,5,5));
	}

}
