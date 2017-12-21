package view.janus_menu;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

public class SidePanel extends JPanel{
	private static final long serialVersionUID = 1L;
	
	
	MenuPanel menuPanel = new MenuPanel();
	InfoPanel infoPanel = new InfoPanel();
	
	public SidePanel(){
		super();
		this.setBackground(Color.BLACK);
//		this.setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
		this.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.gridx = 0;
        c.gridy = 0;
//        c.gridheight = 3;
		JScrollPane pane = new JScrollPane(menuPanel);
		this.add(pane,c);
//		pane.add(menuPanel);
        c.gridy = 3;
        c.gridheight = 1;
        c.weighty = 0.5;
		this.add(infoPanel,c);
	}
}
