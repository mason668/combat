package view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;

import sim.Scenario;
import view.menu.InfoButton;
import view.menu.MenuButton;
import view.menu.MenuController;

public class ReportView extends JPanel implements ReportListener { 
	
	private JTextArea reportArea;
	
	private InfoButton infoButton;
	private MenuButton dataButton;

	public ReportView(){
		super();
		reportArea = new JTextArea();
		reportArea.setFont(new Font("Arial", Font.PLAIN, 10));
		this.setLayout(new BorderLayout());
		JScrollPane pane = new JScrollPane(reportArea);
		this.setBackground(Color.green);
		//pane.setPreferredSize(new Dimension(300,200));
		pane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		pane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
		
		this.add(pane,BorderLayout.CENTER);
		
		JPanel buttonPanel = new JPanel();
		
		infoButton = new InfoButton();
		dataButton = new MenuButton("Data");

		JButton clearBtn = new SmallButton("Clear");
		clearBtn.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				reportArea.setText("");
			}
		});
		
		buttonPanel.add(infoButton);
		buttonPanel.add(dataButton);
		buttonPanel.add(clearBtn);
		
		this.add(buttonPanel,BorderLayout.NORTH);
	}


	@Override
	public void write(String message) {
		reportArea.setText(message + "\n");
//		traceArea.update(traceArea.getGraphics());
		reportArea.setCaretPosition(reportArea.getText().length() - 1);	
	}
	
	public void setMenuController(MenuController controller){
		if (controller == null) return;
		controller.addMenuButton(infoButton);
		controller.addMenuButton(dataButton);
		controller.setMenuState(infoButton);
	}
	
}
