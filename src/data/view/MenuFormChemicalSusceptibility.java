package data.view;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import data.CSData;

public class MenuFormChemicalSusceptibility extends MenuPanel{
	
	private static final long serialVersionUID = 1L;
	private JTextField txtSelfRecognitionDose = new JTextField("1.5",20);
	private JTextField txtIncapacityMean = new JTextField("3.33",20);
	private JTextField txtIncapacitySigma = new JTextField("0.3",20);
	private JTextField txtDeathMean = new JTextField("4.25",20);
	private JTextField txtDeathSigma = new JTextField("0.31",20);
	private JTextField txtAlarmConcentration = new JTextField("0.74",20);
	private JTextField txtSelfRecognitionTime = new JTextField("3.0",20);
	private JTextField txtMaskingTime = new JTextField("7.5",20);
	private JTextField txtCrewAlarmTime = new JTextField("2.0",20);
	private JTextField txtIncapacitationRespopnseTime = new JTextField("15.0",20);
	private JTextField txtExpirationResponseTime = new JTextField("30.0",20);
	private JTextField txtDetectorAlarmTime = new JTextField("3.0",20);
	
	private CSData csdata;

	public MenuFormChemicalSusceptibility(ActionListener actionListener, CSData csd){
		super(actionListener);
		csdata = csd;
		populateData();
		setupHandler();

		setLines(14);
//		this.setLayout(new GridLayout(13,1,5,5));
		this.add(new JLabel("Chemical Susceptibility Levels and Response Times"));
		{
			JPanel p1 = new JPanel();
			p1.add(new JLabel("Self-recognition dose (mg/cubic metre) "));
			p1.add(txtSelfRecognitionDose);
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(new JLabel("Assuming that incapacitation levels are "
					+ "distributed Log Normal, provide the Mean and Sigma "
					+ "of the underlying Normal"));
			p1.add(txtIncapacityMean);
			p1.add(txtIncapacitySigma);
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(new JLabel("Assuming that death levels are "
					+ "distributed Log Normal, privide the Mean and Sigma "
					+ "of the underlying Normal"));
			p1.add(txtDeathMean);
			p1.add(txtDeathSigma);
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(new JLabel("Chemical concentration over time "
					+ "for chemical alarms"));
			p1.add(txtAlarmConcentration);
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(new JLabel("Self-recognition response time (sec)"));
			p1.add(txtSelfRecognitionTime);
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(new JLabel("Personnel masking time (sec)"));
			p1.add(txtMaskingTime);
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(new JLabel("Crew alarm time (sec)"));
			p1.add(txtCrewAlarmTime);
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(new JLabel("Incapacitation response time (sec)"));
			p1.add(txtIncapacitationRespopnseTime);
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(new JLabel("Expiration response time (sec)"));
			p1.add(txtExpirationResponseTime);
			this.add(p1);
		}
		{
			JPanel p1 = new JPanel();
			p1.add(new JLabel("Time (sec) that a detector must wait after "
					+ "being in a concentration of 0.5 mg/m3 before "
					+ "it can alarm"));
			p1.add(txtDetectorAlarmTime);
			this.add(p1);
		}

		this.add(new JLabel(""));
		this.add(makeButton("Back",actionListener));
	}
	
	private void populateData(){
		if (csdata == null) return;

		this.txtSelfRecognitionDose.setText(((Double)csdata.getSelfRecognitionDose()).toString());
		this.txtIncapacityMean.setText(((Double) csdata.getIncapacityMean()).toString());
		this.txtIncapacitySigma.setText(((Double) csdata.getIncapacitySigma()).toString());
		this.txtDeathMean.setText(((Double) csdata.getDeathMean()).toString());
		this.txtDeathSigma.setText(((Double) csdata.getDeathSigma()).toString());
		this.txtAlarmConcentration.setText(((Double) csdata.getAlarmConcentration()).toString());
		this.txtSelfRecognitionTime.setText(((Double) csdata.getSelfRecognitionTime()).toString());
		this.txtMaskingTime.setText(((Double) csdata.getMaskingTime()).toString());
		this.txtCrewAlarmTime.setText(((Double) csdata.getCrewAlarmTime()).toString());
		this.txtIncapacitationRespopnseTime.setText(((Double) csdata.getIncapacitationResponseTime()).toString());
		this.txtExpirationResponseTime.setText(((Double) csdata.getExpirationResponseTime()).toString());
		this.txtDetectorAlarmTime.setText(((Double) csdata.getDetectorAlarmTime()).toString());
	}
	
	private void setupHandler(){
		Handler handler = new Handler();
		txtSelfRecognitionDose.addActionListener(handler);
		txtIncapacityMean.addActionListener(handler);
		txtIncapacitySigma.addActionListener(handler);
		txtDeathMean.addActionListener(handler);
		txtDeathSigma.addActionListener(handler);
		txtAlarmConcentration.addActionListener(handler);
		txtSelfRecognitionTime.addActionListener(handler);
		txtMaskingTime.addActionListener(handler);
		txtCrewAlarmTime.addActionListener(handler);
		txtIncapacitationRespopnseTime.addActionListener(handler);
		txtExpirationResponseTime.addActionListener(handler);
		txtDetectorAlarmTime.addActionListener(handler);
	}

	private class Handler implements ActionListener {

		@Override
		public void actionPerformed(ActionEvent event) {
			if (csdata == null) return;
			if (event.getSource()== txtSelfRecognitionDose){
				csdata.setSelfRecognitionDose(Double.parseDouble(txtSelfRecognitionDose.getText()));
			} else if (event.getSource()== txtIncapacityMean){
				csdata.setIncapacityMean(Double.parseDouble(txtIncapacityMean.getText()));
			} else if (event.getSource()== txtIncapacitySigma){
				csdata.setIncapacityMean(Double.parseDouble(txtIncapacitySigma.getText()));
			} else if (event.getSource()== txtDeathMean){
				csdata.setIncapacityMean(Double.parseDouble(txtDeathMean.getText()));
			} else if (event.getSource()== txtDeathSigma){
				csdata.setIncapacityMean(Double.parseDouble(txtDeathSigma.getText()));
			} else if (event.getSource()== txtAlarmConcentration){
				csdata.setIncapacityMean(Double.parseDouble(txtAlarmConcentration.getText()));
			} else if (event.getSource()== txtSelfRecognitionTime){
				csdata.setIncapacityMean(Double.parseDouble(txtSelfRecognitionTime.getText()));
			} else if (event.getSource()== txtMaskingTime){
				csdata.setIncapacityMean(Double.parseDouble(txtMaskingTime.getText()));
			} else if (event.getSource()== txtCrewAlarmTime){
				csdata.setIncapacityMean(Double.parseDouble(txtCrewAlarmTime.getText()));
			} else if (event.getSource()== txtIncapacitationRespopnseTime){
				csdata.setIncapacityMean(Double.parseDouble(txtIncapacitationRespopnseTime.getText()));
			} else if (event.getSource()== txtExpirationResponseTime){
				csdata.setExpirationResponseTime(Double.parseDouble(txtExpirationResponseTime.getText()));
			} else if (event.getSource()== txtDetectorAlarmTime){
				csdata.setDetectorAlarmTime(Double.parseDouble(txtDetectorAlarmTime.getText()));
			}else {
			}
		}
	}
}
