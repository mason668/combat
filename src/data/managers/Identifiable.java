package data.managers;

public interface Identifiable {
	public String getName();
	public void setName(String name);
	public int getNumber(); //TODO should this be index?
	public void setNumber(int number);

}
