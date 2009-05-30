
public class Money {

}

class Dollar {
	int amount;
	
	public Dollar(int amount) {
		this.amount = amount;
	}

	public void times(int multiplier) {
		this.amount *= multiplier;
	}

}