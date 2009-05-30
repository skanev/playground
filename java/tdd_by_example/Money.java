
public class Money {

}

class Dollar {
	private int amount;
	
	public Dollar(int amount) {
		this.amount = amount;
	}

	public Dollar times(int multiplier) {
		return new Dollar(amount * multiplier);
	}
	
	@Override
	public boolean equals(Object object) {
		Dollar dollar = (Dollar) object;
		return dollar.amount == this.amount;
	}

}