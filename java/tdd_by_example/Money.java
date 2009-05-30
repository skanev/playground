
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

class Franc {
	private int amount;
	
	public Franc(int amount) {
		this.amount = amount;
	}

	public Franc times(int multiplier) {
		return new Franc(amount * multiplier);
	}
	
	@Override
	public boolean equals(Object object) {
		Franc franc = (Franc) object;
		return franc.amount == this.amount;
	}

}