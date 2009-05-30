
public class Money {
	protected int amount;
	
	@Override
	public boolean equals(Object object) {
		Money money = (Money) object;
		return money.amount == this.amount;
	}
}

class Dollar extends Money {
	
	public Dollar(int amount) {
		this.amount = amount;
	}

	public Dollar times(int multiplier) {
		return new Dollar(amount * multiplier);
	}
	
}

class Franc extends Money {
	
	public Franc(int amount) {
		this.amount = amount;
	}

	public Franc times(int multiplier) {
		return new Franc(amount * multiplier);
	}

}