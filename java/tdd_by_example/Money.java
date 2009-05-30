
public abstract class Money {
	protected int amount;
	
	@Override
	public boolean equals(Object object) {
		Money money = (Money) object;
		return money.amount == this.amount && money.getClass().equals(this.getClass());
	}

	public static Money dollar(int amount) {
		return new Dollar(amount);
	}
	
	public static Money franc(int amount) {
		return new Franc(amount);
	}
	
	public abstract Money times(int multiplier);
}

class Dollar extends Money {
	
	public Dollar(int amount) {
		this.amount = amount;
	}

	public Money times(int multiplier) {
		return new Dollar(amount * multiplier);
	}
	
}

class Franc extends Money {
	
	public Franc(int amount) {
		this.amount = amount;
	}

	public Money times(int multiplier) {
		return new Franc(amount * multiplier);
	}

}