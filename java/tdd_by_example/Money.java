
public abstract class Money {
	protected int amount;
	protected String currency;
	
	public Money(int amount, String currency) {
		this.amount = amount;
		this.currency = currency;
	}
	
	@Override
	public boolean equals(Object object) {
		Money money = (Money) object;
		return money.amount == this.amount && money.getClass().equals(this.getClass());
	}

	public static Money dollar(int amount) {
		return new Dollar(amount, "USD");
	}
	
	public static Money franc(int amount) {
		return new Franc(amount, "CHF");
	}
	
	public abstract Money times(int multiplier);

	public String currency() {
		return currency;
	}
}

class Dollar extends Money {
	
	public Dollar(int amount, String currency) {
		super(amount, currency);
	}

	public Money times(int multiplier) {
		return Money.dollar(amount * multiplier);
	}

}

class Franc extends Money {
	
	public Franc(int amount, String currency) {
		super(amount, currency);
	}

	public Money times(int multiplier) {
		return new Franc(amount * multiplier, null);
	}

}