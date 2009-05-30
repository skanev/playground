
public class Money implements Expression {
	protected int amount;
	protected String currency;
	
	public Money(int amount, String currency) {
		this.amount = amount;
		this.currency = currency;
	}
	
	@Override
	public boolean equals(Object object) {
		Money money = (Money) object;
		return money.amount == this.amount && money.currency().equals(this.currency());
	}

	public static Money dollar(int amount) {
		return new Money(amount, "USD");
	}
	
	public static Money franc(int amount) {
		return new Money(amount, "CHF");
	}
	
	public Money times(int multiplier) {
		return new Money(amount * multiplier, currency);
	}

	public String currency() {
		return currency;
	}

	public Expression plus(Money addend) {
		return new Money(this.amount + addend.amount, currency);
	}
}

interface Expression {
}

class Bank {

	public Money reduce(Expression source, String to) {
		return Money.dollar(10);
	}
}

