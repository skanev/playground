
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
		return new Sum(this, addend);
	}

	public Money reduce(String to) {
		return this;
	}
}

interface Expression {
	Money reduce(String to);
}

class Sum implements Expression {
	Money augend;
	Money addend;

	public Sum(Money augend, Money addend) {
		this.augend = augend;
		this.addend = addend;
	}

	public Money reduce(String to) {
		int amount = augend.amount + addend.amount;
		return new Money(amount, to);
	}
}

class Bank {

	public Money reduce(Expression source, String to) {
		return source.reduce(to);
	}
}

