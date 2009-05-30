import java.util.HashMap;
import java.util.Map;


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

	public Money reduce(Bank bank, String to) {
		
		return new Money(amount / bank.rate(currency, to), to);
	}
}

interface Expression {
	Money reduce(Bank bank, String to);
}

class Sum implements Expression {
	Money augend;
	Money addend;

	public Sum(Money augend, Money addend) {
		this.augend = augend;
		this.addend = addend;
	}

	public Money reduce(Bank bank, String to) {
		int amount = augend.amount + addend.amount;
		return new Money(amount, to);
	}
}

class Bank {
	
	private Map<Pair, Integer> rates = new HashMap<Pair, Integer>();

	public Money reduce(Expression source, String to) {
		return source.reduce(this, to);
	}

	public void addRate(String from, String to, int amount) {
		rates.put(new Pair(from, to), amount);
	}
	
	int rate(String from, String to) {
		if (from.equals(to)) return 1;
		return rates.get(new Pair(from, to));
	}
}

class Pair {
	private String from;
	private String to;
	
	Pair(String from, String to) {
		this.from = from;
		this.to = to;
	}
	
	@Override
	public boolean equals(Object obj) {
		Pair pair = (Pair) obj;
		return from.equals(pair.from) && to.equals(pair.to);
	}
	
	@Override
	public int hashCode() {
		return 0;
	}
}
