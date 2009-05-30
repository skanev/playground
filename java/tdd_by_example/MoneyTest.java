import junit.framework.TestCase;


public class MoneyTest extends TestCase {
	public void testMultiplication() throws Exception {
		Dollar five = new Dollar(5);
		five.times(2);
		assertEquals(10, five.amount);
	}
}
