import junit.framework.TestCase;


public class MoneyTest extends TestCase {
	public void testMultiplication() throws Exception {
		Dollar five = new Dollar(5);
		assertEquals(new Dollar(10), five.times(2));
		assertEquals(new Dollar(15), five.times(3));
	}
	
	public void testEquality() throws Exception {
		assertTrue(new Dollar(5).equals(new Dollar(5)));
		assertFalse(new Dollar(5).equals(new Dollar(6)));
	}
}
