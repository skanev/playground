import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.text.ParseException;

import org.junit.Test;

public class ArgsTest {
    @Test
    public void noSchemaAndNoArgumentsShouldBeFine() throws Exception {
        Args args = new Args("", new String[0]);
        assertTrue(args.isValid());
        assertEquals(0, args.cardinality());
    }
    
    @Test
    public void noSchemaButWithOneArgumentShouldError() throws Exception {
        Args args = new Args("", new String[]{"-x"});
        assertFalse(args.isValid());
        assertEquals("Argument(s) -x unexpected", args.errorMessage());
    }
    
    @Test
    public void noSchemaButMultipleArgumentsShouldError() throws Exception {
        Args args = new Args("", new String[]{"-x", "-y"});
        assertFalse(args.isValid());
        assertEquals("Argument(s) -xy unexpected", args.errorMessage());
    }
    
    @Test(expected = ParseException.class)
    public void nonLetterSchemaShouldThrowException() throws Exception {
        new Args("*", new String[]{});
    }
    
    @Test(expected = ParseException.class)
    public void invalidArgumentFormatShouldThrowException() throws Exception {
        new Args("f~", new String[]{});
    }
    
    @Test
    public void parsingSimpleBoolean() throws Exception {
        Args args = new Args("x", new String[]{"-x"});
        assertTrue(args.isValid());
        assertEquals(1, args.cardinality());
        assertEquals(true, args.getBoolean('x'));
    }
    
    @Test
    public void noBooleanShouldDefaultToFalse() throws Exception {
        Args args = new Args("x", new String[]{});
        assertTrue(args.isValid());
        assertEquals(false, args.getBoolean('x'));
    }
    
    @Test
    public void parsingSimpleString() throws Exception {
        Args args = new Args("x*", new String[]{"-x", "param"});
        assertTrue(args.isValid());
        assertEquals(1, args.cardinality());
        assertTrue(args.has('x'));
        assertEquals("param", args.getString('x'));
    }
    
    @Test
    public void noStringShouldDefaultToEmpty() throws Exception {
        Args args = new Args("x*", new String[]{});
        assertTrue(args.isValid());
        assertEquals("", args.getString('x'));
    }
    
    @Test
    public void missingStringArgumentShouldBeAnError() throws Exception {
        Args args = new Args("x*", new String[]{"-x"});
        assertFalse(args.isValid());
        assertEquals("Could not find string parameter for -x", args.errorMessage());
    }
    
    @Test
    public void parsingSimpleInteger() throws Exception {
        Args args = new Args("x#", new String[]{"-x", "42"});
        assertTrue(args.isValid());
        assertEquals(1, args.cardinality());
        assertTrue(args.has('x'));
        assertEquals(42, args.getInt('x'));
    }
    
    @Test
    public void noIntegerShouldDefaultToZero() throws Exception {
        Args args = new Args("x#", new String[]{});
        assertTrue(args.isValid());
        assertEquals(0, args.getInt('x'));
    }
    
    @Test
    public void parsingInvalidIntegerShouldBeAnError() throws Exception {
        Args args = new Args("x#", new String[]{"-x", "forty two"});
        assertFalse(args.isValid());
        assertEquals("Invalid integer for parameter -x: forty two", args.errorMessage());
    }
    
    @Test
    public void missingIntegerShuoldBeAnError() throws Exception {
        Args args = new Args("x#", new String[]{"-x"});
        assertFalse(args.isValid());
        assertEquals("Could not find integer parameter for -x", args.errorMessage());
    }
    
    @Test
    public void spacesInSchemaShouldBeOK() throws Exception {
        Args args = new Args("x, y", new String[]{"-x", "-y"});
        assertTrue(args.isValid());
        assertEquals(2, args.cardinality());
        assertTrue(args.has('x'));
        assertTrue(args.has('y'));
    }

    @Test
    public void twoBooleansAndTwoStrings() throws Exception {
        Args args = new Args("a,b,x*,y*", new String[]{"-x", "foo", "-a", "-y", "bar"});
        
        assertTrue(args.isValid());
        assertEquals(3, args.cardinality());
        
        assertTrue(args.getBoolean('a'));
        assertFalse(args.getBoolean('b'));
        
        assertEquals("foo", args.getString('x'));
        assertEquals("bar", args.getString('y'));
    }
}

