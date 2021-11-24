import java.text.ParseException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeSet;

public class Args {
	public enum ErrorCode {
	    OK, MISSING_STRING, MISSING_INTEGER, INVALID_INTEGER, UNEXPECTED_ARGUMENT;
	}

    private String schema;
    private List<String> argsList;
    private Iterator<String> currentArgument;
    private boolean valid = true;
    private Set<Character> unexpectedArguments = new TreeSet<Character>();
    private Map<Character, ArgumentMarshaler> marshalers = new HashMap<Character, Args.ArgumentMarshaler>();
    private Set<Character> argsFound = new HashSet<Character>();
    private char errorArgumentId = '\0';
    private String errorParamenter = "TILT";
    private ErrorCode errorCode = ErrorCode.OK;
	
	public Args(String schema, String [] args) throws ParseException {
	    this.schema = schema;
	    this.argsList = Arrays.asList(args);
	    parse();
    }

    private void parse() throws ParseException {
        if (schema.length() == 0 && argsList.isEmpty()) {
            this.valid = true;
        }
        parseSchema();
        try {
            parseArguments();
        } catch (ArgsException e) {
            this.valid = false;
        }
    }
    
    private boolean parseSchema() throws ParseException {
        for (String element : schema.split(",")) {
            if (element.length() > 0) {
                String trimmedElement = element.trim();
                parseSchemaElement(trimmedElement);
            }
        }
        return true;
    }

    private void parseSchemaElement(String element) throws ParseException {
        char elementId = element.charAt(0);
        String elementTail = element.substring(1);
        validateSchemaElementId(elementId);
        if (isBooleanSchemaElement(elementTail)) {
            parseBooleanSchemaElement(elementId);
        } else if (isStringSchemaElement(elementTail)) {
            parseStringSchemaElement(elementId);
        } else if (isIntegerSchemaElement(elementTail)) {
            parseIntegerSchemaElement(elementId);
        } else {
            throw new ParseException("Bad schema type: " + elementTail, 0);
        }
    }

    private void validateSchemaElementId(char elementId) throws ParseException {
        if (!Character.isLetter(elementId)) {
            throw new ParseException("Bad character: " + elementId + " in Args format: " + schema, 0);
        }
    }
    
    private void parseBooleanSchemaElement(char elementId) {
        BooleanMarshaler marshaler = new BooleanMarshaler();
        marshalers.put(elementId, marshaler);
    }
    
    private void parseStringSchemaElement(char elementId) {
        StringMarshaler marshaler = new StringMarshaler();
        marshalers.put(elementId, marshaler);
    }

    private void parseIntegerSchemaElement(char elementId) {
        IntegerMarshaler marshaler = new IntegerMarshaler();
        marshalers.put(elementId, marshaler);
    }
    
    private boolean isStringSchemaElement(String elementTail) {
        return elementTail.equals("*");
    }
    
    private boolean isBooleanSchemaElement(String elementTail) {
        return elementTail.length() == 0;
    }

    private boolean isIntegerSchemaElement(String elementTail) {
        return elementTail.equals("#");
    }
    
    private boolean parseArguments() throws ArgsException {
        this.currentArgument = argsList.iterator();
        while (currentArgument.hasNext()) {
            String arg = currentArgument.next();
            parseArgument(arg);
        }
        return true;
    }

    private void parseArgument(String arg) throws ArgsException {
        if (arg.startsWith("-")) {
            parseElements(arg);
        }
    }

    private void parseElements(String arg) throws ArgsException {
        for (int i = 1; i < arg.length(); i++) {
            parseElement(arg.charAt(i));
        }
    }

    private void parseElement(char argChar) throws ArgsException {
        if (setArgument(argChar)) {
            argsFound.add(argChar);
        } else {
            unexpectedArguments.add(argChar);
            this.errorCode = ErrorCode.UNEXPECTED_ARGUMENT;
            this.valid = false;
        }
    }

    private boolean setArgument(char argChar) throws ArgsException {
        ArgumentMarshaler marshaler = marshalers.get(argChar);
        if (marshaler == null) {
            return false;
        }
        try {
            marshaler.set(currentArgument);
            return true;
        } catch (ArgsException e) {
            this.errorArgumentId = argChar;
            this.valid = false;
            throw e;
        }
    }

    public int cardinality() {
        return argsFound.size();
    }
    
    public char getErrorArgumentId() {
        return errorArgumentId;
    }
    
    public ErrorCode getErrorCode() {
        return errorCode;
    }
    
    public String getErrorParamenter() {
        return errorParamenter;
    }
    
    public String errorMessage() {
        if (unexpectedArguments.size() > 0) {
            return unexpectedArgumentMessage();
        } else {
            switch (errorCode) {
            case MISSING_STRING:
                return String.format("Could not find string parameter for -%c", errorArgumentId);
            case MISSING_INTEGER:
                return String.format("Could not find integer parameter for -%c", errorArgumentId);
            case INVALID_INTEGER:
                return String.format("Invalid integer for parameter -%c: %s", errorArgumentId, errorParamenter);
                
            default:
                throw new RuntimeException("TILT: Should not get here.");
            }
        }
    }
    
    private String unexpectedArgumentMessage() {
        StringBuffer message = new StringBuffer("Argument(s) -");
        for (char c : unexpectedArguments) {
            message.append(c);
        }
        message.append(" unexpected");
        
        return message.toString();
    }
    
    public boolean getBoolean(char arg) {
        return (Boolean) marshalers.get(arg).get();
    }

    public String getString(char arg) {
        return (String) marshalers.get(arg).get();
    }

    public int getInt(char arg) {
        return (Integer) marshalers.get(arg).get();
    }
    
    public boolean has(char arg) {
        return argsFound.contains(arg);
    }
    
    public boolean isValid() {
        return valid;
    }
    
    private abstract class ArgumentMarshaler {
        public abstract Object get();
        public abstract void set(Iterator<String> currentArgument) throws ArgsException;
    }
    
    private class BooleanMarshaler extends ArgumentMarshaler {
        private boolean booleanValue = false;
        
        @Override
        public Boolean get() {
            return booleanValue;
        }
        
        @Override
        public void set(Iterator<String> currentArgument) {
            booleanValue = true;
        }
    }
    
    private class StringMarshaler extends ArgumentMarshaler {
        private String stringValue = "";
        
        @Override
        public String get() {
            return stringValue;
        }
        
        @Override
        public void set(Iterator<String> currentArgument) throws ArgsException {
            try {
                stringValue = currentArgument.next();
            } catch (NoSuchElementException e) {
                Args.this.errorCode  = ErrorCode.MISSING_STRING;
                throw new ArgsException();
            }
        }
    }
    
    private class IntegerMarshaler extends ArgumentMarshaler {
        private int intValue = 0;
        
        @Override
        public Integer get() {
            return intValue;
        }
        
        @Override
        public void set(Iterator<String> currentArgument) throws ArgsException {
            String parameter = null;
            try {
                parameter = currentArgument.next();
                intValue = new Integer(parameter);
            } catch (NoSuchElementException e) {
                Args.this.errorCode = ErrorCode.MISSING_INTEGER;
                throw new ArgsException();
            } catch (NumberFormatException e) {
                Args.this.errorParamenter = parameter;
                Args.this.errorCode = ErrorCode.INVALID_INTEGER;
                throw new ArgsException();
            }
        }
    }
}