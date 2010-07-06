import java.text.ParseException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class Args {
	private enum ErrorCode {
	    OK, MISSING_STRING, MISSING_INTEGER, INVALID_INTEGER, UNEXPECTED_ARGUMENT;
	}

    private String schema;
    private String[] args;
    private boolean valid = true;
    private Set<Character> unexpectedArguments = new TreeSet<Character>();
    private Map<Character, BooleanMarshaler> booleanArgs = new HashMap<Character, BooleanMarshaler>();
    private Map<Character, StringMarshaler> stringArgs = new HashMap<Character, StringMarshaler>();
    private Map<Character, IntegerMarshaler> intArgs = new HashMap<Character, IntegerMarshaler>();
    private Map<Character, ArgumentMarshaler> marshalers = new HashMap<Character, Args.ArgumentMarshaler>();
    private Set<Character> argsFound = new HashSet<Character>();
    private int currentArgument;
    private char errorArgumentId = '\0';
    private String errorParamenter = "TILT";
    private ErrorCode errorCode = ErrorCode.OK;
	
	public Args(String schema, String [] args) throws ParseException {
	    this.schema = schema;
	    this.args = args;
	    parse();
    }

    private void parse() throws ParseException {
        if (schema.length() == 0 && args.length == 0) {
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
        booleanArgs.put(elementId, marshaler);
        marshalers.put(elementId, marshaler);
    }
    
    private void parseStringSchemaElement(char elementId) {
        StringMarshaler marshaler = new StringMarshaler();
        stringArgs.put(elementId, marshaler);
        marshalers.put(elementId, marshaler);
    }

    private void parseIntegerSchemaElement(char elementId) {
        IntegerMarshaler marshaler = new IntegerMarshaler();
        intArgs.put(elementId, marshaler);
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
        for (currentArgument = 0; currentArgument < args.length; currentArgument++) {
            String arg = args[currentArgument];
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
        boolean set = true;
        if (isBooleanArg(argChar)) {
            setBooleanArg(argChar, true);
        } else if (isStringArg(argChar)) {
            setStringArg(argChar, "");
        } else if (isIntArg(argChar)) {
            setIntArg(argChar);
        } else {
            set = false;
        }
        
        return set;
    }

    private boolean isBooleanArg(char argChar) {
        return marshalers.get(argChar) instanceof BooleanMarshaler;
    }

    private boolean isStringArg(char argChar) {
        return marshalers.get(argChar) instanceof StringMarshaler;
    }
    
    private boolean isIntArg(char argChar) {
        return marshalers.get(argChar) instanceof IntegerMarshaler;
    }
    
    private void setBooleanArg(char argChar, boolean value) {
        booleanArgs.get(argChar).set("true");
    }
    
    private void setStringArg(char argChar, String s) throws ArgsException {
        currentArgument++;
        try {
            stringArgs.get(argChar).set(args[currentArgument]);
        } catch (ArrayIndexOutOfBoundsException e) {
            this.valid = false;
            this.errorArgumentId = argChar;
            this.errorCode  = ErrorCode.MISSING_STRING;
            throw new ArgsException();
        }
    }
    
    private void setIntArg(char argChar) throws ArgsException {
        currentArgument++;
        String parameter = null;
        try {
            parameter = args[currentArgument];
            intArgs.get(argChar).set(parameter);
        } catch (ArrayIndexOutOfBoundsException e) {
            this.valid = false;
            this.errorArgumentId = argChar;
            this.errorCode = ErrorCode.MISSING_INTEGER;
            throw new ArgsException();
        } catch (ArgsException e) {
            this.valid = false;
            this.errorArgumentId = argChar;
            this.errorParamenter = parameter;
            this.errorCode = ErrorCode.INVALID_INTEGER;
            throw e;
        }
    }
    
    public int cardinality() {
        return argsFound.size();
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
        return (Boolean) booleanArgs.get(arg).get();
    }

    public String getString(char arg) {
        return (String) stringArgs.get(arg).get();
    }

    public int getInt(char arg) {
        return intArgs.get(arg).get();
    }
    
    public boolean has(char arg) {
        return argsFound.contains(arg);
    }
    
    public boolean isValid() {
        return valid;
    }
    
    private abstract class ArgumentMarshaler {
        public abstract void set(String value) throws ArgsException;
        public abstract Object get();
    }
    
    private class BooleanMarshaler extends ArgumentMarshaler {
        private boolean booleanValue = false;
        
        @Override
        public void set(String value) {
            booleanValue = true;
        }
        
        @Override
        public Boolean get() {
            return booleanValue;
        }
    }
    
    private class StringMarshaler extends ArgumentMarshaler {
        private String stringValue = "";
        
        @Override
        public void set(String value) {
            stringValue = value;
        }
        
        @Override
        public String get() {
            return stringValue;
        }
    }
    
    private class IntegerMarshaler extends ArgumentMarshaler {
        private int intValue = 0;
        
        @Override
        public void set(String value) throws ArgsException {
            try {
                intValue = new Integer(value);
            } catch (NumberFormatException e) {
                throw new ArgsException();
            }
        }
        
        @Override
        public Integer get() {
            return intValue;
        }
    }
}