package jtypeclasses.printable.lib;

public interface Printable<A> {

    String format(A value);


    static <A> String format(A value, Printable<A> printable) {
        return printable.format(value);
    }

    static <A> void print(A value, Printable<A> printable) {
        System.out.println(format(value, printable));
    }

    interface instances {
        static Printable<Integer> intPrintable = Object::toString;
        static Printable<Double> doublePrintable = Object::toString;
        static Printable<String> stringPrintable = value -> value;
    }
}
