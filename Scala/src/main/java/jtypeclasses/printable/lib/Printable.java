package jtypeclasses.printable.lib;

import java.util.Optional;
import java.util.function.Function;

public interface Printable<A> {

    String stringify(A value);


    static <A> String stringify(A value, Printable<A> printable) {
        return printable.stringify(value);
    }

    static <A> void pprint(A value, Printable<A> printable) {
        System.out.println(stringify(value, printable));
    }

    interface instances {

        Printable<String> stringPrintable = value -> value;
        Printable<Integer> intPrintable = Object::toString;
        Printable<Double> doublePrintable = Object::toString;

        static <X> Function<Optional<X>, String> optionalPrintable(Printable<X> printable) {
            return opt -> opt
                    .map(printable::stringify)
                    .map(s -> "Optional(" + s + ")")
                    .orElse("Optional.empty");
        }
    }
}
