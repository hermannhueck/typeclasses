package jtypeclasses.printable;

import jtypeclasses.printable.domain.Cat;
import jtypeclasses.printable.lib.Printable;

import java.util.Optional;
import java.util.function.Function;

public class Main {

    private static void println(String str) {
        System.out.println(str);
    }

    private static Printable<Integer> localIntPrintable = new Printable<Integer>() {
        @Override
        public String stringify(Integer value) {
            return "How many cats? " + value.toString();
        }
    };

    public static void main(String[] args) {

        println("-----");

        Printable.pprint(2, localIntPrintable);
        Printable.pprint(18, Printable.instances.intPrintable);
        Printable.pprint(17.33, Printable.instances.doublePrintable);
        Printable.pprint("a String", Printable.instances.stringPrintable);

        Cat mizzi = new Cat("Mizzi", 1, "black");
        Cat garfield = new Cat("Garfield", 38, "ginger and black");

        println(Printable.stringify(mizzi, Cat.catPrintable));

        Printable.pprint(garfield, Cat.catPrintable);

        Printable<Optional<Cat>> optCatPrintable = optCat -> Printable.instances.optionalPrintable(Cat.catPrintable).apply(optCat);

        Optional<Cat> maybeCat = Optional.of(garfield);
        Printable.pprint(maybeCat, optCatPrintable);

        Optional<Cat> maybeNoCat = Optional.empty();
        Printable.pprint(maybeNoCat, optCatPrintable);

        println("-----");
    }
}
