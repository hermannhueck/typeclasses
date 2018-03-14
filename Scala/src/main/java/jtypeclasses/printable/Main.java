package jtypeclasses.printable;

import jtypeclasses.printable.domain.Cat;
import jtypeclasses.printable.lib.Printable;

public class Main {

    private static void println(String str) {
        System.out.println(str);
    }

    private static Printable<Integer> intPrintable = new Printable<Integer>() {
        @Override
        public String format(Integer value) {
            return "How many cats? " + value.toString();
        }
    };


    public static void main(String[] args) {

        println("-----");

        Printable.print(2, intPrintable);
        Printable.print(18, Printable.instances.intPrintable);
        Printable.print(17.33, Printable.instances.doublePrintable);
        Printable.print("a String", Printable.instances.stringPrintable);

        Cat mizzi = new Cat("Mizzi", 1, "black");
        Cat garfield = new Cat("Garfield", 38, "ginger and black");

        String mizziFormatted = Printable.format(mizzi, Cat.catPrintable);
        println(mizziFormatted);

        Printable.print(garfield, Cat.catPrintable);

        println("-----");
    }
}
