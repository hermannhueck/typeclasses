package jtypeclasses.printable.domain;

import jtypeclasses.printable.lib.Printable;

public class Cat {

    public final String name;
    public final Integer age;
    public final String color;

    public Cat(String name, Integer age, String color) {
        this.name = name;
        this.age = age;
        this.color = color;
    }

    @Override
    public String toString() {
        return "Cat{" +
                "name='" + name + '\'' +
                ", age=" + age +
                ", color='" + color + '\'' +
                '}';
    }


    public static Printable<Cat> catPrintable = new Printable<Cat>() {
        @Override
        public String format(Cat cat) {
            return cat.name + " is a " + cat.age + " year-old " + cat.color + " cat.";
        }
    };
}
