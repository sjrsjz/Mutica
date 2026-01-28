let {
    String::(String: any) &
    println::(println: any) &
    nat_to_string::(nat_to_string: any)
} = import "lib/string.mu";

let Cat: any = (name: String, age: nat) => {
    Name::name & Age::age
};

let my_cat: any = Cat("Whiskers", 3);

discard println(my_cat.`Name);
discard println("Age: " + nat_to_string(my_cat.`Age));

discard println! `ABC;