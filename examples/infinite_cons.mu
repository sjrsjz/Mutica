let infinite_cons: any = rec tail: (!~tail);
let tuple: any = (1, 2, 3);
println!(infinite_cons);
println!(infinite_cons is tuple);
println!(tuple is infinite_cons);