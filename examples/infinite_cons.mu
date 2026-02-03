let infinite_cons: any = rec tail: (!~tail);
let tuple: any = (1, 2, 3);
discard println!(infinite_cons);
discard println!(infinite_cons is tuple);
discard println!(tuple is infinite_cons);