match (1, 1)
    | [rec list:(() | (x: any ~ list))] => x
    | panic