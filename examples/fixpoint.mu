match (1, 1)
    | constraint [rec list:(() | (x: any ~ list))] => x
    | panic