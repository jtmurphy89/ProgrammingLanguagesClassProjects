type Option['A] = Some 'A | None

let polyMap = ['A, 'B](f: ('A) => 'B, o: Option<'A>) =>
     case o of
     | None => Option!None<'B>
     | Some(a) => Option!Some<'B>(f(a))
in polyMap<num, num>((n: num) => n + 1, Option!Some<num>(2))

