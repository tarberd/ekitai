# 液体 (えきたい)

液体, romanized *ekitai*, means liquid. It is built from two parts: 液 (えき, eki) meaning fluid; and 体 (たい, tai) meaning body.

Ekitai is a small programming language with refinement types that compiles to binary code using the llvm ecosystem.

## Examples

```
// withdraws checks if amount is <= than balance and returns the new balance.
// this is all statically checked by the type checker.
fn withdraw(
  balance: {x:i64| true},
  amount: {y: i64 | y <= x},
) -> {new_balance:i64| new_balance == balance-amount} {
//...
}

withdraw(50, 100) // compiler statically verifies this line to be incorrect
withdraw(100, 50) // compiler statically verifies this line to be correct
```

