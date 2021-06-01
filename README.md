# 液体 (えきたい)

液体, romanized *ekitai*, means liquid. It is built from two parts: 液 (えき, eki) meaning fluid; and 体 (たい, tai) meaning body.

Ekitai is a small programming language with refinement types that compiles to binary code using the llvm ecosystem.

a grammar is everything from tokens to syntax to semantics and its analysis

the lexer crate holds the token definitions and the lexical analyser. It's job is to output a stream of tokens wich have the token_kind (token name) lexeme and span;

the parser crate holds the syntax analyser. It will output an event stream that can than be used to build a sintax tree