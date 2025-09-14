# Donkey Kong (Português)

Projeto desenvolvido em grupo na UC de LI1 no primeiro ano da Licenciatura em Engenharia Informática.

## Executável

Pode compilar e executar o programa através dos comandos `build` e `run` do `cabal`.

```bash
cabal run primate-kong
```

## Interpretador

Pode abrir o interpretador do Haskell (GHCi) utilizando o cabal com o projecto automaticamente carregado.

```bash
cabal repl
```

## Testes

O projecto utiliza a biblioteca [HUnit](https://hackage.haskell.org/package/HUnit) para fazer testes unitários.

Pode correr os testes utilizando o seguinte comando.

```bash
cabal test
```

Se pretender executar os exemplos da documentação como testes unitários utiliza-se a biblioteca [Doctest](https://hackage.haskell.org/package/doctest).

```bash
cabal repl --build-depends=QuickCheck,doctest --with-ghc=doctest
```

## Documentação

Pode gerar a documentação com o [Haddock](https://haskell-haddock.readthedocs.io/).

```bash
cabal haddock
```


# Donkey Kong (English)

Group project developed for the LI course.

## Executable

You can compile and run the program using the `build` and `run` commands from `cabal`.

```bash
cabal run primate-kong
```

## Interpreter

You can open the Haskell interpreter (GHCi) using cabal with the project automatically loaded.

```bash
cabal repl
```

## Tests

The project uses the [HUnit](https://hackage.haskell.org/package/HUnit) library for unit testing.

You can run the tests using the following command.

```bash

cabal test
```

If you want to run the examples from the documentation as unit tests, use the [Doctest](https://hackage.haskell.org/package/doctest) library.

```bash
cabal repl --build-depends=QuickCheck,doctest --with-ghc=doctest
```

## Documentation
You can generate the documentation with [Haddock](https://haskell-haddock.readthedocs.io/).

```bash
cabal haddock
```