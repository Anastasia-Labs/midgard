<p align="center">
  <img width="150px" src="documentation/public/midgard-icon-green.png" align="center" alt="Midgard Logo"/>
  <h1 align="center">Midgard</h1>
  <p align="center">Cardano's first optimistic rollup protocol</p>
</p>

## Build the onchain code

Install [Aiken](https://aiken-lang.org/) and run 

``` 
aiken build
```

## Technical Specification

Run 
```
make spec
```

 and open `technical-spec/midgard.pdf`.

## Contributor guidelines

All contributors must enable the project's standardized git hooks:

```
make enable-git-hooks
```

Take a look at the [contribution guidelines](./CONTRIBUTING.md) for more details.

## Documentation / Architectural Decision Records (ADRs)

To run the documentation site locally:

1. Navigate to documentation

```
cd documentation
```

2. Install dependencies

```
pnpm install
```

3. Start development server

```
pnpm dev
```

> Local development server will be available at http://localhost:3000, you can read more about how to add new documentation [here](https://github.com/Anastasia-Labs/midgard/blob/main/documentation/README.md).
