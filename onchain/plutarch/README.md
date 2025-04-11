# Midgard Merkle Proofs

This library contains all the utilities required for the construction and onchain verification of the merkle proofs required to interact with 
Midgard fraud proof system.  

## Features

- TODO
- TODO

## How It Works

1. 

### Creating the Merkle Patricia Forestry for a State Commitment

Here's an example using the `@aiken-lang/merkle-patricia-forestry` library:

```javascript
import { Store, Trie } from '@aiken-lang/merkle-patricia-forestry';
const rawData = [
    { key: '0x049b3472ebe63bff0f092d7e3c464169829c4369', value: '168285894526485760' },
    { key: '0x4b4dc012894fa59917b1155f348e639059ed6238', value: '204169096277495776' },
    // ... more key-value pairs ...
    ];
console.log("Building Merkle Patricia Trie...");
const data = rawData.map(item => ({
    key: Buffer.from(item.key.slice(2), 'hex'),
    value: item.value
    }));
const trie = await Trie.fromList(data);
```

### Exporting plutarch funtions into files as staking validators

1. Import your Plutarch functions alongside the corresponding staking wrapper based on the arity of your function into `Main.hs`. Staking wrappers support functions with up to four arguments.

```haskell
module Main where

import MyModule (scriptExample)
import Plutarch.Prelude ((#))
import Utils (stakingWrapper2)
```

2. Add your function, wrapped into staking wrapper, alongiside with it's name into scripts variable:

```haskell
scripts :: [(String, ClosedTerm a)]
scripts = ["scriptExample", stakingWrapper2 # scriptExample]
```

3. Run the app:

```
cabal run midgard
```

4. Your compiled scripts will be in the compiled folder.

# Set up nix config 
Put the following lines in your nix configuration file (usually located at /etc/nix/nix.conf)

extra-experimental-features = nix-command flakes ca-derivations
extra-trusted-substituters = https://cache.iog.io https://cache.nixos.org/ https://public-plutonomicon.cachix.org https://cache.zw3rk
extra-trusted-public-keys = cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=

# Installation 
After setting up nix config, restart your computer or VM. 
Then run:
    nix develop 

# License
See the [LICENSE](LICENSE) file for license rights and limitations (MIT).