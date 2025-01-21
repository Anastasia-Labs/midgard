# Midgard Transaction Generator

A tool for generating test transactions for the Midgard L2 network. This tx generator is based on the [Hydra Transaction Generator](https://github.com/Anastasia-Labs/hydra-deployment-poc) with modifications/tweaks to support the Midgard L2 MVP.

Currently, the generator creates Cardano L1 transactions using Lucid Evolution and wraps them in a Midgard L2 envelope. The actual transaction CBOR is still in Cardano L1 format.

To generate true Midgard L2 transactions (as per codec.cddl specification), we need:

1. Midgard Provider implementation in Lucid Evolution
2. Transaction builder modifications to use Midgard format
3. Update serialization to output proper Midgard CBOR

## Structure

```
src/
  ├── core/         # Core types and utilities
  │   ├── types.ts  # Includes target Midgard format from codec.cddl
  │   └── utils.ts  # Helper functions
  ├── generators/   # Transaction generators
  └── tests/        # Test files
```

## Setup

```bash
cd offchain/midgard-tx-generator
```

```bash
pnpm install
```

## Testing

Run the test suite and generate test outputs:

```bash
pnpm test
```

Generated test outputs will be saved in the `offchain/midgard-tx-generator/test-output` directory.

## Output Format

Both transaction generators output in the Midgard L2 format:

```typescript
interface SerializedMidgardTransaction {
  type: 'Midgard L2 User Transaction';
  description: string; // Transaction type specific description
  cborHex: string; // CBOR serialized transaction
  txId: string; // tx hash
}
```

## TX Generators

### One-to-One Transactions

- Simple single-input, single-output transfers
- Basic transaction format for testing Midgard node functionality

### Multi-Output Transactions

- Multiple outputs per transaction
- Distributes funds across multiple addresses

## Customization

To modify the number of transactions generated in tests, update both fields in test examples:

```typescript
const result = await generateMockUserTransactions({
  // ... other config
  txsCount: 100, // change this number
});

expect(txs).toHaveLength(100); // change to match txsCount
```

## Info

- MVP implementation focuses on basic transaction capabilities
- Uses simplified transaction format (no staking, DReps)
- Outputs CBOR-serialized transactions for midgard-node testing
