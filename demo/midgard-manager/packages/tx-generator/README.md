# Midgard Transaction Generator

Generate test transactions for Midgard L2. This package provides tools for generating and submitting different types of transactions for testing and development purposes.

## Quick Start

```bash
# Install dependencies
pnpm install

# Build the package
pnpm build

# Generate transactions with test wallet
pnpm tx-generator start --test-wallet --type one-to-one --batch-size 10
```

## Transaction Types

### One-to-One Transactions

Simple transactions with one input and one output.

```bash
pnpm tx-generator start --test-wallet --type one-to-one --batch-size 10 --concurrency 1
```

### Multi-Output Transactions

Complex transactions with one input and multiple outputs (1-to-20).

```bash
pnpm tx-generator start --test-wallet --type multi-output --batch-size 5 --concurrency 1
```

### Mixed Transactions

Combination of both types with configurable ratio:

```bash
pnpm tx-generator start --test-wallet --type mixed --ratio 70 --batch-size 10 --concurrency 5
```

## Operating Modes

### Online Mode (Node Available)

When the node is available, transactions are submitted directly:

- Transactions are sent to the configured node endpoint
- Configurable retry attempts and delays

### Offline Mode (Node Unavailable)

When the node is unavailable, transactions are stored in files:

- Files are saved in `generated-transactions` directory
- Timestamp-based filenames (e.g., `one-to-one-2025-02-28T03-35-48-464Z.json`)
- Important concurrency considerations (see below)

## CLI Options

| Option                       | Description                                      | Default                    |
| ---------------------------- | ------------------------------------------------ | -------------------------- |
| `-e, --endpoint <url>`       | Node endpoint URL                                | http://localhost:3000      |
| `-t, --type <type>`          | Transaction type (one-to-one/multi-output/mixed) | mixed                      |
| `-r, --ratio <number>`       | % of one-to-one txs in mixed mode                | 70                         |
| `-b, --batch-size <number>`  | Transactions per batch                           | 10                         |
| `-i, --interval <seconds>`   | Time between batches                             | 5                          |
| `-c, --concurrency <number>` | Parallel transaction batches                     | 5                          |
| `--test-wallet`              | Generate a test wallet                           | false                      |
| `-k, --private-key <key>`    | Wallet private key                               | required if no test wallet |

## Important Behaviors

### Concurrency and File Generation

When running with concurrency > 1:

- Multiple batches may generate files at the same millisecond
- Timestamp collisions can result in fewer files than expected
- Use `-c 1` for exact file counts
- Higher concurrency is better for throughput testing

### Transaction Uniqueness

- Each transaction has a unique datum
- Datums combine timestamp, random value, and counter
- No duplicate transactions even with high concurrency

## Project Structure

```
midgard-tx-generator/
├── src/
│   ├── bin/           # CLI implementation
│   ├── lib/
│   │   ├── client/    # Node communication
│   │   ├── generators/# TX generators
│   │   └── scheduler/ # Periodic generation
│   └── utils/         # Utility functions
```

## Configuration

The transaction generator primarily uses CLI arguments for configuration. The `settings.json` file provides some defaults that can be overridden by CLI arguments:

```json
{
  "node": {
    "endpoint": "http://localhost:3000"
  },
  "generator": {
    "enabled": false,
    "batchSize": 10,
    "intervalMs": 1000
  }
}
```

### Configuration Options

| Option                 | Description                      | Default               |
| ---------------------- | -------------------------------- | --------------------- |
| `node.endpoint`        | Node endpoint URL                | http://localhost:3000 |
| `generator.enabled`    | Enable/disable generator         | false                 |
| `generator.batchSize`  | Number of transactions per batch | 10                    |
| `generator.intervalMs` | Interval between batches (ms)    | 1000                  |

Note:

- CLI arguments take precedence over configuration file settings
- Concurrency is controlled by the CLI argument `-c, --concurrency` (default: 5, max: 20)

## Output Format

Each transaction follows this structure:

```typescript
{
  cborHex: string; // CBOR-encoded transaction
  description: string; // Transaction description
  txId: string; // Transaction hash
  type: string; // Transaction type
}
```
