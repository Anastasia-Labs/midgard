# Midgard Manager

Tools for managing Midgard MVP L2 node and generating test transactions.

## Structure

- **CLI** (`packages/cli`): Command-line interface for Midgard nodes
- **TX Generator** (`packages/tx-generator`): Transaction generator for testing

### Interactive Mode

```bash
pnpm start interactive
```

### Direct CLI Commands

```bash
# Show help
pnpm start --help

# Transaction Generator Examples

# For exact number of transactions
pnpm tx-generator start --test-wallet --type one-to-one --batch-size 100

# For higher throughput testing
pnpm tx-generator start --test-wallet --type mixed --batch-size 100 --interval 5 --concurrency 5
```

## Configuration

The CLI uses a centralized configuration system that can be modified through:

- Interactive mode menus
- CLI commands
- Manual edit of `config/settings.json`

Example configuration:

```json
{
  "node": {
    "endpoint": "http://localhost:3000"
  },
  "generator": {
    "enabled": false,
    "maxConcurrent": 10,
    "batchSize": 100,
    "intervalMs": 1000
  },
  "logging": {
    "level": "info",
    "format": "pretty"
  }
}
```

For detailed transaction generator options and behaviors, see the tx-generator package README.
