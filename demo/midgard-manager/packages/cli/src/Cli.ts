import * as Command from '@effect/cli/Command';

import { interactiveCommand } from './cli/interactive/index.js';
import { generateTxCommand, stopTxCommand, txStatusCommand } from './commands/generate-tx.js';
import { configureNodeCommand, nodeStatusCommand } from './commands/node.js';

/**
 * Main CLI help text
 */
const helpText = `Midgard Manager - CLI tool for Cardano development

CONFIGURATION:

Configuration is stored in the project's config directory:
- Main settings: config/settings.json

Example configuration structure:
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

EXAMPLES:

# Interactive mode (recommended)
$ pnpm start interactive

# Transaction Generator
$ pnpm tx-generator start --test-wallet --type mixed --batch-size 10 --interval 5 --concurrency 1
$ pnpm tx-generator status

# Node Operations
$ pnpm start node-status
$ pnpm start configure-node --interactive

For more detailed transaction generator options:
$ pnpm tx-generator --help`;

/**
 * Group transaction commands
 */
const txCommands = Command.make('tx')
  .pipe(Command.withDescription('Transaction generator operations'))
  .pipe(Command.withSubcommands([generateTxCommand, stopTxCommand, txStatusCommand]));

/**
 * Group node commands
 */
const nodeCommands = Command.make('node')
  .pipe(Command.withDescription('Midgard node operations'))
  .pipe(Command.withSubcommands([nodeStatusCommand, configureNodeCommand]));

/**
 * Create the main command with subcommands
 */
const mainCommand = Command.make('midgard-manager')
  .pipe(Command.withDescription(helpText))
  .pipe(
    Command.withSubcommands([
      // Interactive mode
      interactiveCommand,

      // Command groups
      nodeCommands,
      txCommands,
    ])
  );

/**
 * Export the run function
 */
export const run = Command.run(mainCommand, {
  name: 'Midgard Manager',
  version: '0.1.0',
});
