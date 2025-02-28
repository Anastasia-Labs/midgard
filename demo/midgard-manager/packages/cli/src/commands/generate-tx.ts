import { Command, Options } from '@effect/cli';
import { getGeneratorStatus, startGenerator, stopGenerator } from '@midgard-manager/tx-generator';
import chalk from 'chalk';
import { Effect, pipe } from 'effect';
import fs from 'fs/promises';
import ora from 'ora-classic';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

import { getWallet } from '../config/wallets.js';
import { listWallets } from '../config/wallets.js';
import { displayStatus } from '../utils/display.js';

// Transaction types supported by the generator
const transactionTypes = ['one-to-one', 'multi-output', 'mixed'];

/**
 * Command to generate transactions
 * Usage: midgard-manager generate-tx [options]
 */
export const generateTxCommand = Command.make(
  'generate-tx',
  {
    // Basic options
    type: Options.choice('type', transactionTypes)
      .pipe(Options.withDescription('Type of transactions to generate'))
      .pipe(Options.withDefault('mixed')),

    batchSize: Options.integer('batch-size')
      .pipe(Options.withDescription('Number of transactions per batch'))
      .pipe(Options.withDefault(10)),

    interval: Options.integer('interval')
      .pipe(Options.withDescription('Interval between batches in seconds'))
      .pipe(Options.withDefault(5)),

    wallet: Options.text('wallet')
      .pipe(Options.withDescription('Wallet to use for transaction signing'))
      .pipe(Options.withDefault('test')),

    // Mixed transaction options
    oneToOneRatio: Options.integer('one-to-one-ratio')
      .pipe(
        Options.withDescription(
          "For 'mixed' type only: Percentage of one-to-one transactions (0-100)"
        )
      )
      .pipe(Options.withDefault(70)),

    // Advanced options
    concurrency: Options.integer('concurrency')
      .pipe(Options.withDescription('Number of transaction batches to process simultaneously'))
      .pipe(Options.withDefault(5)),

    nodeEndpoint: Options.text('node-endpoint')
      .pipe(Options.withDescription('Midgard node endpoint URL'))
      .pipe(Options.withDefault('http://localhost:3000')),

    // New option for interactive mode
    interactive: Options.boolean('interactive')
      .pipe(Options.withDescription('Use interactive mode to configure options'))
      .pipe(Options.withDefault(false)),
  },
  ({
    type,
    batchSize,
    interval,
    wallet,
    oneToOneRatio,
    concurrency,
    nodeEndpoint,
    interactive,
  }) => {
    return pipe(
      Effect.tryPromise(async () => {
        try {
          // Config setup
          const config = {
            transactionType: type as 'one-to-one' | 'multi-output' | 'mixed',
            oneToOneRatio,
            batchSize,
            interval,
            concurrency,
            nodeEndpoint,
            wallet,
          };

          // If interactive mode is requested, prompt for all options
          if (interactive) {
            try {
              const { confirm, select, number, input } = await import('@inquirer/prompts');

              // Get list of wallets for selection
              const wallets = await listWallets();
              if (wallets.length === 0) {
                console.error(chalk.red('❌ No wallets configured. Please add a wallet first.'));
                console.log(chalk.gray('Run: midgard-manager wallet add <n> --private-key <key>'));
                return;
              }

              // Interactive configuration
              const spinner = ora('Preparing interactive setup...').start();
              spinner.succeed('Interactive setup ready');

              console.log(chalk.blue.bold('\n📝 Transaction Generator Configuration\n'));
              console.log(
                chalk.dim(
                  'Configure the transaction generator to simulate different transaction patterns.\n'
                )
              );

              // Section divider for better visual organization
              console.log(chalk.yellow.bold('▶ Basic Configuration'));

              // Type selection with explanations
              config.transactionType = (await select({
                message: 'Select transaction type:',
                choices: [
                  {
                    value: 'one-to-one',
                    name: 'One-to-one (single output)',
                    description:
                      'Simple transactions with a single sender and receiver. Good for basic testing.',
                  },
                  {
                    value: 'multi-output',
                    name: 'Multi-output (multiple recipients)',
                    description:
                      'Complex transactions that send to multiple recipients at once. Tests more advanced transaction handling.',
                  },
                  {
                    value: 'mixed',
                    name: 'Mixed (combination of both types)',
                    description:
                      'Realistic mix of simple and complex transactions. Best for simulating real-world usage.',
                  },
                ],
                default: config.transactionType,
              })) as 'one-to-one' | 'multi-output' | 'mixed';

              // Only show ratio option if mixed type is selected
              if (config.transactionType === 'mixed') {
                const ratioResult = await number({
                  message: 'Percentage of one-to-one transactions (0-100):',
                  default: config.oneToOneRatio,
                  validate: (value) =>
                    value !== undefined && value >= 0 && value <= 100
                      ? true
                      : 'Please enter a value between 0 and 100',
                });

                config.oneToOneRatio = ratioResult ?? config.oneToOneRatio;

                console.log(
                  chalk.dim(
                    `  ${chalk.cyan('ℹ')} This means ${config.oneToOneRatio}% simple and ${
                      100 - config.oneToOneRatio
                    }% complex transactions will be generated`
                  )
                );
              }

              // Batch size with explanation
              const batchSizeResult = await number({
                message: 'Number of transactions per batch:',
                default: config.batchSize,
                validate: (value) =>
                  value !== undefined && value > 0 ? true : 'Batch size must be at least 1',
              });

              config.batchSize = batchSizeResult ?? config.batchSize;

              console.log(
                chalk.dim(
                  `  ${chalk.cyan('ℹ')} Smaller batches use less memory but may process slower`
                )
              );

              // Interval with explanation
              const intervalResult = await number({
                message: 'Interval between batches (seconds):',
                default: config.interval,
                validate: (value) =>
                  value !== undefined && value >= 0 ? true : 'Interval must be non-negative',
              });

              config.interval = intervalResult ?? config.interval;

              console.log(
                chalk.dim(
                  `  ${chalk.cyan(
                    'ℹ'
                  )} Shorter intervals generate transactions faster but may increase system load`
                )
              );

              // Advanced options section
              console.log(chalk.yellow.bold('\n▶ Advanced Configuration'));

              // Ask if user wants to configure advanced options
              const configureAdvanced = await confirm({
                message: 'Do you want to configure advanced options?',
                default: false,
              });

              if (configureAdvanced) {
                // Concurrency with explanation
                const concurrencyResult = await number({
                  message: 'Number of transaction batches to process simultaneously:',
                  default: config.concurrency,
                  validate: (value) =>
                    value !== undefined && value > 0 && value <= 20
                      ? true
                      : 'Concurrency must be between 1 and 20',
                });

                config.concurrency = concurrencyResult ?? config.concurrency;

                console.log(
                  chalk.dim(
                    `  ${chalk.cyan(
                      'ℹ'
                    )} Higher concurrency increases throughput but requires more system resources`
                  )
                );

                // Node endpoint with explanation
                config.nodeEndpoint = await input({
                  message: 'Node endpoint URL:',
                  default: config.nodeEndpoint,
                  validate: (value) =>
                    value.startsWith('http') ? true : 'URL must begin with http:// or https://',
                });

                console.log(
                  chalk.dim(
                    `  ${chalk.cyan('ℹ')} The endpoint where transactions will be submitted`
                  )
                );
              }

              // Wallet selection section
              console.log(chalk.yellow.bold('\n▶ Wallet Configuration'));

              // Wallet selection
              config.wallet = await select({
                message: 'Select a wallet for signing transactions:',
                choices: wallets.map((w) => ({
                  value: w,
                  name: w,
                })),
                default: config.wallet,
              });

              // Summary display of configuration before starting
              console.log(chalk.yellow.bold('\n▶ Configuration Summary'));

              const summaryTable = [
                ['Setting', 'Value', 'Description'],
                ['Type', config.transactionType, getTypeDescription(config.transactionType)],
                config.transactionType === 'mixed'
                  ? [
                      'Ratio',
                      `${config.oneToOneRatio}% / ${100 - config.oneToOneRatio}%`,
                      'One-to-one / Multi-output split',
                    ]
                  : ['', '', ''],
                ['Batch Size', config.batchSize.toString(), 'Transactions per batch'],
                ['Interval', `${config.interval} seconds`, 'Time between batches'],
                ['Concurrency', config.concurrency.toString(), 'Simultaneous batches'],
                ['Wallet', config.wallet, 'For transaction signing'],
                ['Node', config.nodeEndpoint, 'Submission endpoint'],
              ].filter((row) => row[0] !== '');

              // Display formatted summary
              summaryTable.forEach((row, index) => {
                if (index === 0) {
                  console.log(chalk.cyan(`  ${row[0].padEnd(12)}${row[1].padEnd(18)}${row[2]}`));
                  console.log(chalk.dim('  ' + '─'.repeat(60)));
                } else if (row[0] !== '') {
                  console.log(
                    `  ${chalk.bold(row[0].padEnd(12))}${chalk.green(
                      row[1].padEnd(18)
                    )}${chalk.dim(row[2])}`
                  );
                }
              });

              // Final confirmation
              const confirm_start = await confirm({
                message: 'Start transaction generator with these settings?',
                default: true,
              });

              if (!confirm_start) {
                console.log(chalk.yellow('Transaction generator setup cancelled.'));
                return;
              }

              console.log(chalk.green('\nStarting transaction generator...'));
            } catch (error) {
              console.error(
                chalk.red(
                  `Error setting up interactive mode: ${
                    error instanceof Error ? error.message : String(error)
                  }`
                )
              );
              return;
            }
          } else {
            // Non-interactive validation
            // Validate the one-to-one ratio
            if (oneToOneRatio < 0 || oneToOneRatio > 100) {
              console.error(chalk.red('❌ one-to-one-ratio must be between 0 and 100'));
              return;
            }
          }

          // Get wallet configuration
          const walletConfig = await getWallet(config.wallet);
          if (!walletConfig) {
            console.error(chalk.red(`❌ Wallet '${config.wallet}' not found`));
            return;
          }

          // Start the generator with wallet configuration
          const spinner = ora('Starting transaction generator...').start();

          await startGenerator({
            transactionType: config.transactionType,
            oneToOneRatio: config.oneToOneRatio,
            batchSize: config.batchSize,
            interval: config.interval,
            concurrency: config.concurrency,
            nodeEndpoint: config.nodeEndpoint,
            walletPrivateKey: walletConfig.privateKey,
            walletAddress: walletConfig.address || '', // Pass the wallet address
          });

          spinner.succeed('Transaction generator started successfully');

          // Show configuration in a nice format
          console.log(chalk.bold('\n✓ Transaction generator is running with configuration:'));

          console.log(
            `• Type: ${config.transactionType}${
              config.transactionType === 'mixed'
                ? ` (${config.oneToOneRatio}% one-to-one, ${
                    100 - config.oneToOneRatio
                  }% multi-output)`
                : ''
            }`
          );
          console.log(`• Batch Size: ${config.batchSize} transactions`);
          console.log(`• Interval: ${config.interval} seconds`);
          console.log(`• Concurrency: ${config.concurrency} simultaneous batches`);
          console.log(`• Wallet: ${config.wallet}`);
          console.log(`• Node Endpoint: ${config.nodeEndpoint}`);

          console.log(chalk.dim('\nStatus monitoring:'));
          console.log(chalk.dim('• View status: midgard-manager tx-status'));
          console.log(chalk.dim('• Stop generator: midgard-manager stop-tx'));
          console.log(
            chalk.dim('• Press Ctrl+C to stop this process (generator will continue running)')
          );

          // Keep process running and handle graceful shutdown
          process.on('SIGINT', async () => {
            console.log('\n');
            const stopSpinner = ora('Stopping transaction generator...').start();
            await stopGenerator();
            stopSpinner.succeed('Transaction generator stopped');
            process.exit(0);
          });
        } catch (error) {
          spinner.fail('Failed to start transaction generator');
          console.error(
            chalk.red(`Error: ${error instanceof Error ? error.message : String(error)}`)
          );

          if (error instanceof Error && error.message.includes('connection')) {
            console.log(chalk.yellow('\nTroubleshooting:'));
            console.log(chalk.gray('• Check that the Midgard node is running'));
            console.log(chalk.gray(`• Verify the node endpoint: ${config.nodeEndpoint}`));
            console.log(chalk.gray('• Check network connectivity'));
          }
        }
      })
    );
  }
).pipe(Command.withDescription('Generate and submit transactions to the Midgard node'));

/**
 * Command to stop the transaction generator
 * Usage: midgard-manager stop-tx
 */
export const stopTxCommand = Command.make('stop-tx', {}, () => {
  return pipe(
    Effect.tryPromise(async () => {
      const spinner = ora('Stopping transaction generator...').start();

      try {
        await stopGenerator();
        spinner.succeed('Transaction generator stopped');
      } catch (error) {
        spinner.fail('Failed to stop transaction generator');
        console.error(
          chalk.red(`Error: ${error instanceof Error ? error.message : String(error)}`)
        );
      }
    })
  );
}).pipe(Command.withDescription('Stop the running transaction generator'));

/**
 * Command to get the status of the transaction generator
 * Usage: midgard-manager tx-status
 */
export const txStatusCommand = Command.make('tx-status', {}, () => {
  return pipe(
    Effect.tryPromise(async () => {
      const spinner = ora('Fetching transaction generator status...').start();

      try {
        // Get the directory path relative to the monorepo
        const __filename = fileURLToPath(import.meta.url);
        const __dirname = dirname(__filename);
        const MONOREPO_ROOT = join(__dirname, '../../../../../..');
        const PROJECT_ROOT = join(MONOREPO_ROOT, 'demo/midgard-manager');

        // Load configurations from project's config directory
        const CONFIG_DIR = join(PROJECT_ROOT, 'config');
        const settingsPath = join(CONFIG_DIR, 'settings.json');
        const nodeConfigPath = join(CONFIG_DIR, 'node.json');

        // Load node config
        let nodeConfig = { endpoint: 'http://localhost:3000' };
        try {
          const nodeConfigExists = await fs
            .access(nodeConfigPath)
            .then(() => true)
            .catch(() => false);

          if (nodeConfigExists) {
            const nodeConfigData = await fs.readFile(nodeConfigPath, 'utf-8');
            nodeConfig = JSON.parse(nodeConfigData);
          }
        } catch (error) {
          console.warn(
            chalk.green.dim(
              `Warning: Could not load node config: ${
                error instanceof Error ? error.message : String(error)
              }`
            )
          );
        }

        // Load generator config from settings.json
        let generatorConfig = {
          enabled: true,
          maxConcurrent: 10,
          batchSize: 100,
          intervalMs: 1000,
        };
        try {
          const configExists = await fs
            .access(settingsPath)
            .then(() => true)
            .catch(() => false);

          if (configExists) {
            const configData = await fs.readFile(settingsPath, 'utf-8');
            const fullConfig = JSON.parse(configData);
            if (fullConfig.generator) {
              generatorConfig = fullConfig.generator;
            }
          }
        } catch (error) {
          console.warn(
            chalk.green.dim(
              `Warning: Could not load generator config: ${
                error instanceof Error ? error.message : String(error)
              }`
            )
          );
        }

        // Build the complete config
        const config = {
          node: nodeConfig,
          generator: generatorConfig,
        };

        spinner.succeed('Status retrieved');

        // Use our displayStatus function for consistent UI
        await displayStatus(config);

        console.log(chalk.bold('\nOptions:'));
        if (getGeneratorStatus().running) {
          console.log(chalk.dim('• Stop generator: midgard-manager stop-tx'));
        } else {
          console.log(chalk.dim('• Start generator: midgard-manager generate-tx'));
        }
        console.log(chalk.dim('• Refresh status: midgard-manager tx-status'));
      } catch (error) {
        spinner.fail('Failed to retrieve status');
        console.error(
          chalk.red(`Error: ${error instanceof Error ? error.message : String(error)}`)
        );
      }
    })
  );
}).pipe(Command.withDescription('Show the status of the transaction generator'));

/**
 * Helper function to get a description for each transaction type
 */
function getTypeDescription(type: string): string {
  switch (type) {
    case 'one-to-one':
      return 'Simple single-output transactions';
    case 'multi-output':
      return 'Complex multi-recipient transactions';
    case 'mixed':
      return 'Combination of simple and complex transactions';
    default:
      return 'Unknown transaction type';
  }
}
