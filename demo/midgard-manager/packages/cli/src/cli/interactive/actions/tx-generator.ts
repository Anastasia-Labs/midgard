import { confirm, number, select } from '@inquirer/prompts';
import { generateEmulatorAccountFromPrivateKey } from '@lucid-evolution/lucid';
import { getGeneratorStatus, startGenerator, stopGenerator } from '@midgard-manager/tx-generator';
import chalk from 'chalk';
import { Effect } from 'effect';
import ora from 'ora-classic';

import { saveConfig } from '../../../config/index.js';
import { getWallet, listWallets } from '../../../config/wallets.js';
import { MidgardError } from '../../../utils/errors.js';
import type { Action } from '../types.js';

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

export const configureTxGenerator: Action = {
  name: 'Configure Transaction Generator',
  description: 'Configure and start transaction generator',
  execute: async (context) => {
    try {
      console.log(chalk.dim('Press Ctrl+C to cancel this operation and return to menu\n'));

      // Create a local abort controller
      const abortController = new AbortController();

      // Listen for the parent abort signal to propagate it
      const parentAbortHandler = () => {
        abortController.abort();
        throw new Error('AbortPromptError');
      };

      // Add a listener for SIGINT that will abort our local controller
      process.once('SIGINT', parentAbortHandler);

      try {
        console.log(chalk.bold.green('\n📝 Transaction Generator Configuration\n'));
        console.log(
          chalk.dim(
            'Configure the transaction generator to simulate different transaction patterns.\n'
          )
        );

        // Add parameter definition guide at the top
        console.log(chalk.bold('\n📋 Parameter Definitions:\n'));

        const parameterDefinitions = [
          {
            name: 'Transaction Type',
            description: 'Determines what kinds of transactions will be generated',
            options: [
              'One-to-one: Simple transactions with one recipient',
              'Multi-output: Complex transactions with multiple recipients',
              'Mixed: Combination of both simple and complex transactions',
            ],
          },
          {
            name: 'One-to-One Ratio',
            description: 'For mixed type only - percentage of simple vs. complex transactions',
          },
          {
            name: 'Batch Size',
            description: 'Number of transactions to generate in each batch',
          },
          {
            name: 'Interval',
            description: 'Time to wait between transaction batches (in seconds)',
          },
          {
            name: 'Concurrency',
            description: 'Number of batches that can run in parallel',
          },
          {
            name: 'Node Endpoint',
            description: 'The URL where transactions will be submitted',
          },
          {
            name: 'Wallet',
            description: 'The wallet that will sign and pay for transactions',
          },
        ];

        // Format and display parameter definitions
        parameterDefinitions.forEach((param) => {
          console.log(chalk.bold(`• ${param.name}:`));
          console.log(`  ${chalk.white(param.description)}`);
          if (param.options) {
            param.options.forEach((option) => {
              console.log(`  ${chalk.dim('- ' + option)}`);
            });
          }
          console.log(); // Empty line for spacing
        });

        console.log(chalk.dim('─'.repeat(70)));
        console.log(chalk.bold('\n🔍 Beginning Configuration Process:\n'));

        // Basic config to populate
        const txConfig = {
          transactionType: 'mixed' as 'one-to-one' | 'multi-output' | 'mixed',
          oneToOneRatio: 70,
          batchSize: context.config.generator.batchSize,
          interval: context.config.generator.intervalMs / 1000,
          concurrency: context.config.generator.maxConcurrent,
          nodeEndpoint: context.config.node.endpoint,
        };

        // Section divider for better visual organization
        console.log(chalk.bold('▶ Basic Configuration'));

        // Type selection with explanations
        txConfig.transactionType = (await select({
          message: 'Select transaction type:',
          choices: [
            {
              value: 'one-to-one',
              name: 'One-to-one (single output)',
              description:
                'Simple transactions with a single input and output. Each transaction transfers ADA between addresses.',
            },
            {
              value: 'multi-output',
              name: 'Multi-output (multiple recipients)',
              description:
                'Complex transactions with one input and multiple outputs (1-to-20). Followed by consolidation transactions that gather outputs back.',
            },
            {
              value: 'mixed',
              name: 'Mixed (combination of both types)',
              description:
                'Realistic mix of simple and complex transactions with configurable ratio.',
            },
          ],
          default: txConfig.transactionType,
        })) as 'one-to-one' | 'multi-output' | 'mixed';

        // Only show ratio option if mixed type is selected
        if (txConfig.transactionType === 'mixed') {
          const ratioResult = await number({
            message: 'Percentage of one-to-one transactions (0-100):',
            default: txConfig.oneToOneRatio,
            validate: (value) =>
              value !== undefined && value >= 0 && value <= 100
                ? true
                : 'Please enter a value between 0 and 100',
          });

          txConfig.oneToOneRatio = ratioResult ?? txConfig.oneToOneRatio;

          console.log(
            chalk.dim(
              `  ${chalk.green('ℹ')} This means ${txConfig.oneToOneRatio}% simple and ${
                100 - txConfig.oneToOneRatio
              }% complex transactions will be generated`
            )
          );
        }

        // Batch size without unnecessary explanation
        const batchSizeResult = await number({
          message: 'Number of transactions per batch:',
          default: txConfig.batchSize,
          validate: (value) =>
            value !== undefined && value > 0 ? true : 'Batch size must be at least 1',
        });

        txConfig.batchSize = batchSizeResult ?? txConfig.batchSize;

        // Interval without unnecessary explanation
        const intervalResult = await number({
          message: 'Interval between batches (seconds):',
          default: txConfig.interval,
          validate: (value) =>
            value !== undefined && value >= 0 ? true : 'Interval must be non-negative',
        });

        txConfig.interval = intervalResult ?? txConfig.interval;

        // Advanced options section
        console.log(chalk.bold('\n▶ Advanced Configuration'));

        // Ask if user wants to configure advanced options
        const configureAdvanced = await confirm({
          message: 'Do you want to configure advanced options?',
          default: false,
        });

        if (configureAdvanced) {
          // Concurrency with explanation
          const concurrencyResult = await number({
            message: 'Number of transaction batches to process simultaneously:',
            default: txConfig.concurrency,
            validate: (value) =>
              value !== undefined && value > 0 && value <= 20
                ? true
                : 'Concurrency must be between 1 and 20',
          });

          txConfig.concurrency = concurrencyResult ?? txConfig.concurrency;

          // Node endpoint with explanation
          txConfig.nodeEndpoint = await select({
            message: 'Node endpoint:',
            choices: [
              {
                value: 'http://localhost:3000',
                name: 'Local (http://localhost:3000)',
                description: 'Local Midgard node instance',
              },
              {
                value: context.config.node.endpoint,
                name: `Current (${context.config.node.endpoint})`,
                description: 'Currently configured endpoint',
              },
            ],
            default: txConfig.nodeEndpoint,
          });
        }

        // Generate test wallet
        console.log(chalk.bold('\n▶ Wallet Setup'));
        console.log(chalk.dim('Generating a test wallet for transaction signing...'));

        const spinner = ora('Setting up test wallet...').start();

        try {
          // Generate test wallet
          const account = await generateEmulatorAccountFromPrivateKey({});
          const initialUTxO = {
            txHash: Buffer.from(Array(32).fill(0)).toString('hex'),
            outputIndex: 0,
            address: account.address,
            assets: {
              lovelace: 10_000_000_000n, // 10,000 ADA for testing
            },
            datum: null,
            datumHash: null,
            scriptRef: null,
          };

          spinner.succeed('Test wallet generated successfully');
          console.log(chalk.dim(`Address: ${account.address}`));

          // Summary display
          console.log(chalk.bold('\n▶ Configuration Summary'));

          const summaryTable = [
            ['Setting', 'Value', 'Description'],
            ['Type', txConfig.transactionType, getTypeDescription(txConfig.transactionType)],
            txConfig.transactionType === 'mixed'
              ? [
                  'Ratio',
                  `${txConfig.oneToOneRatio}% / ${100 - txConfig.oneToOneRatio}%`,
                  'One-to-one / Multi-output split',
                ]
              : ['', '', ''],
            ['Batch Size', txConfig.batchSize.toString(), 'Transactions per batch'],
            ['Interval', `${txConfig.interval}s`, 'Time between batches'],
            ['Concurrency', txConfig.concurrency.toString(), 'Simultaneous batches'],
            ['Node', txConfig.nodeEndpoint, 'Submission endpoint'],
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
            return {
              success: true,
              message: 'Transaction generator setup cancelled.',
            };
          }

          spinner.text = 'Starting transaction generator...';
          spinner.start();

          try {
            // Start the generator
            await startGenerator({
              transactionType: txConfig.transactionType,
              oneToOneRatio: txConfig.oneToOneRatio,
              batchSize: txConfig.batchSize,
              interval: txConfig.interval,
              concurrency: txConfig.concurrency,
              nodeEndpoint: txConfig.nodeEndpoint,
              network: 'Preview',
              initialUTxO,
              walletSeedOrPrivateKey: account.privateKey,
              outputDir: 'generated-transactions',
            });

            spinner.succeed('Transaction generator started successfully');

            // Save the corresponding values to the main config
            const newConfig = {
              ...context.config,
              generator: {
                enabled: true,
                maxConcurrent: txConfig.concurrency,
                batchSize: txConfig.batchSize,
                intervalMs: txConfig.interval * 1000, // Convert to ms
              },
              node: {
                ...context.config.node,
                endpoint: txConfig.nodeEndpoint,
              },
            };

            await Effect.runPromise(saveConfig(newConfig));

            return {
              success: true,
              message: `Transaction generator started with type: ${txConfig.transactionType}, batch size: ${txConfig.batchSize}, interval: ${txConfig.interval}s`,
            };
          } catch (error) {
            spinner.fail('Failed to start transaction generator');
            throw new Error(`Failed to start transaction generator: ${error}`);
          }
        } catch (error) {
          spinner.fail('Failed to generate test wallet');
          throw new Error(`Failed to generate test wallet: ${error}`);
        }
      } finally {
        // Clean up our SIGINT handler
        process.off('SIGINT', parentAbortHandler);
      }
    } catch (error) {
      if (error instanceof Error && error.name === 'AbortPromptError') {
        return {
          success: true,
          message: 'Operation cancelled',
        };
      }
      throw MidgardError.transaction(`Failed to configure transaction generator: ${error}`);
    }
  },
};

export const toggleTxGenerator: Action = {
  name: 'Toggle Transaction Generator',
  description: 'Turn transaction generator on or off',
  execute: async (context) => {
    try {
      console.log(chalk.dim('Press Ctrl+C to cancel this operation and return to menu\n'));

      // Create a local abort controller
      const abortController = new AbortController();

      // Listen for the parent abort signal to propagate it
      const parentAbortHandler = () => {
        abortController.abort();
        throw new Error('AbortPromptError');
      };

      // Add a listener for SIGINT that will abort our local controller
      process.once('SIGINT', parentAbortHandler);

      try {
        // Check current status
        const currentStatus = getGeneratorStatus();

        // Display current status with more detail
        if (context.config.generator.enabled) {
          console.log(chalk.green.bold('✓ Transaction generator is currently ENABLED'));
          console.log(chalk.dim('The generator is running with the following configuration:'));
          console.log(chalk.dim(`• Batch Size: ${context.config.generator.batchSize}`));
          console.log(chalk.dim(`• Interval: ${context.config.generator.intervalMs / 1000}s`));
          console.log(chalk.dim(`• Concurrency: ${context.config.generator.maxConcurrent}`));

          // Show clearer options
          const confirmed = await confirm(
            {
              message: 'Do you want to stop the transaction generator?',
              default: true,
            },
            { signal: abortController.signal }
          );

          if (!confirmed) {
            return {
              success: true,
              message: 'Transaction generator remains running',
            };
          }

          // We'll disable it
          const stopSpinner = ora('Stopping transaction generator...').start();
          await stopGenerator();

          stopSpinner.succeed('Transaction generator stopped successfully');

          // Update config
          const newConfig = {
            ...context.config,
            generator: {
              ...context.config.generator,
              enabled: false,
            },
          };

          await Effect.runPromise(saveConfig(newConfig));

          return {
            success: true,
            message: 'Transaction generator stopped',
          };
        } else {
          // If not running, show saved configuration summary
          console.log(chalk.bold('\nSaved Configuration:'));
          console.log(`${chalk.dim('Batch Size:')} ${context.config.generator.batchSize}`);
          console.log(
            `${chalk.dim('Interval:')} ${context.config.generator.intervalMs / 1000} seconds`
          );
          console.log(`${chalk.dim('Concurrency:')} ${context.config.generator.maxConcurrent}`);
          console.log(`${chalk.dim('Node Endpoint:')} ${context.config.node.endpoint}`);

          // Clearer choices with better descriptions
          const action = await select(
            {
              message: 'What would you like to do?',
              choices: [
                {
                  value: 'quick-start',
                  name: 'Quick Start',
                  description: 'Start with saved settings (shown above)',
                },
                {
                  value: 'configure',
                  name: 'Full Configuration',
                  description: 'Set up detailed transaction parameters from scratch',
                },
                {
                  value: 'cancel',
                  name: 'Cancel',
                  description: 'Return to menu without changes',
                },
              ],
            },
            { signal: abortController.signal }
          );

          if (action === 'cancel') {
            return {
              success: true,
              message: 'Operation cancelled',
            };
          }

          if (action === 'configure') {
            // Use the configureTxGenerator action for full configuration
            return configureTxGenerator.execute(context);
          }

          // Quick start with current saved configuration
          // First, check if we have wallets
          const wallets = await listWallets();
          if (wallets.length === 0) {
            console.log(chalk.red('❌ No wallets configured. Please add a wallet first.'));
            console.log(chalk.gray("Select 'Wallet Management > Add Wallet' from the main menu."));
            return {
              success: false,
              message: 'No wallets available for transaction signing',
            };
          }

          // Select a wallet with clearer context
          console.log(chalk.bold('\n▶ Wallet Selection'));
          console.log(chalk.dim('Select the wallet that will be used to sign transactions'));

          const wallet = await select(
            {
              message: 'Choose wallet:',
              choices: wallets.map((w) => ({
                value: w,
                name: w,
              })),
              default: wallets[0],
            },
            { signal: abortController.signal }
          );

          const walletConfig = await getWallet(wallet);
          if (!walletConfig) {
            return {
              success: false,
              message: `Wallet '${wallet}' not found.`,
            };
          }

          // Show a final summary before starting
          console.log(chalk.bold('\n▶ Ready to Start'));
          console.log(
            chalk.dim('The transaction generator will start with the following configuration:')
          );

          console.log(`${chalk.bold('Type:')} ${'mixed'} (70% one-to-one, 30% multi-output)`);
          console.log(
            `${chalk.bold('Batch Size:')} ${
              context.config.generator.batchSize
            } transactions per batch`
          );
          console.log(
            `${chalk.bold('Interval:')} ${context.config.generator.intervalMs / 1000} seconds`
          );
          console.log(
            `${chalk.bold('Concurrency:')} ${
              context.config.generator.maxConcurrent
            } simultaneous batches`
          );
          console.log(`${chalk.bold('Wallet:')} ${wallet}`);

          // Final confirmation
          const confirmStart = await confirm(
            {
              message: 'Start transaction generator now?',
              default: true,
            },
            { signal: abortController.signal }
          );

          if (!confirmStart) {
            return {
              success: true,
              message: 'Transaction generator start cancelled',
            };
          }

          const spinner = ora('Starting transaction generator...').start();

          try {
            // Start the generator with saved configuration but mixed type
            await startGenerator({
              transactionType: 'mixed',
              oneToOneRatio: 70,
              batchSize: context.config.generator.batchSize,
              interval: context.config.generator.intervalMs / 1000, // Convert from ms
              concurrency: context.config.generator.maxConcurrent,
              nodeEndpoint: context.config.node.endpoint,
              walletSeedOrPrivateKey: walletConfig.privateKey,
            });

            spinner.succeed('Transaction generator started successfully');

            // Update config
            const newConfig = {
              ...context.config,
              generator: {
                ...context.config.generator,
                enabled: true,
              },
            };

            await Effect.runPromise(saveConfig(newConfig));

            return {
              success: true,
              message: 'Transaction generator is now running',
            };
          } catch (error) {
            spinner.fail('Failed to start transaction generator');
            throw new Error(`Failed to start transaction generator: ${error}`);
          }
        }
      } finally {
        // Clean up our SIGINT handler
        process.off('SIGINT', parentAbortHandler);
      }
    } catch (error: unknown) {
      // Check if it's an abort error and rethrow with the expected name
      if (
        error instanceof Error &&
        (error.name === 'AbortError' || error.message === 'AbortPromptError')
      ) {
        const abortError = new Error('Operation cancelled');
        abortError.name = 'AbortPromptError';
        throw abortError;
      }
      throw MidgardError.config(`Failed to toggle transaction generator: ${error}`);
    }
  },
};
