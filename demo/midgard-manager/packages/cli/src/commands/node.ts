import { Command, Options } from '@effect/cli';
import chalk from 'chalk';
import { Effect, pipe } from 'effect';
import fs from 'fs/promises';
import ora from 'ora-classic';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

// Get the directory path relative to the monorepo
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const MONOREPO_ROOT = join(__dirname, '../../../../../..');
const PROJECT_ROOT = join(MONOREPO_ROOT, 'demo/midgard-manager');

// Store node configuration in the project's config directory
const CONFIG_DIR = join(PROJECT_ROOT, 'config');
const NODE_CONFIG_PATH = join(CONFIG_DIR, 'node.json');

/**
 * Check the status of the Midgard node
 * Usage: midgard-manager node-status
 */
export const nodeStatusCommand = Command.make(
  'node-status',
  {
    endpoint: Options.text('endpoint')
      .pipe(Options.withDescription('Node endpoint URL'))
      .pipe(Options.withDefault('http://localhost:3000')),
  },
  ({ endpoint }) => {
    return pipe(
      Effect.tryPromise(async () => {
        const spinner = ora(`Checking Midgard node status at ${endpoint}...`).start();

        try {
          // Try to fetch the node status
          const statusEndpoint = `${endpoint}/api/status`;
          const controller = new AbortController();

          // Set a timeout for the request
          const timeoutId = setTimeout(() => controller.abort(), 5000);

          const response = await fetch(statusEndpoint, {
            signal: controller.signal,
          });
          clearTimeout(timeoutId);

          if (!response.ok) {
            throw new Error(`HTTP error: ${response.status}`);
          }

          const data = await response.json();
          spinner.succeed('Node is online');

          // Display node information
          console.log(chalk.blue.bold('\n📊 Midgard Node Status\n'));
          console.log(chalk.white(`• Version: ${chalk.green(data.version || 'unknown')}`));
          console.log(chalk.white(`• Chain: ${chalk.green(data.network || 'unknown')}`));

          if (data.blockHeight) {
            console.log(chalk.white(`• Block Height: ${chalk.green(data.blockHeight)}`));
          }

          if (data.peers) {
            console.log(chalk.white(`• Connected Peers: ${chalk.green(data.peers.length)}`));
          }

          if (data.mempool) {
            console.log(chalk.white(`• Mempool Transactions: ${chalk.green(data.mempool.count)}`));
          }

          console.log(chalk.white(`• Endpoint: ${chalk.cyan(endpoint)}`));
        } catch (error) {
          spinner.fail('Failed to connect to node');

          if (error.name === 'AbortError') {
            console.error(chalk.red('Connection timed out'));
          } else {
            console.error(chalk.red(`Error: ${error.message}`));
          }

          // Troubleshooting tips
          console.log(chalk.yellow('\nTroubleshooting:'));
          console.log(chalk.gray('• Check that the Midgard node is running'));
          console.log(chalk.gray(`• Verify the node endpoint: ${endpoint}`));
          console.log(chalk.gray('• Check network connectivity'));
          console.log(chalk.gray('• Try configuring a different endpoint:'));
          console.log(chalk.gray(`  $ midgard-manager configure-node --endpoint <url>`));
          console.log(chalk.gray('\nCommon endpoint configurations:'));
          console.log(chalk.gray('• Local development: http://localhost:3000'));
          console.log(chalk.gray('• Docker container: http://localhost:8080'));
          console.log(chalk.gray('• Remote node: https://api.your-server.com'));
        }
      })
    );
  }
).pipe(Command.withDescription('Check the status of the Midgard node'));

/**
 * Command to configure node settings
 * Usage: midgard-manager configure-node
 */
export const configureNodeCommand = Command.make(
  'configure-node',
  {
    endpoint: Options.text('endpoint')
      .pipe(Options.withDescription('Node endpoint URL'))
      .pipe(Options.withDefault('http://localhost:3000')),

    interactive: Options.boolean('interactive')
      .pipe(Options.withDescription('Use interactive mode to configure'))
      .pipe(Options.withDefault(false)),
  },
  ({ endpoint, interactive }) => {
    return pipe(
      Effect.tryPromise(async () => {
        const nodeConfig = {
          endpoint,
        };

        // Interactive mode
        if (interactive) {
          const { input, confirm } = await import('@inquirer/prompts');

          nodeConfig.endpoint = await input({
            message: 'Enter Midgard node endpoint URL:',
            default: endpoint,
            validate: (value) =>
              /^https?:\/\//.test(value) || 'Must be a valid URL starting with http:// or https://',
          });

          // Advanced settings could be added here

          // Confirm settings
          console.log(chalk.yellow.bold('\nConfiguration Summary:'));
          console.log(chalk.yellow(`• Node Endpoint: ${nodeConfig.endpoint}`));

          const confirmed = await confirm({
            message: 'Save these settings?',
            default: true,
          });

          if (!confirmed) {
            console.log(chalk.yellow('⚠️ Configuration cancelled'));
            return;
          }
        }

        // Validate endpoint format
        if (!/^https?:\/\//.test(nodeConfig.endpoint)) {
          console.error(
            chalk.red('❌ Invalid endpoint format. URL must start with http:// or https://')
          );
          return;
        }

        const spinner = ora('Saving node configuration...').start();

        try {
          // Ensure directory exists
          await fs.mkdir(CONFIG_DIR, { recursive: true });

          // Save configuration
          await fs.writeFile(NODE_CONFIG_PATH, JSON.stringify(nodeConfig, null, 2));

          spinner.succeed('Node configuration saved');

          // Test connection
          console.log(chalk.gray('Testing connection to node...'));

          try {
            const controller = new AbortController();
            const timeoutId = setTimeout(() => controller.abort(), 5000);

            const response = await fetch(`${nodeConfig.endpoint}/api/status`, {
              signal: controller.signal,
            });

            clearTimeout(timeoutId);

            if (response.ok) {
              console.log(chalk.green('✓ Successfully connected to node'));
            } else {
              console.log(chalk.yellow(`⚠️ Node responded with status: ${response.status}`));
            }
          } catch (error) {
            console.log(chalk.yellow(`⚠️ Could not connect to node: ${error.message}`));
          }

          console.log();
          console.log(chalk.gray('To check node status:'));
          console.log(chalk.gray(`$ midgard-manager node-status`));
        } catch (error) {
          spinner.fail('Failed to save configuration');
          console.error(chalk.red(`Error: ${error.message}`));
        }
      })
    );
  }
).pipe(Command.withDescription('Configure Midgard node settings'));
