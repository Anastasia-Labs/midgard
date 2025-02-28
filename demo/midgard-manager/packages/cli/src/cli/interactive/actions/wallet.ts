import { confirm, input, select } from '@inquirer/prompts';
import chalk from 'chalk';
import { Effect, pipe } from 'effect';
import ora from 'ora-classic';

import { addWallet, getWallet, listWallets, removeWallet } from '../../../config/wallets.js';
import type { Action } from '../types.js';

// Validate ed25519 private key format
const PRIVATE_KEY_REGEX = /^ed25519_sk[a-zA-Z0-9]+$/;

/**
 * Add Wallet Action
 * Adds a new wallet to the configuration
 */
export const addWalletAction: Action = {
  name: 'Add Wallet',
  description: 'Add a new wallet for transaction signing',
  execute: async () => {
    try {
      console.log(chalk.bold('\n🔑 Add New Wallet\n'));
      console.log(
        chalk.dim(
          "Add a new wallet that can be used for signing transactions. You'll need to provide a name and a private key.\n"
        )
      );

      // Prompt for wallet name
      const name = await input({
        message: 'Enter wallet name:',
        validate: (value) => (value && value.trim() !== '' ? true : 'Wallet name cannot be empty'),
      });

      // Check if wallet already exists
      const existingWallet = await getWallet(name);
      if (existingWallet) {
        const overwrite = await confirm({
          message: `Wallet '${name}' already exists. Overwrite?`,
          default: false,
        });

        if (!overwrite) {
          console.log(chalk.yellow('⚠️ Operation cancelled'));
          return {
            success: false,
            message: 'Operation cancelled by user',
          };
        }
      }

      // Prompt for private key
      const privateKey = await input({
        message: 'Enter private key (ed25519 format):',
        validate: (value) =>
          PRIVATE_KEY_REGEX.test(value)
            ? true
            : 'Invalid private key format. Expected ed25519_sk format',
      });

      // Add the wallet
      const spinner = ora('Adding wallet...').start();
      await addWallet(name, privateKey);
      spinner.succeed(`Wallet '${name}' added successfully`);

      return {
        success: true,
        message: `Wallet '${name}' added successfully`,
      };
    } catch (error) {
      console.error(chalk.red('Error adding wallet:'), error);
      return {
        success: false,
        message: error instanceof Error ? error.message : 'Unknown error',
      };
    }
  },
};

/**
 * Remove Wallet Action
 * Removes a wallet from the configuration
 */
export const removeWalletAction: Action = {
  name: 'Remove Wallet',
  description: 'Remove an existing wallet',
  execute: async () => {
    try {
      // Get list of wallets
      const wallets = await listWallets();
      if (wallets.length === 0) {
        console.log(chalk.yellow('⚠️ No wallets configured'));
        return {
          success: false,
          message: 'No wallets available to remove',
        };
      }

      // Filter out the test wallet which cannot be removed
      const availableWallets = wallets.filter((w) => w !== 'test');
      if (availableWallets.length === 0) {
        console.log(
          chalk.yellow('⚠️ Only the default test wallet exists, which cannot be removed')
        );
        return {
          success: false,
          message: 'No removable wallets available',
        };
      }

      console.log(chalk.bold('\n🗑️ Remove Wallet\n'));
      console.log(
        chalk.dim('Remove a wallet from your configuration. This action cannot be undone.\n')
      );

      // Select wallet to remove
      const name = await select({
        message: 'Select wallet to remove:',
        choices: availableWallets.map((w) => ({
          value: w,
          name: w,
        })),
      });

      // Confirm deletion
      const confirmDelete = await confirm({
        message: `Are you sure you want to remove wallet '${name}'?`,
        default: false,
      });

      if (!confirmDelete) {
        console.log(chalk.yellow('⚠️ Operation cancelled'));
        return {
          success: false,
          message: 'Operation cancelled by user',
        };
      }

      // Remove the wallet
      const spinner = ora('Removing wallet...').start();
      await removeWallet(name);
      spinner.succeed(`Wallet '${name}' removed successfully`);

      return {
        success: true,
        message: `Wallet '${name}' removed successfully`,
      };
    } catch (error) {
      console.error(chalk.red('Error removing wallet:'), error);
      return {
        success: false,
        message: error instanceof Error ? error.message : 'Unknown error',
      };
    }
  },
};

/**
 * List Wallets Action
 * Lists all available wallets
 */
export const listWalletsAction: Action = {
  name: 'List Wallets',
  description: 'View all available wallets',
  execute: async () => {
    try {
      const wallets = await listWallets();

      console.log(chalk.bold('\n📋 Available Wallets\n'));

      if (wallets.length === 0) {
        console.log(chalk.yellow('⚠️ No wallets configured'));
        return {
          success: true,
          message: 'No wallets available',
        };
      }

      console.log(chalk.blue('Available wallets:'));
      wallets.forEach((name) => {
        const isDefault = name === 'test';
        console.log(` ${chalk.green('•')} ${name}${isDefault ? chalk.gray(' (default)') : ''}`);
      });

      console.log();
      return {
        success: true,
        message: `Found ${wallets.length} wallet(s)`,
      };
    } catch (error) {
      console.error(chalk.red('Error listing wallets:'), error);
      return {
        success: false,
        message: error instanceof Error ? error.message : 'Unknown error',
      };
    }
  },
};

/**
 * Wallet Details Action
 * Shows details for a specific wallet
 */
export const walletDetailsAction: Action = {
  name: 'Wallet Details',
  description: 'View details of a specific wallet',
  execute: async () => {
    try {
      // Get list of wallets
      const wallets = await listWallets();
      if (wallets.length === 0) {
        console.log(chalk.yellow('⚠️ No wallets configured'));
        return {
          success: false,
          message: 'No wallets available',
        };
      }

      console.log(chalk.bold('\n🔍 Wallet Details\n'));
      console.log(
        chalk.dim(
          'View details of a specific wallet, including a masked version of the private key.\n'
        )
      );

      // Select wallet to view
      const name = await select({
        message: 'Select wallet to view:',
        choices: wallets.map((w) => ({
          value: w,
          name: w,
        })),
      });

      // Get wallet details
      const wallet = await getWallet(name);
      if (!wallet) {
        console.error(chalk.red(`❌ Wallet '${name}' not found`));
        return {
          success: false,
          message: `Wallet '${name}' not found`,
        };
      }

      console.log(chalk.blue(`Details for wallet: ${name}`));

      // Show masked private key for security
      const maskedKey =
        wallet.privateKey.substring(0, 10) +
        '...' +
        wallet.privateKey.substring(wallet.privateKey.length - 5);

      console.log(chalk.gray(`Private Key: ${maskedKey}`));
      console.log();
      console.log(chalk.gray('This wallet can be used for transaction signing in:'));
      console.log(
        chalk.gray("- Transaction Generator configuration\n- Command-line with '--wallet' option")
      );

      return {
        success: true,
        message: `Displayed details for wallet '${name}'`,
      };
    } catch (error) {
      console.error(chalk.red('Error viewing wallet details:'), error);
      return {
        success: false,
        message: error instanceof Error ? error.message : 'Unknown error',
      };
    }
  },
};
