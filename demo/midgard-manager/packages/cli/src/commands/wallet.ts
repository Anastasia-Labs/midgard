import { Args, Command } from '@effect/cli';
import chalk from 'chalk';
import { Effect, pipe } from 'effect';

import {
  generateWallet,
  getWalletDetails,
  initializeDefaultWallet,
  listWallets,
  removeWallet,
} from '../utils/wallet.js';

/**
 * Command to generate a new wallet
 */
const generateCommand = Command.make(
  'generate',
  {
    name: Args.text('NAME').pipe(Args.withDescription('Name of the wallet to generate')),
  },
  ({ name }) => {
    return pipe(
      Effect.tryPromise(async () => {
        // Generate new wallet
        const wallet = await generateWallet(name);

        console.log(chalk.green(`✓ Generated new wallet: ${name}`));
        console.log(chalk.gray(`Address: ${wallet.address}`));
        console.log(
          chalk.gray(
            `Private Key: ${wallet.privateKey.substring(0, 10)}...${wallet.privateKey.slice(-5)}`
          )
        );
      })
    );
  }
);

/**
 * Command to show wallet details
 */
const detailsCommand = Command.make(
  'details',
  {
    name: Args.text('NAME').pipe(Args.withDescription('Name of the wallet to show details for')),
  },
  ({ name }) => {
    return pipe(
      Effect.tryPromise(async () => {
        const wallet = getWalletDetails(name);
        if (!wallet) {
          console.error(chalk.red(`❌ Wallet '${name}' not found`));
          return;
        }

        console.log(chalk.blue(`Details for wallet: ${name}`));
        console.log(chalk.gray(`Address: ${wallet.address}`));
        console.log(
          chalk.gray(
            `Private Key: ${wallet.privateKey.substring(0, 10)}...${wallet.privateKey.slice(-5)}`
          )
        );
      })
    );
  }
);

/**
 * Command to list all wallets
 */
const listCommand = Command.make('list', {}, () => {
  return pipe(
    Effect.tryPromise(async () => {
      // Initialize default wallet if needed
      await initializeDefaultWallet();

      const wallets = listWallets();
      if (wallets.length === 0) {
        console.log(chalk.yellow('⚠️ No wallets found'));
        return;
      }

      console.log(chalk.blue('Available wallets:'));
      for (const name of wallets) {
        const wallet = getWalletDetails(name);
        if (wallet) {
          console.log(
            ` ${chalk.green('•')} ${name}${wallet.isDefault ? chalk.gray(' (default)') : ''}`
          );
        }
      }

      console.log();
      console.log(chalk.gray('For details on a specific wallet, use:'));
      console.log(chalk.gray(`$ midgard-manager wallet details <name>`));
    })
  );
});

/**
 * Command to remove a wallet
 */
const removeCommand = Command.make(
  'remove',
  {
    name: Args.text('NAME').pipe(Args.withDescription('Name of the wallet to remove')),
  },
  ({ name }) => {
    return pipe(
      Effect.tryPromise(async () => {
        try {
          removeWallet(name);
          console.log(chalk.green(`✓ Removed wallet: ${name}`));
        } catch (error) {
          console.error(chalk.red(error instanceof Error ? error.message : 'Unknown error'));
        }
      })
    );
  }
);

/**
 * Main wallet command group
 */
export const walletCommand = Command.make('wallet')
  .pipe(Command.withDescription('Manage wallets for transaction signing'))
  .pipe(Command.withSubcommands([generateCommand, detailsCommand, listCommand, removeCommand]));
