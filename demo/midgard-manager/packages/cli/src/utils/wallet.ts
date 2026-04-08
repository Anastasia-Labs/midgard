import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

import { generateEmulatorAccountFromPrivateKey, LucidEvolution } from '@lucid-evolution/lucid';

/**
 * Wallet metadata persisted by the manager CLI.
 */
interface WalletInfo {
  name: string;
  privateKey: string;
  address: string;
  isDefault?: boolean;
}

const WALLET_DIR = './wallets';
const WALLET_FILE = join(WALLET_DIR, 'wallets.json');

/**
 * Ensures the wallet storage directory and JSON file exist before any read or
 * write operation.
 */
function initializeStorage(): void {
  try {
    mkdirSync(WALLET_DIR, { recursive: true });
    if (!existsSync(WALLET_FILE)) {
      writeFileSync(WALLET_FILE, JSON.stringify({}, null, 2));
    }
  } catch (error) {
    console.error('Error initializing wallet storage:', error);
    throw error;
  }
}

/**
 * Loads the persisted wallet map from disk.
 *
 * The CLI falls back to an empty map on parse or read failure so wallet setup
 * flows can recover by re-creating the file.
 */
function loadWallets(): Record<string, WalletInfo> {
  try {
    initializeStorage();
    const data = readFileSync(WALLET_FILE, 'utf-8');
    return JSON.parse(data);
  } catch (error) {
    console.error('Error loading wallets:', error);
    return {};
  }
}

/**
 * Persists the full wallet map to disk.
 */
function saveWallets(wallets: Record<string, WalletInfo>): void {
  try {
    initializeStorage();
    writeFileSync(WALLET_FILE, JSON.stringify(wallets, null, 2));
  } catch (error) {
    console.error('Error saving wallets:', error);
    throw error;
  }
}

/**
 * Returns the bech32 address for a named wallet.
 */
export function getWalletAddress(walletName: string): string {
  const wallets = loadWallets();
  const wallet = wallets[walletName.toLowerCase()];
  if (!wallet) {
    throw new Error(`Wallet '${walletName}' not found`);
  }
  return wallet.address;
}

/**
 * Returns the stored private key for a named wallet.
 */
export function getWalletPrivateKey(walletName: string): string {
  const wallets = loadWallets();
  const wallet = wallets[walletName.toLowerCase()];
  if (!wallet) {
    throw new Error(`Wallet '${walletName}' not found`);
  }
  return wallet.privateKey;
}

/**
 * Lists every wallet key currently present in local storage.
 */
export function listWallets(): string[] {
  const wallets = loadWallets();
  return Object.keys(wallets);
}

/**
 * Generates and persists a new emulator wallet under the provided name.
 */
export async function generateWallet(name: string): Promise<WalletInfo> {
  const wallets = loadWallets();
  const walletName = name.toLowerCase();

  if (wallets[walletName]) {
    throw new Error(`Wallet '${name}' already exists`);
  }

  const account = await generateEmulatorAccountFromPrivateKey({});

  const wallet: WalletInfo = {
    name: walletName,
    privateKey: account.privateKey,
    address: account.address,
  };

  wallets[walletName] = wallet;
  saveWallets(wallets);

  return wallet;
}

/**
 * Ensures the default `test` wallet exists for local/demo flows.
 */
export async function initializeDefaultWallet(): Promise<void> {
  const wallets = loadWallets();

  if (!wallets.test) {
    const account = await generateEmulatorAccountFromPrivateKey({});

    const wallet: WalletInfo = {
      name: 'test',
      privateKey: account.privateKey,
      address: account.address,
      isDefault: true,
    };

    wallets.test = wallet;
    saveWallets(wallets);
  }
}

/**
 * Removes a non-default wallet from local storage.
 */
export function removeWallet(name: string): void {
  const wallets = loadWallets();
  const walletName = name.toLowerCase();
  const wallet = wallets[walletName];

  if (!wallet) {
    throw new Error(`Wallet '${name}' not found`);
  }

  if (wallet.isDefault) {
    throw new Error('Cannot remove the default test wallet');
  }

  delete wallets[walletName];
  saveWallets(wallets);
}

/**
 * Returns the full stored record for a named wallet, if present.
 */
export function getWalletDetails(name: string): WalletInfo | undefined {
  const wallets = loadWallets();
  return wallets[name.toLowerCase()];
}

/**
 * Selects the named wallet inside a Lucid client instance.
 */
export async function selectWallet(walletName: string, lucid: LucidEvolution): Promise<void> {
  const wallet = getWalletDetails(walletName);
  if (!wallet) {
    throw new Error(`Wallet '${walletName}' not found`);
  }
  await lucid.selectWallet.fromPrivateKey(wallet.privateKey);
}
