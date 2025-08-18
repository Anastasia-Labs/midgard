import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

import { generateEmulatorAccountFromPrivateKey, LucidEvolution } from '@lucid-evolution/lucid';

// Simple wallet storage
interface WalletInfo {
  name: string;
  privateKey: string;
  address: string;
  isDefault?: boolean;
}

const WALLET_DIR = './wallets';
const WALLET_FILE = join(WALLET_DIR, 'wallets.json');

// Initialize wallet storage
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

// Load wallets from storage
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

// Save wallets to storage
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
 * Gets a wallet's address
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
 * Gets a wallet's private key in bech32 format
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
 * Lists all available wallets
 */
export function listWallets(): string[] {
  const wallets = loadWallets();
  return Object.keys(wallets);
}

/**
 * Generates a new wallet
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
 * Initializes the default test wallet if not already initialized
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
 * Removes a wallet
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
 * Gets wallet details
 */
export function getWalletDetails(name: string): WalletInfo | undefined {
  const wallets = loadWallets();
  return wallets[name.toLowerCase()];
}

/**
 * Selects a wallet for use with Lucid
 */
export async function selectWallet(walletName: string, lucid: LucidEvolution): Promise<void> {
  const wallet = getWalletDetails(walletName);
  if (!wallet) {
    throw new Error(`Wallet '${walletName}' not found`);
  }
  await lucid.selectWallet.fromPrivateKey(wallet.privateKey);
}
