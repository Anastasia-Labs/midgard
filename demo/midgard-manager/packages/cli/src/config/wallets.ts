import { mkdir, readFile, writeFile } from 'fs/promises';
import { join } from 'path';
import { dirname } from 'path';
import { fileURLToPath } from 'url';

// Get the directory path relative to the project
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const PROJECT_ROOT = join(__dirname, '../../../..'); // Simplified path to project root
const CONFIG_DIR = join(PROJECT_ROOT, 'config/wallets');
const WALLET_CONFIG_PATH = join(CONFIG_DIR, 'default.json');

export interface WalletConfig {
  [name: string]: {
    name?: string;
    privateKey: string;
    address: string;
    description?: string;
    isDefault?: boolean;
    isTestOnly?: boolean;
  };
}

/**
 * Load wallet configuration from disk
 * Creates default config if it doesn't exist
 */
export const loadWallets = async (): Promise<WalletConfig> => {
  try {
    // Ensure the test-wallets directory exists
    await mkdir(CONFIG_DIR, { recursive: true });

    const data = await readFile(WALLET_CONFIG_PATH, 'utf-8');
    return JSON.parse(data);
  } catch (error) {
    console.error('Error loading wallets:', error);
    throw new Error('Failed to load wallet configuration');
  }
};

/**
 * Save wallet configuration to disk
 */
const saveWallets = async (wallets: WalletConfig): Promise<void> => {
  await mkdir(CONFIG_DIR, { recursive: true });
  await writeFile(WALLET_CONFIG_PATH, JSON.stringify(wallets, null, 2));
};

/**
 * Add a test wallet to the configuration
 */
export const addWallet = async (
  name: string,
  privateKey: string,
  address: string,
  description?: string
): Promise<void> => {
  const wallets = await loadWallets();

  // Check if trying to modify a default wallet
  if (wallets[name]?.isDefault) {
    throw new Error('Cannot modify a default wallet');
  }

  wallets[name] = {
    name,
    privateKey,
    address,
    description,
    isTestOnly: true,
  };
  await saveWallets(wallets);
};

/**
 * Remove a test wallet from the configuration
 */
export const removeWallet = async (name: string): Promise<void> => {
  const wallets = await loadWallets();

  // Check if trying to remove a default wallet
  if (wallets[name]?.isDefault) {
    throw new Error('Cannot remove a default wallet');
  }

  delete wallets[name];
  await saveWallets(wallets);
};

/**
 * Get a wallet by name
 */
export const getWallet = async (name: string): Promise<WalletConfig[string] | null> => {
  const wallets = await loadWallets();
  return wallets[name] || null;
};

/**
 * List all wallet names
 */
export const listWallets = async (): Promise<string[]> => {
  const wallets = await loadWallets();
  return Object.keys(wallets);
};
