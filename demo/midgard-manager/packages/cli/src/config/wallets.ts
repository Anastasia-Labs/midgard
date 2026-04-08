import { mkdir, readFile, writeFile } from 'fs/promises';
import { join } from 'path';
import { dirname } from 'path';
import { fileURLToPath } from 'url';

/**
 * Filesystem-backed wallet storage used by the manager CLI.
 *
 * The interactive and command-based wallet flows both converge on this module,
 * so keeping the path resolution and persistence rules documented here reduces
 * the risk of accidentally diverging wallet formats across entrypoints.
 */

/**
 * Resolve the manager package root from the compiled module location so wallet
 * storage works regardless of the current working directory.
 */
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const PROJECT_ROOT = join(__dirname, '../../../..'); // Simplified path to project root
const CONFIG_DIR = join(PROJECT_ROOT, 'config/wallets');
const WALLET_CONFIG_PATH = join(CONFIG_DIR, 'default.json');

/**
 * On-disk wallet map keyed by the operator-facing wallet name.
 */
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
 * Loads the wallet configuration file from disk.
 *
 * The directory is created eagerly so later write paths can assume the storage
 * location exists. Errors are surfaced as a single stable message because the
 * calling CLI flows already provide user-facing context.
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
 * Persists the full wallet map to disk.
 *
 * Writes are whole-file updates so callers operate on a simple read-modify-
 * write model instead of reasoning about partial updates.
 */
const saveWallets = async (wallets: WalletConfig): Promise<void> => {
  await mkdir(CONFIG_DIR, { recursive: true });
  await writeFile(WALLET_CONFIG_PATH, JSON.stringify(wallets, null, 2));
};

/**
 * Adds or replaces a wallet entry in the local wallet store.
 *
 * Default wallets are protected from modification because other local manager
 * workflows may assume their presence and semantics.
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
 * Removes a wallet entry from the local wallet store.
 *
 * Default wallets are protected for the same reason they cannot be mutated:
 * they act as stable fixtures for local tooling.
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
 * Looks up one wallet entry by its configured name.
 */
export const getWallet = async (name: string): Promise<WalletConfig[string] | null> => {
  const wallets = await loadWallets();
  return wallets[name] || null;
};

/**
 * Lists the currently configured wallet names.
 */
export const listWallets = async (): Promise<string[]> => {
  const wallets = await loadWallets();
  return Object.keys(wallets);
};
