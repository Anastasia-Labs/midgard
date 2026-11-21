/**
 * A deployment command's own wallets over the node's ledger: read together
 * once, on a node connection of their own (closed after the read), then
 * advanced by every transaction the provider submits. The provider reads no
 * other address.
 */
import type { L1NodeTransport } from "@al-ft/l1-node-transport";

import { decodeTransaction } from "../decode/tx.js";
import type { LedgerUtxo } from "../decode/utxo.js";
import { readWalletUtxos } from "../follow/wallet-seed.js";
import { walletView, type WalletViewIntent } from "../intents/wallet-view.js";

/** A deployment command's own wallets (`LedgerProviderOptions.ownWallets`). */
export type OwnWalletsOptions = Readonly<{
  addresses: readonly Buffer[];
  ledger: () => L1NodeTransport;
}>;

export class OwnWallets {
  readonly #options: OwnWalletsOptions;
  #read: Promise<readonly LedgerUtxo[]> | undefined;
  /** The provider's accepted submissions, which advance the view. */
  readonly #submitted: WalletViewIntent[] = [];

  constructor(options: OwnWalletsOptions) {
    this.#options = options;
  }

  owns(address: Buffer): boolean {
    return this.#options.addresses.some((own) => own.equals(address));
  }

  /** Advances the view by the accepted transaction `bytes`. */
  submitted(txHash: string, bytes: Uint8Array): void {
    const { inputs, outputs } = decodeTransaction(bytes);
    this.#submitted.push({
      txHash: Buffer.from(txHash, "hex"),
      inputs,
      // A collateral is spent only by a failing script, which fails the command.
      collaterals: [],
      outputs: outputs.map((output, index) => ({ index, output })),
    });
  }

  /**
   * The outputs available at `address`, one of the own wallets. `guard` maps
   * the read's failures; a failed read is not an answer and is made again.
   */
  async outputsAt(
    address: Buffer,
    guard: (
      read: () => Promise<readonly LedgerUtxo[]>,
    ) => Promise<readonly LedgerUtxo[]>,
  ) {
    const { addresses, ledger } = this.#options;
    this.#read ??= guard(async () => {
      const connection = ledger();
      try {
        return await readWalletUtxos(connection, "tip", addresses);
      } finally {
        await connection.close();
      }
    }).catch((error: unknown) => {
      this.#read = undefined;
      throw error;
    });
    return walletView(await this.#read, this.#submitted, address).available;
  }
}
