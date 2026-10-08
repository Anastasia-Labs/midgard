/**
 * Building and signing over the wallet view (plan §8.5,
 * `services/intent-journal.wallet-view.ts`).
 *
 * - A builder reads the view of its wallet (`readSelectedWalletView`) and
 *   hands its UTxOs to coin selection explicitly (`presetWalletInputs`,
 *   never empty: `requireWalletViewInputs`). No builder pins a UTxO set on
 *   the wallet, and none reads the wallet from the provider.
 * - The node selects its wallets with `selectNodeWallet`, which keeps the
 *   seed's keys for the submit seam. The seam signs over the view
 *   (`signOverWalletView`): the own keys a transaction needs are found from
 *   the view's outputs, so a transaction spending the predicted change of a
 *   live own intent gets its witness. A wallet's own `signTx` would look its
 *   inputs up at the provider, which has not seen that change; it signs
 *   only where no follower runs, where the view is the provider's.
 */
import {
  CML,
  discoverOwnUsedTxKeyHashes,
  type LucidEvolution,
  type PrivateKey,
  type TxSignBuilder,
  type UTxO,
  type Wallet,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { IntentJournal } from "../services/intent-journal.js";
import {
  type NodeWalletView,
  WalletViewUnavailable,
} from "../services/intent-journal.wallet-view.js";
import { TxSignError } from "./utils.await-required-output-visibility.js";

type WalletSigner = Readonly<{
  address: string;
  /** Payment and stake private keys, by key hash. */
  keys: ReadonlyMap<string, PrivateKey>;
}>;

const signers = new WeakMap<Wallet, WalletSigner>();

const keyHashOf = (key: PrivateKey): string => {
  const priv = CML.PrivateKey.from_bech32(key);
  const pub = priv.to_public();
  const hash = pub.hash();
  try {
    return hash.to_hex();
  } finally {
    hash.free();
    pub.free();
    priv.free();
  }
};

/**
 * Selects the seed's wallet (Base address, account 0, as
 * `selectWallet.fromSeed`) on `lucid` and keeps its keys for
 * `signOverWalletView`.
 */
export const selectNodeWallet = (lucid: LucidEvolution, seed: string): void => {
  lucid.selectWallet.fromSeed(seed);
  const wallet = lucid.wallet();
  const { address, paymentKey, stakeKey } = walletFromSeed(seed, {
    network: lucid.config().network,
    addressType: "Base",
    accountIndex: 0,
  });
  const keys = new Map<string, PrivateKey>([
    [keyHashOf(paymentKey), paymentKey],
  ]);
  if (stakeKey !== null) keys.set(keyHashOf(stakeKey), stakeKey);
  signers.set(wallet, { address, keys });
};

/** The wallet view (§8.5) of `address` under the journal in scope. */
export const readWalletView = (
  lucid: LucidEvolution,
  address: string,
): Effect.Effect<NodeWalletView, WalletViewUnavailable, IntentJournal> =>
  Effect.flatMap(IntentJournal, (journal) =>
    journal.walletView(lucid, address),
  );

/** The wallet view of the wallet selected on `lucid`. */
export const readSelectedWalletView = (
  lucid: LucidEvolution,
): Effect.Effect<NodeWalletView, WalletViewUnavailable, IntentJournal> =>
  Effect.tryPromise({
    try: () => lucid.wallet().address(),
    catch: (cause) =>
      new WalletViewUnavailable({
        address: "<no wallet>",
        message: "no wallet is selected: there is no wallet view to read",
        cause,
      }),
  }).pipe(Effect.flatMap((address) => readWalletView(lucid, address)));

/**
 * The view's UTxOs for `presetWalletInputs`. Never empty: an empty preset
 * makes coin selection read the wallet from the provider.
 */
export const requireWalletViewInputs = (
  view: NodeWalletView,
  label: string,
): Effect.Effect<UTxO[], WalletViewUnavailable> =>
  view.utxos.length > 0
    ? Effect.succeed([...view.utxos])
    : Effect.fail(
        new WalletViewUnavailable({
          address: view.address,
          message: `the wallet view of ${view.address} has no spendable output to fund ${label}`,
          cause: "wallet_view_empty",
        }),
      );

/** The selected wallet's view, as `presetWalletInputs` for `label`. */
export const readSelectedWalletViewInputs = (
  lucid: LucidEvolution,
  label: string,
): Effect.Effect<UTxO[], WalletViewUnavailable, IntentJournal> =>
  readSelectedWalletView(lucid).pipe(
    Effect.flatMap((view) => requireWalletViewInputs(view, label)),
  );

/**
 * Adds the selected node wallet's witnesses to `signBuilder` for the own
 * keys the transaction needs, found from the wallet view read now (inputs
 * and collaterals among its outputs; certificates, withdrawals and required
 * signers naming the keys). A wallet not selected with `selectNodeWallet`
 * signs itself where no follower runs (its view is the provider's) and is
 * refused under a follower.
 */
export const signOverWalletView = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
): Effect.Effect<TxSignBuilder, TxSignError, IntentJournal> =>
  Effect.gen(function* () {
    const txHash = signBuilder.toHash();
    const unavailable = (cause: WalletViewUnavailable) =>
      new TxSignError({
        message: `Failed to sign transaction: ${cause.message}`,
        cause,
        txHash,
      });
    const wallet = lucid.wallet();
    const signer = wallet === undefined ? undefined : signers.get(wallet);
    if (signer === undefined) {
      // A wallet selected some other way (a CLI user wallet, a test's own
      // wallet) keeps no keys here. Where no follower runs, its view is the
      // provider's UTxOs, which is exactly what the wallet's own `signTx`
      // reads (no node code pins a UTxO set): signing with it is signing
      // over the view. Under a follower it is refused.
      const view = yield* readSelectedWalletView(lucid).pipe(
        Effect.mapError(unavailable),
      );
      if (view.source === "provider") return signBuilder.sign.withWallet();
      return yield* Effect.fail(
        new TxSignError({
          message: `the wallet ${view.address} was not selected with selectNodeWallet: under a follower the node signs only over its wallet view`,
          cause: "wallet_without_view_signer",
          txHash,
        }),
      );
    }
    const view = yield* readWalletView(lucid, signer.address).pipe(
      Effect.mapError(unavailable),
    );
    const needed = new Set(
      discoverOwnUsedTxKeyHashes(
        signBuilder.toTransaction(),
        [...signer.keys.keys()],
        [...view.utxos],
      ),
    );
    for (const keyHash of needed)
      signBuilder.sign.withPrivateKey(signer.keys.get(keyHash) as PrivateKey);
    return signBuilder;
  });
