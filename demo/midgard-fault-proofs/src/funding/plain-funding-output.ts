import type { LucidEvolution, OutRef, UTxO } from "@lucid-evolution/lucid";

import { selectFeeInput } from "../step-support.js";

/**
 * Whether `utxo`, an output at the prover's funding address, is a coin the
 * prover may spend to fund a transaction: nothing but ada and no reference
 * script. A datum or datum hash does not matter: the chain pays refunds
 * (a dispute Close's bond refund and case remainder, a finished thread's
 * remainder) to the funding address with a tag datum, and those are money.
 * The prover's own proof materials, which also carry an inline datum, never
 * live there (`proofMaterialsAddress`), so the caller must have read the
 * funding address by its full address. Any output carrying other assets or a
 * reference script stays out of every coin selection.
 */
export const isPlainFundingOutput = (utxo: UTxO): boolean =>
  utxo.scriptRef == null &&
  Object.entries(utxo.assets).every(
    ([unit, quantity]) => unit === "lovelace" || quantity === 0n,
  );

const outRefKey = ({ txHash, outputIndex }: OutRef): string =>
  `${txHash}#${outputIndex.toString()}`;

/**
 * Whether Lucid takes the selected wallet's own collateral: the same test as
 * its `fetchWalletCollateral` (@lucid-evolution/lucid 0.8.9,
 * dist/index.js:5057), which only a CIP-30 wallet such as the watcher's
 * restricted one (`restrictWorkflowFundingSigner`) passes.
 */
const walletReservesCollateral = (lucid: LucidEvolution): boolean =>
  lucid.wallet().getCollateral !== undefined;

/**
 * `complete`'s wallet-input option: the wallet coins coin selection may
 * spend, those that are plain (`isPlainFundingOutput`) and not `excluding`,
 * from the one wallet read made here. Without a preset Lucid re-reads the
 * wallet inside `complete`, and a coin that reached it in between would join
 * coin selection. It is `{}` only for a wallet that reserves collateral and
 * whose coins are all selectable: Lucid takes collateral only from among
 * preset inputs, so a preset would hide that reserved collateral. It is `{}`
 * for an empty wallet too, which Lucid reads as no preset; the fee input
 * selection reports the shortfall. It throws when the wallet holds coins but
 * none is selectable: Lucid reads an empty preset as none and would bring the
 * removed coins back into coin selection.
 */
const coinSelectionWalletInputs = (
  walletUtxos: readonly UTxO[],
  reservesCollateral: boolean,
  excluding: readonly OutRef[] = [],
): { presetWalletInputs?: UTxO[] } => {
  if (walletUtxos.length === 0) return {};
  const excluded = new Set(excluding.map(outRefKey));
  const selectable = walletUtxos.filter(
    (utxo) => isPlainFundingOutput(utxo) && !excluded.has(outRefKey(utxo)),
  );
  if (selectable.length === 0)
    throw new Error(
      "Prover wallet must contain a pure-ADA UTxO outside the transaction's own inputs and references for coin selection.",
    );
  if (reservesCollateral && selectable.length === walletUtxos.length) return {};
  return { presetWalletInputs: selectable };
};

/**
 * A step's wallet funding from one wallet read: its fee input
 * (`selectFeeInput` over the coins coin selection may spend) and
 * `complete`'s wallet-input option (`coinSelectionWalletInputs`).
 */
export const stepWalletFunding = async (
  lucid: LucidEvolution,
  { excluding }: { readonly excluding?: readonly OutRef[] } = {},
): Promise<{
  readonly feeInput: UTxO;
  readonly walletInputs: { presetWalletInputs?: UTxO[] };
}> => {
  const walletUtxos = await lucid.wallet().getUtxos();
  const walletInputs = coinSelectionWalletInputs(
    walletUtxos,
    walletReservesCollateral(lucid),
    excluding,
  );
  return {
    feeInput: selectFeeInput(walletInputs.presetWalletInputs ?? walletUtxos),
    walletInputs,
  };
};

/** `coinSelectionWalletInputs` over the wallet `lucid` has selected. */
export const plainWalletInputs = async (
  lucid: LucidEvolution,
  excluding?: readonly OutRef[],
): Promise<{ presetWalletInputs?: UTxO[] }> =>
  coinSelectionWalletInputs(
    await lucid.wallet().getUtxos(),
    walletReservesCollateral(lucid),
    excluding,
  );
