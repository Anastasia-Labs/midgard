/**
 * Flow-local wallet overlay for operator-managed spend paths.
 * This module keeps a per-flow view of spendable wallet UTxOs so commit,
 * merge, and lifecycle code can account for locally submitted transactions
 * without relying on a provider to reflect those spends immediately.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  CML,
  coreToTxOutput,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import { dedupeByOutRef, outRefLabel, type OutRefLike } from "@/tx-context.js";

export type OperatorWalletView = {
  readonly walletAddress: string;
  readonly knownUtxos: readonly UTxO[];
  readonly consumedOutRefs: readonly string[];
};

const STALE_OPERATOR_WALLET_VIEW_ERROR_PATTERNS = [
  "BadInputsUTxO",
  "TranslationLogicMissingInput",
  "ValueNotConservedUTxO",
] as const;

/**
 * Materializes the consumed-outref list into a set for efficient lookups.
 */
const toConsumedSet = (view: OperatorWalletView): ReadonlySet<string> =>
  new Set(view.consumedOutRefs);

/**
 * Creates an operator-wallet overlay from the currently observed wallet UTxOs.
 */
export const makeOperatorWalletView = (
  walletAddress: string,
  walletUtxos: readonly UTxO[],
): OperatorWalletView => ({
  walletAddress,
  knownUtxos: dedupeByOutRef(walletUtxos),
  consumedOutRefs: [],
});

/**
 * Fetches the current wallet address and spendable UTxOs from Lucid and wraps
 * them in an operator-wallet overlay.
 */
export const fetchOperatorWalletView = async (
  lucid: LucidEvolution,
): Promise<OperatorWalletView> => {
  const walletAddress = await lucid.wallet().address();
  const walletUtxos = await lucid.wallet().getUtxos();
  return makeOperatorWalletView(walletAddress, walletUtxos);
};

/**
 * Returns the subset of known wallet UTxOs that have not been marked as
 * locally consumed.
 */
export const availableOperatorWalletUtxos = (
  view: OperatorWalletView,
): readonly UTxO[] => {
  const consumedOutRefs = toConsumedSet(view);
  return view.knownUtxos.filter(
    (utxo) => !consumedOutRefs.has(outRefLabel(utxo)),
  );
};

/**
 * Extracts outputs from a submitted transaction that pay back into the tracked
 * operator wallet.
 */
export const extractWalletOutputsFromSubmittedTx = (
  tx: CML.Transaction,
  txHash: string,
  walletAddress: string,
): readonly UTxO[] => {
  const outputs = tx.body().outputs();
  const matching: UTxO[] = [];
  for (let outputIndex = 0; outputIndex < outputs.len(); outputIndex += 1) {
    const txOutput = coreToTxOutput(outputs.get(outputIndex));
    if (txOutput.address !== walletAddress) {
      continue;
    }
    matching.push({
      txHash,
      outputIndex,
      address: txOutput.address,
      assets: txOutput.assets,
      datumHash: txOutput.datumHash,
      datum: txOutput.datum,
      scriptRef: txOutput.scriptRef,
    });
  }
  return matching;
};

/**
 * Marks specific wallet inputs as locally consumed.
 */
export const noteConsumedOperatorWalletInputs = (
  view: OperatorWalletView,
  spentInputs: readonly OutRefLike[],
): OperatorWalletView => ({
  ...view,
  consumedOutRefs: [
    ...new Set([
      ...view.consumedOutRefs,
      ...spentInputs.map((input) => outRefLabel(input)),
    ]),
  ],
});

/**
 * Adds newly produced wallet outputs to the overlay while filtering anything
 * already marked as consumed.
 */
export const noteProducedOperatorWalletOutputs = (
  view: OperatorWalletView,
  producedOutputs: readonly UTxO[],
): OperatorWalletView => {
  const consumedOutRefs = toConsumedSet(view);
  return {
    ...view,
    knownUtxos: dedupeByOutRef([...view.knownUtxos, ...producedOutputs]).filter(
      (utxo) => !consumedOutRefs.has(outRefLabel(utxo)),
    ),
  };
};

/**
 * Applies the effects of a successfully submitted transaction to the wallet
 * overlay.
 */
export const applySubmittedTxToOperatorWalletView = (
  view: OperatorWalletView,
  tx: CML.Transaction,
  txHash: string,
): OperatorWalletView => {
  const txInputs = tx.body().inputs();
  const spentInputs: OutRefLike[] = [];
  for (let index = 0; index < txInputs.len(); index += 1) {
    const input = txInputs.get(index);
    spentInputs.push({
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    });
  }
  return noteProducedOperatorWalletOutputs(
    noteConsumedOperatorWalletInputs(view, spentInputs),
    extractWalletOutputsFromSubmittedTx(tx, txHash, view.walletAddress),
  );
};

/**
 * Heuristically detects errors that likely mean the local wallet overlay has
 * gone stale relative to the provider.
 */
export const isPotentiallyStaleOperatorWalletViewError = (
  cause: unknown,
): boolean => {
  const message = formatUnknownError(cause);
  return STALE_OPERATOR_WALLET_VIEW_ERROR_PATTERNS.some((pattern) =>
    message.includes(pattern),
  );
};
