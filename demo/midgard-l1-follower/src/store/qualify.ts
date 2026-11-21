import { outRefKey } from "../codec.js";
import type {
  BlockSummary,
  OutputSummary,
  OutRef,
  TrackedSet,
  TxSummary,
} from "../types.js";

/** An output gets a row iff it matches the tracked set by address or credential (§5.2). */
export const isTrackedOutput = (
  output: OutputSummary,
  tracked: TrackedSet,
): boolean =>
  tracked.addresses.has(output.address.toString("hex")) ||
  (output.paymentCredential !== null &&
    tracked.paymentCredentials.has(
      output.paymentCredential.hash.toString("hex"),
    ));

export type CreatedOutput = Readonly<{ outRef: OutRef; output: OutputSummary }>;

/** A qualifying tx and the tracked rows it touches. */
export type QualifiedTx = Readonly<{
  tx: TxSummary;
  /** Tracked outputs it creates: its outputs, or a failed tx's collateral return. */
  created: readonly CreatedOutput[];
  /** Tracked outrefs it consumes: inputs, or a failed tx's collaterals. */
  spent: readonly OutRef[];
  /** Tracked outrefs it references. */
  referenced: readonly OutRef[];
  /** It mints or burns under a tracked policy. */
  trackedMint: boolean;
}>;

/** The outputs a tx creates on chain, in the phase it ran. */
export const createdOutputs = (tx: TxSummary): CreatedOutput[] => {
  if (tx.isValid)
    return tx.outputs.map((output, index) => ({
      outRef: { txHash: tx.hash, index },
      output,
    }));
  return tx.collateralReturn === null
    ? []
    : [
        {
          outRef: { txHash: tx.hash, index: tx.outputs.length },
          output: tx.collateralReturn,
        },
      ];
};

/**
 * Qualification rules (a)–(d) of §5.2 for one block, in tx order. `isLive`
 * answers for the tracked-outref set before the block; outputs the block's
 * own earlier qualifying txs created are staged here, so a tx that spends or
 * references one of them qualifies too (§6 item 4).
 */
export const qualifyBlock = (
  block: BlockSummary,
  tracked: TrackedSet,
  isLive: (key: string) => boolean,
): QualifiedTx[] => {
  const staged = new Set<string>();
  const known = (outRef: OutRef): boolean => {
    const key = outRefKey(outRef);
    return staged.has(key) || isLive(key);
  };
  const qualified: QualifiedTx[] = [];
  for (const tx of block.txs) {
    const created = createdOutputs(tx).filter(({ output }) =>
      isTrackedOutput(output, tracked),
    );
    const spent = (tx.isValid ? tx.inputs : tx.collaterals).filter(known);
    const referenced = tx.referenceInputs.filter(known);
    const trackedMint = [...tx.mint.keys()].some((policy) =>
      tracked.policies.has(policy),
    );
    if (
      created.length === 0 &&
      spent.length === 0 &&
      referenced.length === 0 &&
      !trackedMint
    )
      continue;
    for (const outRef of spent) staged.delete(outRefKey(outRef));
    for (const { outRef } of created) staged.add(outRefKey(outRef));
    qualified.push({ tx, created, spent, referenced, trackedMint });
  }
  return qualified;
};
