import { CML, type Data } from "@lucid-evolution/lucid";

import {
  type ComputationThreadStep,
  isComputationThreadStep,
} from "./computation-thread-steps.js";
import { historicalTransactions } from "./raw-l1-family-derivation.derive-retained-state-queue-header-observation-from-raw-l1.js";
import { authenticateFamilySnapshot } from "./raw-l1-family-derivation.derive-terminal.js";
import {
  type FraudProofOwnThreads,
  standingThreadOf,
} from "./raw-l1-family-derivation.own-thread.js";
import {
  type FraudProofRawL1FamilyDefinition,
  outputQuantity,
  ownsThread,
  rawToUtxo,
} from "./raw-l1-family-derivation.state-queue-topology.js";
import type {
  FraudProofRawL1Snapshot,
  FraudProofRawL1Utxo,
} from "./raw-l1-snapshot.js";

/** One place a family's thread can stand, in the family's physical chain
 * order: its address and the datum the thread carries there. */
export type FraudProofThreadPosition = Readonly<{
  address: string;
  datumSchema: Parameters<typeof Data.from>[1];
}>;

/**
 * Our threads over every physical position of a family whose thread runs
 * past the steps its snapshot scopes, read from the thread unit's history
 * from genesis: an output of that history at a position, holding one thread
 * unit under a datum naming our key, stands while no transaction of the
 * history spends it. Its step is its position's place in the chain. Among
 * standing threads the newest comes first, then the furthest step, then the
 * lowest out-ref, as `ownThread` orders them.
 */
export const deriveFraudProofRawL1OwnThreadsAtPositions = ({
  snapshot,
  definition,
  positions,
}: {
  readonly snapshot: FraudProofRawL1Snapshot;
  readonly definition: FraudProofRawL1FamilyDefinition;
  readonly positions: readonly FraudProofThreadPosition[];
}): FraudProofOwnThreads => {
  const { threadUnit } = authenticateFamilySnapshot({ snapshot, definition });
  const history = historicalTransactions({ snapshot, unit: threadUnit });
  const stepOf = (
    raw: FraudProofRawL1Utxo,
  ): ComputationThreadStep | undefined => {
    if (outputQuantity(raw, threadUnit) !== 1n) return undefined;
    const { address } = rawToUtxo(raw);
    const index = positions.findIndex(
      (position) =>
        position.address === address &&
        ownsThread({
          raw,
          schema: position.datumSchema,
          proverCredential: definition.proverCredential,
        }),
    );
    const step = index + 1;
    return index >= 0 && isComputationThreadStep(step) ? step : undefined;
  };
  const spent = new Set(
    history.flatMap(({ resolvedInputs }) =>
      resolvedInputs.map(({ outRef }) => outRef),
    ),
  );
  const standing = history.flatMap((transaction) => {
    const outputs = CML.TransactionBody.from_cbor_hex(
      transaction.bodyCbor,
    ).outputs();
    const created = [];
    for (let index = 0; index < outputs.len(); index += 1) {
      const output = outputs.get(index);
      const raw: FraudProofRawL1Utxo = {
        outRef: `${transaction.txHash}#${index.toString()}`,
        outputCbor: output.to_canonical_cbor_hex(),
        datumCbor: output.datum()?.as_datum()?.to_canonical_cbor_hex() ?? null,
        referenceScriptCbor:
          output.script_ref()?.to_canonical_cbor_hex() ?? null,
      };
      const step = spent.has(raw.outRef) ? undefined : stepOf(raw);
      if (step !== undefined)
        created.push({
          step,
          raw,
          slot: BigInt(transaction.inclusionPoint.slot),
        });
    }
    return created;
  });
  const newest = standing.reduce<(typeof standing)[number] | undefined>(
    (best, candidate) => {
      if (best === undefined) return candidate;
      if (candidate.slot !== best.slot)
        return candidate.slot > best.slot ? candidate : best;
      if (candidate.step !== best.step)
        return candidate.step > best.step ? candidate : best;
      return candidate.raw.outRef < best.raw.outRef ? candidate : best;
    },
    undefined,
  );
  const spends = history.flatMap((transaction) =>
    transaction.resolvedInputs
      .filter((raw) => stepOf(raw) !== undefined)
      .map((raw) => ({
        txHash: transaction.txHash,
        threadOutRef: raw.outRef,
        confirmationDepth: transaction.confirmationDepth,
      })),
  );
  return {
    ...(newest === undefined ? {} : { standing: standingThreadOf(newest) }),
    spends,
  };
};
