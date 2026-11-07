/** Complete `scriptIntegrityHashMismatch` replay of one block: every accepted
 * or forced-valid transaction, every forced rejection recording a script
 * integrity hash mismatch, and every forced rejection whose reason ranks after
 * the script-integrity phase, is planned through the prover's verdict. */
import * as SDK from "@al-ft/midgard-sdk";

import { prepareDirectScriptMaterial } from "../direct-script-derivation/material.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CheckedLedger } from "../ledger-at.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import { eventSubject } from "../workflow/detection-subject.js";
import {
  CanonicalReplayPrerequisiteError,
  collectReplayFindings,
} from "../workflow/replay-prerequisite.js";
import { scriptIntegrityHashMismatchDetection } from "./prover.js";

export const SCRIPT_INTEGRITY_HASH_MISMATCH_VIOLATION_ID =
  "script-integrity-hash-mismatch";

/** The script-integrity phase of the first-fault order. */
const SCRIPT_INTEGRITY_PHASE = 10;

/** The committed source a finding names: an accepted (0) or forced (1)
 * transaction, by its index in the block's committed list. */
export type ScriptIntegrityHashMismatchCoordinate = Readonly<{
  sourceKind: 0 | 1;
  sourceIndex: number;
}>;

/** `<violation>:<source kind>:<source index>`. */
export const scriptIntegrityHashMismatchDetectionId = (
  coordinate: ScriptIntegrityHashMismatchCoordinate,
): string =>
  `${SCRIPT_INTEGRITY_HASH_MISMATCH_VIOLATION_ID}:${coordinate.sourceKind.toString()}:${coordinate.sourceIndex.toString()}`;

/** A rejection this family can contradict: one recording this reason, or one
 * whose reason ranks after the script-integrity phase. The network context
 * moves only `NetworkIdMismatch`, which ranks before it either way. */
const contradictableRejection = (verdict: SDK.OperatorVerdict) =>
  verdict !== "ForcedTxValid" &&
  (verdict.ForcedTxInvalid.reason === "ScriptIntegrityHashMismatch" ||
    SDK.rejectionReasonPhase(verdict.ForcedTxInvalid.reason, {
      body_network_mismatch: true,
    }) > SCRIPT_INTEGRITY_PHASE);

/** A rejected leaf whose bytes the replay cannot open yields no finding; an
 * unresolved replay prerequisite still surfaces. */
const unlessUnopened = async <T>(task: () => Promise<T | null>) => {
  try {
    return await task();
  } catch (error) {
    if (error instanceof CanonicalReplayPrerequisiteError) throw error;
    return null;
  }
};

/** The event a coordinate names; `null` when the block has no such source. */
export const scriptIntegrityHashMismatchEventKey = (
  current: TransitionTraceReconstruction,
  { sourceKind, sourceIndex }: ScriptIntegrityHashMismatchCoordinate,
): SDK.EventKey | null => {
  if (sourceKind === 0) {
    const entry = current.transactions[sourceIndex];
    return entry === undefined
      ? null
      : { L2TransactionEventKey: { tx_id: entry.txId } };
  }
  const entry = current.forcedTransactions[sourceIndex];
  return entry === undefined
    ? null
    : { ForcedTransactionEventKey: { tx_order_id: entry.key } };
};

/** The planned finding at one coordinate, or `null` when it does not
 * convict. */
export const prepareScriptIntegrityHashMismatchReplay = async ({
  current,
  predecessor,
  coordinate,
}: {
  readonly current: TransitionTraceReconstruction;
  readonly predecessor?: CheckedLedger;
  readonly coordinate: ScriptIntegrityHashMismatchCoordinate;
}) => {
  const eventKey = scriptIntegrityHashMismatchEventKey(current, coordinate);
  if (eventKey === null) return null;
  const prepare = async () => {
    const material = await prepareDirectScriptMaterial({
      current,
      eventKey,
      ...(predecessor === undefined ? {} : { predecessor }),
    });
    const detection = await scriptIntegrityHashMismatchDetection(material);
    return detection === null
      ? null
      : {
          coordinate,
          eventKey,
          material,
          mismatch: detection.mismatch,
          detectionId: scriptIntegrityHashMismatchDetectionId(coordinate),
        };
  };
  const verdict =
    coordinate.sourceKind === 1
      ? current.forcedTransactions[coordinate.sourceIndex]?.value.verdict
      : undefined;
  return verdict !== undefined && verdict !== "ForcedTxValid"
    ? await unlessUnopened(prepare)
    : await prepare();
};

export const detectScriptIntegrityHashMismatchReplay = async ({
  block,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor?: CheckedLedger;
}) => {
  const current = block.reconstruction;
  const candidates: ScriptIntegrityHashMismatchCoordinate[] = [
    ...current.transactions.map((_, sourceIndex) => ({
      sourceKind: 0 as const,
      sourceIndex,
    })),
    ...current.forcedTransactions.flatMap((entry, sourceIndex) =>
      entry.value.verdict === "ForcedTxValid" ||
      contradictableRejection(entry.value.verdict)
        ? [{ sourceKind: 1 as const, sourceIndex }]
        : [],
    ),
  ];
  return collectReplayFindings(
    candidates.map(async (coordinate) => {
      const prepared = await prepareScriptIntegrityHashMismatchReplay({
        current,
        coordinate,
        ...(predecessor === undefined ? {} : { predecessor }),
      });
      return prepared === null
        ? null
        : {
            ...eventSubject(prepared.eventKey),
            detectionId: prepared.detectionId,
            headerHash: block.headerHash,
            violationId: SCRIPT_INTEGRITY_HASH_MISMATCH_VIOLATION_ID,
            position: BigInt(coordinate.sourceIndex),
            diagnostic: prepared.mismatch
              ? "the committed script integrity hash differs from the one the transaction's executions require"
              : "the forced rejection claims a script integrity hash mismatch the transaction does not have",
            prepared,
          };
    }),
  );
};
