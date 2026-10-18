/** Complete `missingRedeemer` replay of one block: every accepted or
 * forced-valid transaction with a required observer, and every forced
 * `RedeemerMissing` rejection, is planned through the prover's verdict. */
import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
import type * as SDK from "@al-ft/midgard-sdk";

import { prepareDirectScriptMaterial } from "../direct-script-derivation/material.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CheckedLedger } from "../ledger-at.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import { eventSubject } from "../workflow/detection-subject.js";
import { collectReplayFindings } from "../workflow/replay-prerequisite.js";
import { missingRedeemerDetection } from "./prover.js";

export const MISSING_REDEEMER_VIOLATION_ID = "redeemer-missing";

/** The committed source a finding names: an accepted (0) or forced (1)
 * transaction, by its index in the block's committed list. */
export type MissingRedeemerCoordinate = Readonly<{
  sourceKind: 0 | 1;
  sourceIndex: number;
}>;

export const missingRedeemerDetectionId = (
  coordinate: MissingRedeemerCoordinate,
  observedIndex: bigint | null,
): string =>
  `${MISSING_REDEEMER_VIOLATION_ID}:${coordinate.sourceKind.toString()}:${coordinate.sourceIndex.toString()}:${observedIndex === null ? "claim" : observedIndex.toString()}`;

const hasRequiredObserver = (fullTransactionCbor: Buffer, forced: boolean) =>
  decodeMidgardFieldPreimage(
    (forced
      ? decodeMidgardForcedTxFullFromCanonicalCbor
      : decodeMidgardNativeTxFullFromCanonicalCbor)(fullTransactionCbor).body
      .requiredObserversPreimageCbor,
  ).length > 0;

const redeemerMissingReason = (verdict: SDK.OperatorVerdict) =>
  verdict !== "ForcedTxValid" &&
  typeof verdict.ForcedTxInvalid.reason === "object" &&
  "RedeemerMissing" in verdict.ForcedTxInvalid.reason;

/** The event a coordinate names; `null` when the block has no such source. */
export const missingRedeemerEventKey = (
  current: TransitionTraceReconstruction,
  { sourceKind, sourceIndex }: MissingRedeemerCoordinate,
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

/** The planned finding at one coordinate, or `null` when it does not convict
 * on a shape the prover carries. */
export const prepareMissingRedeemerReplay = async ({
  current,
  predecessor,
  coordinate,
}: {
  readonly current: TransitionTraceReconstruction;
  readonly predecessor?: CheckedLedger;
  readonly coordinate: MissingRedeemerCoordinate;
}) => {
  const eventKey = missingRedeemerEventKey(current, coordinate);
  if (eventKey === null) return null;
  const material = await prepareDirectScriptMaterial({
    current,
    eventKey,
    ...(predecessor === undefined ? {} : { predecessor }),
  });
  const detection = await missingRedeemerDetection(material);
  return detection === null
    ? null
    : {
        coordinate,
        eventKey,
        material,
        observedIndex: detection.observedIndex,
        detectionId: missingRedeemerDetectionId(
          coordinate,
          detection.observedIndex,
        ),
      };
};

export const detectMissingRedeemerReplay = async ({
  block,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor?: CheckedLedger;
}) => {
  const current = block.reconstruction;
  const candidates: MissingRedeemerCoordinate[] = [];
  for (const [sourceIndex, entry] of current.transactions.entries())
    if (hasRequiredObserver(entry.fullTransactionCbor, false))
      candidates.push({ sourceKind: 0, sourceIndex });
  for (const [sourceIndex, entry] of current.forcedTransactions.entries())
    if (
      entry.value.verdict === "ForcedTxValid"
        ? hasRequiredObserver(entry.fullTransactionCbor, true)
        : redeemerMissingReason(entry.value.verdict)
    )
      candidates.push({ sourceKind: 1, sourceIndex });
  return collectReplayFindings(
    candidates.map(async (coordinate) => {
      const prepared = await prepareMissingRedeemerReplay({
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
            violationId: MISSING_REDEEMER_VIOLATION_ID,
            position: BigInt(coordinate.sourceIndex),
            diagnostic:
              prepared.observedIndex === null
                ? "the forced rejection claims a missing redeemer the transaction does not lack"
                : `required observer ${prepared.observedIndex.toString()} has a redeemer-bearing source and no redeemer`,
            prepared,
          };
    }),
  );
};
