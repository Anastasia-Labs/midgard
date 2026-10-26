/** Complete `unusedRedeemer` replay of one block: every accepted transaction
 * that carries a redeemer, every forced `UnusedRedeemer` rejection, and every
 * forced rejection with a redeemer whose reason is recorded at or after the
 * unused audit, is planned through the prover's verdict. An admitted forced
 * transaction carries no redeemers. */
import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";

import {
  directPurposeEventKey,
  type DirectPurposeSourceCoordinate,
  unlessUnopened,
} from "../direct-purpose-family/replay-support.js";
import { prepareDirectScriptMaterial } from "../direct-script-derivation/material.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CheckedLedger } from "../ledger-at.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import { eventSubject } from "../workflow/detection-subject.js";
import { collectReplayFindings } from "../workflow/replay-prerequisite.js";
import { unusedRedeemerDetection } from "./prover.js";

export const UNUSED_REDEEMER_VIOLATION_ID = "unused-redeemer";

export type UnusedRedeemerCoordinate = DirectPurposeSourceCoordinate;

/** `<source kind>:<source index>:<raw field-8 index>`. */
export const unusedRedeemerDetectionId = (
  coordinate: UnusedRedeemerCoordinate,
  redeemerIndex: bigint,
): string =>
  `${UNUSED_REDEEMER_VIOLATION_ID}:${coordinate.sourceKind.toString()}:${coordinate.sourceIndex.toString()}:${redeemerIndex.toString()}`;

/** Whether the transaction's field 8 carries a redeemer. */
const redeemersPresent = (
  fullTransactionCbor: Buffer,
  forced: boolean,
): boolean => {
  const { witnessSet } = (
    forced
      ? decodeMidgardForcedTxFullFromCanonicalCbor
      : decodeMidgardNativeTxFullFromCanonicalCbor
  )(fullTransactionCbor);
  return (
    decodeMidgardFieldPreimage(witnessSet.redeemerTxWitsPreimageCbor).length > 0
  );
};

const isUnusedRedeemerRejection = (verdict: SDK.OperatorVerdict): boolean =>
  verdict !== "ForcedTxValid" &&
  typeof verdict.ForcedTxInvalid.reason === "object" &&
  "UnusedRedeemer" in verdict.ForcedTxInvalid.reason;

/** A rejection whose reason some network context places at or after the
 * unused audit; the prover decides admission under the bound context. */
const auditPhaseRejection = (verdict: SDK.OperatorVerdict): boolean =>
  verdict !== "ForcedTxValid" &&
  SDK.rejectionReasonPhase(verdict.ForcedTxInvalid.reason, {
    body_network_mismatch: false,
  }) >= 8;

/** The planned finding at one coordinate, or `null` when it does not convict
 * on a shape the prover carries. */
export const prepareUnusedRedeemerReplay = async ({
  current,
  predecessor,
  coordinate,
}: {
  readonly current: TransitionTraceReconstruction;
  readonly predecessor?: CheckedLedger;
  readonly coordinate: UnusedRedeemerCoordinate;
}) => {
  const eventKey = directPurposeEventKey(current, coordinate);
  if (eventKey === null) return null;
  const prepare = async () => {
    const material = await prepareDirectScriptMaterial({
      current,
      eventKey,
      ...(predecessor === undefined ? {} : { predecessor }),
    });
    const detection = await unusedRedeemerDetection(material);
    return detection === null
      ? null
      : {
          coordinate,
          eventKey,
          material,
          redeemerIndex: detection.redeemerIndex,
          detectionId: unusedRedeemerDetectionId(
            coordinate,
            detection.redeemerIndex,
          ),
        };
  };
  return coordinate.sourceKind === 1
    ? await unlessUnopened(prepare)
    : await prepare();
};

export const detectUnusedRedeemerReplay = async ({
  block,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor?: CheckedLedger;
}) => {
  const current = block.reconstruction;
  const candidates: UnusedRedeemerCoordinate[] = [];
  for (const [sourceIndex, entry] of current.transactions.entries())
    if (redeemersPresent(entry.fullTransactionCbor, false))
      candidates.push({ sourceKind: 0, sourceIndex });
  for (const [sourceIndex, entry] of current.forcedTransactions.entries()) {
    const verdict = entry.value.verdict;
    if (
      isUnusedRedeemerRejection(verdict) ||
      (auditPhaseRejection(verdict) &&
        redeemersPresent(entry.fullTransactionCbor, true))
    )
      candidates.push({ sourceKind: 1, sourceIndex });
  }
  return collectReplayFindings(
    candidates.map(async (coordinate) => {
      const prepared = await prepareUnusedRedeemerReplay({
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
            violationId: UNUSED_REDEEMER_VIOLATION_ID,
            position: BigInt(coordinate.sourceIndex),
            diagnostic: `redeemer ${prepared.redeemerIndex.toString()} refutes the recorded verdict`,
            prepared,
          };
    }),
  );
};
