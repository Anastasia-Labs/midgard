/** Complete `unusedScriptWitness` replay of one block: every accepted or
 * forced-valid transaction that carries a script witness, every forced
 * `UnusedScriptWitness` rejection, and every forced rejection with a script
 * witness whose reason is recorded at or after the unused audit, is planned
 * through the prover's verdict. */
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
import { unusedScriptWitnessDetection } from "./prover.js";

export const UNUSED_SCRIPT_WITNESS_VIOLATION_ID = "unused-script-witness";

export type UnusedScriptWitnessCoordinate = DirectPurposeSourceCoordinate;

/** `<violation>:<source kind>:<source index>:<witness index>`. */
export const unusedScriptWitnessDetectionId = (
  coordinate: UnusedScriptWitnessCoordinate,
  scriptIndex: bigint,
): string =>
  `${UNUSED_SCRIPT_WITNESS_VIOLATION_ID}:${coordinate.sourceKind.toString()}:${coordinate.sourceIndex.toString()}:${scriptIndex.toString()}`;

/** Whether the transaction's field 6 carries a script witness. */
const witnessesPresent = (
  fullTransactionCbor: Buffer,
  forced: boolean,
): boolean => {
  const { witnessSet } = (
    forced
      ? decodeMidgardForcedTxFullFromCanonicalCbor
      : decodeMidgardNativeTxFullFromCanonicalCbor
  )(fullTransactionCbor);
  return (
    decodeMidgardFieldPreimage(witnessSet.scriptTxWitsPreimageCbor).length > 0
  );
};

const isUnusedScriptWitnessRejection = (
  verdict: SDK.OperatorVerdict,
): boolean =>
  verdict !== "ForcedTxValid" &&
  typeof verdict.ForcedTxInvalid.reason === "object" &&
  "UnusedScriptWitness" in verdict.ForcedTxInvalid.reason;

/** A rejection whose reason some network context places at or after the
 * unused audit; the prover decides admission under the bound context. */
const auditPhaseRejection = (verdict: SDK.OperatorVerdict): boolean =>
  verdict !== "ForcedTxValid" &&
  [true, false].some(
    (body_network_mismatch) =>
      SDK.rejectionReasonPhase(verdict.ForcedTxInvalid.reason, {
        body_network_mismatch,
      }) >= 8,
  );

/** The planned finding at one coordinate, or `null` when it does not
 * convict on a shape the prover carries. */
export const prepareUnusedScriptWitnessReplay = async ({
  current,
  predecessor,
  coordinate,
}: {
  readonly current: TransitionTraceReconstruction;
  readonly predecessor?: CheckedLedger;
  readonly coordinate: UnusedScriptWitnessCoordinate;
}) => {
  const eventKey = directPurposeEventKey(current, coordinate);
  if (eventKey === null) return null;
  const prepare = async () => {
    const material = await prepareDirectScriptMaterial({
      current,
      eventKey,
      ...(predecessor === undefined ? {} : { predecessor }),
    });
    const detection = await unusedScriptWitnessDetection(material);
    return detection === null
      ? null
      : {
          coordinate,
          eventKey,
          material,
          scriptIndex: detection.scriptIndex,
          unused: detection.unused,
          detectionId: unusedScriptWitnessDetectionId(
            coordinate,
            detection.scriptIndex,
          ),
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

export const detectUnusedScriptWitnessReplay = async ({
  block,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor?: CheckedLedger;
}) => {
  const current = block.reconstruction;
  const candidates: UnusedScriptWitnessCoordinate[] = [];
  for (const [sourceIndex, entry] of current.transactions.entries())
    if (witnessesPresent(entry.fullTransactionCbor, false))
      candidates.push({ sourceKind: 0, sourceIndex });
  for (const [sourceIndex, entry] of current.forcedTransactions.entries()) {
    const verdict = entry.value.verdict;
    if (
      isUnusedScriptWitnessRejection(verdict) ||
      ((verdict === "ForcedTxValid" || auditPhaseRejection(verdict)) &&
        witnessesPresent(entry.fullTransactionCbor, true))
    )
      candidates.push({ sourceKind: 1, sourceIndex });
  }
  return collectReplayFindings(
    candidates.map(async (coordinate) => {
      const prepared = await prepareUnusedScriptWitnessReplay({
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
            violationId: UNUSED_SCRIPT_WITNESS_VIOLATION_ID,
            position: BigInt(coordinate.sourceIndex),
            diagnostic: prepared.unused
              ? `script witness ${prepared.scriptIndex.toString()} is unused`
              : `the rejection claims script witness ${prepared.scriptIndex.toString()} is unused, but it is used or absent`,
            prepared,
          };
    }),
  );
};
