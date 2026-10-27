/** Complete `executionNativeScriptInvalid` replay of one block: every
 * accepted or forced-valid transaction that can carry a script purpose,
 * every forced `ExecutionNativeScriptFalse` rejection, and every forced
 * rejection whose reason some network context places at or after the
 * execution phase, is planned through the prover's verdict. A leaf whose
 * bytes the replay cannot open yields no finding. */
import * as SDK from "@al-ft/midgard-sdk";

import {
  DIRECT_PURPOSE_NAMES,
  directPurposeEventKey,
  directPurposeFieldPresent,
  type DirectPurposeSourceCoordinate,
  unlessUnopened,
} from "../direct-purpose-family/replay-support.js";
import { prepareDirectScriptMaterial } from "../direct-script-derivation/material.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CheckedLedger } from "../ledger-at.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import { eventSubject } from "../workflow/detection-subject.js";
import { collectReplayFindings } from "../workflow/replay-prerequisite.js";
import { executionNativeScriptInvalidDetection } from "./prover.js";

export const EXECUTION_NATIVE_SCRIPT_INVALID_VIOLATION_ID =
  "execution-native-script-invalid";

/** The execution phase's rank in the first-fault order. */
const EXECUTION_PHASE = 9;

export type ExecutionNativeScriptInvalidCoordinate =
  DirectPurposeSourceCoordinate;

/** `<violation>:<source kind>:<source index>:<purpose kind>:<purpose index>`. */
export const executionNativeScriptInvalidDetectionId = (
  coordinate: ExecutionNativeScriptInvalidCoordinate,
  purpose: SDK.DirectPurposeCoordinate,
): string =>
  `${EXECUTION_NATIVE_SCRIPT_INVALID_VIOLATION_ID}:${coordinate.sourceKind.toString()}:${coordinate.sourceIndex.toString()}:${purpose.kind.toString()}:${purpose.index.toString()}`;

/** A rejection whose reason some network context places at or after the
 * execution phase; the prover decides admission under the bound context. */
const executionPhaseRejection = (verdict: SDK.OperatorVerdict): boolean =>
  verdict !== "ForcedTxValid" &&
  [true, false].some(
    (body_network_mismatch) =>
      SDK.rejectionReasonPhase(verdict.ForcedTxInvalid.reason, {
        body_network_mismatch,
      }) >= EXECUTION_PHASE,
  );

/** The planned finding at one coordinate, or `null` when it does not
 * convict or its bytes do not open. */
export const prepareExecutionNativeScriptInvalidReplay = async ({
  current,
  predecessor,
  coordinate,
}: {
  readonly current: TransitionTraceReconstruction;
  readonly predecessor?: CheckedLedger;
  readonly coordinate: ExecutionNativeScriptInvalidCoordinate;
}) => {
  const eventKey = directPurposeEventKey(current, coordinate);
  if (eventKey === null) return null;
  return await unlessUnopened(async () => {
    const material = await prepareDirectScriptMaterial({
      current,
      eventKey,
      ...(predecessor === undefined ? {} : { predecessor }),
    });
    const detection = await executionNativeScriptInvalidDetection(material);
    if (detection === null) return null;
    const purpose = SDK.executionNativeScriptInvalidFactCoordinate(
      detection.final.fact,
    );
    return {
      coordinate,
      eventKey,
      material,
      entry: detection.entry,
      final: detection.final,
      detectionId: executionNativeScriptInvalidDetectionId(coordinate, purpose),
    };
  });
};

export const detectExecutionNativeScriptInvalidReplay = async ({
  block,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor?: CheckedLedger;
}) => {
  const current = block.reconstruction;
  const candidates: ExecutionNativeScriptInvalidCoordinate[] = [];
  const present = (cbor: Buffer, forced: boolean) => {
    try {
      return directPurposeFieldPresent(cbor, forced);
    } catch {
      return false;
    }
  };
  for (const [sourceIndex, entry] of current.transactions.entries())
    if (present(entry.fullTransactionCbor, false))
      candidates.push({ sourceKind: 0, sourceIndex });
  for (const [sourceIndex, entry] of current.forcedTransactions.entries())
    if (
      entry.value.verdict === "ForcedTxValid"
        ? present(entry.fullTransactionCbor, true)
        : executionPhaseRejection(entry.value.verdict)
    )
      candidates.push({ sourceKind: 1, sourceIndex });
  return collectReplayFindings(
    candidates.map(async (coordinate) => {
      const prepared = await prepareExecutionNativeScriptInvalidReplay({
        current,
        coordinate,
        ...(predecessor === undefined ? {} : { predecessor }),
      });
      if (prepared === null) return null;
      const purpose = SDK.executionNativeScriptInvalidFactCoordinate(
        prepared.final.fact,
      );
      const name =
        DIRECT_PURPOSE_NAMES[Number(purpose.kind)] ?? "invalid coordinate";
      return {
        ...eventSubject(prepared.eventKey),
        detectionId: prepared.detectionId,
        headerHash: block.headerHash,
        violationId: EXECUTION_NATIVE_SCRIPT_INVALID_VIOLATION_ID,
        position: BigInt(coordinate.sourceIndex),
        diagnostic:
          "Evaluated" in prepared.final.fact &&
          !prepared.final.fact.Evaluated.satisfied
            ? `the native script of ${name} purpose ${purpose.index.toString()} evaluates false`
            : `the rejection claims the native script of ${name} purpose ${purpose.index.toString()} evaluates false, but it does not`,
        prepared,
      };
    }),
  );
};
