/** Complete `scriptIntegrityHashMismatch` replay of one block: every accepted
 * or forced-valid transaction, every forced rejection recording a script
 * integrity hash mismatch, and every forced rejection whose reason ranks after
 * the script-integrity phase, is planned through the prover's verdict. */
import type * as SDK from "@al-ft/midgard-sdk/core";

import { directPurposeArtifactFamily } from "../direct-purpose-family/artifact-family.js";
import {
  acceptedScriptSources,
  directPurposeEventKey,
  type DirectPurposeSourceCoordinate,
  unlessUnopened,
} from "../direct-purpose-family/replay-support.js";
import { prepareDirectScriptMaterial } from "../direct-script-derivation/material.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { claimedReasonReachesPhase } from "../evidence/claimed-coordinate.js";
import type { Ledger } from "../ledger-store/index.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import { eventSubject } from "../workflow/detection-subject.js";
import { collectReplayFindings } from "../workflow/replay-prerequisite.js";
import {
  requireConvictingScriptIntegrityHashMismatchProof,
  scriptIntegrityHashMismatchDetection,
  scriptIntegrityHashMismatchPaging,
} from "./prover.js";

export const SCRIPT_INTEGRITY_HASH_MISMATCH_VIOLATION_ID =
  "script-integrity-hash-mismatch";

/** The script-integrity phase of the first-fault order. */
const SCRIPT_INTEGRITY_PHASE = 10;

export type ScriptIntegrityHashMismatchCoordinate =
  DirectPurposeSourceCoordinate;

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
    claimedReasonReachesPhase(
      verdict.ForcedTxInvalid.reason,
      { body_network_mismatch: true },
      SCRIPT_INTEGRITY_PHASE + 1,
    ));

/** The planned finding at one coordinate, or `null` when it does not
 * convict. */
export const prepareScriptIntegrityHashMismatchReplay = async ({
  current,
  predecessor,
  coordinate,
}: {
  readonly current: TransitionTraceReconstruction;
  readonly predecessor?: Ledger;
  readonly coordinate: ScriptIntegrityHashMismatchCoordinate;
}) => {
  const eventKey = directPurposeEventKey(current, coordinate);
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
  readonly predecessor?: Ledger;
}) => {
  const current = block.reconstruction;
  const candidates: ScriptIntegrityHashMismatchCoordinate[] = [
    ...acceptedScriptSources(current).map(({ sourceIndex }) => ({
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
  return collectReplayFindings(candidates, async (coordinate) => {
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
  });
};

/** The `scriptIntegrityHashMismatch` workflow artifact: the retained
 * payload, the predecessor ledger and the coordinate, re-planned on every
 * admission. */
export const SCRIPT_INTEGRITY_HASH_MISMATCH_ARTIFACT_FAMILY =
  directPurposeArtifactFamily({
    label: "scriptIntegrityHashMismatch",
    artifactVersion:
      "midgard-script-integrity-hash-mismatch-workflow-artifact-v1",
    prepare: prepareScriptIntegrityHashMismatchReplay,
    detect: detectScriptIntegrityHashMismatchReplay,
    prove: (admitted, contracts, prover) =>
      requireConvictingScriptIntegrityHashMismatchProof({
        material: admitted.material,
        contracts,
        prover,
      }),
    paging: scriptIntegrityHashMismatchPaging,
  });
