/** Complete `distinctAssetAccumulationLimit` replay of one block: every
 * accepted or forced-valid transaction, and every forced rejection whose
 * reason ranks in or after the value phase, is planned through the prover's
 * verdict. */
import * as SDK from "@al-ft/midgard-sdk";

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
import type { CheckedLedger } from "../ledger-at.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import { eventSubject } from "../workflow/detection-subject.js";
import { collectReplayFindings } from "../workflow/replay-prerequisite.js";
import {
  distinctAssetAccumulationLimitDetection,
  distinctAssetAccumulationLimitPaging,
  requireConvictingDistinctAssetAccumulationLimitProof,
} from "./prover.js";

export const DISTINCT_ASSET_ACCUMULATION_LIMIT_VIOLATION_ID =
  "distinct-asset-accumulation-limit";

/** The value phase of the first-fault order, which holds the three
 * accumulation-limit arms. */
const VALUE_PHASE = 12;

export type DistinctAssetAccumulationLimitCoordinate =
  DirectPurposeSourceCoordinate;

/** `<violation>:<source kind>:<source index>`. */
export const distinctAssetAccumulationLimitDetectionId = (
  coordinate: DistinctAssetAccumulationLimitCoordinate,
): string =>
  `${DISTINCT_ASSET_ACCUMULATION_LIMIT_VIOLATION_ID}:${coordinate.sourceKind.toString()}:${coordinate.sourceIndex.toString()}`;

/** A rejection this family can contradict: one whose reason ranks in or
 * after the value phase. The network context moves only `NetworkIdMismatch`,
 * which ranks before it either way. */
const contradictableRejection = (verdict: SDK.OperatorVerdict) =>
  verdict !== "ForcedTxValid" &&
  claimedReasonReachesPhase(
    verdict.ForcedTxInvalid.reason,
    { body_network_mismatch: true },
    VALUE_PHASE,
  );

/** The planned finding at one coordinate, or `null` when it does not
 * convict. */
export const prepareDistinctAssetAccumulationLimitReplay = async ({
  current,
  predecessor,
  coordinate,
}: {
  readonly current: TransitionTraceReconstruction;
  readonly predecessor?: CheckedLedger;
  readonly coordinate: DistinctAssetAccumulationLimitCoordinate;
}) => {
  const eventKey = directPurposeEventKey(current, coordinate);
  if (eventKey === null) return null;
  const prepare = async () => {
    const material = await prepareDirectScriptMaterial({
      current,
      eventKey,
      ...(predecessor === undefined ? {} : { predecessor }),
    });
    const detection = distinctAssetAccumulationLimitDetection(material);
    return detection === null
      ? null
      : {
          coordinate,
          eventKey,
          material,
          crossing: detection.crossing,
          detectionId: distinctAssetAccumulationLimitDetectionId(coordinate),
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

export const detectDistinctAssetAccumulationLimitReplay = async ({
  block,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor?: CheckedLedger;
}) => {
  const current = block.reconstruction;
  const candidates: DistinctAssetAccumulationLimitCoordinate[] = [
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
    const prepared = await prepareDistinctAssetAccumulationLimitReplay({
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
          violationId: DISTINCT_ASSET_ACCUMULATION_LIMIT_VIOLATION_ID,
          position: BigInt(coordinate.sourceIndex),
          diagnostic:
            prepared.crossing !== null
              ? "an asset entry of the transaction exceeds the distinct-asset accumulation limit"
              : "the forced rejection claims an asset accumulation-limit crossing the transaction does not have",
          prepared,
        };
  });
};

/** The journaled `distinctAssetAccumulationLimit` workflow artifact: the
 * retained payload, the predecessor ledger and the coordinate, re-planned on
 * every admission. */
export const DISTINCT_ASSET_ACCUMULATION_LIMIT_ARTIFACT_FAMILY =
  directPurposeArtifactFamily({
    label: "distinctAssetAccumulationLimit",
    artifactVersion:
      "midgard-distinct-asset-accumulation-limit-workflow-artifact-v1",
    prepare: prepareDistinctAssetAccumulationLimitReplay,
    detect: detectDistinctAssetAccumulationLimitReplay,
    prove: (admitted, contracts, prover) =>
      requireConvictingDistinctAssetAccumulationLimitProof({
        material: admitted.material,
        contracts,
        prover,
      }),
    paging: distinctAssetAccumulationLimitPaging,
  });
