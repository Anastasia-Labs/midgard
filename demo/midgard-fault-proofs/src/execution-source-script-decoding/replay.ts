/** Complete `executionSourceScriptDecoding` replay of one block: every forced
 * rejection recorded with a native script decoding arm is planned through the
 * prover's verdict. A leaf whose bytes the replay cannot open, or whose
 * inputs have no predecessor descriptor, yields no finding. */
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
import { executionSourceScriptDecodingDetection } from "./prover.js";

/** Violation id of each recorded arm. */
const VIOLATION_IDS: Readonly<Record<string, string>> = {
  ExecutionNativeScriptMalformed: "execution-native-script-malformed",
  ExecutionNativeScriptNodeLimit: "execution-native-script-node-limit",
  ExecutionNativeScriptDepthLimit: "execution-native-script-depth-limit",
};

export type ExecutionSourceScriptDecodingCoordinate =
  DirectPurposeSourceCoordinate;

const recordedArm = (verdict: SDK.OperatorVerdict | undefined) => {
  if (verdict === undefined || verdict === "ForcedTxValid") return null;
  const arm = SDK.rejectionReasonArmOf(verdict.ForcedTxInvalid.reason);
  return SDK.EXECUTION_SOURCE_SCRIPT_DECODING_ARMS.includes(arm) ? arm : null;
};

/** `<violation id>:<source kind>:<source index>`. */
export const executionSourceScriptDecodingDetectionId = (
  violationId: string,
  coordinate: ExecutionSourceScriptDecodingCoordinate,
): string =>
  `${violationId}:${coordinate.sourceKind.toString()}:${coordinate.sourceIndex.toString()}`;

/** The planned finding at one coordinate, or `null` when it does not
 * convict. */
export const prepareExecutionSourceScriptDecodingReplay = async ({
  current,
  predecessor,
  coordinate,
}: {
  readonly current: TransitionTraceReconstruction;
  readonly predecessor?: CheckedLedger;
  readonly coordinate: ExecutionSourceScriptDecodingCoordinate;
}) => {
  if (coordinate.sourceKind !== 1) return null;
  const arm = recordedArm(
    current.forcedTransactions[coordinate.sourceIndex]?.value.verdict,
  );
  const eventKey = directPurposeEventKey(current, coordinate);
  if (arm === null || eventKey === null) return null;
  const violationId = VIOLATION_IDS[arm]!;
  return await unlessUnopened(async () => {
    const material = await prepareDirectScriptMaterial({
      current,
      eventKey,
      ...(predecessor === undefined ? {} : { predecessor }),
    });
    return executionSourceScriptDecodingDetection(material) === null
      ? null
      : {
          coordinate,
          eventKey,
          material,
          violationId,
          detectionId: executionSourceScriptDecodingDetectionId(
            violationId,
            coordinate,
          ),
        };
  });
};

export const detectExecutionSourceScriptDecodingReplay = async ({
  block,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor?: CheckedLedger;
}) => {
  const current = block.reconstruction;
  const candidates: ExecutionSourceScriptDecodingCoordinate[] = [];
  for (const [sourceIndex, entry] of current.forcedTransactions.entries())
    if (recordedArm(entry.value.verdict) !== null)
      candidates.push({ sourceKind: 1, sourceIndex });
  return collectReplayFindings(
    candidates.map(async (coordinate) => {
      const prepared = await prepareExecutionSourceScriptDecodingReplay({
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
            violationId: prepared.violationId,
            position: BigInt(coordinate.sourceIndex),
            diagnostic:
              "the forced rejection records a native script decoding reason the protocol limits cannot produce",
            prepared,
          };
    }),
  );
};
