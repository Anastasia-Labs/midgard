/** Complete `missingScriptSource` replay of one block: every accepted or
 * forced-valid transaction that can carry a script purpose (a spend, an
 * output, a required observer or a mint), every forced `ScriptSourceMissing`
 * or `RedeemerMissing` rejection, and every forced rejection whose reason
 * names no purpose and is not recorded before the purpose phase, is planned
 * through the prover's verdict. */
import type * as SDK from "@al-ft/midgard-sdk";

import {
  DIRECT_PURPOSE_NAMES,
  directPurposeEventKey,
  directPurposeFieldPresent,
  type DirectPurposeSourceCoordinate,
  purposelessRejection,
  purposePhaseRejection,
  rejectionReasonIs,
  unlessUnopened,
} from "../direct-purpose-family/replay-support.js";
import { prepareDirectScriptMaterial } from "../direct-script-derivation/material.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CheckedLedger } from "../ledger-at.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import { eventSubject } from "../workflow/detection-subject.js";
import { collectReplayFindings } from "../workflow/replay-prerequisite.js";
import { missingScriptSourceDetection } from "./prover.js";

export const MISSING_SCRIPT_SOURCE_VIOLATION_ID = "script-source-missing";

export type MissingScriptSourceCoordinate = DirectPurposeSourceCoordinate;

/** `<source kind>:<source index>:` then the observed purpose's
 * `<kind>:<index>`, or `claim` for a rejection's claim verdict. */
export const missingScriptSourceDetectionId = (
  coordinate: MissingScriptSourceCoordinate,
  observed: SDK.DirectPurposeCoordinate | null,
): string =>
  `${MISSING_SCRIPT_SOURCE_VIOLATION_ID}:${coordinate.sourceKind.toString()}:${coordinate.sourceIndex.toString()}:${observed === null ? "claim" : `${observed.kind.toString()}:${observed.index.toString()}`}`;

/** The planned finding at one coordinate, or `null` when it does not convict
 * on a shape the prover carries. */
export const prepareMissingScriptSourceReplay = async ({
  current,
  predecessor,
  coordinate,
}: {
  readonly current: TransitionTraceReconstruction;
  readonly predecessor?: CheckedLedger;
  readonly coordinate: MissingScriptSourceCoordinate;
}) => {
  const eventKey = directPurposeEventKey(current, coordinate);
  if (eventKey === null) return null;
  const prepare = async () => {
    const material = await prepareDirectScriptMaterial({
      current,
      eventKey,
      ...(predecessor === undefined ? {} : { predecessor }),
    });
    const detection = await missingScriptSourceDetection(material);
    return detection === null
      ? null
      : {
          coordinate,
          eventKey,
          material,
          observed: detection.observed,
          detectionId: missingScriptSourceDetectionId(
            coordinate,
            detection.observed,
          ),
        };
  };
  return coordinate.sourceKind === 1 &&
    purposelessRejection(
      current.forcedTransactions[coordinate.sourceIndex]?.value.verdict,
    )
    ? await unlessUnopened(prepare)
    : await prepare();
};

export const detectMissingScriptSourceReplay = async ({
  block,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor?: CheckedLedger;
}) => {
  const current = block.reconstruction;
  const candidates: MissingScriptSourceCoordinate[] = [];
  for (const [sourceIndex, entry] of current.transactions.entries())
    if (directPurposeFieldPresent(entry.fullTransactionCbor, false))
      candidates.push({ sourceKind: 0, sourceIndex });
  for (const [sourceIndex, entry] of current.forcedTransactions.entries())
    if (
      entry.value.verdict === "ForcedTxValid"
        ? directPurposeFieldPresent(entry.fullTransactionCbor, true)
        : rejectionReasonIs(entry.value.verdict, "ScriptSourceMissing") ||
          rejectionReasonIs(entry.value.verdict, "RedeemerMissing") ||
          purposePhaseRejection(entry.value.verdict)
    )
      candidates.push({ sourceKind: 1, sourceIndex });
  return collectReplayFindings(
    candidates.map(async (coordinate) => {
      const prepared = await prepareMissingScriptSourceReplay({
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
            violationId: MISSING_SCRIPT_SOURCE_VIOLATION_ID,
            position: BigInt(coordinate.sourceIndex),
            diagnostic:
              prepared.observed === null
                ? "the forced rejection claims a missing script source the transaction does not lack"
                : `${DIRECT_PURPOSE_NAMES[Number(prepared.observed.kind)]!} purpose ${prepared.observed.index.toString()} has no script source`,
            prepared,
          };
    }),
  );
};
