/** Complete `missingRedeemer` replay of one block: every accepted or
 * forced-valid transaction that can carry a script purpose (a spend, an
 * output, a required observer or a mint), every forced `RedeemerMissing`
 * rejection, and every forced rejection whose reason names no purpose and is
 * not recorded before the purpose phase, is planned through the prover's
 * verdict. */
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
import { missingRedeemerDetection } from "./prover.js";

export const MISSING_REDEEMER_VIOLATION_ID = "redeemer-missing";

export type MissingRedeemerCoordinate = DirectPurposeSourceCoordinate;

/** `<source kind>:<source index>:` then the observed purpose's
 * `<kind>:<index>`, or `claim` for a rejection's claim verdict. */
export const missingRedeemerDetectionId = (
  coordinate: MissingRedeemerCoordinate,
  observed: SDK.DirectPurposeCoordinate | null,
): string =>
  `${MISSING_REDEEMER_VIOLATION_ID}:${coordinate.sourceKind.toString()}:${coordinate.sourceIndex.toString()}:${observed === null ? "claim" : `${observed.kind.toString()}:${observed.index.toString()}`}`;

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
  const eventKey = directPurposeEventKey(current, coordinate);
  if (eventKey === null) return null;
  const prepare = async () => {
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
          observed: detection.observed,
          detectionId: missingRedeemerDetectionId(
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
    if (directPurposeFieldPresent(entry.fullTransactionCbor, false))
      candidates.push({ sourceKind: 0, sourceIndex });
  for (const [sourceIndex, entry] of current.forcedTransactions.entries())
    if (
      entry.value.verdict === "ForcedTxValid"
        ? directPurposeFieldPresent(entry.fullTransactionCbor, true)
        : rejectionReasonIs(entry.value.verdict, "RedeemerMissing") ||
          purposePhaseRejection(entry.value.verdict)
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
              prepared.observed === null
                ? "the forced rejection claims a missing redeemer the transaction does not lack"
                : `${DIRECT_PURPOSE_NAMES[Number(prepared.observed.kind)]!} purpose ${prepared.observed.index.toString()} has a redeemer-bearing source and no redeemer`,
            prepared,
          };
    }),
  );
};
