/** Complete `missingRedeemer` replay of one block: every accepted or
 * forced-valid transaction that can carry a script purpose (a spend, an
 * output, a required observer or a mint), every forced `RedeemerMissing`
 * rejection, and every forced rejection whose reason names no purpose and is
 * not recorded before the purpose phase, is planned through the prover's
 * verdict. */
import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
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
import { missingRedeemerDetection } from "./prover.js";

export const MISSING_REDEEMER_VIOLATION_ID = "redeemer-missing";

/** The committed source a finding names: an accepted (0) or forced (1)
 * transaction, by its index in the block's committed list. */
export type MissingRedeemerCoordinate = Readonly<{
  sourceKind: 0 | 1;
  sourceIndex: number;
}>;

/** `<source kind>:<source index>:` then the observed purpose's
 * `<kind>:<index>`, or `claim` for a rejection's claim verdict. */
export const missingRedeemerDetectionId = (
  coordinate: MissingRedeemerCoordinate,
  observed: SDK.DirectPurposeCoordinate | null,
): string =>
  `${MISSING_REDEEMER_VIOLATION_ID}:${coordinate.sourceKind.toString()}:${coordinate.sourceIndex.toString()}:${observed === null ? "claim" : `${observed.kind.toString()}:${observed.index.toString()}`}`;

const PURPOSE_NAMES = ["spend", "mint", "required observer", "receive"];

const hasPurposeField = (fullTransactionCbor: Buffer, forced: boolean) => {
  const { body } = (
    forced
      ? decodeMidgardForcedTxFullFromCanonicalCbor
      : decodeMidgardNativeTxFullFromCanonicalCbor
  )(fullTransactionCbor);
  return [
    body.spendInputsPreimageCbor,
    body.outputsPreimageCbor,
    body.requiredObserversPreimageCbor,
    body.mintPreimageCbor,
  ].some((preimage) => decodeMidgardFieldPreimage(preimage).length > 0);
};

const redeemerMissingReason = (verdict: SDK.OperatorVerdict) =>
  verdict !== "ForcedTxValid" &&
  typeof verdict.ForcedTxInvalid.reason === "object" &&
  "RedeemerMissing" in verdict.ForcedTxInvalid.reason;

/** A rejection whose reason names no purpose coordinate. */
const purposelessReason = (
  verdict: SDK.OperatorVerdict | undefined,
): verdict is Exclude<SDK.OperatorVerdict, "ForcedTxValid"> =>
  verdict !== undefined &&
  verdict !== "ForcedTxValid" &&
  SDK.purposeCoordinateOfReason(verdict.ForcedTxInvalid.reason) === null;

/** Such a reason recorded at or after the purpose phase. The network context
 * moves only `NetworkIdMismatch`, which the purpose phase admits at either of
 * its phases; the prover decides admission under the bound context. */
const purposePhaseReason = (verdict: SDK.OperatorVerdict) =>
  purposelessReason(verdict) &&
  SDK.rejectionReasonPhase(verdict.ForcedTxInvalid.reason, {
    body_network_mismatch: true,
  }) >= 8;

/** A leaf whose bytes the replay cannot open yields no finding; an unresolved
 * replay prerequisite still surfaces. */
const unlessUnopened = async <T>(task: () => Promise<T | null>) => {
  try {
    return await task();
  } catch (error) {
    if (error instanceof CanonicalReplayPrerequisiteError) throw error;
    return null;
  }
};

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
    purposelessReason(
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
    if (hasPurposeField(entry.fullTransactionCbor, false))
      candidates.push({ sourceKind: 0, sourceIndex });
  for (const [sourceIndex, entry] of current.forcedTransactions.entries())
    if (
      entry.value.verdict === "ForcedTxValid"
        ? hasPurposeField(entry.fullTransactionCbor, true)
        : redeemerMissingReason(entry.value.verdict) ||
          purposePhaseReason(entry.value.verdict)
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
                : `${PURPOSE_NAMES[Number(prepared.observed.kind)]!} purpose ${prepared.observed.index.toString()} has a redeemer-bearing source and no redeemer`,
            prepared,
          };
    }),
  );
};
