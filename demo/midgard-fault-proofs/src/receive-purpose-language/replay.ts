/** Complete `receivePurposeLanguage` replay of one block: every accepted
 * transaction that carries a redeemer and every forced rejection recording
 * `ReceivePurposePlutusV3Forbidden` is planned through the prover's verdict.
 * A forced transaction carries no redeemers, so no forced execution is
 * forbidden and only its recorded claim can convict. */
import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";

import {
  directPurposeEventKey,
  type DirectPurposeSourceCoordinate,
} from "../direct-purpose-family/replay-support.js";
import { prepareDirectScriptMaterial } from "../direct-script-derivation/material.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CheckedLedger } from "../ledger-at.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import { eventSubject } from "../workflow/detection-subject.js";
import { collectReplayFindings } from "../workflow/replay-prerequisite.js";
import { receivePurposeLanguageDetection } from "./prover.js";

export const RECEIVE_PURPOSE_LANGUAGE_VIOLATION_ID =
  "receive-purpose-plutus-v3-forbidden";

export type ReceivePurposeLanguageCoordinate = DirectPurposeSourceCoordinate;

/** `<source kind>:<source index>:` then the forbidden execution's receive
 * ordinal, or `claim` for a recorded claim's verdict. */
export const receivePurposeLanguageDetectionId = (
  coordinate: ReceivePurposeLanguageCoordinate,
  observed: SDK.DirectPurposeCoordinate | null,
): string =>
  `${RECEIVE_PURPOSE_LANGUAGE_VIOLATION_ID}:${coordinate.sourceKind.toString()}:${coordinate.sourceIndex.toString()}:${observed === null ? "claim" : observed.index.toString()}`;

/** The planned finding at one coordinate, or `null` when it does not
 * convict. */
export const prepareReceivePurposeLanguageReplay = async ({
  current,
  predecessor,
  coordinate,
}: {
  readonly current: TransitionTraceReconstruction;
  readonly predecessor?: CheckedLedger;
  readonly coordinate: ReceivePurposeLanguageCoordinate;
}) => {
  const eventKey = directPurposeEventKey(current, coordinate);
  if (eventKey === null) return null;
  const material = await prepareDirectScriptMaterial({
    current,
    eventKey,
    ...(predecessor === undefined ? {} : { predecessor }),
  });
  const detection = await receivePurposeLanguageDetection(material);
  return detection === null
    ? null
    : {
        coordinate,
        eventKey,
        material,
        observed: detection.observed,
        detectionId: receivePurposeLanguageDetectionId(
          coordinate,
          detection.observed,
        ),
      };
};

const redeemersPresent = (fullTransactionCbor: Buffer): boolean =>
  decodeMidgardFieldPreimage(
    decodeMidgardNativeTxFullFromCanonicalCbor(fullTransactionCbor).witnessSet
      .redeemerTxWitsPreimageCbor,
  ).length > 0;

const recordsReceiveLanguage = (verdict: SDK.OperatorVerdict): boolean =>
  verdict !== "ForcedTxValid" &&
  typeof verdict.ForcedTxInvalid.reason === "object" &&
  "ReceivePurposePlutusV3Forbidden" in verdict.ForcedTxInvalid.reason;

export const detectReceivePurposeLanguageReplay = async ({
  block,
  predecessor,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor?: CheckedLedger;
}) => {
  const current = block.reconstruction;
  const candidates: ReceivePurposeLanguageCoordinate[] = [];
  for (const [sourceIndex, entry] of current.transactions.entries())
    if (redeemersPresent(entry.fullTransactionCbor))
      candidates.push({ sourceKind: 0, sourceIndex });
  for (const [sourceIndex, entry] of current.forcedTransactions.entries())
    if (recordsReceiveLanguage(entry.value.verdict))
      candidates.push({ sourceKind: 1, sourceIndex });
  return collectReplayFindings(
    candidates.map(async (coordinate) => {
      const prepared = await prepareReceivePurposeLanguageReplay({
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
            violationId: RECEIVE_PURPOSE_LANGUAGE_VIOLATION_ID,
            position: BigInt(coordinate.sourceIndex),
            diagnostic:
              prepared.observed === null
                ? "the forced rejection records a receive-language outcome the transaction does not have"
                : `receive purpose ${prepared.observed.index.toString()} executes a PlutusV3 source with a redeemer`,
            prepared,
          };
    }),
  );
};
