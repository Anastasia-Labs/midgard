/** Replay helpers shared by the direct purpose families: which committed
 * sources can carry a script purpose, which recorded reasons name none, and
 * the event a committed source coordinate names. */
import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";

import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import { CanonicalReplayPrerequisiteError } from "../workflow/replay-prerequisite.js";

/** The committed source a finding names: an accepted (0) or forced (1)
 * transaction, by its index in the block's committed list. */
export type DirectPurposeSourceCoordinate = Readonly<{
  sourceKind: 0 | 1;
  sourceIndex: number;
}>;

export const DIRECT_PURPOSE_NAMES = [
  "spend",
  "mint",
  "required observer",
  "receive",
] as const;

/** Whether the transaction has a spend, output, required observer or mint. */
export const directPurposeFieldPresent = (
  fullTransactionCbor: Buffer,
  forced: boolean,
): boolean => {
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

/** A rejection whose reason has the arm `arm`. */
export const rejectionReasonIs = (
  verdict: SDK.OperatorVerdict,
  arm: "ScriptSourceMissing" | "RedeemerMissing",
): boolean =>
  verdict !== "ForcedTxValid" &&
  typeof verdict.ForcedTxInvalid.reason === "object" &&
  arm in verdict.ForcedTxInvalid.reason;

/** A rejection whose reason names no purpose coordinate. */
export const purposelessRejection = (
  verdict: SDK.OperatorVerdict | undefined,
): verdict is Exclude<SDK.OperatorVerdict, "ForcedTxValid"> =>
  verdict !== undefined &&
  verdict !== "ForcedTxValid" &&
  SDK.purposeCoordinateOfReason(verdict.ForcedTxInvalid.reason) === null;

/** Such a reason recorded at or after the purpose phase. The network context
 * moves only `NetworkIdMismatch`, which the purpose phase admits at either of
 * its phases; the prover decides admission under the bound context. */
export const purposePhaseRejection = (verdict: SDK.OperatorVerdict): boolean =>
  purposelessRejection(verdict) &&
  SDK.rejectionReasonPhase(verdict.ForcedTxInvalid.reason, {
    body_network_mismatch: true,
  }) >= 8;

/** A leaf whose bytes the replay cannot open yields no finding; an unresolved
 * replay prerequisite still surfaces. */
export const unlessUnopened = async <T>(
  task: () => Promise<T | null>,
): Promise<T | null> => {
  try {
    return await task();
  } catch (error) {
    if (error instanceof CanonicalReplayPrerequisiteError) throw error;
    return null;
  }
};

/** The event a coordinate names; `null` when the block has no such source. */
export const directPurposeEventKey = (
  current: TransitionTraceReconstruction,
  { sourceKind, sourceIndex }: DirectPurposeSourceCoordinate,
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
