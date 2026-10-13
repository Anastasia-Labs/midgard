/**
 * The node's alarm for a forced order the chain admitted but the off-chain
 * admission screen (`screenMidgardForcedTxAdmission`) refuses. The tx-order
 * mint and the screen are twins, so a refusal here means they disagree. It
 * is an alarm only: the order is ingested and classified as any other, and
 * the node stays ready. Each divergence logs one `ForcedAdmissionDivergence`
 * error event and is named on `/readyz` as a detail.
 */
import { decodeMidgardForcedTxCanonicalEnvelopeForFaultEvidence } from "@al-ft/midgard-core/codec";
import { screenMidgardForcedTxAdmission } from "@al-ft/midgard-core/consensus-validation";

/** The `/readyz` detail prefix: `forced_admission_divergence:<orderIdHex>:<refusalCode>`. */
export const FORCED_ADMISSION_DIVERGENCE = "forced_admission_divergence";
/** The structured log event a divergence emits, once per order. */
export const FORCED_ADMISSION_DIVERGENCE_EVENT = "ForcedAdmissionDivergence";
/** How many divergences `/readyz` names: the latest distinct orders. */
export const FORCED_ADMISSION_DIVERGENCES_REPORTED = 8;
/** The refusal code of a transaction whose §5.1 envelope does not decode. */
export const ENVELOPE_UNDECODABLE = "envelope_undecodable";
/** The refusal code of a transaction the screen throws on. */
export const SCREEN_FAILED = "screen_failed";

/**
 * The screen's refusal of a chain-admitted forced transaction: the name of
 * the mint check it fails and why, or null when the screen admits it.
 */
export const forcedAdmissionRefusal = (
  nativeTxCbor: Uint8Array,
): Readonly<{ refusalCode: string; detail: string }> | null => {
  const message = (error: unknown) =>
    error instanceof Error ? error.message : String(error);
  let tx;
  try {
    tx = decodeMidgardForcedTxCanonicalEnvelopeForFaultEvidence(nativeTxCbor);
  } catch (error) {
    return { refusalCode: ENVELOPE_UNDECODABLE, detail: message(error) };
  }
  // The alarm never fails ingestion: a screen that throws is a divergence.
  try {
    const refusal = screenMidgardForcedTxAdmission(tx);
    return refusal === null
      ? null
      : { refusalCode: refusal.check, detail: refusal.detail };
  } catch (error) {
    return { refusalCode: SCREEN_FAILED, detail: message(error) };
  }
};

export const forcedAdmissionDivergenceDetail = (
  orderIdHex: string,
  refusalCode: string,
): string => `${FORCED_ADMISSION_DIVERGENCE}:${orderIdHex}:${refusalCode}`;

/** The latest distinct divergent orders, for `/readyz`. */
export type ForcedAdmissionDivergences = Readonly<{
  /** Records a divergence; false when the order is already named. */
  record: (orderIdHex: string, refusalCode: string) => boolean;
  /** The `/readyz` details, oldest first. */
  details: () => readonly string[];
}>;

export const forcedAdmissionDivergences = (
  limit = FORCED_ADMISSION_DIVERGENCES_REPORTED,
): ForcedAdmissionDivergences => {
  const latest = new Map<string, string>();
  return {
    record: (orderIdHex, refusalCode) => {
      if (latest.has(orderIdHex)) return false;
      latest.set(
        orderIdHex,
        forcedAdmissionDivergenceDetail(orderIdHex, refusalCode),
      );
      for (const oldest of latest.keys()) {
        if (latest.size <= limit) break;
        latest.delete(oldest);
      }
      return true;
    },
    details: () => [...latest.values()],
  };
};
