/**
 * The node's alarm for a forced order the chain admitted but the off-chain
 * admission screen (`screenMidgardForcedTxAdmission`) refuses. The tx-order
 * mint and the screen are twins, so a refusal here means they disagree. It
 * is an alarm only: the order is ingested and classified as any other, and
 * the node stays ready.
 *
 * Each newly observed divergent order logs one `ForcedAdmissionDivergence`
 * error event and increments `forced_admission_divergence_total` (tagged
 * `refusal_code`); alerting keys on those two. The `/readyz` details are
 * RECENT OBSERVATIONS, not an active set: the latest
 * `FORCED_ADMISSION_DIVERGENCES_REPORTED` distinct orders seen in this
 * process's lifetime. An order leaving that list is not resolved, and the
 * list starts empty on restart.
 */
import { decodeMidgardForcedTxCanonicalEnvelopeForFaultEvidence } from "@al-ft/midgard-core/codec";
import { screenMidgardForcedTxAdmission } from "@al-ft/midgard-core/consensus-validation";
import { Metric } from "effect";

/** The `/readyz` detail prefix: `forced_admission_divergence:<orderIdHex>:<refusalCode>`. */
export const FORCED_ADMISSION_DIVERGENCE = "forced_admission_divergence";
/** The structured log event a divergence emits, once per order. */
export const FORCED_ADMISSION_DIVERGENCE_EVENT = "ForcedAdmissionDivergence";
/**
 * How many divergences `/readyz` names: the latest distinct orders observed,
 * never the set still active.
 */
export const FORCED_ADMISSION_DIVERGENCES_REPORTED = 8;
/** The refusal code of a transaction whose §5.1 envelope does not decode. */
export const ENVELOPE_UNDECODABLE = "envelope_undecodable";
/** The refusal code of a transaction the screen throws on. */
export const SCREEN_FAILED = "screen_failed";

/**
 * Newly observed divergent orders, tagged `refusal_code`; monotonic for the
 * process lifetime, unlike the bounded `/readyz` list.
 */
export const forcedAdmissionDivergenceCounter = Metric.counter(
  "forced_admission_divergence_total",
  {
    description:
      "Chain-admitted forced orders the node's admission screen refuses",
    bigint: true,
    incremental: true,
  },
);

/**
 * The screen's refusal of a chain-admitted forced transaction: the name of
 * the mint check it fails and why, or null when the screen admits it.
 */
export const forcedAdmissionRefusal = (
  nativeTxCbor: Uint8Array,
  screen: typeof screenMidgardForcedTxAdmission = screenMidgardForcedTxAdmission,
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
    const refusal = screen(tx);
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

/**
 * The latest distinct divergent orders observed, for `/readyz`: recent
 * observations, not an active set.
 */
export type ForcedAdmissionDivergences = Readonly<{
  /** Records a divergence; false when the order is already named. */
  record: (orderIdHex: string, refusalCode: string) => boolean;
  /** The `/readyz` details, oldest first; a dropped order is not resolved. */
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
