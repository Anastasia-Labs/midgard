import type {
  CanonicalDecodabilityStep02State,
  CanonicalDecodabilityWalkState,
} from "./canonical-decodability.canonical-decodability-evidence-from-committed-field.js";
import {
  midgardEnvelopeFirstBatch,
  midgardEnvelopeItemsBatch,
} from "./canonical-decodability.walk-midgard-envelope-items.js";

/**
 * One step-01 transaction's outcome: the walk continues at step 01 with the
 * next state, or it ends and step 02 receives the verdict.
 */
export type CanonicalDecodabilityWalkTransition =
  | Readonly<{ kind: "continue"; state: CanonicalDecodabilityWalkState }>
  | Readonly<{ kind: "verdict"; state: CanonicalDecodabilityStep02State }>;

/**
 * The state the step-01 validator requires its successor to carry, derived
 * from the thread's current state and the authenticated field bytes. `previous`
 * is `null` on the thread's first step.
 *
 * Throws when `previous` names another transaction or field: the validator
 * refuses that resume, so no transaction built from it could land.
 */
export const advanceCanonicalDecodabilityWalk = ({
  badTxId,
  fieldIndex,
  committedPreimage,
  previous,
}: {
  readonly badTxId: string;
  readonly fieldIndex: number;
  readonly committedPreimage: Uint8Array;
  readonly previous: CanonicalDecodabilityWalkState | null;
}): CanonicalDecodabilityWalkTransition => {
  const subject = {
    bad_tx_id: badTxId.toLowerCase(),
    field_index: BigInt(fieldIndex),
  };
  if (
    previous !== null &&
    (previous.bad_tx_id.toLowerCase() !== subject.bad_tx_id ||
      previous.field_index !== subject.field_index)
  ) {
    throw new Error(
      "canonical-decodability walk state names another transaction or field",
    );
  }
  const walk =
    previous === null
      ? midgardEnvelopeFirstBatch(committedPreimage)
      : midgardEnvelopeItemsBatch(
          committedPreimage,
          Number(previous.remaining),
          Number(previous.offset),
        );
  return walk.kind === "items"
    ? Object.freeze({
        kind: "continue",
        state: {
          ...subject,
          remaining: BigInt(walk.remaining),
          offset: BigInt(walk.offset),
        },
      })
    : Object.freeze({
        kind: "verdict",
        state: { ...subject, verdict: BigInt(walk.verdict) },
      });
};
