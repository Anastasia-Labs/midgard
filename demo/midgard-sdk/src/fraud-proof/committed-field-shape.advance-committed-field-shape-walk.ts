import {
  midgardEnvelopeFirstBatch,
  midgardEnvelopeItemsBatch,
} from "./canonical-decodability.js";
import {
  type CommittedFieldShapeStep02State,
  type CommittedFieldShapeWalkState,
  midgardShapeVerdictAfterWalk,
} from "./committed-field-shape.js";

/**
 * One committed-field-shape step-01 transaction's outcome: the walk continues
 * at step 01 with the next cursor, or it ends and step 02 receives the slot's
 * shape verdict.
 */
export type CommittedFieldShapeWalkTransition =
  | Readonly<{ kind: "continue"; state: CommittedFieldShapeWalkState }>
  | Readonly<{ kind: "verdict"; state: CommittedFieldShapeStep02State }>;

/**
 * The state the committed-field-shape step-01 validator requires its successor
 * to carry, derived from the thread's current state and the authenticated field
 * bytes. `previous` is `null` on the thread's first step.
 *
 * Throws when `previous` names another transaction or field: the validator
 * refuses that resume, so no transaction built from it could land.
 */
export const advanceCommittedFieldShapeWalk = ({
  badTxId,
  fieldIndex,
  committedPreimage,
  previous,
}: {
  readonly badTxId: string;
  readonly fieldIndex: number;
  readonly committedPreimage: Uint8Array;
  readonly previous: CommittedFieldShapeWalkState | null;
}): CommittedFieldShapeWalkTransition => {
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
      "committed-field-shape walk state names another transaction or field",
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
        state: {
          ...subject,
          verdict: BigInt(
            midgardShapeVerdictAfterWalk(
              fieldIndex,
              committedPreimage,
              walk.verdict,
            ),
          ),
        },
      });
};
