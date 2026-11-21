/**
 * The PhaseAScriptPreconditions producer: the required observers in strict
 * ascending order, under the native scan's word on non-native scripts.
 */
import { encodeCbor } from "@al-ft/midgard-core";

import { proofBinding } from "./prepare-producers.shared.js";
import {
  preparedFieldPreimage,
  type PreparedValidationTransaction,
} from "./prepared-validation-transaction.js";
import {
  phaseContinues,
  type PhaseOutcome,
  phaseStops,
  type TraceWalk,
} from "./trace-walk.js";

export const producePhaseAScriptPreconditions = (
  prepared: Pick<
    PreparedValidationTransaction,
    | "proofSource"
    | "contextCbor"
    | "fieldPreimages"
    | "requiredObserversCollection"
    | "resolutionScheduleHash"
    | "signerFrontier"
    | "signerFrontierCommitment"
  >,
  containsNonNativeScript: 0 | 1,
  { push }: Pick<TraceWalk, "push">,
): PhaseOutcome => {
  const { items } = prepared.requiredObserversCollection;
  let observerCount = 0;
  let observerSeen = 0;
  let previousObserver = Buffer.alloc(0);
  const witness = (): Buffer =>
    encodeCbor([
      ...proofBinding(prepared),
      prepared.resolutionScheduleHash,
      BigInt(prepared.signerFrontier.count),
      prepared.signerFrontierCommitment,
      BigInt(containsNonNativeScript),
      previousObserver,
      BigInt(observerCount),
      BigInt(observerSeen),
    ]);
  for (const observer of items) {
    push("phaseAScriptPreconditions", witness(), {
      kind: "transactionFieldChunk",
      fieldIndex: 3,
      itemIndex: observer.itemIndex,
      fieldPreimage: preparedFieldPreimage(prepared, 3),
    });
    if (
      observerSeen > 0 &&
      Buffer.compare(previousObserver, observer.bytes) >= 0
    ) {
      return phaseStops(
        { ObserverOrderInvalid: { observer_index: BigInt(observerSeen) } },
        "bounded observer scan found a duplicate or noncanonical ordering",
      );
    }
    if (observerCount === 0) observerCount = items.length;
    observerSeen += 1;
    previousObserver = observer.bytes;
  }
  push("phaseAScriptPreconditions", witness());
  return phaseContinues;
};
