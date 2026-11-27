/**
 * The CEK context's required observers (`cek-context-observer.ak`).
 */
import {
  finalizeMidgardCekObserverItems,
  type MidgardCekContextControl,
  prependMidgardCekObserverItem,
  validateMidgardCekObserverCollection,
} from "../cek-context.js";
import { type CekContextRun } from "./cek-producer.context-step.js";
import { preparedFieldPreimage } from "./prepared-validation-transaction.js";
import { sameSummary } from "./trace-descriptor-summary.js";

/** The observers, folded downwards in strict order, stage 5 to 6
 * (`next_observer`). */
export const observerContextItems = (
  run: CekContextRun,
  start: MidgardCekContextControl,
): MidgardCekContextControl => {
  const { items } = run.prepared.requiredObserversCollection;
  const observerCount = items.length;
  validateMidgardCekObserverCollection(items.map((observer) => observer.bytes));
  const midgardEncoding = run.languageTag === 128;
  let control = start;
  for (
    let observerIndex = observerCount - 1;
    observerIndex >= 0;
    observerIndex -= 1
  ) {
    const observer = items[observerIndex]!;
    if (
      control.previousObserver.length > 0 &&
      Buffer.compare(observer.bytes, control.previousObserver) >= 0
    ) {
      throw new Error(
        "CEK observer context is not strictly ordered and unique",
      );
    }
    run.at(control, {
      kind: "transactionFieldChunk",
      fieldIndex: 3,
      itemIndex: observer.itemIndex,
      fieldPreimage: preparedFieldPreimage(run.prepared, 3),
    });
    control = {
      ...control,
      observerCount,
      observerItems: prependMidgardCekObserverItem({
        observerHash: observer.bytes,
        midgardEncoding,
        tail: control.observerItems,
      }),
      previousObserver: observer.bytes,
    };
  }
  run.at(control);
  const observerSummary = finalizeMidgardCekObserverItems({
    items: control.observerItems,
    midgardEncoding,
  });
  if (!sameSummary(observerSummary, run.parts.observer)) {
    throw new Error("CEK observer context differs from the evaluated context");
  }
  return { ...control, stage: 6, observerSummary };
};
