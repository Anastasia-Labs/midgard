import type { WatcherLocalBackfillFinalityReceipt } from "../l1/finality-engine.js";
import type { WatcherLocalBackfillObservationReceipt } from "../l1/l1-adapter.js";
import type { WatcherUserEventReferenceAuthority } from "./user-event-reference-authority.js";

export type LocalPublicationInput = Readonly<{
  finality: WatcherLocalBackfillFinalityReceipt;
  observation: WatcherLocalBackfillObservationReceipt;
  referenceAuthority: WatcherUserEventReferenceAuthority;
}>;
