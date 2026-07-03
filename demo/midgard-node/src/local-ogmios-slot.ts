import {
  fetchLocalOgmiosSubmitSlotSnapshot,
  makeLocalOgmiosSubmitSlotSnapshotProvider,
} from "@/local-ledger-slot.js";

export {
  fetchLocalOgmiosSubmitSlotSnapshot,
  localOgmiosSubmitSlotEvidence,
  type LocalOgmiosSubmitSlotOptions,
  makeLocalOgmiosSubmitSlotSnapshotProvider,
  normalizeOgmiosHttpUrl,
  parseOgmiosHealthEvidence,
  parseOgmiosTipSlot,
  queryLocalOgmiosSubmitSlotSnapshot,
  SUBMIT_SLOT_LENGTH_MS,
  SUBMIT_SLOT_VALIDITY_BUFFER,
  type SubmitSlotSnapshot,
} from "@/local-ledger-slot.js";

export const makeLocalOgmiosSubmitSlotProvider =
  makeLocalOgmiosSubmitSlotSnapshotProvider;

export const readLocalOgmiosSubmitSlot = fetchLocalOgmiosSubmitSlotSnapshot;
