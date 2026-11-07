/**
 * The intent simulation's check of S6's kept statuses
 * (`createIntentStatesView`): after a pass, every status S6 holds equals a
 * fresh derivation of the whole journal, but for intents terminal for k
 * (S6 drops them as the prune hook deletes them) and for this pass's own
 * abandon writes (they follow its derivation, as do their dependants; the
 * next pass re-derives them).
 */
import {
  deriveIntentStatusesIn,
  type FactStore,
  type ReconcileReport,
} from "../../src/index.js";
import { statusText } from "./intent-sim.text.js";

const hex = (bytes: Buffer): string => bytes.toString("hex");

/** Null when S6's kept statuses match a fresh derivation, else the difference. */
export const keptStatusesDiffer = async (
  store: FactStore,
  report: ReconcileReport,
): Promise<string | null> => {
  const afterPass = await store.transaction("read", (tx) =>
    deriveIntentStatusesIn(tx, store.dialect),
  );
  const kept = new Map(report.entries().map((e) => [hex(e.intent.txHash), e]));
  const boundary = afterPass.cursor?.prunedThroughSlot ?? 0;
  const ownWrites = new Set(
    report.intents
      .filter((e) => e.action === "abandon")
      .map((e) => hex(e.intent.txHash)),
  );
  for (let grew = true; grew; ) {
    grew = false;
    for (const state of afterPass.states)
      if (
        !ownWrites.has(hex(state.intent.txHash)) &&
        state.intent.dependsOn.some((d) => ownWrites.has(hex(d)))
      ) {
        ownWrites.add(hex(state.intent.txHash));
        grew = true;
      }
  }
  for (const state of afterPass.states) {
    const key = hex(state.intent.txHash);
    const entry = kept.get(key);
    kept.delete(key);
    if (state.terminalSlot !== null && state.terminalSlot <= boundary) continue;
    if (entry === undefined)
      return `S6 lost intent ${key} (${statusText(state)})`;
    if (ownWrites.has(key)) continue;
    const held = statusText({ ...state, status: entry.status });
    if (held !== statusText(state))
      return `S6 holds intent ${key} as ${held}, fresh ${statusText(state)}`;
  }
  return kept.size > 0
    ? `S6 holds intents no longer journaled: ${[...kept.keys()].join(", ")}`
    : null;
};
