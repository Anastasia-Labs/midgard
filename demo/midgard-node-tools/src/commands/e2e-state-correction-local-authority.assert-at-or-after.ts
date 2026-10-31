import {
  type ChainPoint,
  type LiveTip,
} from "./e2e-state-correction-local-authority.fetch-json.js";

export const assertAtOrAfter = (
  live: LiveTip,
  prior: ChainPoint,
  field: string,
): void => {
  const priorSlot = Number(prior.slot);
  const liveSlot = Number(live.slot);
  if (!Number.isSafeInteger(priorSlot) || liveSlot < priorSlot) {
    throw new Error(`${field} rolled back before the accepted observation`);
  }
  if (liveSlot === priorSlot && live.blockHash !== prior.blockHash) {
    throw new Error(`${field} disagrees at the accepted slot`);
  }
};
