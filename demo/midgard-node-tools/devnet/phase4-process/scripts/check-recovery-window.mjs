import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

/** A frozen single-producer fork cannot obtain missing history from a peer. */
export function checkRecoveryWindow(genesis, tip, nowMs) {
  const startMs = Date.parse(genesis.systemStart);
  const slotMs = genesis.slotLength * 1000;
  const forecastSlots = Math.ceil(
    (3 * genesis.securityParam) / genesis.activeSlotsCoeff,
  );
  if (
    !Number.isFinite(startMs) ||
    !Number.isSafeInteger(slotMs) ||
    slotMs <= 0 ||
    !Number.isSafeInteger(forecastSlots) ||
    forecastSlots <= 0 ||
    !Number.isSafeInteger(tip.slot) ||
    tip.slot < 0 ||
    !Number.isFinite(nowMs)
  )
    throw new Error("Invalid genesis or snapshot recovery point");
  const currentSlot = Math.floor((nowMs - startMs) / slotMs);
  if (currentSlot < tip.slot)
    throw new Error("Snapshot tip is ahead of the current slot");
  if (currentSlot >= tip.slot + forecastSlots)
    throw new Error(
      "Frozen snapshot is outside the ledger forecast window; recover by synchronizing from a canonical peer, or create a fresh isolated chain. Do not extend consensus parameters or rewind the clock.",
    );
  return {
    snapshotSlot: tip.slot,
    currentSlot,
    forecastSlots,
    remainingSlots: tip.slot + forecastSlots - currentSlot,
  };
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const [genesisPath, tipPath] = process.argv.slice(2);
  if (!genesisPath || !tipPath)
    throw new Error(
      "Usage: check-recovery-window.mjs <shelley genesis> <snapshot tip>",
    );
  checkRecoveryWindow(
    JSON.parse(await readFile(genesisPath, "utf8")),
    JSON.parse(await readFile(tipPath, "utf8")),
    Date.now(),
  );
}
