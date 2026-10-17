declare const tx: any;
declare const emulator: { now(): number };
declare const lowerBound: number;
declare const threshold: number;
declare const validTo: number;
declare const window: number;
declare const slotStart: number;

const BACKOFF_MS = 30_000;
const now = Date.now();

// ruleid: midgard/valid-from-wall-clock-margin
tx.validFrom(Date.now());

// ruleid: midgard/valid-from-wall-clock-margin
tx.validFrom(Date.now() - 1_000);

// ruleid: midgard/valid-from-wall-clock-margin
tx.validFrom(Math.max(lowerBound, now));

// ruleid: midgard/valid-from-wall-clock-margin
export const literal = { validFrom: new Date().getTime(), validTo };

export const takeover = (nowMs = Date.now()) => {
  const validFrom = Math.max(nowMs, threshold + 1);
  // ruleid: midgard/valid-from-wall-clock-margin
  return { validFrom, validTo: validFrom + window };
};

export const fromParameter = (currentTimeMs: number) => {
  // ruleid: midgard/valid-from-wall-clock-margin
  return { txValidFrom: currentTimeMs };
};

// ok: midgard/valid-from-wall-clock-margin
tx.validFrom(Date.now() - 60_000);

// ok: midgard/valid-from-wall-clock-margin
tx.validFrom(Math.max(lowerBound, Date.now() - BACKOFF_MS));

// ok: midgard/valid-from-wall-clock-margin
tx.validFrom(emulator.now());

// ok: midgard/valid-from-wall-clock-margin
tx.validFrom(slotStart);
