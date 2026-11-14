import { heightAtDepth, isSafe } from "@al-ft/midgard-l1-follower/heads";

declare const manifest: {
  l1Finality: {
    confirmationDepth: number;
    automaticRecoveryMaxDepth: number;
  };
};
declare const config: { confirmationDepth: number };
declare const tipHeight: number;
declare const blockHeight: number;
declare const observedDepth: number;
declare const missingHeight: number;

const cd = manifest.l1Finality.confirmationDepth;
const k = manifest.l1Finality.automaticRecoveryMaxDepth;
const inclusive = tipHeight - blockHeight + 1;

// The settlement variant: inclusive depth against cd.
// ruleid: midgard/depth-through-heads
export const settled = inclusive >= manifest.l1Finality.confirmationDepth;

// The coverage variant: descendants only, through a local constant.
// ruleid: midgard/depth-through-heads
export const covered = tipHeight - blockHeight >= cd;

// The membership variant: head − cd against a height.
// ruleid: midgard/depth-through-heads
export const target = tipHeight - cd < missingHeight;

// k, through `??`.
// ruleid: midgard/depth-through-heads
export const durable = observedDepth > (k ?? 0);

// ruleid: midgard/depth-through-heads
export const configured = observedDepth < config.confirmationDepth;

// Shape validation reads only parameters and constants.
// ok: midgard/depth-through-heads
export const badShape = cd < 1 || k < cd;

// ok: midgard/depth-through-heads
export const badRange = k > Number.MAX_SAFE_INTEGER;

// Configuration agreement is equality, not depth.
// ok: midgard/depth-through-heads
export const agrees = config.confirmationDepth !== cd;

// Decided by the heads module.
// ok: midgard/depth-through-heads
export const safe = isSafe(inclusive, { confirmationDepth: cd });

// A height the heads module computed, compared to another height.
// ok: midgard/depth-through-heads
export const reached = heightAtDepth(tipHeight, cd + 1) < missingHeight;
