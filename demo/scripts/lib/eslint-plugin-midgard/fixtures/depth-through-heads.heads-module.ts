// fixture-path: midgard-l1-follower/src/heads.ts
declare const parameters: { confirmationDepth: number };
declare const atDepth: number;

// The heads module is the one place that compares against cd and k.
// ok: midgard/depth-through-heads
export const isSafe = atDepth >= parameters.confirmationDepth;
