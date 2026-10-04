/** The native owner model of a preparation test. Imports nothing from src,
 * so a mock factory (of the native owner service) can load it. */

/** A native owner model: its durable root moves from the plan's expected
 * root to its target on `restoreCanonicalRoot`, after which `afterRestore`
 * runs (a stop after the native CAS), and to a replay's candidate on
 * `recover`. `beforeDiagnostics` runs as its diagnostics are read. */
export type OwnerModel = {
  durableRoot: string;
  restores: number;
  recovers: number;
  operations: string[];
  afterRestore?: () => Promise<void>;
  beforeRestore?: (native: {
    recoveryId: string;
    expectedRoot: string;
    targetRoot: string;
  }) => Promise<void>;
  /** Runs when the preparation reads the native diagnostics: after its
   * decision and before the plan re-derives it, the window another fiber or
   * the correction observer may change the journal's state in. */
  beforeDiagnostics?: () => Promise<void>;
};

export const ownerModel = (durableRoot: string): OwnerModel => ({
  durableRoot,
  restores: 0,
  recovers: 0,
  operations: [],
});

export const fakeOwner = (model: OwnerModel) => ({
  diagnostics: async () => {
    await model.beforeDiagnostics?.();
    return { durableRoot: model.durableRoot };
  },
  restoreCanonicalRoot: async (native: {
    recoveryId: string;
    expectedRoot: string;
    targetRoot: string;
  }) => {
    await model.beforeRestore?.(native);
    if (
      model.durableRoot !== native.expectedRoot &&
      model.durableRoot !== native.targetRoot
    )
      throw new Error("modelled native CAS refused");
    model.operations.push(native.recoveryId);
    model.durableRoot = native.targetRoot;
    model.restores += 1;
    await model.afterRestore?.();
  },
  /** A journal replay moves the durable root to its candidate. */
  recover: async (replay: { candidateRoot: string }) => {
    model.durableRoot = replay.candidateRoot;
    model.recovers += 1;
  },
});
