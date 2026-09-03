/**
 * The proving core's two consumers (offchain plan §4.3), thin by
 * construction: both compose `prover-v1.ts` and add only their own policy
 * stance and wiring. The CLI's `bin.ts` verb and the watcher's `families[]`
 * mounting are registration-wave surfaces (§9 step 4); the functions ship
 * with the family so both consumers import, not reimplement.
 */
import { Effect } from "effect";

import type { NativeScriptDecodingFinding } from "./finding-v1.js";
import {
  NATIVE_SCRIPT_DECODING_PROVER_POLICY_DEFAULTS,
  type NativeScriptDecodingProofOutcome,
  type NativeScriptDecodingProverDeps,
  type NativeScriptDecodingProverPolicy,
  runNativeScriptDecodingProver,
} from "./prover-v1.js";
import { nativeScriptDecodingSubmitError } from "./submit-common-v1.js";

// ## CLI (manual, one-shot)

/**
 * The CLI stance: the operator IS the policy. Every autonomous gate is
 * disabled — the operator invoking the verb has already decided to prove —
 * while the §3.2/3.3 provability boundary still holds (the core never
 * relaxes it).
 */
export const NATIVE_SCRIPT_DECODING_CLI_PROVER_POLICY: NativeScriptDecodingProverPolicy =
  Object.freeze({
    ...NATIVE_SCRIPT_DECODING_PROVER_POLICY_DEFAULTS,
    minSettlementDepth: 0n,
    maxThreadBudgetLovelace: null,
    maturityGuardFactor: 0,
  });

/**
 * One-shot manual proving: drives the finding end-to-end (or resumes its
 * thread) under the permissive CLI policy and the operator's wallet.
 */
export const proveNativeScriptDecodingFaultOnce = async ({
  finding,
  deps,
  policyOverrides,
}: {
  readonly finding: NativeScriptDecodingFinding;
  readonly deps: Omit<NativeScriptDecodingProverDeps, "policy">;
  /** Operator-set gates on top of the permissive base, when wanted. */
  readonly policyOverrides?: Partial<NativeScriptDecodingProverPolicy>;
}): Promise<NativeScriptDecodingProofOutcome> =>
  runNativeScriptDecodingProver(finding, {
    ...deps,
    policy: {
      ...NATIVE_SCRIPT_DECODING_CLI_PROVER_POLICY,
      ...policyOverrides,
    },
  });

// ## Watcher (autonomous fiber)

/**
 * The §10 Q5 enablement contract. The adapter ships default OFF; turning
 * it on is an explicit owner configuration act that must name a dedicated
 * prover wallet distinct from the watcher's operational identity — the
 * adapter refuses to run autonomously on the operational wallet.
 */
export type NativeScriptDecodingProverFiberConfig = {
  /** Default false: the fiber exits immediately without proving. */
  readonly enabled: boolean;
  /** Payment key hash of the dedicated prover wallet `deps.signer` holds. */
  readonly proverPaymentKeyHash: string;
  /** Payment key hash of the watcher's operational wallet. */
  readonly watcherOperationalPaymentKeyHash: string;
};

export type NativeScriptDecodingProverFiberSummary = {
  readonly processed: number;
  readonly proven: number;
  readonly refused: number;
  readonly stalled: number;
};

/**
 * The watcher's prover entry: consume §3.4 finding records, apply the
 * configured policy through the core, journal outcomes. Threads run under
 * the policy's single-flight cap; each finding resolves to an outcome
 * before its slot frees (stalls do not retry here — a stalled thread is
 * an operator decision, resumable by re-emitting the finding).
 */
export const runNativeScriptDecodingProverFiber = async ({
  deps,
  config,
  findings,
  onOutcome,
}: {
  readonly deps: NativeScriptDecodingProverDeps;
  readonly config: NativeScriptDecodingProverFiberConfig;
  readonly findings: AsyncIterable<NativeScriptDecodingFinding>;
  /** Optional hook past the journal, e.g. for watcher metrics. */
  readonly onOutcome?: (
    finding: NativeScriptDecodingFinding,
    outcome: NativeScriptDecodingProofOutcome,
  ) => void | Promise<void>;
}): Promise<NativeScriptDecodingProverFiberSummary> => {
  if (!config.enabled) {
    return { processed: 0, proven: 0, refused: 0, stalled: 0 };
  }
  if (config.proverPaymentKeyHash !== deps.signer.paymentKeyHash) {
    throw nativeScriptDecodingSubmitError(
      `autonomous proving is configured for prover wallet ${config.proverPaymentKeyHash}, but the signer holds ${deps.signer.paymentKeyHash}.`,
    );
  }
  if (config.proverPaymentKeyHash === config.watcherOperationalPaymentKeyHash) {
    throw nativeScriptDecodingSubmitError(
      "autonomous proving refuses to run on the watcher's operational wallet — configure a dedicated prover wallet (§10 Q5).",
    );
  }

  const singleFlight = Math.max(1, deps.policy.singleFlight);
  const inFlight = new Set<Promise<void>>();
  let processed = 0;
  let proven = 0;
  let refused = 0;
  let stalled = 0;

  const prove = async (finding: NativeScriptDecodingFinding) => {
    const outcome = await runNativeScriptDecodingProver(finding, deps);
    processed += 1;
    if (outcome.kind === "proven") {
      proven += 1;
    } else if (outcome.kind === "refused") {
      refused += 1;
    } else {
      stalled += 1;
    }
    await onOutcome?.(finding, outcome);
  };

  for await (const finding of findings) {
    while (inFlight.size >= singleFlight) {
      await Promise.race(inFlight);
    }
    const slot: Promise<void> = prove(finding).finally(() => {
      inFlight.delete(slot);
    });
    inFlight.add(slot);
  }
  await Promise.all(inFlight);
  return { processed, proven, refused, stalled };
};

/** The fiber as an Effect, for mounting in the watcher's runtime. */
export const nativeScriptDecodingProverFiber = (args: {
  readonly deps: NativeScriptDecodingProverDeps;
  readonly config: NativeScriptDecodingProverFiberConfig;
  readonly findings: AsyncIterable<NativeScriptDecodingFinding>;
  readonly onOutcome?: (
    finding: NativeScriptDecodingFinding,
    outcome: NativeScriptDecodingProofOutcome,
  ) => void | Promise<void>;
}): Effect.Effect<NativeScriptDecodingProverFiberSummary, Error> =>
  Effect.tryPromise({
    try: () => runNativeScriptDecodingProverFiber(args),
    catch: (cause) =>
      cause instanceof Error ? cause : new Error(String(cause)),
  });
