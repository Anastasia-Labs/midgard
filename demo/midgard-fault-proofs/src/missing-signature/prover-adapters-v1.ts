/** Thin CLI and watcher adapters over the consumer-agnostic proving core. */
import { Effect } from "effect";

import type { MissingSignatureFinding } from "./finding-v1.js";
import {
  MISSING_SIGNATURE_PROVER_POLICY_DEFAULTS,
  type MissingSignatureProofOutcome,
  type MissingSignatureProverDeps,
  type MissingSignatureProverPolicy,
  runMissingSignatureProver,
} from "./prover-v1.js";
import { missingSignatureSubmitError } from "./submit-common-v1.js";

/** Manual invocation: the operator is the policy. */
export const MISSING_SIGNATURE_CLI_PROVER_POLICY: MissingSignatureProverPolicy =
  Object.freeze({
    ...MISSING_SIGNATURE_PROVER_POLICY_DEFAULTS,
    minSettlementDepth: 0n,
    maxThreadBudgetLovelace: null,
  });

export const proveMissingSignatureFaultOnce = async ({
  finding,
  deps,
  policyOverrides,
}: {
  readonly finding: MissingSignatureFinding;
  readonly deps: Omit<MissingSignatureProverDeps, "policy">;
  readonly policyOverrides?: Partial<MissingSignatureProverPolicy>;
}): Promise<MissingSignatureProofOutcome> =>
  runMissingSignatureProver(finding, {
    ...deps,
    policy: {
      ...MISSING_SIGNATURE_CLI_PROVER_POLICY,
      ...policyOverrides,
    },
  });

export type MissingSignatureProverFiberConfig = {
  /** Default-off at the mounting site; registration enables no runtime. */
  readonly enabled: boolean;
  readonly proverPaymentKeyHash: string;
  readonly watcherOperationalPaymentKeyHash: string;
};

export type MissingSignatureProverFiberSummary = {
  readonly processed: number;
  readonly proven: number;
  readonly refused: number;
  readonly stalled: number;
};

export const runMissingSignatureProverFiber = async ({
  deps,
  config,
  findings,
  onOutcome,
}: {
  readonly deps: MissingSignatureProverDeps;
  readonly config: MissingSignatureProverFiberConfig;
  readonly findings: AsyncIterable<MissingSignatureFinding>;
  readonly onOutcome?: (
    finding: MissingSignatureFinding,
    outcome: MissingSignatureProofOutcome,
  ) => void | Promise<void>;
}): Promise<MissingSignatureProverFiberSummary> => {
  if (!config.enabled)
    return { processed: 0, proven: 0, refused: 0, stalled: 0 };
  if (config.proverPaymentKeyHash !== deps.signer.paymentKeyHash) {
    throw missingSignatureSubmitError(
      `autonomous proving is configured for ${config.proverPaymentKeyHash}, but signer holds ${deps.signer.paymentKeyHash}.`,
    );
  }
  if (config.proverPaymentKeyHash === config.watcherOperationalPaymentKeyHash) {
    throw missingSignatureSubmitError(
      "autonomous proving refuses the watcher's operational wallet; configure a dedicated prover wallet.",
    );
  }

  const inFlight = new Set<Promise<void>>();
  const counts = { processed: 0, proven: 0, refused: 0, stalled: 0 };
  const prove = async (finding: MissingSignatureFinding) => {
    const outcome = await runMissingSignatureProver(finding, deps);
    counts.processed += 1;
    counts[outcome.kind] += 1;
    await onOutcome?.(finding, outcome);
  };
  for await (const finding of findings) {
    while (inFlight.size >= Math.max(1, deps.policy.singleFlight)) {
      await Promise.race(inFlight);
    }
    const slot: Promise<void> = prove(finding).finally(() =>
      inFlight.delete(slot),
    );
    inFlight.add(slot);
  }
  await Promise.all(inFlight);
  return counts;
};

export const missingSignatureProverFiber = (args: {
  readonly deps: MissingSignatureProverDeps;
  readonly config: MissingSignatureProverFiberConfig;
  readonly findings: AsyncIterable<MissingSignatureFinding>;
  readonly onOutcome?: (
    finding: MissingSignatureFinding,
    outcome: MissingSignatureProofOutcome,
  ) => void | Promise<void>;
}): Effect.Effect<MissingSignatureProverFiberSummary, Error> =>
  Effect.tryPromise({
    try: () => runMissingSignatureProverFiber(args),
    catch: (cause) =>
      cause instanceof Error ? cause : new Error(String(cause)),
  });
