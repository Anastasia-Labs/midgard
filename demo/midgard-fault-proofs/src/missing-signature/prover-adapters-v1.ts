/** Thin CLI and watcher adapters over the consumer-agnostic proving core. */
import { Effect } from "effect";

import type { MissingSignatureFindingV1 } from "./finding-v1.js";
import {
  MISSING_SIGNATURE_PROVER_POLICY_DEFAULTS_V1,
  type MissingSignatureProofOutcomeV1,
  type MissingSignatureProverDepsV1,
  type MissingSignatureProverPolicyV1,
  runMissingSignatureProverV1,
} from "./prover-v1.js";
import { missingSignatureSubmitError } from "./submit-common-v1.js";

/** Manual invocation: the operator is the policy. */
export const MISSING_SIGNATURE_CLI_PROVER_POLICY_V1: MissingSignatureProverPolicyV1 =
  Object.freeze({
    ...MISSING_SIGNATURE_PROVER_POLICY_DEFAULTS_V1,
    minSettlementDepth: 0n,
    maxThreadBudgetLovelace: null,
  });

export const proveMissingSignatureFaultOnceV1 = async ({
  finding,
  deps,
  policyOverrides,
}: {
  readonly finding: MissingSignatureFindingV1;
  readonly deps: Omit<MissingSignatureProverDepsV1, "policy">;
  readonly policyOverrides?: Partial<MissingSignatureProverPolicyV1>;
}): Promise<MissingSignatureProofOutcomeV1> =>
  runMissingSignatureProverV1(finding, {
    ...deps,
    policy: {
      ...MISSING_SIGNATURE_CLI_PROVER_POLICY_V1,
      ...policyOverrides,
    },
  });

export type MissingSignatureProverFiberConfigV1 = {
  /** Default-off at the mounting site; registration enables no runtime. */
  readonly enabled: boolean;
  readonly proverPaymentKeyHash: string;
  readonly watcherOperationalPaymentKeyHash: string;
};

export type MissingSignatureProverFiberSummaryV1 = {
  readonly processed: number;
  readonly proven: number;
  readonly refused: number;
  readonly stalled: number;
};

export const runMissingSignatureProverFiberV1 = async ({
  deps,
  config,
  findings,
  onOutcome,
}: {
  readonly deps: MissingSignatureProverDepsV1;
  readonly config: MissingSignatureProverFiberConfigV1;
  readonly findings: AsyncIterable<MissingSignatureFindingV1>;
  readonly onOutcome?: (
    finding: MissingSignatureFindingV1,
    outcome: MissingSignatureProofOutcomeV1,
  ) => void | Promise<void>;
}): Promise<MissingSignatureProverFiberSummaryV1> => {
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
  const prove = async (finding: MissingSignatureFindingV1) => {
    const outcome = await runMissingSignatureProverV1(finding, deps);
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

export const missingSignatureProverFiberV1 = (args: {
  readonly deps: MissingSignatureProverDepsV1;
  readonly config: MissingSignatureProverFiberConfigV1;
  readonly findings: AsyncIterable<MissingSignatureFindingV1>;
  readonly onOutcome?: (
    finding: MissingSignatureFindingV1,
    outcome: MissingSignatureProofOutcomeV1,
  ) => void | Promise<void>;
}): Effect.Effect<MissingSignatureProverFiberSummaryV1, Error> =>
  Effect.tryPromise({
    try: () => runMissingSignatureProverFiberV1(args),
    catch: (cause) =>
      cause instanceof Error ? cause : new Error(String(cause)),
  });
