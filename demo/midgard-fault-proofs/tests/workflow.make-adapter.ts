import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { expect, vi } from "vitest";

import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../src/evidence/canonical-block-evidence.js";
import type { RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import type { CanonicalViolationDetection } from "../src/workflow/classification.js";
import { BLOCK_SUBJECT } from "../src/workflow/detection-subject.js";
import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowTerminal,
} from "../src/workflow/journal.fraud-proof-workflow-terminal.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
} from "../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { createFraudProofWorkflowRegistry } from "../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runFraudProofWorkflow } from "../src/workflow/orchestrator.run-da-hash-preimage-workflow-from-retained-da.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofReleaseFinalityAuthority,
} from "../src/workflow/release-finality-policy.js";
import type {
  SignedTransactionRecoveryObservation,
  SignedWorkflowTransaction,
} from "../src/workflow/signed-transaction-reconciliation.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  signedRecoveryObservation,
  signedWorkflowTransactionFixture,
} from "./support/signed-workflow-transaction.js";
import { terminalVerifierOf } from "./support/terminal-verifier.js";
import { WALLET_ONLY_SEND_ALWAYS_OPEN } from "./support/wallet-only-send-always-open.js";
import type { TestWorkflowProcess } from "./support/workflow-process.js";

export const DEPLOYMENT_FINGERPRINT = "d1".repeat(32);

/** The proof's and the removal's exact signed bytes: two wallet spends of
 * different coins, so neither spends a coin the other does. */
const [PROOF, REMOVAL] = await Promise.all([
  signedWorkflowTransactionFixture(),
  signedWorkflowTransactionFixture({ coin: 1 }),
]);

export const PROOF_TX_HASH = PROOF.input.transactionHash;

export const REMOVAL_TX_HASH = REMOVAL.input.transactionHash;

/** The signed transaction the fixture's preflight builds for `actionId`. */
export const signedFor = (actionId: string) =>
  actionId === "prove" ? PROOF.signed : REMOVAL.signed;

export const REFERENCE_OUT_REF = `${"b2".repeat(32)}#0`;

export const REFERENCE_SCRIPT_HASH = "c3".repeat(28);

export const RELEASE_FINALITY_POLICY = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };

export const releaseFinalityAuthority = (
  overrides: Partial<{
    readonly deploymentIdentityDigest: string;
    readonly blueprintHash: string;
  }> = {},
): FraudProofReleaseFinalityAuthority => ({
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  verifyForWorkflow: async () => ({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest:
      overrides.deploymentIdentityDigest ?? DEPLOYMENT_FINGERPRINT,
    blueprintHash: overrides.blueprintHash ?? "e1".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(
      RELEASE_FINALITY_POLICY,
    ),
    policy: RELEASE_FINALITY_POLICY,
  }),
});

export const canonicalEvidence = async (): Promise<CanonicalBlockEvidence> => {
  const fixture = await buildCanonicalBlockFixture({ transactions: [] });
  return await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/peer-a",
      grade: "security",
    },
  });
};

export const retainedDaSource = (
  payloadEnvelopeCbor: Buffer,
): RetainedDaPayloadSource => ({
  sourceId: "libp2p",
  fetchPayloadByHeaderHash: async () => ({
    ok: true,
    provenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/peer-a",
      grade: "security",
    },
    sourceId: "libp2p",
    sourcePeerId: "peer-a",
    payloadEnvelopeCbor,
    attempts: [],
  }),
});

export const detection = (
  evidence: CanonicalBlockEvidence,
  violationId = "double-spend",
  overrides: Partial<CanonicalViolationDetection> = {},
): CanonicalViolationDetection => ({
  ...BLOCK_SUBJECT,
  detectionId: `${violationId}-0`,
  headerHash: evidence.headerHash,
  violationId,
  position: 0n,
  ...overrides,
});

/**
 * Evidence with one normal transaction per id and ids in event order. The
 * fixture commits an empty transition trace, so no event has a step and each
 * falls back to its source-event order: the committed list order.
 */
export const canonicalEvidenceWithEvents = async (
  count: number,
): Promise<readonly [CanonicalBlockEvidence, readonly string[]]> => {
  const transactions = Array.from({ length: count }, (_, index) =>
    buildFixtureTransaction({
      spendInputs: [outRefCbor(0x40 + index, 0n)],
      fee: 0n,
    }),
  );
  const fixture = await buildCanonicalBlockFixture({ transactions });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/peer-a",
      grade: "security",
    },
  });
  expect(evidence.reconstruction.transitionTrace).toEqual([]);
  return [evidence, evidence.transactions.map(({ nodeTxId }) => nodeTxId)];
};

/**
 * The L1 a fixture workflow runs against: where each of its signed
 * transactions stands. A transaction the chain does not name is pending;
 * a landed one is included past the release-final point unless marked
 * reversible.
 */
export type FixtureChain = {
  readonly status: Map<string, SignedTransactionRecoveryObservation["status"]>;
  /** Landed transactions still reversible: included after the release-final
   * point. */
  readonly reversible: Set<string>;
  readonly land: (txHash: string) => void;
  readonly landed: (txHash: string) => boolean;
  observeSignedTransaction: FraudProofWorkflowTerminalVerifier["observeSignedTransaction"];
};

export const createFixtureChain = (): FixtureChain => {
  const status = new Map<
    string,
    SignedTransactionRecoveryObservation["status"]
  >();
  const reversible = new Set<string>();
  return {
    status,
    reversible,
    land: (txHash) => {
      status.set(txHash, "included");
    },
    landed: (txHash) => status.get(txHash) === "included",
    observeSignedTransaction: async (input: SignedWorkflowTransaction) => {
      const observed = signedRecoveryObservation(
        input,
        status.get(input.transactionHash) ?? "pending",
      );
      return reversible.has(input.transactionHash)
        ? { ...observed, inclusionPoint: observed.canonicalPoint }
        : observed;
    },
  };
};

type AdapterControls = {
  readonly submit?: FraudProofFamilyWorkflowAdapter["submit"];
  readonly referenceScripts?: boolean;
  /** The L1 the adapter reads; a fresh one by default. */
  readonly chain?: FixtureChain;
  /** Whether a submission lands at once; by default it does. */
  readonly lands?: boolean;
};

const chains = new WeakMap<FraudProofFamilyWorkflowAdapter, FixtureChain>();

/** The L1 `adapter` reads. */
export const chainOf = (adapter: FraudProofFamilyWorkflowAdapter) => {
  const chain = chains.get(adapter);
  if (chain === undefined) throw new Error("not a fixture adapter");
  return chain;
};

export const terminal = (headerHash: string): FraudProofWorkflowTerminal => ({
  schemaVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  category: "doubleSpend",
  headerHash,
  proofToken: {
    unit: "11".repeat(28),
    outRef: `${PROOF_TX_HASH}#0`,
    createdByTxHash: PROOF_TX_HASH,
    retainedAtFinalState: true,
  },
  correction: {
    removalTxHash: REMOVAL_TX_HASH,
    removedStateQueueOutRef: `${"a3".repeat(32)}#0`,
    fraudulentHeaderAbsent: true,
    referencedProofTokenOutRef: `${PROOF_TX_HASH}#0`,
  },
  economics: {
    operatorCredential: "22".repeat(28),
    proverCredential: "33".repeat(28),
    operatorBondInputOutRef: `${"a4".repeat(32)}#0`,
    operatorBondInputLovelace: "10000000",
    slashedLovelace: "10000000",
    proverRewardOutputOutRef: `${REMOVAL_TX_HASH}#0`,
    proverRewardLovelace: "5000000",
    removalFeeLovelace: "200000",
    duplicateRewardAbsent: true,
  },
  observedAt: {
    slot: "4242",
    blockHash: "44".repeat(32),
    confirmationDepth: RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2,
  },
});

export const terminalVerifier = terminalVerifierOf(
  async ({ candidate }) => candidate,
);

export const makeAdapter = (
  controls: AdapterControls = {},
): FraudProofFamilyWorkflowAdapter => {
  const chain = controls.chain ?? createFixtureChain();
  const adapter: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category: "doubleSpend",
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    // The family's L1 view of our threads: none of ours on the header.
    observeOwnThreads: vi.fn(async () => ({ spends: [] })),
    prepare: vi.fn(
      async ({
        evidence,
      }: Parameters<FraudProofFamilyWorkflowAdapter["prepare"]>[0]) => ({
        headerHash: evidence.headerHash,
        txIds: [],
      }),
    ),
    // The family reads the chain: the removal landed completes the
    // workflow, the proof landed requires the removal.
    observe: vi.fn(
      async ({
        artifact,
      }: Parameters<FraudProofFamilyWorkflowAdapter["observe"]>[0]) =>
        chain.landed(REMOVAL_TX_HASH)
          ? {
              kind: "completed" as const,
              terminal: terminal(String(artifact.headerHash)),
            }
          : chain.landed(PROOF_TX_HASH)
            ? {
                kind: "action_required" as const,
                action: { actionId: "remove", input: { step: 1 } },
              }
            : {
                kind: "action_required" as const,
                action: { actionId: "prove", input: { step: 0 } },
              },
    ),
    preflight: vi.fn(
      async ({
        action,
      }: Parameters<FraudProofFamilyWorkflowAdapter["preflight"]>[0]) =>
        bindWorkflowPreflightTransaction(
          {
            actionId: action.actionId,
            txHash: signedFor(action.actionId).toHash(),
            scriptExecution: "reference_scripts" as const,
            localUplcEvaluation: {
              status: "passed" as const,
              evaluator: "uplc-v1",
            },
            referenceScripts:
              controls.referenceScripts === false
                ? ([] as unknown as [
                    {
                      readonly role: string;
                      readonly outRef: string;
                      readonly scriptHash: string;
                    },
                  ])
                : ([
                    {
                      role: "family-step",
                      outRef: REFERENCE_OUT_REF,
                      scriptHash: REFERENCE_SCRIPT_HASH,
                    },
                  ] as const),
          },
          signedFor(action.actionId),
        ),
    ),
    submit: vi.fn(
      controls.submit ??
        (async ({ preflight }) => {
          if (controls.lands !== false) chain.land(preflight.txHash);
          return { kind: "submitted" as const, txHash: preflight.txHash };
        }),
    ),
  };
  chains.set(adapter, chain);
  return adapter;
};

/** `verifier` reading where each transaction stands from `adapter`'s chain,
 * looked up at each read so a test may stub the chain's answer. */
export const chainVerifier = (
  adapter: FraudProofFamilyWorkflowAdapter,
  verifier: FraudProofWorkflowTerminalVerifier = terminalVerifier,
  chain: FixtureChain = chainOf(adapter),
): FraudProofWorkflowTerminalVerifier =>
  // A verifier that cannot read a sent transaction stays one, for the run to
  // refuse.
  verifier.observeSignedTransaction === undefined
    ? verifier
    : {
        ...verifier,
        observeSignedTransaction: async (input) =>
          await chain.observeSignedTransaction(input),
      };

/** One objective's run of `adapter` in `process`, against the adapter's
 * chain: `verifier` authenticates the terminal, the chain says where each
 * in-flight transaction stands. */
export const run = async ({
  evidence,
  adapter,
  process,
  verifier = terminalVerifier,
  finalityAuthority = releaseFinalityAuthority(),
  nowMs,
  chain,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  /** The chain `adapter` reads, when it wraps a fixture adapter. */
  readonly chain?: FixtureChain;
  readonly process: TestWorkflowProcess;
  readonly verifier?: FraudProofWorkflowTerminalVerifier;
  readonly finalityAuthority?: FraudProofReleaseFinalityAuthority;
  /** The clock rebroadcasts are spaced on. */
  readonly nowMs?: () => number;
}) =>
  await runFraudProofWorkflow({
    walletOnlySend: WALLET_ONLY_SEND_ALWAYS_OPEN,
    deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
    evidence,
    detections: [detection(evidence)],
    registry: createFraudProofWorkflowRegistry({
      adapters: [adapter],
      launchScope: ["doubleSpend"],
    }),
    run: process.run({
      category: "doubleSpend",
      headerHash: evidence.headerHash,
    }),
    terminalVerifier: chainVerifier(
      adapter,
      verifier,
      chain ?? chainOf(adapter),
    ),
    releaseFinalityAuthority: finalityAuthority,
    ...(nowMs === undefined ? {} : { nowMs }),
  });
