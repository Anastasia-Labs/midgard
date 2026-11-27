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
} from "../src/workflow/identity.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
} from "../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { createFraudProofWorkflowRegistry } from "../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runFraudProofWorkflow } from "../src/workflow/orchestrator.run-da-hash-preimage-workflow-from-retained-da.js";
import type { FraudProofReleaseFinalityAuthority } from "../src/workflow/release-finality-policy.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  createFixtureChain,
  type FixtureChain,
} from "./support/fixture-chain.js";
import { signedWorkflowTransactionFixture } from "./support/signed-workflow-transaction.js";
import {
  HUB_ORACLE_POLICY_ID,
  RELEASE_FINALITY_POLICY,
  releaseFinalityAuthority,
  terminalVerifier,
} from "./support/workflow-authorities.js";
import type { TestWorkflowProcess } from "./support/workflow-process.js";

export { createFixtureChain, type FixtureChain };
export {
  HUB_ORACLE_POLICY_ID,
  RELEASE_FINALITY_POLICY,
  releaseFinalityAuthority,
  terminalVerifier,
};

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
 * The L1 a fixture workflow runs against, as the node shows it: the ledger
 * and the node's mempool. One the mempool holds is seen by the next state
 * but completes nothing.
 */
type AdapterControls = {
  readonly submit?: FraudProofFamilyWorkflowAdapter["submit"];
  readonly referenceScripts?: boolean;
  /** The L1 the adapter reads; a fresh one by default. */
  readonly chain?: FixtureChain;
  /** Whether a submission lands at once; by default it does, and otherwise
   * it waits in the mempool. */
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
    // The family reads the chain with the mempool applied: the removal
    // landed completes the workflow, and in the mempool awaits inclusion;
    // the proof landed or in the mempool requires the removal.
    observe: vi.fn(
      async ({
        artifact,
      }: Parameters<FraudProofFamilyWorkflowAdapter["observe"]>[0]) =>
        chain.landed(REMOVAL_TX_HASH)
          ? {
              kind: "completed" as const,
              terminal: terminal(String(artifact.headerHash)),
            }
          : chain.seen(REMOVAL_TX_HASH)
            ? {
                kind: "pending" as const,
                reason: `${REMOVAL_TX_HASH} awaits inclusion`,
              }
            : chain.seen(PROOF_TX_HASH)
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
          if (controls.lands === false) chain.accept(preflight.txHash);
          else chain.land(preflight.txHash);
          return { kind: "submitted" as const, txHash: preflight.txHash };
        }),
    ),
  };
  chains.set(adapter, chain);
  return adapter;
};

/** One objective's run of `adapter` in `process`: `verifier` authenticates
 * the terminal, and the process records what the adapter is asked to
 * submit. */
export const run = async ({
  evidence,
  adapter,
  process,
  verifier = terminalVerifier,
  finalityAuthority = releaseFinalityAuthority(),
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly adapter: FraudProofFamilyWorkflowAdapter;
  readonly process: TestWorkflowProcess;
  readonly verifier?: FraudProofWorkflowTerminalVerifier;
  readonly finalityAuthority?: FraudProofReleaseFinalityAuthority;
}) =>
  await runFraudProofWorkflow({
    hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
    evidence,
    detections: [detection(evidence)],
    registry: createFraudProofWorkflowRegistry({
      adapters: [process.recording(adapter)],
      launchScope: ["doubleSpend"],
    }),
    run: process.run({
      category: "doubleSpend",
      headerHash: evidence.headerHash,
    }),
    terminalVerifier: verifier,
    releaseFinalityAuthority: finalityAuthority,
  });
