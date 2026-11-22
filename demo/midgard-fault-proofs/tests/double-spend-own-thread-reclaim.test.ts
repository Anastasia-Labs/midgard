/** The double-spend workflow with the reclaim of our threads in front of it,
 * as the deployment builds it, over the real emulator chain: our thread
 * stands at step 01 of a header the family reports removed against another
 * record. The run cancels that thread back to our wallet through the
 * family's linear cancel (the thread's lovelace less the cancel's fee), and
 * once the cancel has landed ends resolved by the other record. */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk/fraud-proof";
import { CML, getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { buildInit } from "../src/submit-init.submit-init.js";
import { createDoubleSpendRawL1ObservationPort } from "../src/workflow/double-spend-adapter.create-double-spend-raw-l1-observation-port.js";
import type { ManifestBoundDoubleSpendWorkflow } from "../src/workflow/double-spend-adapter.preflight-of.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { createFraudProofWorkflowRegistry } from "../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runAdmittedFraudProofWorkflow } from "../src/workflow/orchestrator.run-admitted-fraud-proof-workflow.js";
import { withDoubleSpendOwnThreadReclaim } from "../src/workflow/own-thread-reclaim.special-families.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-economics-policy.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import { FraudProofResolvedByAnotherRecordError } from "../src/workflow/resolved-by-another-record.js";
import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import { network } from "./support/emulator/blueprints.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import {
  makeHeader,
  withPlaceholderDeposit,
} from "./support/emulator/header-fixtures.js";
import { publishRemovalReferenceScripts } from "./support/emulator/reference-scripts.publish-fault-proof-witness-reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.build-removal-deployment-info.js";
import { submitSetupTx } from "./support/emulator/setup-tx.submit-setup-tx.js";
import { ownThreadCancellerOf } from "./support/own-thread-canceller.js";
import {
  expectResolvedWithReclaim,
  walletLovelace,
} from "./support/own-thread-reclaim-expectations.js";
import { buildAndSubmit } from "./support/submit-steps.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import {
  HUB_ORACLE_POLICY_ID,
  makeAdapter,
  terminalVerifier,
} from "./workflow.make-adapter.js";

const finalityPolicy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
const FINAL_DEPTH = finalityPolicy.automaticRecoveryMaxDepth + 2;
const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
  blueprintHash: "bb".repeat(32),
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(finalityPolicy),
  policy: finalityPolicy,
} as const;
const economicsPolicy = {
  profile: "bounded-acceptance-v1",
  requiredBondLovelace: "900000000",
  slashingPenaltyLovelace: "500000000",
  fraudProverRewardLovelace: "400000000",
  inactivitySlashingPenaltyLovelace: "100000000",
  proverCollateralFloorLovelace: "5000000",
} as const;
const releaseEconomics = {
  schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
  blueprintHash: "bb".repeat(32),
  policyDigest: computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
  policy: economicsPolicy,
} as const;
const ROLES = [
  "computation_thread_step_01",
  "computation_thread_step_02",
  "computation_thread_step_03",
  "computation_thread_step_04",
] as const;
const DATUM_SCHEMAS = [
  SDK.FraudProofComputationThreadStepDatum,
  SDK.DoubleSpendStep02Datum,
  SDK.DoubleSpendStep03Datum,
  SDK.DoubleSpendStep04Datum,
];
const REMOVAL = {
  removalTxHash: "c5".repeat(32),
  referencedProofTokenOutRef: `${"c4".repeat(32)}#0`,
};

/** A header with our double-spend thread at step 01, and our run over the
 * recorded raw L1 with the family reporting the header removed against a
 * record not ours. */
const scene = async () => {
  const recorder = recordCrossBlockRawEmulator();
  const h = await makeFaultProofEmulatorHarness();
  const operator = getAddressDetails(
    await h.funderLucid.wallet().address(),
  ).paymentCredential!;
  const setup = await submitSetupTx({
    lucid: h.funderLucid,
    contracts: h.contracts,
    nonceUtxo: h.nonceUtxo,
    catalogue: h.catalogue,
    header: withPlaceholderDeposit(
      makeHeader(
        operator.hash,
        alignUnixTimeToEmulatorSlotBoundary(
          h.funderLucid,
          h.emulator.now() + 120_000,
        ) - 1,
      ),
    ),
  });
  const removal = await publishRemovalReferenceScripts({
    lucid: h.proverLucid,
    contracts: h.contracts,
  });
  await buildAndSubmit(buildInit, {
    lucid: h.proverLucid,
    witnessReferenceScripts: h.witnessReferenceScripts,
    blueprint: h.realBlueprint,
    deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
      removalReferenceScripts: removal.published,
    }),
    network,
    signer: h.proverSigner,
    fraudCategory: "doubleSpend",
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    awaitConfirmation: true,
  });
  const { headerHash } = setup;
  const categoryId = h.catalogue.categories.doubleSpend.categoryId;
  const steps = h.contracts.fraudProofContracts.doubleSpend.steps;
  const published = Object.values(h.faultProofReferenceScripts);
  const stepReferences = steps.map(
    (step) =>
      published.find(
        ({ scriptHash }) => scriptHash === step.spendingScriptHash,
      )!.utxo,
  );
  const port = createDoubleSpendRawL1ObservationPort({
    authority: recorder.authority,
    releaseFinality,
    releaseEconomics,
    definition: {
      category: "doubleSpend",
      categoryId,
      headerHash,
      proverCredential: h.proverSigner.paymentKeyHash,
      stateQueue: {
        policyId: h.contracts.stateQueue.policyId,
        address: h.contracts.stateQueue.spendingScriptAddress,
      },
      computationThread: {
        policyId: h.contracts.computationThread.policyId,
        steps: steps.map((step, index) => ({
          role: ROLES[index]!,
          address: step.spendingScriptAddress,
          datumSchema: DATUM_SCHEMAS[index]!,
        })),
      },
      proofToken: {
        policyId: h.contracts.fraudProof.policyId,
        address: h.contracts.fraudProof.spendingScriptAddress,
      },
      operatorDirectory: {
        activePolicyId: h.contracts.activeOperators.policyId,
        activeAddress: h.contracts.activeOperators.spendingScriptAddress,
        retiredPolicyId: h.contracts.retiredOperators.policyId,
        retiredAddress: h.contracts.retiredOperators.spendingScriptAddress,
      },
      schedulerAddress: h.contracts.scheduler.spendingScriptAddress,
    },
  });
  // The family's own view: the header removed against another record, the
  // removal `removalAt.depth` blocks deep.
  const removalAt = { depth: FINAL_DEPTH - 1 };
  const base: FraudProofFamilyWorkflowAdapter = {
    ...makeAdapter(),
    observe: async () => {
      throw new FraudProofResolvedByAnotherRecordError({
        ...REMOVAL,
        confirmationDepth: removalAt.depth,
      });
    },
  };
  const workflow = withDoubleSpendOwnThreadReclaim({
    binding: {
      definition: { categoryId, headerHash },
      releaseFinality,
      resolvedContracts: {
        contracts: {
          doubleSpend: { steps },
          computationThread: h.contracts.computationThread,
        },
      },
    },
    adapterConfig: {
      lucid: h.proverLucid,
      signer: h.proverSigner,
      l1: port,
      referenceScripts: {
        steps: stepReferences,
        witnesses: h.witnessReferenceScripts,
      },
    },
    adapter: base,
  } as unknown as ManifestBoundDoubleSpendWorkflow);
  // One watcher process across every run of the scene.
  const process = createTestWorkflowProcess();
  const run = async (): Promise<FraudProofWorkflowRunResult> =>
    await runAdmittedFraudProofWorkflow({
      hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
      category: "doubleSpend",
      headerHash,
      prepareFamilyArtifact: async () => ({}),
      registry: createFraudProofWorkflowRegistry({
        adapters: [workflow.adapter],
        launchScope: ["doubleSpend"],
      }),
      run: process.run({ category: "doubleSpend", headerHash }),
      terminalVerifier,
      releaseFinality,
    });
  return {
    h,
    headerHash,
    port,
    removalAt,
    run,
    restore: () => recorder.restore(),
  };
};

describe("double-spend: our thread on a header another record removed", () => {
  it("cancels our step-01 thread to our wallet, then resolves by the other record", async () => {
    const s = await scene();
    try {
      const sent: string[] = [];
      const submit = s.h.emulator.submitTx.bind(s.h.emulator);
      s.h.emulator.submitTx = async (cbor: string) => {
        sent.push(cbor);
        return await submit(cbor);
      };
      s.h.emulator.awaitBlock(FINAL_DEPTH);
      const { standing } = await s.port.observeOwnThreads({
        headerHash: s.headerHash,
      });
      expect(standing?.step).toBe(1);
      const thread = standing!;

      // The removal is still within rollback recovery: nothing is sent.
      expect(await s.run()).toMatchObject({
        kind: "pending",
        reason: expect.stringContaining(
          `header removal awaits release finality before our thread ${thread.threadOutRef} is reclaimed`,
        ),
      });
      expect(sent).toEqual([]);

      s.removalAt.depth = FINAL_DEPTH;
      const before = await walletLovelace(s.h.proverLucid);
      expect((await s.run()).kind).toBe("pending");
      expect(sent).toHaveLength(1);
      const reclaim = CML.Transaction.from_cbor_hex(sent[0]!);
      const reclaimTxHash = CML.hash_transaction(reclaim.body()).to_hex();
      s.h.emulator.awaitBlock();
      // Landed: our wallet holds the thread's lovelace less the fee.
      expect(await walletLovelace(s.h.proverLucid)).toBe(
        before + BigInt(thread.lovelace) - reclaim.body().fee(),
      );
      expect(
        (await s.port.observeOwnThreads({ headerHash: s.headerHash })).standing,
      ).toBeUndefined();
      expectResolvedWithReclaim(
        await s.run(),
        thread.threadOutRef,
        reclaimTxHash,
      );
      expect(sent).toHaveLength(1);
    } finally {
      s.restore();
    }
  }, 900_000);

  it("the workflow's reclaim and the header-free cancel of one thread: one lands, the other settles quietly", async () => {
    const s = await scene();
    try {
      s.h.emulator.awaitBlock(FINAL_DEPTH);
      s.removalAt.depth = FINAL_DEPTH;
      const { standing } = await s.port.observeOwnThreads({
        headerHash: s.headerHash,
      });
      const thread = standing!;
      const sent: string[] = [];
      const submit = s.h.emulator.submitTx.bind(s.h.emulator);
      s.h.emulator.submitTx = async (cbor: string) => {
        sent.push(cbor);
        return await submit(cbor);
      };
      const before = await walletLovelace(s.h.proverLucid);
      expect((await s.run()).kind).toBe("pending");
      expect(sent).toHaveLength(1);

      // The sweep reaches the same thread while the reclaim waits in our
      // node's mempool, which shows the thread spent: nothing is sent.
      const { canceller } = ownThreadCancellerOf(s.h);
      const subject = {
        threadOutRef: thread.threadOutRef,
        categoryId: s.h.catalogue.categories.doubleSpend.categoryId,
        paymentScriptHash:
          s.h.contracts.fraudProofContracts.doubleSpend.steps[0]!
            .spendingScriptHash,
      };
      expect(
        await canceller.cancel({ subject, stillDue: async () => true }),
      ).toEqual({ kind: "spent_elsewhere" });
      expect(sent).toHaveLength(1);

      s.h.emulator.awaitBlock();
      const reclaim = CML.Transaction.from_cbor_hex(sent[0]!);
      expect(await walletLovelace(s.h.proverLucid)).toBe(
        before + BigInt(thread.lovelace) - reclaim.body().fee(),
      );
      // Once it has landed, the later attempt finds the thread gone.
      expect(
        await canceller.cancel({ subject, stillDue: async () => true }),
      ).toEqual({ kind: "spent_elsewhere" });
      expect(sent).toHaveLength(1);
    } finally {
      s.restore();
    }
  }, 900_000);

  it("the header-free cancel landing first leaves the workflow nothing to send", async () => {
    const s = await scene();
    try {
      s.h.emulator.awaitBlock(FINAL_DEPTH);
      s.removalAt.depth = FINAL_DEPTH;
      const { standing } = await s.port.observeOwnThreads({
        headerHash: s.headerHash,
      });
      const { canceller } = ownThreadCancellerOf(s.h);
      expect(
        await canceller.cancel({
          subject: {
            threadOutRef: standing!.threadOutRef,
            categoryId: s.h.catalogue.categories.doubleSpend.categoryId,
            paymentScriptHash:
              s.h.contracts.fraudProofContracts.doubleSpend.steps[0]!
                .spendingScriptHash,
          },
          stillDue: async () => true,
        }),
      ).toMatchObject({ kind: "submitted" });
      s.h.emulator.awaitBlock(FINAL_DEPTH);
      const sent: string[] = [];
      s.h.emulator.submitTx = async (cbor: string) => {
        sent.push(cbor);
        throw new Error("the workflow has nothing of ours to send");
      };
      const result = await s.run();
      expect(sent).toEqual([]);
      expect(result).toMatchObject({ kind: "resolved_by_another_record" });
    } finally {
      s.restore();
    }
  }, 900_000);
});
