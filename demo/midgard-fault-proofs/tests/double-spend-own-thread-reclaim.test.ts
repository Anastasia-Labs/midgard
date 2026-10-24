/** The double-spend workflow with the reclaim of our threads in front of it,
 * as the deployment builds it, over the real emulator chain: our thread
 * stands at step 01 of a header the family reports removed against another
 * record. The run cancels that thread back to our wallet through the
 * family's linear cancel (the thread's lovelace less the cancel's fee),
 * waits out the cancel's rollback recovery, and only then ends resolved by
 * the other record. */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { submitInit } from "../src/submit-init.js";
import { createDoubleSpendRawL1ObservationPort } from "../src/workflow/double-spend-adapter.create-double-spend-raw-l1-observation-port.js";
import type { ManifestBoundDoubleSpendWorkflow } from "../src/workflow/double-spend-adapter.preflight-of.js";
import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
} from "../src/workflow/orchestrator.js";
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
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import {
  expectResolvedWithReclaim,
  walletLovelace,
} from "./support/own-thread-reclaim-expectations.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  makeFaultProofEmulatorHarness,
  makeHeader,
  network,
  publishRemovalReferenceScripts,
  submitSetupTx,
  withPlaceholderDeposit,
} from "./support/submit-init-emulator-shared.js";
import {
  DEPLOYMENT_FINGERPRINT,
  makeAdapter,
  terminalVerifier,
} from "./workflow.make-adapter.js";

const finalityPolicy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
const FINAL_DEPTH = finalityPolicy.automaticRecoveryMaxDepth + 2;
const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT_FINGERPRINT,
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
  deploymentIdentityDigest: DEPLOYMENT_FINGERPRINT,
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
  await submitInit({
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
      l1: {
        ...port,
        observeSignedTransaction: async () => {
          throw new Error(
            "no signed bytes are recorded without a funding reservation",
          );
        },
      },
      referenceScripts: {
        steps: stepReferences,
        witnesses: h.witnessReferenceScripts,
      },
    },
    adapter: base,
  } as unknown as ManifestBoundDoubleSpendWorkflow);
  const journal = new MemoryFraudProofWorkflowJournalStore();
  const run = async (): Promise<FraudProofWorkflowRunResult> =>
    await runAdmittedFraudProofWorkflow({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "doubleSpend",
      headerHash,
      evidenceBinding: {
        route: "canonical_block",
        headerHash,
        payloadEnvelopeSha256: "77".repeat(32),
        payloadSha256: "78".repeat(32),
        l1BlockHash: "79".repeat(32),
        l1Slot: "1",
      },
      prepareFamilyArtifact: async () => ({}),
      registry: createFraudProofWorkflowRegistry({
        adapters: [workflow.adapter],
        launchScope: ["doubleSpend"],
      }),
      journal,
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
      let result = await s.run();
      const awaitingOurCancel = `reclaim ${reclaimTxHash} of our thread ${thread.threadOutRef} awaits release finality`;
      if (!JSON.stringify(result).includes(awaitingOurCancel))
        result = await s.run();
      expect(result).toMatchObject({
        kind: "pending",
        reason: expect.stringContaining(awaitingOurCancel),
      });

      s.h.emulator.awaitBlock(FINAL_DEPTH);
      expectResolvedWithReclaim(await s.run(), thread, reclaimTxHash);
      expect(sent).toHaveLength(1);
    } finally {
      s.restore();
    }
  }, 900_000);
});
