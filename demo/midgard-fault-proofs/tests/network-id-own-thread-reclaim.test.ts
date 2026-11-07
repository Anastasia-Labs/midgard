/** Another prover convicts a network-id header over a wrongfully rejected
 * forced transaction and removes it while a thread of ours still stands off
 * the linear chain: at the forced door, or in the forced outputs scan. Our
 * run, over the real emulator chain, waits for the removal to pass rollback
 * recovery, cancels that thread back to our wallet through the family's own
 * cancel (the thread's lovelace less the cancel's fee), waits out the
 * cancel's own rollback recovery, and only then ends resolved by the other
 * record: no conviction of ours, no terminal. */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, generateEmulatorAccount, Lucid } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { resolveProverSigner } from "../src/index.js";
import {
  createNetworkIdRawL1ObservationPort,
  createNetworkIdWorkflowAdapter,
  networkIdForcedArtifactFromPrepared,
  networkIdOwnThreadPositions,
  type NetworkIdWorkflowAdapterConfig,
  withNetworkIdOwnThreadReclaim,
} from "../src/network-id/workflow-adapter.js";
import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
} from "../src/workflow/orchestrator.js";
import { runAdmittedFraudProofWorkflow } from "../src/workflow/orchestrator.run-admitted-fraud-proof-workflow.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-economics-policy.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import {
  commitForcedBlock,
  forcedTransactionAt,
  network,
  NETWORK_ID_MISMATCH,
  preparedFor,
} from "./network-id-wrongful-rejection-lifecycle.commit-forced-block.js";
import {
  makeHarness,
  makeStages,
} from "./network-id-wrongful-rejection-lifecycle.make-stages.js";
import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import {
  expectResolvedWithReclaim,
  walletLovelace,
} from "./support/own-thread-reclaim-expectations.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";
import {
  DEPLOYMENT_FINGERPRINT,
  terminalVerifier,
} from "./workflow.make-adapter.js";

const finalityPolicy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
const FINAL_DEPTH = finalityPolicy.automaticRecoveryMaxDepth + 2;
const economicsPolicy = {
  profile: "bounded-acceptance-v1",
  requiredBondLovelace: "900000000",
  slashingPenaltyLovelace: "500000000",
  fraudProverRewardLovelace: "400000000",
  inactivitySlashingPenaltyLovelace: "100000000",
  proverCollateralFloorLovelace: "5000000",
} as const;
const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT_FINGERPRINT,
  blueprintHash: "bb".repeat(32),
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(finalityPolicy),
  policy: finalityPolicy,
} as const;
const releaseEconomics = {
  schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT_FINGERPRINT,
  blueprintHash: "bb".repeat(32),
  policyDigest: computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
  policy: economicsPolicy,
} as const;

/**
 * A wrongfully rejected forced transaction committed on the emulator, our
 * thread taken as far as `ours` says, then another prover's whole conviction
 * and removal of the header. Returns our run over the recorded raw L1, with
 * the reclaim of our own threads in front of the family adapter as the
 * deployment wires it.
 */
const scene = async (ours: "forcedStep" | "forcedScan") => {
  const recorder = recordCrossBlockRawEmulator();
  const { harness, refs } = await makeHarness();
  const transaction = forcedTransactionAt({ outputNetworkIds: [0, 0] });
  const block = await commitForcedBlock(harness, [
    {
      outputIndex: 0n,
      value: {
        tx_id: transaction.transactionId,
        submitted_source: transaction.source,
        verdict: NETWORK_ID_MISMATCH,
      },
    },
  ]);
  const headerHash = block.setup.headerHash;
  const prepared = preparedFor({
    transaction,
    headerHash,
    header: block.header,
    membership: block.memberships[0]!,
  });

  // Our thread, off the linear chain.
  const stages = makeStages(harness, refs, block.setup);
  const dispatched = await stages.dispatch(await stages.initialize(), prepared);
  const ourThread =
    ours === "forcedStep"
      ? dispatched
      : (await stages.bind(dispatched, prepared)).nextThreadOutRef;

  // Another prover, its own wallet and key, convicts and removes.
  const theirSigner = resolveProverSigner({
    network,
    walletSeedPhrase: generateEmulatorAccount({ lovelace: 0n }).seedPhrase,
  });
  expect(theirSigner.paymentKeyHash).not.toBe(
    harness.proverSigner.paymentKeyHash,
  );
  const funding = await harness.proverLucid
    .newTx()
    .pay.ToAddress(theirSigner.address, { lovelace: 4_000_000_000n })
    .pay.ToAddress(theirSigner.address, { lovelace: 1_000_000_000n })
    .complete({ localUPLCEval: true });
  await harness.proverLucid.awaitTx(
    await (await funding.sign.withWallet().complete()).submit(),
  );
  const theirLucid = await Lucid(harness.emulator, "Custom");
  theirSigner.selectWallet(theirLucid);
  const theirs = makeStages(
    { ...harness, proverLucid: theirLucid, proverSigner: theirSigner },
    refs,
    block.setup,
  );
  const bound = await theirs.bind(
    await theirs.dispatch(await theirs.initialize(), prepared),
    prepared,
  );
  const { plan } = theirs.scanPlanFor(prepared);
  await theirs.finalize(
    await theirs.scan(bound.nextThreadOutRef, prepared, plan),
    prepared,
  );
  await theirs.remove();

  const certificateRef = await publishPlainReferenceScriptUtxo({
    lucid: harness.proverLucid,
    script: harness.networkId.fieldPreimageCertificateMintingScript,
    label: "network-id field certificate",
  });
  const removalRefs = await publishRemovalReferenceScripts({
    lucid: harness.proverLucid,
    contracts: harness.contracts,
  });
  const rawL1 = createNetworkIdRawL1ObservationPort({
    authority: recorder.authority,
    releaseFinality,
    releaseEconomics,
    definition: {
      category: "networkId",
      categoryId: harness.category.categoryId,
      headerHash,
      proverCredential: harness.proverSigner.paymentKeyHash,
      stateQueue: {
        policyId: harness.contracts.stateQueue.policyId,
        address: harness.contracts.stateQueue.spendingScriptAddress,
      },
      computationThread: {
        policyId: harness.networkId.computationThread.policyId,
        steps: [
          {
            role: "computation_thread_step_01",
            address: harness.networkId.steps[0].spendingScriptAddress,
            datumSchema: SDK.FraudProofComputationThreadStepDatum,
          },
          {
            role: "computation_thread_step_02",
            address: harness.networkId.steps[1].spendingScriptAddress,
            datumSchema: SDK.NetworkIdStep02Datum,
          },
        ],
      },
      proofToken: {
        policyId: harness.networkId.fraudProof.policyId,
        address: harness.networkId.fraudProof.spendingScriptAddress,
      },
      operatorDirectory: {
        activePolicyId: harness.contracts.activeOperators.policyId,
        activeAddress: harness.contracts.activeOperators.spendingScriptAddress,
        retiredPolicyId: harness.contracts.retiredOperators.policyId,
        retiredAddress:
          harness.contracts.retiredOperators.spendingScriptAddress,
      },
      schedulerAddress: harness.contracts.scheduler.spendingScriptAddress,
    },
    positions: networkIdOwnThreadPositions(harness.networkId),
  });
  const now = BigInt(harness.emulator.now());
  const config: NetworkIdWorkflowAdapterConfig = {
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: harness.networkId,
    stateQueueAddress: harness.contracts.stateQueue.spendingScriptAddress,
    category: harness.category,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    signer: harness.proverSigner,
    stepReferenceScripts: [refs[0], refs[1]],
    forcedStepReferenceScript: refs[2],
    forcedScanReferenceScript: refs[3],
    fieldPreimageCertificateReferenceScript: certificateRef.utxo,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    removal: {
      deploymentInfo: buildRemovalDeploymentInfo(
        harness.contracts,
        harness.catalogue,
        { removalReferenceScripts: removalRefs.published },
      ),
      category: "networkId",
      requireReferenceScripts: true,
      validFrom: now - 120_000n,
      validTo: now + 300_000n,
    },
    rawL1: {
      ...rawL1,
      observeSignedTransaction: async () => {
        throw new Error(
          "no signed bytes are recorded without a funding reservation",
        );
      },
    },
  };
  const journal = new MemoryFraudProofWorkflowJournalStore();
  const artifact = JSON.parse(
    JSON.stringify(networkIdForcedArtifactFromPrepared(prepared)),
  );
  const run = async (): Promise<FraudProofWorkflowRunResult> =>
    await runAdmittedFraudProofWorkflow({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      category: "networkId",
      headerHash,
      evidenceBinding: {
        route: "canonical_block",
        headerHash,
        payloadEnvelopeSha256: "77".repeat(32),
        payloadSha256: "78".repeat(32),
        l1BlockHash: "79".repeat(32),
        l1Slot: "1",
      },
      prepareFamilyArtifact: async () => artifact,
      registry: createFraudProofWorkflowRegistry({
        adapters: [
          withNetworkIdOwnThreadReclaim({
            base: createNetworkIdWorkflowAdapter(config),
            config,
            contracts: harness.networkId,
            releaseFinality,
          }),
        ],
        launchScope: ["networkId"],
      }),
      journal,
      terminalVerifier,
      releaseFinality,
    });
  return {
    harness,
    headerHash,
    ourThread,
    rawL1,
    run,
    restore: () => recorder.restore(),
  };
};

describe("network-id: our thread off the linear chain on a header another prover's record removed", () => {
  it.each([
    { ours: "forcedStep" as const, step: 2 },
    { ours: "forcedScan" as const, step: 3 },
  ])(
    "cancels our thread at the $ours to our wallet, then resolves by the other record",
    async ({ ours, step }) => {
      const s = await scene(ours);
      try {
        const sent: string[] = [];
        const submit = s.harness.emulator.submitTx.bind(s.harness.emulator);
        s.harness.emulator.submitTx = async (cbor: string) => {
          sent.push(cbor);
          return await submit(cbor);
        };
        const standing = (
          await s.rawL1.observeOwnThreads!({
            headerHash: s.headerHash,
          })
        ).standing;
        expect(standing).toMatchObject({ threadOutRef: s.ourThread, step });
        const thread = standing!;

        // The removal is still within rollback recovery: nothing is sent.
        expect(await s.run()).toMatchObject({
          kind: "pending",
          reason: expect.stringContaining(
            `header removal awaits release finality before our thread ${thread.threadOutRef} is reclaimed`,
          ),
        });
        expect(sent).toEqual([]);

        s.harness.emulator.awaitBlock(FINAL_DEPTH);
        const before = await walletLovelace(s.harness.proverLucid);
        expect((await s.run()).kind).toBe("pending");
        expect(sent).toHaveLength(1);
        const reclaim = CML.Transaction.from_cbor_hex(sent[0]!);
        const reclaimTxHash = CML.hash_transaction(reclaim.body()).to_hex();
        s.harness.emulator.awaitBlock();
        // Landed: our wallet holds the thread's lovelace less the fee.
        expect(await walletLovelace(s.harness.proverLucid)).toBe(
          before + BigInt(thread.lovelace) - reclaim.body().fee(),
        );
        expect(
          (await s.rawL1.observeOwnThreads!({ headerHash: s.headerHash }))
            .standing,
        ).toBeUndefined();
        let result = await s.run();
        const awaitingOurCancel = `reclaim ${reclaimTxHash} of our thread ${thread.threadOutRef} awaits release finality`;
        if (!JSON.stringify(result).includes(awaitingOurCancel))
          result = await s.run();
        expect(result).toMatchObject({
          kind: "pending",
          reason: expect.stringContaining(awaitingOurCancel),
        });

        s.harness.emulator.awaitBlock(FINAL_DEPTH);
        expectResolvedWithReclaim(await s.run(), thread, reclaimTxHash);
        expect(sent).toHaveLength(1);
      } finally {
        s.restore();
      }
    },
    900_000,
  );
});
