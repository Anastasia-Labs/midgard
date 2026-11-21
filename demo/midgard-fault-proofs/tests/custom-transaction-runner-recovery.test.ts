import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML } from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { CURSOR_FAMILY_TRANSACTION_PORT } from "../src/workflow/cursor-family-adapter.admit-reference-scripts.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.create-cursor-family-workflow-adapter.js";
import { BLOCK_SUBJECT } from "../src/workflow/detection-subject.js";
import { FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT } from "../src/workflow/family-l1-observation.js";
import { createFraudProofWorkflowRegistry } from "../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runFraudProofWorkflow } from "../src/workflow/orchestrator.run-da-hash-preimage-workflow-from-retained-da.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import type { SignedWorkflowTransaction } from "../src/workflow/signed-transaction-reconciliation.js";
import type { LocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { terminalVerifierOf } from "./support/terminal-verifier.js";
import { WALLET_ONLY_SEND_ALWAYS_OPEN } from "./support/wallet-only-send-always-open.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";

const hash = (byte: string) => byte.repeat(32);
const categories = [
  "mintDeclaredAssetLimit",
  "observersForbiddenOnUntaggedNetwork",
  "observerOrderInvalid",
  "redeemerCanonicity",
] as const;
const deploymentFingerprint = hash("22");
const key = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x33));
const inputs = CML.TransactionInputList.new();
inputs.add(
  CML.TransactionInput.new(CML.TransactionHash.from_hex(hash("34")), 0n),
);
const outputs = CML.TransactionOutputList.new();
outputs.add(
  CML.TransactionOutput.new(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(key.to_public().hash()),
    ).to_address(),
    CML.Value.from_coin(2000000n),
  ),
);
const body = CML.TransactionBody.new(inputs, outputs, 200000n);
const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: deploymentFingerprint,
  blueprintHash: hash("44"),
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
  policy,
};

// The family builders are covered by their emulator suites. This local transport
// test drives actual common cursor/orchestrator continuation with real CML bytes.
it.each(categories)(
  "%s continues a normal cursor through preflight and submission, sending once",
  async (category) => {
    const fixture = await buildCanonicalBlockFixture({
      transactions: [
        buildFixtureTransaction({ spendInputs: [outRefCbor(7, 0n)], fee: 1n }),
      ],
    });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "local-transport-test",
        grade: "security",
      },
    });
    const process = createTestWorkflowProcess();
    const objective = { category, headerHash: fixture.headerHash };
    const referenceInputs = CML.TransactionInputList.new();
    referenceInputs.add(
      CML.TransactionInput.new(CML.TransactionHash.from_hex(hash("cc")), 0n),
    );
    const transactionBody = CML.TransactionBody.from_cbor_hex(
      body.to_cbor_hex(),
    );
    transactionBody.set_reference_inputs(referenceInputs);
    const continuationWitnesses = CML.TransactionWitnessSet.new();
    const continuationVkeys = CML.VkeywitnessList.new();
    continuationVkeys.add(
      CML.Vkeywitness.new(
        key.to_public(),
        key.sign(CML.hash_transaction(transactionBody).to_raw_bytes()),
      ),
    );
    continuationWitnesses.set_vkeywitnesses(continuationVkeys);
    const transaction = CML.Transaction.new(
      transactionBody,
      continuationWitnesses,
      true,
    );
    const txHash = CML.hash_transaction(transactionBody).to_hex();
    const submit = vi.fn(async () => {
      // The process holds the signed transaction in flight before it is sent.
      expect(
        process
          .sends(objective)
          .inFlight()
          .map((sent) => sent.txHash),
      ).toEqual([txHash]);
      return txHash;
    });
    const pendingSignedRead = async (input: SignedWorkflowTransaction) => ({
      ...input,
      status: "pending" as const,
      canonicalPoint: {
        slot: "100",
        blockNo: "100",
        blockHash: hash("ef"),
        pointId: "test-tip",
      },
      releaseFinalPoint: {
        slot: "70",
        blockNo: "70",
        blockHash: hash("ee"),
        pointId: "test-final",
      },
      inputs: [],
      reason: "local mempool confirmation pending",
    });
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: { category, stepCount: 1, successors: { 1: ["proof_token"] } },
      l1: {
        portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
        category,
        observeHeader: async () => authenticatedHeaderObservation(fixture),
        observe: async () => ({
          provenance: {
            trustClass: "authenticated_cardano_l1",
            sourceId: "local-transport-test",
            grade: "security",
          },
          stage: {
            kind: "not_started",
            stateQueueBlockOutRef: `${hash("34")}#0`,
          },
        }),
        transactionConfirmed: async () => false,
        observeSignedTransaction: pendingSignedRead,
        publications: {
          observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
          observeExact: async () => ({ kind: "not_found" }),
        },
      },
      transactions: {
        portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
        category,
        prepare: async () => ({ category, headerHash: fixture.headerHash }),
        capture: async () => ({
          transaction: {
            txHash,
            signed: {
              toHash: () => txHash,
              toTransaction: () => transaction,
              toCBOR: () => transaction.to_cbor_hex(),
              submit,
            } as unknown as LocallyEvaluatedTransaction["signed"],
            referenceScripts: [
              {
                role: "step",
                outRef: `${hash("cc")}#0`,
                scriptHash: "dd".repeat(28),
              },
            ],
          },
        }),
      },
    });
    const wrapped = {
      ...adapter,
      preflight: async (input: Parameters<typeof adapter.preflight>[0]) => {
        // The artifact is the family's, prepared in this invocation.
        expect(input.artifact).toEqual({
          category,
          headerHash: fixture.headerHash,
        });
        return adapter.preflight(input);
      },
    };
    const violationId = {
      mintDeclaredAssetLimit: "mint-declared-asset-limit",
      observersForbiddenOnUntaggedNetwork:
        "observers-forbidden-on-untagged-network",
      observerOrderInvalid: "observer-order-invalid",
      redeemerCanonicity: "redeemer-malformed",
    }[category];
    const result = await runFraudProofWorkflow({
      walletOnlySend: WALLET_ONLY_SEND_ALWAYS_OPEN,
      deploymentFingerprint,
      evidence,
      detections: [
        {
          ...BLOCK_SUBJECT,
          headerHash: fixture.headerHash,
          detectionId: "local-driver-fixture",
          violationId,
          position: 0n,
        },
      ],
      registry: createFraudProofWorkflowRegistry({
        adapters: [wrapped],
        launchScope: [category],
      }),
      run: process.run(objective),
      terminalVerifier: {
        ...terminalVerifierOf(async () => {
          throw new Error("unconfirmed init has no terminal");
        }),
        // The run follows its sent init on the chain.
        observeSignedTransaction: pendingSignedRead,
      },
      releaseFinalityAuthority: {
        authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
        verifyForWorkflow: async () => releaseFinality,
      },
      maxActions: 2,
    });
    if (result.kind === "stalled") throw new Error(result.reason);
    expect(result).toMatchObject({ kind: "pending" });
    expect(submit).toHaveBeenCalledOnce();
    expect(process.sent().map((sent) => sent.txHash)).toEqual([txHash]);
  },
);
