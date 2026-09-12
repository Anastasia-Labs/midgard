import { createHash } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  CML,
  type Script,
  type TxSigned,
  type UTxO,
  utxoToCore,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import * as removalFunding from "../src/remove-fraudulent-block.js";
import { DaLibp2pRetainedDaSource } from "../src/transition-trace/fetch.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
  createWorkflowActuationPermitController,
  isWorkflowActuationRevokedError,
  type WorkflowActuationCheckpoint,
  workflowActuationDecisionDigest,
  WorkflowActuationRevokedError,
} from "../src/workflow/actuation-permit.js";
import {
  assertWorkflowApplicationRegistry,
  installWorkflowApplicationRegistry,
  validateWorkflowAdapterCoverage,
  WORKFLOW_ADAPTER_REGISTRATIONS,
  WORKFLOW_ADAPTER_RUNNER,
  workflowAdapterRunner,
} from "../src/workflow/adapters.js";
import { DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import {
  assertWorkflowFundingReservationReadyToSubmit,
  beginWorkflowFundingReservationAction,
  bindWorkflowFundingReservationJournal,
  createWorkflowFundingReservationPermit,
  prepareWorkflowFundingReservationTransaction,
  unsafeCreateWorkflowFundingReservationPermitForTest,
  unsafeWorkflowFundingReservationSelectedOutRefsForTest,
  type WorkflowFundingReservationSnapshot,
  type WorkflowFundingSubmissionHandoff,
} from "../src/workflow/funding-reservation-permit.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
} from "../src/workflow/header-classifier.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  MemoryFraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import type { FraudProofWorkflowAction } from "../src/workflow/orchestrator.js";
import { continuePendingWorkflow } from "../src/workflow/pending-continuation.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import {
  createDaHashPreimageWorkflowRunner,
  createManifestBoundWorkflowRunner,
  WORKFLOW_RUNNER_FACTORIES,
  WORKFLOW_RUNTIME_CONFIG,
} from "../src/workflow/runtime.js";
import { readWorkflowRuntimeFundingPolicy } from "../src/workflow/runtime-funding-policy.js";
import {
  bindWorkflowPreflightTransaction,
  workflowPreflightTransaction,
} from "../src/workflow/transaction-boundary.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { runtimeFundingPolicyFixture } from "./helpers/runtime-funding-policy-fixture.js";

const DEPLOYMENT = "d7".repeat(32);
const RELEASE_FINALITY_POLICY = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;

const admittedActuation = async () => {
  const sharedInput = outRefCbor(91, 0n);
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({ spendInputs: [sharedInput], fee: 1n }),
      buildFixtureTransaction({ spendInputs: [sharedInput], fee: 2n }),
    ],
  });
  const observation = authenticatedHeaderObservation(fixture);
  const classifier = await createHeaderClassifier({
    deploymentFingerprint: DEPLOYMENT,
    replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
    releaseFinalityAuthority: {
      authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
      verifyForWorkflow: async () => ({
        schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
        deploymentIdentityDigest: DEPLOYMENT,
        blueprintHash: "f7".repeat(32),
        policyDigest: computeFraudProofReleaseFinalityPolicyDigest(
          RELEASE_FINALITY_POLICY,
        ),
        policy: RELEASE_FINALITY_POLICY,
      }),
    },
  });
  const decision = await classifyHeader({
    classifier,
    observation,
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: 30,
      }),
    sources: [
      {
        sourceId: "libp2p-test",
        fetchPayloadByHeaderHash: async () => ({
          ok: true as const,
          provenance: {
            trustClass: "public_or_permissionless_da" as const,
            sourceId: "libp2p-test/peer-a",
            grade: "security" as const,
          },
          sourceId: "libp2p-test",
          sourcePeerId: "peer-a",
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          attempts: [],
        }),
      },
    ],
  });
  if (decision.decision !== "fault_detected") {
    throw new Error("runtime test failed to classify its fault fixture");
  }
  const controller = createWorkflowActuationPermitController({
    decision,
    rollbackGeneration: "7",
  });
  const fundingReservationPermit =
    unsafeCreateWorkflowFundingReservationPermitForTest({
      category: "doubleSpend",
      actuationPermit: controller.permit,
      deploymentFingerprint: DEPLOYMENT,
      decisionDigest: decision.decisionDigest,
      rollbackGeneration: "7",
    });
  return Object.freeze({
    decisionDigest: decision.decisionDigest,
    actuationPermit: controller.permit,
    fundingReservationPermit,
    headerHash: decision.headerHash,
    revoke: controller.revoke,
  });
};

/** Funding-boundary fixtures supply the durable action metadata alongside real CML bytes. */
const fundingHandoff = (
  actuation: Awaited<ReturnType<typeof admittedActuation>>,
  action: FraudProofWorkflowAction,
  preflight: object,
): WorkflowFundingSubmissionHandoff => {
  const signed = workflowPreflightTransaction(preflight)!;
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: DEPLOYMENT,
    category: "doubleSpend",
    target: { kind: "state_queue_header", headerHash: actuation.headerHash },
    decisionDigest: actuation.decisionDigest,
  };
  return {
    workflowId: computeFraudProofWorkflowId(identity),
    identity,
    preparedArtifactDigest: "ab".repeat(32),
    expectedJournalSequence: 2,
    preflight: {
      kind: "preflight_passed",
      actionId: action.actionId,
      txHash: signed.toHash(),
      localEvaluator: "funding-boundary-fixture",
      referenceScripts: [],
    },
    submissionIntent: {
      kind: "submission_intent",
      actionId: action.actionId,
      actionInput: action.input,
      txHash: signed.toHash(),
      attempt: 1,
    },
  };
};

const fundingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x51));
const fundingAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(fundingKey.to_public().hash()),
)
  .to_address()
  .to_bech32();
const fundingReferenceOutRef = `${"74".repeat(32)}#0`;
const fundingReferenceScript = Object.freeze({
  type: "PlutusV3" as const,
  script: "4d01000033222220051200120011",
});

const runtimeFunding = async (
  actionKind: "step-one" | "step-three" | "verify_source",
  options: Readonly<{
    governedReference?: Script;
    resolvedReference?: Script;
    useStage?: boolean;
    collateral?: boolean;
    begin?: boolean;
    additionalInputs?: readonly UTxO[];
    confirmedInput?: UTxO;
    changedLineage?: boolean;
  }> = {},
) => {
  const actuation = await admittedActuation();
  const { runner, policy } = runtimeFundingPolicyFixture({
    deploymentFingerprint: DEPLOYMENT,
    fundingPaymentKeyHash: fundingKey.to_public().hash().to_hex(),
    referenceScripts:
      options.governedReference === undefined
        ? []
        : [
            {
              outRef: fundingReferenceOutRef,
              scriptHash: validatorToScriptHash(options.governedReference),
            },
          ],
  });
  const values = new Map([
    [`${"71".repeat(32)}#0`, 3_000_000n],
    [`${"72".repeat(32)}#0`, 2_000_000n],
    [`${"73".repeat(32)}#0`, 10_000_000n],
  ]);
  if (options.collateral) values.set(`${"76".repeat(32)}#0`, 5_000_000n);
  const activeInputs = Object.freeze(
    [...values].map(([outRef, lovelace]) =>
      Object.freeze({
        outRef,
        role: outRef.startsWith("76".repeat(32))
          ? ("collateral" as const)
          : ("funding" as const),
        lovelace: lovelace.toString(),
        assets: Object.freeze([]),
      }),
    ),
  );
  const snapshot = Object.freeze({
    reservationId: "b1".repeat(32),
    deploymentFingerprint: DEPLOYMENT,
    decisionDigest: actuation.decisionDigest,
    policyDigest: readWorkflowRuntimeFundingPolicy(policy).policyDigest,
    reservationBasisDigest: "b2".repeat(32),
    rollbackGeneration: "7",
    revision: "0",
    walletAddress: fundingAddress,
    fundingPaymentKeyHash: fundingKey.to_public().hash().to_hex(),
    state: "active" as const,
    activeInputs,
  });
  let currentSnapshot: WorkflowFundingReservationSnapshot = snapshot;
  const prepare = vi.fn(async () => snapshot);
  const resolveInputs = vi.fn(async (outRefs: readonly string[]) =>
    outRefs.map((outRef) => {
      const additional = options.additionalInputs?.find(
        (input) => `${input.txHash}#${input.outputIndex.toString()}` === outRef,
      );
      if (additional !== undefined) return additional;
      if (outRef === fundingReferenceOutRef)
        return {
          txHash: "74".repeat(32),
          outputIndex: 0,
          address: fundingAddress,
          assets: { lovelace: 2_000_000n },
          scriptRef: options.resolvedReference,
        };
      const [txHash, outputIndex] = outRef.split("#");
      return {
        txHash: txHash!,
        outputIndex: Number(outputIndex),
        address: fundingAddress,
        assets: { lovelace: values.get(outRef) ?? 1_000_000n },
      };
    }),
  );
  const permit = await createWorkflowFundingReservationPermit({
    category: "doubleSpend",
    runner,
    policy,
    actuationPermit: actuation.actuationPermit,
    rollbackGeneration: "7",
    port: {
      load: async () => currentSnapshot,
      resolveInputs,
      resolveConfirmedInput: async ({ outRef }) => {
        const input = options.confirmedInput;
        if (
          input === undefined ||
          outRef !== `${input.txHash}#${input.outputIndex.toString()}`
        )
          return null;
        return {
          sourceActionKind: "init",
          sourceOutputIndex: input.outputIndex,
          outRef,
          resolvedOutputCborHex: utxoToCore(
            options.changedLineage
              ? { ...input, assets: { lovelace: 1_000_000n } }
              : input,
          )
            .output()
            .to_canonical_cbor_hex(),
        };
      },
      readPendingTransition: async () => null,
      readPendingHandoff: async () => null,
      readCompletionHandoff: async () => null,
      readAbandonmentHandoff: async () => null,
      acknowledgeAbandonment: async () => snapshot,
      resolveProtocolInputAuthority: async () => {
        throw new Error("test action has no protocol input");
      },
      prepare,
      confirm: async () => snapshot,
      abandon: async () => snapshot,
      markConflict: async () => snapshot,
      release: async () => snapshot,
    },
  });
  const journal = Object.freeze({ actionKind });
  bindWorkflowFundingReservationJournal({ journal, permit });
  const begin = () =>
    beginWorkflowFundingReservationAction({
      journal,
      action: {
        actionId: actionKind,
        input: options.useStage ? { stage: actionKind } : { actionKind },
      },
    });
  if (options.begin !== false) await begin();
  return Object.freeze({
    policy,
    begin,
    resolveInputs,
    journal,
    permit,
    prepare,
    prepareTransaction: (input: {
      action: FraudProofWorkflowAction;
      preflight: object;
    }) =>
      prepareWorkflowFundingReservationTransaction({
        journal,
        ...input,
        handoff: fundingHandoff(actuation, input.action, input.preflight),
      }),
    selected: unsafeWorkflowFundingReservationSelectedOutRefsForTest(permit),
    setSnapshot: (value: WorkflowFundingReservationSnapshot) => {
      currentSnapshot = value;
    },
    snapshot,
  });
};

const runtimeFundingSelection = async (actionKind: "step-one" | "step-three") =>
  (await runtimeFunding(actionKind)).selected;

const signedFundingTransaction = (input: {
  readonly inputOutRefs: readonly string[];
  readonly outputLovelace: bigint;
  readonly referenceOutRefs?: readonly string[];
  readonly fee?: bigint;
  readonly outputAddress?: string;
  readonly additionalOutputs?: readonly CML.TransactionOutput[];
  readonly collateral?: {
    outRefs: readonly string[];
    total: bigint;
    returned: bigint;
    returnAddress?: string;
  };
  readonly redeemerMemory?: bigint;
  readonly signingKey?: CML.PrivateKey;
  readonly nonCanonicalBody?: boolean;
}): TxSigned => {
  const inputs = CML.TransactionInputList.new();
  for (const outRef of input.inputOutRefs) {
    const [txHash, outputIndex] = outRef.split("#");
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(txHash!),
        BigInt(outputIndex!),
      ),
    );
  }
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(input.outputAddress ?? fundingAddress),
      CML.Value.from_coin(input.outputLovelace),
    ),
  );
  for (const output of input.additionalOutputs ?? []) outputs.add(output);
  let body = CML.TransactionBody.new(inputs, outputs, input.fee ?? 200_000n);
  if (input.collateral !== undefined) {
    const collateral = CML.TransactionInputList.new();
    for (const outRef of input.collateral.outRefs) {
      const [txHash, index] = outRef.split("#");
      collateral.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(txHash!),
          BigInt(index!),
        ),
      );
    }
    body.set_collateral_inputs(collateral);
    body.set_total_collateral(input.collateral.total);
    body.set_collateral_return(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(
          input.collateral.returnAddress ?? fundingAddress,
        ),
        CML.Value.from_coin(input.collateral.returned),
      ),
    );
  }
  if (input.referenceOutRefs !== undefined) {
    const references = CML.TransactionInputList.new();
    for (const outRef of input.referenceOutRefs) {
      const [txHash, outputIndex] = outRef.split("#");
      references.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(txHash!),
          BigInt(outputIndex!),
        ),
      );
    }
    body.set_reference_inputs(references);
  }
  if (input.nonCanonicalBody)
    body = CML.TransactionBody.from_cbor_hex(
      "bf" + body.to_cbor_hex().slice(2) + "ff",
    );
  const witnesses = CML.TransactionWitnessSet.new();
  if (input.redeemerMemory !== undefined) {
    const redeemers = CML.LegacyRedeemerList.new();
    redeemers.add(
      CML.LegacyRedeemer.new(
        CML.RedeemerTag.Spend,
        0n,
        CML.PlutusData.from_cbor_hex("00"),
        CML.ExUnits.new(input.redeemerMemory, 1n),
      ),
    );
    witnesses.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
  }
  const signingKey = input.signingKey ?? fundingKey;
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      signingKey.to_public(),
      signingKey.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  witnesses.set_vkeywitnesses(vkeys);
  const transaction = CML.Transaction.new(body, witnesses, true, undefined);
  return {
    toTransaction: () => transaction,
    toHash: () => CML.hash_transaction(body).to_hex(),
  } as unknown as TxSigned;
};

const retainedDaSource = (): DaLibp2pRetainedDaSource =>
  new DaLibp2pRetainedDaSource({
    deploymentFingerprint: DEPLOYMENT,
    peers: [{ peerId: "12D3KooWproductionRuntimeTest" }],
    transport: {
      request: async () => {
        throw new Error("transport is not called by runtime-boundary test");
      },
    },
  });

const prepareRuntimeFunding = (
  runtime: Awaited<ReturnType<typeof runtimeFunding>>,
  signed: TxSigned,
) =>
  runtime.prepareTransaction({
    action: { actionId: "step-one", input: { stage: "step-one" } },
    preflight: bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: signed.toHash() }),
      signed,
    ),
  });

const slashFundingFixture = async (
  options: {
    tranche?: "full" | "partially-inactivity-slashed";
    feeDelta?: bigint;
    rewardDelta?: bigint;
    actionStage?: string;
    includeWalletInput?: boolean;
    capability?: boolean;
    foreignAuthority?: boolean;
  } = {},
) => {
  const actuation = await admittedActuation();
  const tranche = options.tranche ?? "full";
  const bond = tranche === "full" ? 900_000_000n : 800_000_000n;
  const exactFee = bond - 400_000_000n;
  const protocolAddress = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_script(CML.ScriptHash.from_hex("a5".repeat(28))),
  )
    .to_address()
    .to_bech32();
  const operator: UTxO = {
    txHash: "81".repeat(32),
    outputIndex: 0,
    address: protocolAddress,
    assets: { lovelace: bond },
  };
  const anchor: UTxO = {
    txHash: "82".repeat(32),
    outputIndex: 0,
    address: protocolAddress,
    assets: { lovelace: 2_000_000n },
  };
  const proof: UTxO = {
    txHash: "83".repeat(32),
    outputIndex: 0,
    address: protocolAddress,
    assets: { lovelace: 2_000_000n },
  };
  const ref = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex}`;
  const { runner, policy } = runtimeFundingPolicyFixture({
    deploymentFingerprint: DEPLOYMENT,
    fundingPaymentKeyHash: fundingKey.to_public().hash().to_hex(),
    contracts: [
      {
        address: protocolAddress,
        scriptHash: "a5".repeat(28),
        role: "protocol_state",
      },
    ],
  });
  const wallets = [
    {
      txHash: "71".repeat(32),
      outputIndex: 0,
      address: fundingAddress,
      assets: { lovelace: 30_000_000n },
    },
    {
      txHash: "76".repeat(32),
      outputIndex: 0,
      address: fundingAddress,
      assets: { lovelace: 500_000_000n },
    },
    {
      txHash: "77".repeat(32),
      outputIndex: 0,
      address: fundingAddress,
      assets: { lovelace: 500_000_000n },
    },
  ];
  let snapshot: WorkflowFundingReservationSnapshot = {
    reservationId: "b1".repeat(32),
    deploymentFingerprint: DEPLOYMENT,
    decisionDigest: actuation.decisionDigest,
    policyDigest: readWorkflowRuntimeFundingPolicy(policy).policyDigest,
    reservationBasisDigest: "b2".repeat(32),
    rollbackGeneration: "7",
    revision: "0",
    walletAddress: fundingAddress,
    fundingPaymentKeyHash: fundingKey.to_public().hash().to_hex(),
    state: "active",
    activeInputs: wallets.map((utxo, index) => ({
      outRef: ref(utxo),
      role: index === 0 ? "funding" : "collateral",
      lovelace: utxo.assets.lovelace.toString(),
      assets: [],
    })),
  };
  const inputByRef = new Map(
    [operator, anchor, proof, ...wallets].map((utxo) => [ref(utxo), utxo]),
  );
  const prepare = vi.fn(async (_input: unknown) => {
    snapshot = { ...snapshot, revision: "1" };
    return snapshot;
  });
  const protocolAuthority = vi.fn(async ({ outRef }: { outRef: string }) => ({
    deploymentFingerprint: options.foreignAuthority
      ? "ff".repeat(32)
      : DEPLOYMENT,
    outRef,
    semanticRole: "protocol_state",
    resolvedOutputCborHex: utxoToCore(inputByRef.get(outRef)!)
      .output()
      .to_canonical_cbor_hex(),
  }));
  const permit = await createWorkflowFundingReservationPermit({
    category: "doubleSpend",
    runner,
    policy,
    actuationPermit: actuation.actuationPermit,
    rollbackGeneration: "7",
    port: {
      load: async () => snapshot,
      resolveInputs: async (outRefs) =>
        outRefs.map((outRef) => inputByRef.get(outRef)!),
      resolveConfirmedInput: async () => {
        throw new Error("slash inputs must reacquire protocol authority");
      },
      readPendingTransition: async () => null,
      readPendingHandoff: async () => null,
      readCompletionHandoff: async () => null,
      readAbandonmentHandoff: async () => null,
      acknowledgeAbandonment: async () => snapshot,
      resolveProtocolInputAuthority: protocolAuthority,
      prepare,
      confirm: async () => snapshot,
      abandon: async () => snapshot,
      markConflict: async () => snapshot,
      release: async () => snapshot,
    },
  });
  const journal = {};
  bindWorkflowFundingReservationJournal({ journal, permit });
  const action = {
    actionId: "remove-current-target",
    input: {
      stage: options.actionStage ?? "remove",
      category: "doubleSpend",
      nextRemovalOutRef: ref(anchor),
      fraudProofOutRef: ref(proof),
    },
  };
  await beginWorkflowFundingReservationAction({ journal, action });
  const collateral = (exactFee * 150n) / 100n;
  const signed = signedFundingTransaction({
    inputOutRefs: [
      ref(operator),
      ref(anchor),
      ...(options.includeWalletInput ? [ref(wallets[0]!)] : []),
    ].sort(),
    referenceOutRefs: [ref(proof)],
    outputLovelace: 400_000_000n + (options.rewardDelta ?? 0n),
    additionalOutputs: [utxoToCore(anchor).output()],
    fee: exactFee + (options.feeDelta ?? 0n),
    collateral: {
      outRefs: wallets.slice(1).map(ref),
      total: collateral,
      returned: 1_000_000_000n - collateral,
    },
    redeemerMemory: 1n,
    nonCanonicalBody: true,
  });
  const authority: removalFunding.FraudSlashFundingAuthority = {
    deploymentFingerprint: DEPLOYMENT,
    economicsPolicyDigest:
      readWorkflowRuntimeFundingPolicy(policy).economicsPolicyDigest,
    category: "doubleSpend",
    headerHash: actuation.headerHash,
    fraudProofOutRef: ref(proof),
    removedStateQueueOutRef: ref(anchor),
    operatorOutRef: ref(operator),
    operatorBondLovelace: bond.toString(),
    tranche,
    exactFeeLovelace: exactFee.toString(),
    rewardLovelace: "400000000",
    rewardAddress: fundingAddress,
    transactionHash: signed.toHash(),
    transactionBodySha256: createHash("sha256")
      .update(Buffer.from(signed.toTransaction().body().to_cbor_hex(), "hex"))
      .digest("hex"),
    signedTransactionCborHex: signed.toTransaction().to_cbor_hex(),
    inputs: [operator, anchor].map((utxo) => ({
      outRef: ref(utxo),
      resolvedOutputCborHex: utxoToCore(utxo).output().to_canonical_cbor_hex(),
    })),
  };
  // The real minter is private to the evaluated removal builder. Isolate that
  // builder seam while checking actual signed CML wire here.
  const originalReader = removalFunding.readFraudSlashFundingAuthority;
  expect(originalReader(signed)).toBeNull();
  const reader = vi
    .spyOn(removalFunding, "readFraudSlashFundingAuthority")
    .mockImplementation((candidate) =>
      candidate === signed && options.capability !== false
        ? authority
        : originalReader(candidate),
    );
  return {
    signed,
    journal,
    action,
    prepare,
    policy,
    protocolAuthority,
    admit: () => {
      const preflight = bindWorkflowPreflightTransaction(
        { txHash: signed.toHash() },
        signed,
      );
      return prepareWorkflowFundingReservationTransaction({
        journal,
        action,
        preflight,
        handoff: fundingHandoff(actuation, action, preflight),
      });
    },
    close: () => reader.mockRestore(),
  };
};

describe("compiled manifest-bound production runtime V1", () => {
  it.each(["full", "partially-inactivity-slashed"] as const)(
    "admits exact signed %s slash economics without spending ordinary wallet funds",
    async (tranche) => {
      const runtime = await slashFundingFixture({ tranche });
      try {
        await runtime.admit();
        expect(runtime.protocolAuthority).toHaveBeenCalledTimes(2);
        expect(runtime.prepare).toHaveBeenCalledOnce();
        expect(runtime.prepare).toHaveBeenCalledWith(
          expect.objectContaining({
            transition: expect.objectContaining({
              actionKind: "remove",
              consumedOutRefs: [],
              signedTransactionCborHex: runtime.signed
                .toTransaction()
                .to_cbor_hex(),
              producedInputs: [
                expect.objectContaining({ lovelace: "400000000" }),
              ],
            }),
          }),
        );
        const policy = readWorkflowRuntimeFundingPolicy(runtime.policy);
        expect(policy.maximumSlashCollateralLovelace).toBe("750000000");
        expect(BigInt(policy.maximumFeeLovelace)).toBeLessThan(400_000_000n);
        expect(BigInt(policy.maximumCollateralLovelace)).toBeLessThan(
          600_000_000n,
        );
        await expect(
          assertWorkflowFundingReservationReadyToSubmit({
            journal: runtime.journal,
            transactionHash: runtime.signed.toHash(),
          }),
        ).resolves.toBeUndefined();
      } finally {
        runtime.close();
      }
    },
  );

  it.each([
    { label: "fee below tranche", feeDelta: -1n },
    { label: "fee above tranche", feeDelta: 1n },
    { label: "reward below release", rewardDelta: -1n },
    { label: "reward above release", rewardDelta: 1n },
    { label: "different action", actionStage: "step-one" },
    { label: "missing private capability", capability: false },
    { label: "ordinary high fee", capability: false, actionStage: "step-one" },
    { label: "ordinary wallet input", includeWalletInput: true },
    { label: "foreign protocol authority", foreignAuthority: true },
  ])(
    "rejects signed slash funding with $label before durable preparation",
    async (options) => {
      const runtime = await slashFundingFixture(options);
      try {
        await expect(runtime.admit()).rejects.toThrow();
        expect(runtime.prepare).not.toHaveBeenCalled();
      } finally {
        runtime.close();
      }
    },
  );

  it("rechecks the exact signed slash bytes before submission", async () => {
    const runtime = await slashFundingFixture();
    try {
      await runtime.admit();
      const changed = signedFundingTransaction({
        inputOutRefs: [],
        outputLovelace: 400_000_001n,
      });
      runtime.signed.toTransaction = changed.toTransaction;
      await expect(
        assertWorkflowFundingReservationReadyToSubmit({
          journal: runtime.journal,
          transactionHash: runtime.signed.toHash(),
        }),
      ).rejects.toThrow("changed before submission");
    } finally {
      runtime.close();
    }
  });

  it("prepares the exact signed wire and original body digest used by default submission", async () => {
    const runtime = await runtimeFunding("step-one");
    const signed = signedFundingTransaction({
      inputOutRefs: runtime.selected.fundingOutRefs,
      outputLovelace: 14_800_000n,
      nonCanonicalBody: true,
    });
    const transaction = signed.toTransaction();
    expect(transaction.to_cbor_hex()).not.toBe(
      transaction.to_canonical_cbor_hex(),
    );
    await prepareRuntimeFunding(runtime, signed);
    expect(runtime.prepare).toHaveBeenCalledWith(
      expect.objectContaining({
        transition: expect.objectContaining({
          signedTransactionCborHex: transaction.to_cbor_hex(),
          transactionBodySha256: createHash("sha256")
            .update(Buffer.from(transaction.body().to_cbor_hex(), "hex"))
            .digest("hex"),
        }),
      }),
    );
  });

  it("reserves the consumed signed subset while leaving unused leased candidates intact", async () => {
    const runtime = await runtimeFunding("step-one");
    const input = `${"73".repeat(32)}#0`;
    await prepareRuntimeFunding(
      runtime,
      signedFundingTransaction({
        inputOutRefs: [input],
        outputLovelace: 9_800_000n,
      }),
    );
    expect(runtime.prepare).toHaveBeenCalledWith(
      expect.objectContaining({
        transition: expect.objectContaining({ consumedOutRefs: [input] }),
      }),
    );
  });

  it("defers live input resolution until after the durable pending intent can reconcile", async () => {
    const runtime = await runtimeFunding("step-one", { begin: false });
    expect(runtime.resolveInputs).not.toHaveBeenCalled();
    await runtime.begin();
    expect(runtime.resolveInputs).toHaveBeenCalledWith(
      runtime.snapshot.activeInputs.map(({ outRef }) => outRef),
    );
  });

  it.each(["foreign-wallet", "signature", "fee", "execution-units"] as const)(
    "refuses actual signed %s before preparing any durable transition",
    async (kind) => {
      const runtime = await runtimeFunding("step-one");
      const maximumFee = BigInt(
        readWorkflowRuntimeFundingPolicy(runtime.policy).maximumFeeLovelace,
      );
      const otherKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x52));
      const otherAddress = CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_pub_key(otherKey.to_public().hash()),
      )
        .to_address()
        .to_bech32();
      const fee = kind === "fee" ? maximumFee + 1n : 200_000n;
      const signed = signedFundingTransaction({
        inputOutRefs: runtime.selected.fundingOutRefs,
        outputLovelace: 15_000_000n - fee,
        fee,
        ...(kind === "foreign-wallet" ? { outputAddress: otherAddress } : {}),
        ...(kind === "signature" ? { signingKey: otherKey } : {}),
        ...(kind === "execution-units" ? { redeemerMemory: 14_000_001n } : {}),
      });
      await expect(prepareRuntimeFunding(runtime, signed)).rejects.toThrow(
        kind === "foreign-wallet"
          ? "escapes"
          : kind === "signature"
            ? "signature"
            : kind === "fee"
              ? "fee"
              : "maxTxExUnits",
      );
      expect(runtime.prepare).not.toHaveBeenCalled();
    },
  );

  it("accepts exact min-Ada custody and refuses an extra wallet-funded lovelace", async () => {
    for (const surplus of [0n, 1n]) {
      const runtime = await runtimeFunding("step-one");
      const policy = readWorkflowRuntimeFundingPolicy(runtime.policy);
      const address = CML.Address.from_bech32(policy.contracts[0]!.address);
      const datum = () =>
        CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex("00"));
      const minimum = CML.min_ada_required(
        CML.TransactionOutput.new(
          address,
          CML.Value.from_coin(2_000_000n),
          datum(),
        ),
        4310n,
      );
      const allocation = minimum + surplus;
      const signed = signedFundingTransaction({
        inputOutRefs: runtime.selected.fundingOutRefs,
        outputLovelace: 14_800_000n - allocation,
        additionalOutputs: [
          CML.TransactionOutput.new(
            address,
            CML.Value.from_coin(allocation),
            datum(),
          ),
        ],
      });
      if (surplus === 0n)
        await expect(
          prepareRuntimeFunding(runtime, signed),
        ).resolves.toBeUndefined();
      else
        await expect(prepareRuntimeFunding(runtime, signed)).rejects.toThrow(
          "custody allocation",
        );
    }
  });

  it("requires exact confirmed lineage before reusing locked workflow capital", async () => {
    const sample = await runtimeFunding("step-one");
    const address = readWorkflowRuntimeFundingPolicy(sample.policy)
      .contracts[0]!.address;
    const locked: UTxO = {
      txHash: "77".repeat(32),
      outputIndex: 0,
      address,
      assets: { lovelace: 3_000_000n },
      datum: "00",
    };
    for (const lineage of ["exact", "missing", "changed"] as const) {
      const runtime = await runtimeFunding("step-one", {
        additionalInputs: [locked],
        ...(lineage === "missing" ? {} : { confirmedInput: locked }),
        changedLineage: lineage === "changed",
      });
      const signed = signedFundingTransaction({
        inputOutRefs: [
          ...runtime.selected.fundingOutRefs,
          `${locked.txHash}#0`,
        ],
        outputLovelace: 14_800_000n,
        additionalOutputs: [utxoToCore(locked).output()],
      });
      if (lineage === "exact")
        await expect(
          prepareRuntimeFunding(runtime, signed),
        ).resolves.toBeUndefined();
      else
        await expect(prepareRuntimeFunding(runtime, signed)).rejects.toThrow(
          "lineage",
        );
    }
  });

  it("keeps the collateral availability floor separate from exact failure forfeiture", async () => {
    const runtime = await runtimeFunding("step-one", { collateral: true });
    await expect(
      prepareRuntimeFunding(
        runtime,
        signedFundingTransaction({
          inputOutRefs: runtime.selected.fundingOutRefs,
          outputLovelace: 14_800_000n,
          redeemerMemory: 1n,
          collateral: {
            outRefs: runtime.selected.collateralOutRefs,
            total: 300_000n,
            returned: 4_700_000n,
          },
        }),
      ),
    ).resolves.toBeUndefined();
    const insufficient = await runtimeFunding("step-one", { collateral: true });
    await expect(
      prepareRuntimeFunding(
        insufficient,
        signedFundingTransaction({
          inputOutRefs: insufficient.selected.fundingOutRefs,
          outputLovelace: 14_800_000n,
          redeemerMemory: 1n,
          collateral: {
            outRefs: insufficient.selected.collateralOutRefs,
            total: 299_999n,
            returned: 4_700_001n,
          },
        }),
      ),
    ).rejects.toThrow("collateral");
  });

  it("uses the unchanged journal stage to prepare actual signed funding", async () => {
    const runtime = await runtimeFunding("verify_source", {
      useStage: true,
    });
    const preflight = bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: "stage-funded" }),
      signedFundingTransaction({
        inputOutRefs: runtime.selected.fundingOutRefs,
        outputLovelace: 14_800_000n,
      }),
    );
    await expect(
      runtime.prepareTransaction({
        action: {
          actionId: "verify_source",
          input: { stage: "verify_source" },
        },
        preflight,
      }),
    ).resolves.toBeUndefined();
    expect(runtime.prepare).toHaveBeenCalledTimes(1);
  });

  it("binds each funding reservation permit to exactly one workflow journal", async () => {
    const authority = await admittedActuation();
    const first = Object.freeze({ id: "first" });
    const second = Object.freeze({ id: "second" });
    expect(
      bindWorkflowFundingReservationJournal({
        journal: first,
        permit: authority.fundingReservationPermit,
      }),
    ).toBe(first);
    expect(() =>
      bindWorkflowFundingReservationJournal({
        journal: second,
        permit: authority.fundingReservationPermit,
      }),
    ).toThrow("already bound to a workflow journal");
  });

  it("exposes only durable leased candidates independently of action samples", async () => {
    await expect(runtimeFundingSelection("step-one")).resolves.toEqual({
      fundingOutRefs: [
        `${"71".repeat(32)}#0`,
        `${"72".repeat(32)}#0`,
        `${"73".repeat(32)}#0`,
      ],
      collateralOutRefs: [],
    });
    await expect(runtimeFundingSelection("step-three")).resolves.toEqual({
      fundingOutRefs: [
        `${"71".repeat(32)}#0`,
        `${"72".repeat(32)}#0`,
        `${"73".repeat(32)}#0`,
      ],
      collateralOutRefs: [],
    });
  });

  it("binds the actual signed body to durable leased wallet inputs", async () => {
    const runtime = await runtimeFunding("step-one");
    const action = { actionId: "step-one", input: { actionKind: "step-one" } };
    const validPreflight = bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: "valid" }),
      signedFundingTransaction({
        inputOutRefs: runtime.selected.fundingOutRefs,
        outputLovelace: 14_800_000n,
      }),
    );
    await expect(
      runtime.prepareTransaction({
        action,
        preflight: validPreflight,
      }),
    ).resolves.toBeUndefined();
    expect(runtime.prepare).toHaveBeenCalledTimes(1);

    const hostile = await runtimeFunding("step-one");
    const substitutedPreflight = bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: "substituted" }),
      signedFundingTransaction({
        inputOutRefs: [
          ...hostile.selected.fundingOutRefs,
          `${"75".repeat(32)}#0`,
        ],
        outputLovelace: 15_800_000n,
      }),
    );
    await expect(
      hostile.prepareTransaction({
        action,
        preflight: substitutedPreflight,
      }),
    ).rejects.toThrow("unreserved wallet input");
    expect(hostile.prepare).not.toHaveBeenCalled();
  });

  it("rejects a substituted reference script even when body topology and byte count match", async () => {
    const substitutedScript = Object.freeze({
      type: "PlutusV3" as const,
      script: "4d01000033222220051200120012",
    });
    const runtime = await runtimeFunding("step-one", {
      governedReference: fundingReferenceScript,
      resolvedReference: substitutedScript,
    });
    const preflight = bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: "reference-substitution" }),
      signedFundingTransaction({
        inputOutRefs: runtime.selected.fundingOutRefs,
        outputLovelace: 14_800_000n,
        referenceOutRefs: [fundingReferenceOutRef],
      }),
    );
    await expect(
      runtime.prepareTransaction({
        action: { actionId: "step-one", input: { actionKind: "step-one" } },
        preflight,
      }),
    ).rejects.toThrow("ungoverned reference script");
    expect(runtime.prepare).not.toHaveBeenCalled();
  });

  it("rechecks exact reserved values after durable refresh", async () => {
    const runtime = await runtimeFunding("step-three");
    runtime.setSnapshot(
      Object.freeze({
        ...runtime.snapshot,
        revision: "1",
        activeInputs: Object.freeze([
          ...runtime.snapshot.activeInputs,
          Object.freeze({
            outRef: `${"75".repeat(32)}#0`,
            role: "collateral" as const,
            lovelace: "1",
            assets: Object.freeze([]),
          }),
        ]),
      }),
    );
    await expect(
      assertWorkflowFundingReservationReadyToSubmit({
        journal: runtime.journal,
        transactionHash: "00".repeat(32),
      }),
    ).rejects.toThrow("resolver changed reserved lovelace");
  });

  it("admits every fixed factory only for its exact application category", () => {
    const categories = Object.keys(
      WORKFLOW_RUNNER_FACTORIES,
    ) as (keyof typeof WORKFLOW_RUNNER_FACTORIES)[];
    for (const category of categories) {
      const runner = WORKFLOW_RUNNER_FACTORIES[category](async () => {
        throw new Error(`${category} loader is not invoked during admission`);
      });
      const registry = installWorkflowApplicationRegistry({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: [category],
        installations: [
          { category, deploymentFingerprint: DEPLOYMENT, runner },
        ],
      });
      expect(
        registry.registrations.find(
          (registration) => registration.category === category,
        ),
      ).toMatchObject({ category, status: "ready", runner });
      const otherCategory = categories.find(
        (candidate) => candidate !== category,
      )!;
      expect(() =>
        installWorkflowApplicationRegistry({
          deploymentFingerprint: DEPLOYMENT,
          requiredInstalledCategories: [otherCategory],
          installations: [
            {
              category: otherCategory,
              deploymentFingerprint: DEPLOYMENT,
              runner,
            },
          ],
        }),
      ).toThrow("module-admitted category-bound runner");
    }
  });

  it("does not admit the public generic constructor as a production family runner", () => {
    const generic = createManifestBoundWorkflowRunner({
      category: "doubleSpend",
      loadRuntimeConfig: async () => {
        throw new Error("generic runner is not invoked during admission");
      },
      constructWorkflow: async () => {
        throw new Error("generic runner is not invoked during admission");
      },
      execute: async () => {
        throw new Error("generic runner is not invoked during admission");
      },
    });
    expect(() =>
      validateWorkflowAdapterCoverage(
        WORKFLOW_ADAPTER_REGISTRATIONS.map((registration) =>
          registration.category === "doubleSpend"
            ? { ...registration, status: "ready", runner: generic }
            : registration,
        ),
      ),
    ).toThrow("no compiled executable runner admitted for its exact category");
  });

  it("installs an immutable deployment-bound application overlay without mutating the static catalogue", () => {
    const runner = createDaHashPreimageWorkflowRunner(async () => {
      throw new Error("installed Q44 loader reached");
    });
    const registry = installWorkflowApplicationRegistry({
      deploymentFingerprint: DEPLOYMENT,
      requiredInstalledCategories: ["daHashPreimage"],
      installations: [
        {
          category: "daHashPreimage",
          deploymentFingerprint: DEPLOYMENT,
          runner,
        },
      ],
    });
    expect(() => assertWorkflowApplicationRegistry(registry)).not.toThrow();
    expect(Object.isFrozen(registry)).toBe(true);
    expect(Object.isFrozen(registry.installedCategories)).toBe(true);
    expect(Object.isFrozen(registry.registrations)).toBe(true);
    expect(registry.registrations).toHaveLength(
      WORKFLOW_ADAPTER_REGISTRATIONS.length,
    );
    expect(
      registry.registrations.find(
        (registration) => registration.category === "daHashPreimage",
      ),
    ).toMatchObject({ status: "ready", runner });
    expect(
      WORKFLOW_ADAPTER_REGISTRATIONS.find(
        (registration) => registration.category === "daHashPreimage",
      ),
    ).toMatchObject({ status: "missing" });
    expect(workflowAdapterRunner("daHashPreimage", registry)).toBe(runner);
  });

  it("rejects incomplete, duplicate, unrecognized, forged, and cross-category application installations", () => {
    const doubleSpend = WORKFLOW_RUNNER_FACTORIES.doubleSpend(async () => {
      throw new Error("not invoked");
    });
    const daHashPreimage = WORKFLOW_RUNNER_FACTORIES.daHashPreimage(
      async () => {
        throw new Error("not invoked");
      },
    );
    const install = (
      input: Parameters<typeof installWorkflowApplicationRegistry>[0],
    ) => installWorkflowApplicationRegistry(input);

    expect(() =>
      install({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: ["doubleSpend", "daHashPreimage"],
        installations: [
          {
            category: "doubleSpend",
            deploymentFingerprint: DEPLOYMENT,
            runner: doubleSpend,
          },
        ],
      }),
    ).toThrow("installation cardinality mismatch");
    expect(() =>
      install({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: ["doubleSpend", "daHashPreimage"],
        installations: [
          {
            category: "doubleSpend",
            deploymentFingerprint: DEPLOYMENT,
            runner: doubleSpend,
          },
          {
            category: "doubleSpend",
            deploymentFingerprint: DEPLOYMENT,
            runner: doubleSpend,
          },
        ],
      }),
    ).toThrow("duplicates doubleSpend");
    expect(() =>
      install({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: ["daHashPreimage"],
        installations: [
          {
            category: "daHashPreimage",
            deploymentFingerprint: "ff".repeat(32),
            runner: daHashPreimage,
          },
        ],
      }),
    ).toThrow("unrecognized deployment identity");
    expect(() =>
      install({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: ["daHashPreimage"],
        installations: [
          {
            category: "daHashPreimage",
            deploymentFingerprint: DEPLOYMENT,
            runner: doubleSpend,
          },
        ],
      }),
    ).toThrow("module-admitted category-bound runner");
    expect(() =>
      install({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: ["daHashPreimage"],
        installations: [
          {
            category: "daHashPreimage",
            deploymentFingerprint: DEPLOYMENT,
            runner: {
              runnerVersion: WORKFLOW_ADAPTER_RUNNER,
              runOrResume: async () => undefined,
            },
          },
        ],
      }),
    ).toThrow("module-admitted category-bound runner");
    expect(() =>
      assertWorkflowApplicationRegistry({
        schemaVersion: "midgard-production-fraud-proof-application-registry-v1",
        deploymentFingerprint: DEPLOYMENT,
        installedCategories: ["daHashPreimage"],
        registrations: WORKFLOW_ADAPTER_REGISTRATIONS,
      }),
    ).toThrow("not installed through the authenticated immutable boundary");
  });

  it("constructs the exact workflow and supplies a restart-durable directory journal", async () => {
    const actuation = await admittedActuation();
    const directory = await mkdtemp(join(tmpdir(), "midgard-runtime-v1-"));
    const journalDirectory = join(directory, "journal");
    const close = vi.fn(async () => undefined);
    const loadRuntimeConfig = vi.fn(async () => ({
      schemaVersion: WORKFLOW_RUNTIME_CONFIG,
      config: { releaseConfig: "manifest-bound" },
      retainedDaSources: [retainedDaSource()],
      close,
    }));
    const constructWorkflow = vi.fn(async () => ({
      binding: {
        deploymentFingerprint: DEPLOYMENT,
        definition: {
          category: "doubleSpend" as const,
          headerHash: actuation.headerHash,
        },
      },
    }));
    const execute = vi.fn(async ({ journal, mode }) => {
      expect(journal).toBeInstanceOf(DirectoryFraudProofWorkflowJournalStore);
      expect(mode).toBe("resume");
      const identity: FraudProofWorkflowIdentity = {
        schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
        deploymentFingerprint: DEPLOYMENT,
        category: "doubleSpend",
        target: {
          kind: "state_queue_header",
          headerHash: actuation.headerHash,
        },
        decisionDigest: actuation.decisionDigest,
      };
      const workflowId = computeFraudProofWorkflowId(identity);
      await journal.append(
        {
          schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
          workflowId,
          identity,
          sequence: 0,
          recordedAt: "2026-08-29T00:00:00.000Z",
          event: { kind: "started" },
        },
        0,
      );
      return { workflowId };
    });
    try {
      const runner = createManifestBoundWorkflowRunner({
        category: "doubleSpend",
        loadRuntimeConfig,
        constructWorkflow,
        execute,
      });
      expect(runner.runnerVersion).toBe(WORKFLOW_ADAPTER_RUNNER);
      const result = await runner.runOrResume({
        mode: "resume",
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: actuation.headerHash,
        decisionDigest: actuation.decisionDigest,
        actuationPermit: actuation.actuationPermit,
        fundingReservationPermit: actuation.fundingReservationPermit,
        journalDirectory,
        runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
      });
      expect(loadRuntimeConfig).toHaveBeenCalledWith({
        runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
        invocation: expect.objectContaining({
          category: "doubleSpend",
          deploymentFingerprint: DEPLOYMENT,
          headerHash: actuation.headerHash,
        }),
      });
      expect(constructWorkflow).toHaveBeenCalledWith({
        releaseConfig: "manifest-bound",
      });
      expect(close).toHaveBeenCalledOnce();
      const workflowId = (result as { readonly workflowId: string }).workflowId;
      await expect(
        new DirectoryFraudProofWorkflowJournalStore(journalDirectory).load(
          workflowId,
        ),
      ).resolves.toHaveLength(1);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("keeps one admitted runner session until pending work completes", async () => {
    const actuation = await admittedActuation();
    const close = vi.fn(async () => undefined);
    const sources = [retainedDaSource()];
    const workflow = {
      binding: {
        deploymentFingerprint: DEPLOYMENT,
        definition: {
          category: "doubleSpend" as const,
          headerHash: actuation.headerHash,
        },
      },
    };
    const loadRuntimeConfig = vi.fn(async () => ({
      schemaVersion: WORKFLOW_RUNTIME_CONFIG,
      config: {},
      retainedDaSources: sources,
      close,
    }));
    const constructWorkflow = vi.fn(async () => workflow);
    const completed = { kind: "completed", workflowId: "existing-workflow" };
    const execute =
      vi.fn<
        Parameters<
          typeof createManifestBoundWorkflowRunner<
            "doubleSpend",
            Record<string, never>,
            typeof workflow
          >
        >[0]["execute"]
      >();
    execute.mockImplementation(async () =>
      execute.mock.calls.length < 3 ? { kind: "pending" } : completed,
    );
    const runner = createManifestBoundWorkflowRunner({
      category: "doubleSpend",
      loadRuntimeConfig,
      constructWorkflow,
      execute,
    });
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    try {
      const running = runner.runOrResume({
        mode: "run",
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: actuation.headerHash,
        decisionDigest: actuation.decisionDigest,
        actuationPermit: actuation.actuationPermit,
        fundingReservationPermit: actuation.fundingReservationPermit,
        journalDirectory: "/tmp/midgard-runtime-pending-session",
        runtimeConfigPath: "/etc/midgard/runtime.json",
      });
      await vi.advanceTimersByTimeAsync(0);
      expect(execute).toHaveBeenCalledTimes(1);
      expect(close).not.toHaveBeenCalled();
      await vi.advanceTimersByTimeAsync(999);
      expect(execute).toHaveBeenCalledTimes(1);
      await vi.advanceTimersByTimeAsync(1);
      expect(execute).toHaveBeenCalledTimes(2);
      expect(close).not.toHaveBeenCalled();
      await vi.advanceTimersByTimeAsync(1_000);
      await expect(running).resolves.toBe(completed);
      expect(execute).toHaveBeenCalledTimes(3);
      const calls = execute.mock.calls;
      expect(calls.map(([input]) => input.mode)).toEqual([
        "run",
        "resume",
        "resume",
      ]);
      for (const [input] of calls) {
        expect(input.journal).toBe(calls[0]![0].journal);
        expect(input.workflow).toBe(workflow);
        expect(input.sources).toBe(sources);
      }
      expect(loadRuntimeConfig).toHaveBeenCalledOnce();
      expect(constructWorkflow).toHaveBeenCalledOnce();
      expect(close).toHaveBeenCalledOnce();
    } finally {
      vi.useRealTimers();
    }
  });

  it("stops pending continuation when authority is revoked and closes the session", async () => {
    const actuation = await admittedActuation();
    const close = vi.fn(async () => undefined);
    const sources = [retainedDaSource()];
    const execute = vi.fn(async () => ({ kind: "pending" }));
    const runner = createManifestBoundWorkflowRunner({
      category: "doubleSpend",
      loadRuntimeConfig: async () => ({
        schemaVersion: WORKFLOW_RUNTIME_CONFIG,
        config: {},
        retainedDaSources: sources,
        close,
      }),
      constructWorkflow: async () => ({
        binding: {
          deploymentFingerprint: DEPLOYMENT,
          definition: {
            category: "doubleSpend" as const,
            headerHash: actuation.headerHash,
          },
        },
      }),
      execute,
    });
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    try {
      const running = runner.runOrResume({
        mode: "resume",
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: actuation.headerHash,
        decisionDigest: actuation.decisionDigest,
        actuationPermit: actuation.actuationPermit,
        fundingReservationPermit: actuation.fundingReservationPermit,
        journalDirectory: "/tmp/midgard-runtime-pending-revocation",
        runtimeConfigPath: "/etc/midgard/runtime.json",
      });
      const rejected = running.catch((error: unknown) => error);
      await vi.advanceTimersByTimeAsync(0);
      expect(execute).toHaveBeenCalledTimes(1);
      actuation.revoke("canonical rollback observed during confirmation");
      await vi.advanceTimersByTimeAsync(1_000);
      expect(isWorkflowActuationRevokedError(await rejected)).toBe(true);
      expect(execute).toHaveBeenCalledTimes(1);
      expect(close).toHaveBeenCalledOnce();
    } finally {
      vi.useRealTimers();
    }
  });

  it("returns every non-pending continuation result unchanged", async () => {
    const actuation = await admittedActuation();
    const journal = bindWorkflowActuationJournal({
      journal: new MemoryFraudProofWorkflowJournalStore(),
      permit: actuation.actuationPermit,
      decisionDigest: actuation.decisionDigest,
      deploymentFingerprint: DEPLOYMENT,
      category: "doubleSpend",
      headerHash: actuation.headerHash,
    });
    const values = [
      { kind: "stalled" },
      { kind: "awaiting_counterparty" },
      { kind: "unknown-result" },
      { state: "pending" },
      undefined,
      null,
      "pending",
    ];
    for (const value of values) {
      const execute = vi.fn(async () => value);
      await expect(
        continuePendingWorkflow({
          invocation: {
            mode: "resume",
            deploymentFingerprint: DEPLOYMENT,
            category: "doubleSpend",
            headerHash: actuation.headerHash,
          },
          journal,
          execute,
        }),
      ).resolves.toBe(value);
      expect(execute).toHaveBeenCalledExactlyOnceWith("resume");
    }
  });

  it("rejects a revoked decision permit before loading runtime infrastructure", async () => {
    const actuation = await admittedActuation();
    actuation.revoke("canonical rollback observed");
    const loadRuntimeConfig = vi.fn(async () => {
      throw new Error("revoked runner must not load infrastructure");
    });
    const runner = createManifestBoundWorkflowRunner({
      category: "doubleSpend",
      loadRuntimeConfig,
      constructWorkflow: async () => {
        throw new Error("revoked runner must not construct a workflow");
      },
      execute: async () => {
        throw new Error("revoked runner must not execute");
      },
    });
    let rejected: unknown;
    try {
      await runner.runOrResume({
        mode: "resume",
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: actuation.headerHash,
        decisionDigest: actuation.decisionDigest,
        actuationPermit: actuation.actuationPermit,
        fundingReservationPermit: actuation.fundingReservationPermit,
        journalDirectory: "/tmp/midgard-runtime-revoked",
        runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
      });
    } catch (error) {
      rejected = error;
    }
    expect(rejected).toBeInstanceOf(WorkflowActuationRevokedError);
    expect(isWorkflowActuationRevokedError(rejected)).toBe(true);
    expect(
      isWorkflowActuationRevokedError(
        new WorkflowActuationRevokedError({
          decisionDigest: actuation.decisionDigest,
          rollbackGeneration: "7",
          checkpoint: "runner_start",
          revocationReason: "forged",
        }),
      ),
    ).toBe(false);
    expect(loadRuntimeConfig).not.toHaveBeenCalled();
  });

  it("checks the live permit at every shared workflow actuation boundary", async () => {
    const actuation = await admittedActuation();
    const journal = bindWorkflowActuationJournal({
      journal: new MemoryFraudProofWorkflowJournalStore(),
      permit: actuation.actuationPermit,
      decisionDigest: actuation.decisionDigest,
      deploymentFingerprint: DEPLOYMENT,
      category: "doubleSpend",
      headerHash: actuation.headerHash,
    });
    expect(workflowActuationDecisionDigest(journal)).toBe(
      actuation.decisionDigest,
    );
    const checkpoints: readonly WorkflowActuationCheckpoint[] = [
      "workflow_resume",
      "before_observe",
      "before_preflight",
      "before_submit",
      "before_reconcile",
      "before_terminal_verify",
    ];
    for (const checkpoint of checkpoints) {
      expect(() =>
        assertWorkflowJournalActuation({
          journal,
          deploymentFingerprint: DEPLOYMENT,
          category: "doubleSpend",
          headerHash: actuation.headerHash,
          checkpoint,
        }),
      ).not.toThrow();
    }
    actuation.revoke("canonical rollback observed");
    for (const checkpoint of checkpoints) {
      let rejected: unknown;
      try {
        assertWorkflowJournalActuation({
          journal,
          deploymentFingerprint: DEPLOYMENT,
          category: "doubleSpend",
          headerHash: actuation.headerHash,
          checkpoint,
        });
      } catch (error) {
        rejected = error;
      }
      expect(isWorkflowActuationRevokedError(rejected)).toBe(true);
      expect(rejected).toMatchObject({
        decisionDigest: actuation.decisionDigest,
        rollbackGeneration: "7",
        checkpoint,
      });
    }
  });

  it("rejects substituted manifest identity and non-libp2p DA sources before execution", async () => {
    const actuation = await admittedActuation();
    const execute = vi.fn(async () => ({ kind: "unexpected" }));
    const identityClose = vi.fn(async () => undefined);
    const runner = createManifestBoundWorkflowRunner({
      category: "doubleSpend",
      loadRuntimeConfig: async () => ({
        schemaVersion: WORKFLOW_RUNTIME_CONFIG,
        config: undefined,
        retainedDaSources: [retainedDaSource()],
        close: identityClose,
      }),
      constructWorkflow: async () => ({
        binding: {
          deploymentFingerprint: "ff".repeat(32),
          definition: {
            category: "doubleSpend" as const,
            headerHash: actuation.headerHash,
          },
        },
      }),
      execute,
    });
    await expect(
      runner.runOrResume({
        mode: "run",
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: actuation.headerHash,
        decisionDigest: actuation.decisionDigest,
        actuationPermit: actuation.actuationPermit,
        fundingReservationPermit: actuation.fundingReservationPermit,
        journalDirectory: "/tmp/midgard-runtime-rejected",
        runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
      }),
    ).rejects.toThrow("identity differs from the compiled CLI invocation");
    expect(identityClose).toHaveBeenCalledOnce();
    expect(execute).not.toHaveBeenCalled();

    const sourceActuation = await admittedActuation();
    const sourceClose = vi.fn(async () => undefined);
    const forgedSourceRunner = createManifestBoundWorkflowRunner({
      category: "doubleSpend",
      loadRuntimeConfig: async () => ({
        schemaVersion: WORKFLOW_RUNTIME_CONFIG,
        config: undefined,
        retainedDaSources: [
          {
            sourceId: "operator-private-file",
            fetchPayloadByHeaderHash: async () => ({
              ok: false as const,
              sourceId: "operator-private-file",
              attempts: [],
            }),
          } as unknown as DaLibp2pRetainedDaSource,
        ],
        close: sourceClose,
      }),
      constructWorkflow: async () => ({
        binding: {
          deploymentFingerprint: DEPLOYMENT,
          definition: {
            category: "doubleSpend" as const,
            headerHash: sourceActuation.headerHash,
          },
        },
      }),
      execute,
    });
    await expect(
      forgedSourceRunner.runOrResume({
        mode: "run",
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: sourceActuation.headerHash,
        decisionDigest: sourceActuation.decisionDigest,
        actuationPermit: sourceActuation.actuationPermit,
        fundingReservationPermit: sourceActuation.fundingReservationPermit,
        journalDirectory: "/tmp/midgard-runtime-rejected",
        runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
      }),
    ).rejects.toThrow("concrete public retained-DA libp2p sources");
    expect(sourceClose).toHaveBeenCalledOnce();
    expect(execute).not.toHaveBeenCalled();
  });
});
