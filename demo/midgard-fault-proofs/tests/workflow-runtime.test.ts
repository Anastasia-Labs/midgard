import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  CML,
  type Script,
  type TxSigned,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

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
import { createWorkflowFundingRequirements } from "../src/workflow/funding-requirements.js";
import { unsafeCreateMeasuredWorkflowRunnerForTest } from "../src/workflow/funding-requirements-test-support.js";
import {
  assertWorkflowFundingReservationReadyToSubmit,
  beginWorkflowFundingReservationAction,
  bindWorkflowFundingReservationJournal,
  createWorkflowFundingReservationPermit,
  prepareWorkflowFundingReservationTransaction,
  unsafeCreateWorkflowFundingReservationPermitForTest,
  unsafeWorkflowFundingReservationSelectedOutRefsForTest,
  type WorkflowFundingReservationSnapshot,
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
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

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
        releaseIdentityDigest: "f7".repeat(32),
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

const fundingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x51));
const fundingAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(fundingKey.to_public().hash()),
)
  .to_address()
  .to_bech32();
const measuredReferenceOutRef = `${"74".repeat(32)}#0`;
const measuredReferenceScript = Object.freeze({
  type: "PlutusV3" as const,
  script: "4d01000033222220051200120011",
});

const measuredFundingAction = (
  actionKind: string,
  coins: readonly bigint[],
  referenceScript?: Script,
) => {
  const inputs = CML.TransactionInputList.new();
  const fundingControlledInputs = coins.map((coin, index) => {
    const txHash = (0x71 + index).toString(16).padStart(2, "0").repeat(32);
    inputs.add(
      CML.TransactionInput.new(CML.TransactionHash.from_hex(txHash), 0n),
    );
    return Object.freeze({
      outRef: `${txHash}#0`,
      resolvedOutputCborHex: CML.TransactionOutput.new(
        CML.Address.from_bech32(fundingAddress),
        CML.Value.from_coin(coin),
      ).to_canonical_cbor_hex(),
      role: "wallet_funding" as const,
      semanticRole: "wallet_funding" as const,
      contractAddress: fundingAddress,
      identityAssets: Object.freeze([]),
      fundingLovelace: coin.toString(),
      fundingAssets: Object.freeze([]),
      sourceActionKind: null,
      sourceOutputIndex: null,
    });
  });
  const fee = 1n;
  const change = coins.reduce((total, coin) => total + coin, 0n) - fee;
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(fundingAddress),
      CML.Value.from_coin(change),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, fee);
  if (referenceScript !== undefined) {
    const references = CML.TransactionInputList.new();
    references.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex("74".repeat(32)),
        0n,
      ),
    );
    body.set_reference_inputs(references);
  }
  const witnesses = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      fundingKey.to_public(),
      fundingKey.sign(CML.hash_transaction(body).to_raw_bytes()),
    ),
  );
  witnesses.set_vkeywitnesses(vkeys);
  return Object.freeze({
    actionKind,
    signedTransactionCborHex: CML.Transaction.new(
      body,
      witnesses,
      true,
      undefined,
    ).to_canonical_cbor_hex(),
    fundingControlledInputs: Object.freeze(fundingControlledInputs),
    fundingControlledOutputs: Object.freeze([
      Object.freeze({
        outputIndex: 0,
        role: "wallet_change" as const,
        custodyRole: "none" as const,
        semanticRole: "wallet_change" as const,
        contractAddress: fundingAddress,
        fundingLovelace: change.toString(),
        fundingAssets: Object.freeze([]),
      }),
    ]),
    referenceInputs: Object.freeze(
      referenceScript === undefined
        ? []
        : [
            Object.freeze({
              role: "proofStep",
              outRef: measuredReferenceOutRef,
              scriptHash: validatorToScriptHash(referenceScript),
              scriptBytes: referenceScript.script.length / 2,
            }),
          ],
    ),
    referenceScriptBytes: referenceScript?.script.length
      ? referenceScript.script.length / 2
      : 0,
    requiredBondLovelace: "0",
    requiredRewardCustodyLovelace: "0",
    requiredNativeAssets: Object.freeze([]),
    collateralRequired: false,
    conflictRetryCount: 0,
  });
};

const measuredFundingRuntime = async (
  actionKind: "step-one" | "step-three",
  options: Readonly<{
    measuredReference?: Script;
    resolvedReference?: Script;
  }> = {},
) => {
  const actuation = await admittedActuation();
  const requirements = createWorkflowFundingRequirements({
    scope: { kind: "fraud_proof_category", category: "doubleSpend" },
    deploymentFingerprint: DEPLOYMENT,
    blueprintSha256: "a1".repeat(32),
    protocolParametersDigest: "a2".repeat(32),
    economicsPolicyDigest: "a3".repeat(32),
    fundingPaymentKeyHash: fundingKey.to_public().hash().to_hex(),
    measurementToolVersion: "funding-selection-test-v1",
    measurementArtifactSha256: "a4".repeat(32),
    actions: [
      measuredFundingAction("step-one", [10n], options.measuredReference),
      measuredFundingAction("step-three", [10n, 3n, 2n]),
    ],
  });
  const runner = unsafeCreateMeasuredWorkflowRunnerForTest({
    category: "doubleSpend",
    fundingRequirements: requirements,
  });
  const values = new Map([
    [`${"71".repeat(32)}#0`, 3n],
    [`${"72".repeat(32)}#0`, 2n],
    [`${"73".repeat(32)}#0`, 10n],
  ]);
  const activeInputs = Object.freeze(
    [...values].map(([outRef, lovelace]) =>
      Object.freeze({
        outRef,
        role: "funding" as const,
        lovelace: lovelace.toString(),
        assets: Object.freeze([]),
      }),
    ),
  );
  const snapshot = Object.freeze({
    reservationId: "b1".repeat(32),
    deploymentFingerprint: DEPLOYMENT,
    decisionDigest: actuation.decisionDigest,
    profileDigest: requirements.profileDigest,
    calculationDigest: "b2".repeat(32),
    rollbackGeneration: "7",
    revision: "0",
    walletAddress: fundingAddress,
    fundingPaymentKeyHash: fundingKey.to_public().hash().to_hex(),
    state: "active" as const,
    activeInputs,
  });
  let currentSnapshot: WorkflowFundingReservationSnapshot = snapshot;
  const prepare = vi.fn(async () => snapshot);
  const permit = await createWorkflowFundingReservationPermit({
    category: "doubleSpend",
    runner,
    actuationPermit: actuation.actuationPermit,
    rollbackGeneration: "7",
    port: {
      load: async () => currentSnapshot,
      resolveInputs: async (outRefs) =>
        outRefs.map((outRef) => {
          if (outRef === measuredReferenceOutRef) {
            return {
              txHash: "74".repeat(32),
              outputIndex: 0,
              address: fundingAddress,
              assets: { lovelace: 2_000_000n },
              scriptRef: options.resolvedReference,
            };
          }
          const [txHash, outputIndex] = outRef.split("#");
          return {
            txHash: txHash!,
            outputIndex: Number(outputIndex),
            address: fundingAddress,
            assets: { lovelace: values.get(outRef)! },
          };
        }),
      resolveConfirmedActionOutput: async () => {
        throw new Error("test action has no released locked input");
      },
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
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: actionKind, input: { actionKind } },
  });
  return Object.freeze({
    journal,
    permit,
    prepare,
    selected: unsafeWorkflowFundingReservationSelectedOutRefsForTest(permit),
    setSnapshot: (value: WorkflowFundingReservationSnapshot) => {
      currentSnapshot = value;
    },
    snapshot,
  });
};

const measuredFundingSelection = async (
  actionKind: "step-one" | "step-three",
) => (await measuredFundingRuntime(actionKind)).selected;

const signedFundingTransaction = (input: {
  readonly inputOutRefs: readonly string[];
  readonly outputLovelace: bigint;
  readonly referenceOutRefs?: readonly string[];
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
      CML.Address.from_bech32(fundingAddress),
      CML.Value.from_coin(input.outputLovelace),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 1n);
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
  const witnesses = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      fundingKey.to_public(),
      fundingKey.sign(CML.hash_transaction(body).to_raw_bytes()),
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

describe("compiled manifest-bound production runtime V1", () => {
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

  it("leases and exposes only the deterministic action-specific funding subset", async () => {
    await expect(measuredFundingSelection("step-one")).resolves.toEqual({
      fundingOutRefs: [`${"73".repeat(32)}#0`],
      collateralOutRefs: [],
    });
    await expect(measuredFundingSelection("step-three")).resolves.toEqual({
      fundingOutRefs: [
        `${"71".repeat(32)}#0`,
        `${"72".repeat(32)}#0`,
        `${"73".repeat(32)}#0`,
      ],
      collateralOutRefs: [],
    });
  });

  it("binds the actual signed body to the selected funding subset and measured shape", async () => {
    const runtime = await measuredFundingRuntime("step-one");
    const action = { actionId: "step-one", input: { actionKind: "step-one" } };
    const validPreflight = bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: "valid" }),
      signedFundingTransaction({
        inputOutRefs: runtime.selected.fundingOutRefs,
        outputLovelace: 9n,
      }),
    );
    await expect(
      prepareWorkflowFundingReservationTransaction({
        journal: runtime.journal,
        action,
        preflight: validPreflight,
      }),
    ).resolves.toBeUndefined();
    expect(runtime.prepare).toHaveBeenCalledTimes(1);

    const hostile = await measuredFundingRuntime("step-one");
    const substitutedPreflight = bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: "substituted" }),
      signedFundingTransaction({
        inputOutRefs: [
          ...hostile.selected.fundingOutRefs,
          `${"71".repeat(32)}#0`,
        ],
        outputLovelace: 12n,
      }),
    );
    await expect(
      prepareWorkflowFundingReservationTransaction({
        journal: hostile.journal,
        action,
        preflight: substitutedPreflight,
      }),
    ).rejects.toThrow("transaction exceeds its admitted measured shape");
    expect(hostile.prepare).not.toHaveBeenCalled();
  });

  it("rejects a substituted reference script even when body topology and byte count match", async () => {
    const substitutedScript = Object.freeze({
      type: "PlutusV3" as const,
      script: "4d01000033222220051200120012",
    });
    const runtime = await measuredFundingRuntime("step-one", {
      measuredReference: measuredReferenceScript,
      resolvedReference: substitutedScript,
    });
    const preflight = bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: "reference-substitution" }),
      signedFundingTransaction({
        inputOutRefs: runtime.selected.fundingOutRefs,
        outputLovelace: 9n,
        referenceOutRefs: [measuredReferenceOutRef],
      }),
    );
    await expect(
      prepareWorkflowFundingReservationTransaction({
        journal: runtime.journal,
        action: { actionId: "step-one", input: { actionKind: "step-one" } },
        preflight,
      }),
    ).rejects.toThrow("reference-script identity differs from measurement");
    expect(runtime.prepare).not.toHaveBeenCalled();
  });

  it("rechecks measured reservation input bounds after durable refresh", async () => {
    const runtime = await measuredFundingRuntime("step-three");
    runtime.setSnapshot(
      Object.freeze({
        ...runtime.snapshot,
        revision: "1",
        activeInputs: Object.freeze([
          ...runtime.snapshot.activeInputs,
          Object.freeze({
            outRef: `${"75".repeat(32)}#0`,
            role: "funding" as const,
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
    ).rejects.toThrow("exceeds its measured input bounds");
  });

  it("registers only the families with complete shared workflow drivers", () => {
    expect(Object.keys(WORKFLOW_RUNNER_FACTORIES)).toEqual([
      "doubleSpend",
      "nonExistentInput",
      "nonExistentInputNoIndex",
      "invalidRange",
      "zeroInput",
      "daHashPreimage",
      "noReferenceInput",
      "referenceInputNoIdx",
      "invalidSignature",
      "fabricatedDeposit",
      "fabricatedWithdrawal",
      "withdrawnReferenceInput",
      "canonicalDecodability",
      "committedFieldShape",
      "minFee",
      "doubleWithdraw",
      "l2TxMistag",
      "withdrawnInput",
      "missingSignature",
      "missingNativeScriptTx",
      "inputSetUniqueness",
      "networkId",
      "missingNativeScriptUtxo",
      "nativeScriptInvalid",
      "minAda",
      "fieldPreimageLengthMismatch",
      "fieldItemWidthIllegal",
      "witnessScriptDecoding",
      "scriptIntegrityHashMissing",
      "transactionOutputNonCanonical",
      "resolvedOutputNonCanonical",
      "mintDeclaredAssetLimit",
      "spendInputSignerMissing",
      "protectedOutputSignerMissing",
      "observersForbiddenOnUntaggedNetwork",
      "observerOrderInvalid",
      "redeemerCanonicity",
      "outputReferenceScriptDecoding",
      "executionSourceScriptDecoding",
      "receivePurposeLanguage",
      "unusedScriptWitness",
      "missingScriptSource",
      "missingRedeemer",
      "unusedRedeemer",
      "scriptIntegrityHashMismatch",
      "distinctAssetAccumulationLimit",
    ]);
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
