import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  AddressData,
  addressDataFromBech32,
  ForcedInclusionTxV1Schema,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { createReceivePurposeLanguageActuator } from "../src/receive-purpose-language/actuator.js";
import { prepareReceivePurposeLanguageArtifact } from "../src/receive-purpose-language/authenticated-replay.js";
import {
  applyReceivePurposeLanguageScripts,
  type ReceivePurposeLanguageContracts,
} from "../src/receive-purpose-language/contracts.js";
import { prepareReceivePurposeLanguageEvidence } from "../src/receive-purpose-language/family.js";
import {
  buildReceivePurposeLanguageAuthenticationFromRetainedDa,
  receivePurposeLanguageDescriptorFromAuthentication,
} from "../src/receive-purpose-language/retained-witness.js";
import { submitReceivePurposeLanguageCancel } from "../src/receive-purpose-language/submit-cancel.js";
import { submitReceivePurposeLanguageInit } from "../src/receive-purpose-language/submit-init.js";
import {
  submitReceivePurposeLanguageStep01Accepted,
  submitReceivePurposeLanguageStep01Forced,
} from "../src/receive-purpose-language/submit-step-01.js";
import {
  type ReceivePurposeLanguageAuthentication,
  submitReceivePurposeLanguageStep02,
} from "../src/receive-purpose-language/submit-step-02.js";
import { submitReceivePurposeLanguageStep03 } from "../src/receive-purpose-language/submit-step-03.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  assertCompleteLifecycleCoverage,
  COMPLETE_LIFECYCLE_BASE_SCENARIOS,
  type CompleteLifecycleCoverage,
} from "../src/testing/complete-lifecycle.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { submitCapturedTransaction } from "../src/workflow/transaction-boundary.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import {
  buildReceivePurposeFixture,
  type ReceivePurposeFixture,
  submitReceiveStep01ForcedRaw,
  submitReceiveStep02Raw,
  submitReceiveStep03Raw,
} from "./support/receive-purpose-language-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  expectOnchainRefusal,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  realBlueprintPath,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

/**
 * The maximum evidence shape the suite drives through the real chain: the
 * accused receive purpose plus 255 native spend purposes (distinct spent
 * out-refs under one trivial script), so the purpose and execution frontiers
 * of the native-scripts control carry 256 leaves (an eight-sibling path from
 * every position) while every widened transaction field stays inside its
 * 32,768-byte consensus bound; the decoys widen the validation-traces trie so
 * the descriptor membership proof has real branch steps. The on-chain envelope itself (every
 * frontier at 4,095 leaves) is measured by
 * `receive_authenticates_consensus_bounded_frontiers` in
 * `onchain/aiken/lib/midgard/fraud-proofs/receive-purpose-language/rule.test.ak`;
 * the size plan combines both rows.
 */
export const MAXIMUM_PURPOSE_COUNT = 256;
export const MAXIMUM_DECOY_TRANSACTION_COUNT = 15;

const REASON_ARM = "ReceivePurposePlutusV3Forbidden";
const AUTHENTICATION_SEAMS = [
  "forced_leaf_reason_coordinate",
  "forced_leaf_header",
  "forced_leaf_root",
  "forced_leaf_direction",
  "validation_traces_root",
  "trace_descriptor",
  "subject_event_key",
  "machine_state",
  "trace_proof",
  "native_control",
  "purpose_item",
  "source_language",
  "execution_membership",
] as const;
const CANCELLABLE_STEPS = ["step01", "step02", "step03"] as const;

const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/receive-purpose-language-v1-fit-ledger.json",
    import.meta.url,
  ),
);

const measurements: VanRossemFitMeasurement[] = [];
const coverage = {
  reasonArms: new Set<string>(),
  successfulDirections: new Set<
    "accepted_invalid" | "forced_rejection_wrong"
  >(),
  scenarios: new Set<(typeof COMPLETE_LIFECYCLE_BASE_SCENARIOS)[number]>(),
  seams: new Set<string>(),
  cancelledSteps: new Set<string>(),
  adjacentOverBoundRefused: false,
};
let publicationsRecorded = false;

const record = (
  name: string,
  maximumShape: string,
  measurement: CompleteSignedTransactionMeasurement,
) => {
  expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
  expect(measurement.executionMemory, name).toBeGreaterThan(0n);
  expect(measurement.executionSteps, name).toBeGreaterThan(0n);
  measurements.push({
    name,
    kind: "lifecycle",
    maximumShape,
    signedBytes: measurement.completeSignedBytes,
    memoryUnits: measurement.executionMemory,
    cpuUnits: measurement.executionSteps,
  });
};

const progress = (message: string) =>
  console.info(`[receive-purpose-language-progress] ${message}`);

const makeHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: { alwaysFraudProofCatalogue: true },
  });
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const applied = applyReceivePurposeLanguageScripts({
    blueprint: harness.realBlueprint,
    network,
    computationThreadPolicyId: harness.contracts.computationThread.policyId,
    fraudProofPolicyId: harness.contracts.fraudProof.policyId,
    fraudProofTokenAddressData: addressData,
    hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
  });
  const contracts: ReceivePurposeLanguageContracts = {
    steps: applied,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
  };
  const catalogue = await buildCatalogueDeploymentInfo({
    ...harness.contracts.fraudProofs,
    receivePurposeLanguage: {
      ...harness.contracts.fraudProofs.receivePurposeLanguage,
      spendingScriptHash: applied[0].spendingScriptHash,
    },
  });
  const category = catalogue.categories.receivePurposeLanguage;
  expect(category.categoryId).toBe("00000034");
  expect(category.scriptHash).toBe(applied[0].spendingScriptHash);
  const references: UTxO[] = [];
  // Published after the block setup so the harness nonce UTxO is still
  // unspent when the state-queue block is committed.
  const publishReferences = async () => {
    for (const [index, step] of applied.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `receive-purpose-language-step-${(index + 1).toString()}`,
      });
      references.push(published.utxo);
      expect(
        published.publicationMeasurement.completeSignedBytes,
        `step ${(index + 1).toString()} publication`,
      ).toBeLessThanOrEqual(15_872);
      if (!publicationsRecorded)
        measurements.push({
          name: `publish-step0${(index + 1).toString()}`,
          kind: "publication",
          maximumShape: `applied testnet ${step.blueprintTitle}`,
          signedBytes: published.publicationMeasurement.completeSignedBytes,
          memoryUnits: published.publicationMeasurement.executionMemory,
          cpuUnits: published.publicationMeasurement.executionSteps,
        });
    }
    publicationsRecorded = true;
  };
  const startTime = () =>
    BigInt(
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    );
  const common = (threadOutRef: string, stepIndex: number) =>
    ({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef,
      referenceScriptUtxo: references[stepIndex]!,
    }) as const;
  const init = async (setup: {
    fraudulentBlockOutRef: string;
    headerHash: string;
  }) =>
    await captureEmulatorSubmission(
      harness.emulator,
      async () =>
        await submitReceivePurposeLanguageInit({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts,
          category,
          catalogue: {
            policyId: harness.contracts.fraudProofCatalogue.policyId,
            spendingScriptAddress:
              harness.contracts.fraudProofCatalogue.spendingScriptAddress,
            root: catalogue.root,
          },
          signer: harness.proverSigner,
          fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
          fraudulentHeaderHash: setup.headerHash,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
  const cancel = async (threadOutRef: string, stepIndex: number) => {
    const captured = await captureEmulatorSubmission(
      harness.emulator,
      async () =>
        await submitReceivePurposeLanguageCancel({
          ...common(threadOutRef, stepIndex),
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    expect(captured.result.txHash).toMatch(/^[0-9a-f]{64}$/u);
    coverage.cancelledSteps.add(CANCELLABLE_STEPS[stepIndex]!);
    return captured;
  };
  const setupBlock = async (fixture: ReceivePurposeFixture) => {
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue,
      header: fixture.header,
    });
    await publishReferences();
    return setup;
  };
  const removalDeployment = async () => {
    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const base = buildRemovalDeploymentInfo(harness.contracts, catalogue, {
      removalReferenceScripts: removalReferences.published,
    });
    const entry = (step: (typeof applied)[number]) => ({
      scriptHash: step.spendingScriptHash,
      contract: {
        type: step.spendingScript.type,
        cborHex: step.spendingScript.script,
      },
    });
    return {
      ...base,
      contracts: {
        ...base.contracts,
        fraudProofReceivePurposeLanguage: entry(applied[0]),
        fraudProofReceivePurposeLanguageStep02: entry(applied[1]),
        fraudProofReceivePurposeLanguageStep03: entry(applied[2]),
      },
    };
  };
  return {
    harness,
    applied,
    contracts,
    catalogue,
    category,
    references,
    startTime,
    common,
    init,
    cancel,
    setupBlock,
    removalDeployment,
  };
};

type Harness = Awaited<ReturnType<typeof makeHarness>>;

const shapeLabel = (fixture: ReceivePurposeFixture) =>
  `${fixture.spec.purposeCount.toString()} purposes (${fixture.spec.language} receive at execution ${fixture.executionIndex.toString()}), ${fixture.header.validationTraceCount.toString()} validation traces`;

const mutate = (
  authentication: ReceivePurposeLanguageAuthentication,
  patch: Partial<ReceivePurposeLanguageAuthentication>,
): ReceivePurposeLanguageAuthentication => ({ ...authentication, ...patch });

/** Every step-02 authentication seam, mutated one at a time against a bound thread. */
const refuseEveryStep02Seam = async (
  h: Harness,
  threadOutRef: string,
  authentication: ReceivePurposeLanguageAuthentication,
) => {
  const attempt = async (
    seam: string,
    mutated: ReceivePurposeLanguageAuthentication,
  ) => {
    progress(`step-02 refusal: ${seam}`);
    await expectOnchainRefusal(
      async () =>
        await submitReceiveStep02Raw({
          ...h.common(threadOutRef, 1),
          authentication: mutated,
        }),
    );
    coverage.seams.add(seam);
  };
  const membership = authentication.trace_membership;
  await attempt(
    "validation_traces_root",
    mutate(authentication, {
      trace_membership: { ...membership, root: "ff".repeat(32) },
    }),
  );
  await attempt(
    "trace_descriptor",
    mutate(authentication, {
      trace_membership: {
        ...membership,
        value: {
          ...membership.value,
          step_count: membership.value.step_count + 1n,
        },
      },
    }),
  );
  await attempt(
    "subject_event_key",
    mutate(authentication, {
      trace_membership: {
        ...membership,
        key: { L2TransactionEventKey: { tx_id: "aa".repeat(32) } },
      },
    }),
  );
  await attempt(
    "machine_state",
    mutate(authentication, {
      machine_state: {
        ...authentication.machine_state,
        prior_ledger_root: "ee".repeat(32),
      },
    }),
  );
  expect(authentication.trace_proof.siblings.length).toBeGreaterThan(0);
  await attempt(
    "trace_proof",
    mutate(authentication, {
      trace_proof: {
        ...authentication.trace_proof,
        siblings: [
          "dd".repeat(32),
          ...authentication.trace_proof.siblings.slice(1),
        ],
      },
    }),
  );
  await attempt(
    "native_control",
    mutate(authentication, {
      control: {
        ...authentication.control,
        purpose_peaks: authentication.control.purpose_peaks.map((peak) => ({
          ...peak,
          hash: "cc".repeat(32),
        })),
      },
    }),
  );
  await attempt(
    "purpose_item",
    mutate(authentication, { script_hash: "bb".repeat(28) }),
  );
  await attempt(
    "source_language",
    mutate(authentication, {
      language_tag: authentication.language_tag === 3n ? 0n : 3n,
    }),
  );
  expect(authentication.execution_siblings.length).toBeGreaterThan(0);
  await attempt(
    "execution_membership",
    mutate(authentication, {
      execution_siblings: [
        "99".repeat(32),
        ...authentication.execution_siblings.slice(1),
      ],
    }),
  );
};

describe("receivePurposeLanguage real lifecycle", () => {
  it("convicts an accepted PlutusV3 receive at the maximum shape: cancels every step, refuses every step-02 seam and the adjacent index, then mints and removes through the actuator", async () => {
    const h = await makeHarness();
    const fixture = await buildReceivePurposeFixture({
      direction: "accepted",
      language: "plutusV3",
      claimedVerdict: "accepted",
      purposeCount: MAXIMUM_PURPOSE_COUNT,
      decoyTransactionCount: MAXIMUM_DECOY_TRANSACTION_COUNT,
      inputByte: 0x74,
      operatorVkey: await funderPaymentKeyHash(h.harness.funderLucid),
      startTime: h.startTime(),
    });
    expect(fixture.header.validationTraceCount).toBe(
      BigInt(MAXIMUM_DECOY_TRANSACTION_COUNT + 1),
    );
    const setup = await h.setupBlock(fixture);
    const artifact = await prepareReceivePurposeLanguageArtifact(
      fixture.canonicalBlock(setup.headerHash),
    );
    expect(artifact.acceptedInclusion).toBeDefined();
    expect(artifact.forcedMembership).toBeUndefined();
    expect(artifact.authentication.language_tag).toBe(3n);
    expect(artifact.authentication.control.purpose_count).toBe(
      BigInt(MAXIMUM_PURPOSE_COUNT),
    );
    expect(artifact.authentication.purpose_siblings).toHaveLength(8);
    expect(artifact.authentication.execution_siblings).toHaveLength(8);
    expect(
      artifact.authentication.trace_membership.proof.length,
    ).toBeGreaterThan(0);
    expect(artifact.evidence.finding.executionIndex).toBe(
      fixture.executionIndex,
    );
    const shape = shapeLabel(fixture);
    const step01 = async (
      threadOutRef: string,
      executionIndex = BigInt(fixture.executionIndex),
    ) =>
      await captureEmulatorSubmission(
        h.harness.emulator,
        async () =>
          await submitReceivePurposeLanguageStep01Accepted({
            ...h.common(threadOutRef, 0),
            blueprint: h.harness.realBlueprint,
            network,
            stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
            txInclusion: artifact.acceptedInclusion!,
            header: fixture.header,
            executionIndex,
            witnessReferenceScripts: h.harness.witnessReferenceScripts,
          }),
      );
    const step02 = async (threadOutRef: string) =>
      await captureEmulatorSubmission(
        h.harness.emulator,
        async () =>
          await submitReceivePurposeLanguageStep02({
            ...h.common(threadOutRef, 1),
            evidence: artifact.evidence,
            authentication: artifact.authentication,
          }),
      );

    progress("cancel at step 01");
    const cancelledAt01 = await h.init(setup);
    record(
      "accepted-cancel-step01",
      shape,
      (await h.cancel(cancelledAt01.result.nextThreadOutRef, 0)).measurement,
    );

    progress("cancel at step 02");
    const bound = await step01((await h.init(setup)).result.nextThreadOutRef);
    record(
      "accepted-cancel-step02",
      shape,
      (await h.cancel(bound.result.nextThreadOutRef, 1)).measurement,
    );

    progress("cancel at step 03");
    const authenticatedThread = await step02(
      (await step01((await h.init(setup)).result.nextThreadOutRef)).result
        .nextThreadOutRef,
    );
    record(
      "accepted-cancel-step03",
      shape,
      (await h.cancel(authenticatedThread.result.nextThreadOutRef, 2))
        .measurement,
    );

    progress("adjacent-over-bound execution index");
    const overBound = await step01(
      (await h.init(setup)).result.nextThreadOutRef,
      BigInt(MAXIMUM_PURPOSE_COUNT),
    );
    await expectOnchainRefusal(
      async () =>
        await submitReceiveStep02Raw({
          ...h.common(overBound.result.nextThreadOutRef, 1),
          authentication: artifact.authentication,
        }),
    );
    coverage.adjacentOverBoundRefused = true;
    // Coordinate mutation on the accepted side: the thread binds a different
    // in-range execution than the one the retained descriptor authenticates.
    const wrongIndex = await step01(
      (await h.init(setup)).result.nextThreadOutRef,
      BigInt(fixture.executionIndex === 0 ? 1 : 0),
    );
    await expectOnchainRefusal(
      async () =>
        await submitReceiveStep02Raw({
          ...h.common(wrongIndex.result.nextThreadOutRef, 1),
          authentication: artifact.authentication,
        }),
    );
    coverage.scenarios.add("reason_or_subject_coordinate_mutation");
    await h.cancel(overBound.result.nextThreadOutRef, 1);
    await h.cancel(wrongIndex.result.nextThreadOutRef, 1);

    progress("step-02 seam mutations");
    const seamThread = await step01(
      (await h.init(setup)).result.nextThreadOutRef,
    );
    await refuseEveryStep02Seam(
      h,
      seamThread.result.nextThreadOutRef,
      artifact.authentication,
    );
    await h.cancel(seamThread.result.nextThreadOutRef, 1);

    progress("actuator lifecycle");
    const deploymentInfo = await h.removalDeployment();
    const actuator = createReceivePurposeLanguageActuator({
      binding: {
        blueprint: h.harness.realBlueprint,
        deploymentInfo,
        network,
        definition: { headerHash: setup.headerHash },
        releaseEconomics: {
          policy: { fraudProverRewardLovelace: "400000000" },
        },
        resolvedContracts: {
          category: { categoryId: h.category.categoryId },
          contracts: {
            fraudProof: {
              spendingScriptHash:
                h.harness.contracts.fraudProof.spendingScriptHash,
            },
          },
        },
      },
      lucid: h.harness.proverLucid,
      signer: h.harness.proverSigner,
      contracts: h.contracts,
      references: {
        steps: h.references as unknown as readonly [UTxO, UTxO, UTxO],
        witnesses: h.harness.witnessReferenceScripts as never,
      },
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => ({
          token: "receive-purpose-language-emulator",
          source: "emulator",
          renew: async () => undefined,
          release: async () => undefined,
          fail: async () => undefined,
        }),
      },
    });
    const drive = async (
      label: string,
      action: Parameters<typeof actuator.capture>[0]["action"],
      locate: (txHash: string) => Promise<string>,
    ) => {
      progress(label);
      const captured = await captureEmulatorSubmission(
        h.harness.emulator,
        async () => {
          const capture = await actuator.capture({ action, artifact });
          const txHash = await submitCapturedTransaction(capture.transaction);
          expect(txHash).toBe(capture.transaction.txHash);
          await h.harness.proverLucid.awaitTx(txHash);
          return { txHash, next: await locate(txHash) };
        },
      );
      record(label, shape, captured.measurement);
      return captured.result;
    };
    const outputAt = (address: string) => async (txHash: string) => {
      const utxo = (await h.harness.proverLucid.utxosAt(address)).find(
        (candidate) => candidate.txHash === txHash,
      );
      if (utxo === undefined) throw new Error(`${address} output absent`);
      return `${utxo.txHash}#${utxo.outputIndex.toString()}`;
    };
    const initialized = await h.init(setup);
    record("accepted-init", shape, initialized.measurement);
    const s1 = await drive(
      "accepted-step01",
      {
        stage: "step_01",
        threadOutRef: initialized.result.nextThreadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      },
      outputAt(h.applied[1].spendingScriptAddress),
    );
    const s2 = await drive(
      "accepted-step02",
      { stage: "step_02", threadOutRef: s1.next },
      outputAt(h.applied[2].spendingScriptAddress),
    );
    const s3 = await drive(
      "accepted-step03-mint",
      { stage: "step_03", threadOutRef: s2.next },
      outputAt(h.harness.contracts.fraudProof.spendingScriptAddress),
    );
    vi.setSystemTime(h.harness.emulator.now());
    await drive(
      "accepted-remove",
      {
        stage: "remove",
        nextRemovalOutRef: setup.fraudulentBlockOutRef,
        fraudProofOutRef: s3.next,
      },
      async (txHash) => txHash,
    );
    coverage.reasonArms.add(REASON_ARM);
    coverage.successfulDirections.add("accepted_invalid");
    coverage.scenarios.add("wrongful_acceptance_success");
    coverage.scenarios.add("permanent_proof_token_and_descendant_removal");
    coverage.scenarios.add("maximum_supported_evidence");
  }, 1_200_000);

  it("contradicts a wrongful forced rejection of a native receive at the maximum shape: refuses every forced-door seam, then mints and removes", async () => {
    const h = await makeHarness();
    const fixture = await buildReceivePurposeFixture({
      direction: "forced",
      language: "native",
      claimedVerdict: "rejected",
      purposeCount: MAXIMUM_PURPOSE_COUNT,
      decoyTransactionCount: MAXIMUM_DECOY_TRANSACTION_COUNT,
      inputByte: 0x75,
      operatorVkey: await funderPaymentKeyHash(h.harness.funderLucid),
      startTime: h.startTime(),
    });
    const setup = await h.setupBlock(fixture);
    const artifact = await prepareReceivePurposeLanguageArtifact(
      fixture.canonicalBlock(setup.headerHash),
    );
    expect(artifact.forcedMembership).toBeDefined();
    expect(artifact.acceptedInclusion).toBeUndefined();
    expect(artifact.authentication.language_tag).toBe(0n);
    expect(artifact.authentication.control.execution_count).toBe(
      BigInt(MAXIMUM_PURPOSE_COUNT),
    );
    const membership = artifact.forcedMembership!;
    const shape = shapeLabel(fixture);
    const initialized = await h.init(setup);
    record("forced-init", shape, initialized.measurement);
    const thread = initialized.result.nextThreadOutRef;
    const door = async (
      seam: string,
      patch: Partial<Parameters<typeof submitReceiveStep01ForcedRaw>[0]>,
    ) => {
      progress(`forced-door refusal: ${seam}`);
      await expectOnchainRefusal(
        async () =>
          await submitReceiveStep01ForcedRaw({
            ...h.common(thread, 0),
            header: fixture.header,
            membership,
            executionIndex: BigInt(fixture.executionIndex),
            direction: 1n,
            ...patch,
          }),
      );
      coverage.seams.add(seam);
    };
    await door("forced_leaf_reason_coordinate", {
      executionIndex: BigInt(fixture.executionIndex + 1),
    });
    coverage.scenarios.add("reason_or_subject_coordinate_mutation");
    await door("forced_leaf_header", {
      header: { ...fixture.header, validationTracesRoot: "ff".repeat(32) },
    });
    await door("forced_leaf_direction", { direction: 0n });
    // A root that does carry the leaf, but which the header never committed.
    const foreign = await (async () => {
      const key = Buffer.from(Data.to(membership.key, OutputReference), "hex");
      const value = Buffer.from(
        Data.to(membership.value as never, ForcedInclusionTxV1Schema as never),
        "hex",
      );
      const decoy = Buffer.from(
        Data.to(
          { transactionId: "ab".repeat(32), outputIndex: 7n },
          OutputReference,
        ),
        "hex",
      );
      const root = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
        { key, value },
        { key: decoy, value },
      ]);
      const store = new Store(undefined);
      await store.ready();
      const trie = new Trie(store);
      await trie.insert(key, value);
      await trie.insert(decoy, value);
      return {
        ...membership,
        root: root.root,
        phas_root: root.phasRoot,
        count: root.count,
        proof: Data.from(
          (await trie.prove(key)).toCBOR().toString("hex"),
          Proof,
        ),
      };
    })();
    await door("forced_leaf_root", { membership: foreign });

    progress("forced step 01");
    const bound = await captureEmulatorSubmission(
      h.harness.emulator,
      async () =>
        await submitReceivePurposeLanguageStep01Forced({
          ...h.common(thread, 0),
          header: fixture.header,
          membership,
          executionIndex: BigInt(fixture.executionIndex),
        }),
    );
    record("forced-step01", shape, bound.measurement);
    progress("forced step 02");
    const authenticated = await captureEmulatorSubmission(
      h.harness.emulator,
      async () =>
        await submitReceivePurposeLanguageStep02({
          ...h.common(bound.result.nextThreadOutRef, 1),
          evidence: artifact.evidence,
          authentication: artifact.authentication,
        }),
    );
    record("forced-step02", shape, authenticated.measurement);
    progress("forced step 03");
    const final = await captureEmulatorSubmission(
      h.harness.emulator,
      async () =>
        await submitReceivePurposeLanguageStep03({
          ...h.common(authenticated.result.nextThreadOutRef, 2),
          evidence: artifact.evidence,
          witnessReferenceScripts: h.harness.witnessReferenceScripts,
        }),
    );
    record("forced-step03-mint", shape, final.measurement);
    expect(
      (
        await h.harness.proverLucid.utxosAt(
          h.harness.contracts.fraudProof.spendingScriptAddress,
        )
      ).some(({ txHash }) => txHash === final.result.txHash),
    ).toBe(true);
    progress("forced removal");
    const deploymentInfo = await h.removalDeployment();
    vi.setSystemTime(h.harness.emulator.now());
    const removal = await captureEmulatorSubmission(
      h.harness.emulator,
      async () =>
        await submitRemoveFraudulentBlock({
          lucid: h.harness.proverLucid,
          blueprint: h.harness.realBlueprint,
          deploymentInfo,
          network,
          signer: h.harness.proverSigner,
          fraudCategory: "receivePurposeLanguage",
          fraudulentHeaderHash: setup.headerHash,
          awaitConfirmation: true,
          requireReferenceScripts: true,
          stateQueueMutationLeaseCoordinator: {
            acquire: async () => ({
              token: "receive-purpose-language-forced",
              source: "emulator",
              renew: async () => undefined,
              release: async () => undefined,
              fail: async () => undefined,
            }),
          },
          validFrom:
            BigInt(h.harness.emulator.now()) > 120_000n
              ? BigInt(h.harness.emulator.now()) - 120_000n
              : 0n,
          validTo: BigInt(h.harness.emulator.now()) + 300_000n,
        }),
    );
    record("forced-remove", shape, removal.measurement);
    coverage.reasonArms.add(REASON_ARM);
    coverage.successfulDirections.add("forced_rejection_wrong");
    coverage.scenarios.add("wrongful_forced_rejection_success");
  }, 1_200_000);

  it("refuses to convict an honest accepted native receive at the terminal step", async () => {
    const h = await makeHarness();
    const fixture = await buildReceivePurposeFixture({
      direction: "accepted",
      language: "native",
      claimedVerdict: "accepted",
      purposeCount: 1,
      inputByte: 0x76,
      operatorVkey: await funderPaymentKeyHash(h.harness.funderLucid),
      startTime: h.startTime(),
    });
    const setup = await h.setupBlock(fixture);
    await expect(
      prepareReceivePurposeLanguageArtifact(
        fixture.canonicalBlock(setup.headerHash),
      ),
    ).rejects.toThrow(/no contradiction/u);
    const rebuilt =
      await buildReceivePurposeLanguageAuthenticationFromRetainedDa({
        eventKey: fixture.eventKey,
        executionIndex: fixture.executionIndex,
        ...fixture.retainedEntries,
        expectedValidationTracesRoot: fixture.header.validationTracesRoot,
        expectedLanguageTag: 0,
      });
    const evidence = prepareReceivePurposeLanguageEvidence({
      finding: {
        subject: fixture.subject,
        executionIndex: fixture.executionIndex,
      },
      descriptor: receivePurposeLanguageDescriptorFromAuthentication(
        rebuilt.authentication,
        fixture.executionIndex,
      ),
    });
    const initialized = await h.init(setup);
    const bound = await submitReceivePurposeLanguageStep01Accepted({
      ...h.common(initialized.result.nextThreadOutRef, 0),
      blueprint: h.harness.realBlueprint,
      network,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.block.txInclusion!,
      header: fixture.header,
      executionIndex: BigInt(fixture.executionIndex),
      witnessReferenceScripts: h.harness.witnessReferenceScripts,
    });
    const authenticated = await submitReceivePurposeLanguageStep02({
      ...h.common(bound.nextThreadOutRef, 1),
      evidence,
      authentication: rebuilt.authentication,
    });
    await expect(
      submitReceivePurposeLanguageStep03({
        ...h.common(authenticated.nextThreadOutRef, 2),
        evidence,
        witnessReferenceScripts: h.harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/not the retained contradiction/u);
    await expectOnchainRefusal(
      async () =>
        await submitReceiveStep03Raw({
          ...h.common(authenticated.nextThreadOutRef, 2),
          witnessReferenceScripts: h.harness.witnessReferenceScripts,
        }),
    );
    await h.cancel(authenticated.nextThreadOutRef, 2);
    coverage.scenarios.add("honest_accepted_block_refusal");
  }, 600_000);

  it("refuses to convict an honest forced rejection of a PlutusV3 receive at the terminal step", async () => {
    const h = await makeHarness();
    const fixture = await buildReceivePurposeFixture({
      direction: "forced",
      language: "plutusV3",
      claimedVerdict: "rejected",
      purposeCount: 1,
      inputByte: 0x77,
      operatorVkey: await funderPaymentKeyHash(h.harness.funderLucid),
      startTime: h.startTime(),
    });
    const setup = await h.setupBlock(fixture);
    await expect(
      prepareReceivePurposeLanguageArtifact(
        fixture.canonicalBlock(setup.headerHash),
      ),
    ).rejects.toThrow(/no contradiction/u);
    const rebuilt =
      await buildReceivePurposeLanguageAuthenticationFromRetainedDa({
        eventKey: fixture.eventKey,
        executionIndex: fixture.executionIndex,
        ...fixture.retainedEntries,
        expectedValidationTracesRoot: fixture.header.validationTracesRoot,
        expectedLanguageTag: 3,
      });
    const evidence = prepareReceivePurposeLanguageEvidence({
      finding: {
        subject: fixture.subject,
        executionIndex: fixture.executionIndex,
      },
      descriptor: receivePurposeLanguageDescriptorFromAuthentication(
        rebuilt.authentication,
        fixture.executionIndex,
      ),
    });
    const { buildForcedTransactionLeafMembershipProof } = await import(
      "../src/transition-trace/witnesses.js"
    );
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: fixture.block.reconstruction,
      eventKey: fixture.eventKey,
    });
    const initialized = await h.init(setup);
    const bound = await submitReceivePurposeLanguageStep01Forced({
      ...h.common(initialized.result.nextThreadOutRef, 0),
      header: fixture.header,
      membership,
      executionIndex: BigInt(fixture.executionIndex),
    });
    const authenticated = await submitReceivePurposeLanguageStep02({
      ...h.common(bound.nextThreadOutRef, 1),
      evidence,
      authentication: rebuilt.authentication,
    });
    await expect(
      submitReceivePurposeLanguageStep03({
        ...h.common(authenticated.nextThreadOutRef, 2),
        evidence,
        witnessReferenceScripts: h.harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/not the retained contradiction/u);
    await expectOnchainRefusal(
      async () =>
        await submitReceiveStep03Raw({
          ...h.common(authenticated.nextThreadOutRef, 2),
          witnessReferenceScripts: h.harness.witnessReferenceScripts,
        }),
    );
    await h.cancel(authenticated.nextThreadOutRef, 2);
    coverage.scenarios.add("honest_forced_rejection_refusal");
  }, 600_000);

  it("closes the coverage gate and the Van Rossem fit ledger", async () => {
    const complete: CompleteLifecycleCoverage = {
      reasonArms: [...coverage.reasonArms],
      successfulDirectionByReason: {
        [REASON_ARM]: [...coverage.successfulDirections],
      },
      scenarios: [...coverage.scenarios],
      authenticatedSeamsMutated: [...coverage.seams],
      cancelledPhysicalSteps: [...coverage.cancelledSteps],
      resumedAfterCheckpoint: false,
      adjacentOverBoundRefused: coverage.adjacentOverBoundRefused,
    };
    assertCompleteLifecycleCoverage({
      coverage: complete,
      expectedReasonArms: [REASON_ARM],
      authenticationSeams: [...AUTHENTICATION_SEAMS],
      cancellablePhysicalSteps: [...CANCELLABLE_STEPS],
      resumable: false,
      hasAdjacentConsensusBound: true,
    });
    const blueprintBytes = readFileSync(realBlueprintPath);
    const preamble = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: "receivePurposeLanguage",
      blueprintSha256: createHash("sha256")
        .update(blueprintBytes)
        .digest("hex"),
      compilerVersion: `aiken ${preamble.preamble?.compiler?.version ?? "unknown"}`,
      measurements,
    });
    expect(ledger.entries.map((entry) => entry.name)).toEqual([
      "accepted-cancel-step01",
      "accepted-cancel-step02",
      "accepted-cancel-step03",
      "accepted-init",
      "accepted-remove",
      "accepted-step01",
      "accepted-step02",
      "accepted-step03-mint",
      "forced-init",
      "forced-remove",
      "forced-step01",
      "forced-step02",
      "forced-step03-mint",
      "publish-step01",
      "publish-step02",
      "publish-step03",
    ]);
    for (const entry of ledger.entries) {
      expect(entry.signedByteMargin, entry.name).toBeGreaterThan(0);
      expect(BigInt(entry.memoryUnitMargin), entry.name).toBeGreaterThan(0n);
      expect(BigInt(entry.cpuUnitMargin), entry.name).toBeGreaterThan(0n);
      if (entry.kind === "publication")
        expect(
          entry.publicationReserveMargin,
          entry.name,
        ).toBeGreaterThanOrEqual(0);
    }
    if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1") {
      await writeVanRossemFitLedger(ledgerPath, ledger);
      console.info(`[receive-purpose-language-fit-ledger] wrote ${ledgerPath}`);
    }
    console.info(
      `[receive-purpose-language-fit-ledger] ${JSON.stringify(ledger.entries)}`,
    );
  });
});
