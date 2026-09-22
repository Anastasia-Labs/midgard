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
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import {
  assertCompleteLifecycleCoverage,
  COMPLETE_LIFECYCLE_BASE_SCENARIOS,
  type CompleteLifecycleCoverage,
} from "../src/testing/complete-lifecycle.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.js";
import { createUnusedScriptWitnessActuator } from "../src/unused-script-witness/actuator.js";
import {
  advanceUnusedScriptWitnessPurposes,
  advanceUnusedScriptWitnessSources,
  initialUnusedScriptWitnessReverseScan,
  UNUSED_SCRIPT_WITNESS_MAXIMUM_SCAN_BATCH,
} from "../src/unused-script-witness/checkpoint.js";
import {
  applyUnusedScriptWitnessScripts,
  type UnusedScriptWitnessContracts,
} from "../src/unused-script-witness/contracts.js";
import { UNUSED_SCRIPT_WITNESS_CATEGORY_ID } from "../src/unused-script-witness/family.js";
import {
  buildUnusedScriptWitnessMaterialFromRetainedDa,
  prepareUnusedScriptWitnessArtifact,
} from "../src/unused-script-witness/replay.js";
import { buildUnusedScriptWitnessDirectionControlFromRetainedDa } from "../src/unused-script-witness/retained-stage-twelve.js";
import { submitUnusedScriptWitnessCancel } from "../src/unused-script-witness/submit-cancel.js";
import { submitUnusedScriptWitnessInit } from "../src/unused-script-witness/submit-init.js";
import {
  submitUnusedScriptWitnessStep01Accepted,
  submitUnusedScriptWitnessStep01Forced,
} from "../src/unused-script-witness/submit-step-01.js";
import {
  submitUnusedScriptWitnessStep02,
  type UnusedScriptWitnessAuthentication,
} from "../src/unused-script-witness/submit-step-02.js";
import { submitUnusedScriptWitnessStep03 } from "../src/unused-script-witness/submit-step-03.js";
import { submitUnusedScriptWitnessStep04 } from "../src/unused-script-witness/submit-step-04.js";
import { submitUnusedScriptWitnessStep05 } from "../src/unused-script-witness/submit-step-05.js";
import { submitUnusedScriptWitnessStep06 } from "../src/unused-script-witness/submit-step-06.js";
import { submitCapturedTransaction } from "../src/workflow/transaction-boundary.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
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
import {
  buildUnusedScriptWitnessFixture,
  readUnusedAuthenticatedWitness,
  readUnusedScanState,
  submitUnusedStep01ForcedRaw,
  submitUnusedStep02Raw,
  submitUnusedStep03Raw,
  submitUnusedStep04Raw,
  submitUnusedStep05Raw,
  submitUnusedStep06Raw,
  type UnusedScriptWitnessFixture,
} from "./support/unused-script-witness-emulator.js";

/**
 * The maximum evidence shape the suite drives through the real chain: 64
 * distinct inline native scripts (the accused one last, so the alternate-source
 * walk of step 04 opens 63 earlier sources, six siblings each, over three
 * self-loop batches) and 256 script purposes of every kind (spend, mint,
 * observer, receive; eight siblings each, eleven step-05 batches), with the
 * validation-traces trie widened to sixteen leaves. Every widened transaction
 * field is checked against the consensus preimage bounds by the fixture. The
 * per-transaction ceiling is fixed by `maximum_scan_batch` (24 openings); the
 * on-chain envelope at consensus-bounded frontier depth is measured by the
 * Aiken selectors in `rule.test.ak`.
 */
export const MAXIMUM_SOURCE_COUNT = 64;
export const MAXIMUM_PURPOSE_COUNT = 256;
export const MAXIMUM_DECOY_TRANSACTION_COUNT = 15;

const REASON_ARM = "UnusedScriptWitness";
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
  "retained_control",
  "source_item",
  "source_language",
  "source_length",
  "source_commitment",
  "source_membership",
  "source_frontier",
  "wrong_successor",
  "alternate_source_item",
  "alternate_source_membership",
  "alternate_source_order",
  "alternate_batch_short",
  "alternate_budget_over_bound",
  "purpose_item",
  "purpose_kind",
  "purpose_membership",
  "purpose_order",
  "scan_checkpoint",
  "scan_batch_short",
  "scan_budget_over_bound",
] as const;
const CANCELLABLE_STEPS = [
  "step01",
  "step02",
  "step03",
  "step04",
  "step05",
  "step06",
] as const;

const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/unused-script-witness-v1-fit-ledger.json",
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
  resumedAfterCheckpoint: false,
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
  console.info(`[unused-script-witness-progress] ${message}`);

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

const makeHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: { alwaysFraudProofCatalogue: true },
  });
  const addressData = await Effect.runPromise(
    addressDataFromBech32(
      harness.contracts.fraudProof.spendingScriptAddress,
    ).pipe(Effect.map((address) => Data.from(Data.to(address, AddressData)))),
  );
  const applied = applyUnusedScriptWitnessScripts({
    blueprint: harness.realBlueprint,
    network,
    computationThreadPolicyId: harness.contracts.computationThread.policyId,
    fraudProofPolicyId: harness.contracts.fraudProof.policyId,
    fraudProofTokenAddressData: addressData,
    hubOracleScriptHash: harness.contracts.hubOracle.policyId,
  });
  const contracts: UnusedScriptWitnessContracts = {
    steps: applied,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
  };
  const catalogue = await buildCatalogueDeploymentInfo({
    ...harness.contracts.fraudProofs,
    unusedScriptWitness: {
      ...harness.contracts.fraudProofs.unusedScriptWitness,
      spendingScriptHash: applied[0].spendingScriptHash,
    },
  });
  const category = catalogue.categories.unusedScriptWitness;
  expect(category.categoryId).toBe(UNUSED_SCRIPT_WITNESS_CATEGORY_ID);
  expect(category.scriptHash).toBe(applied[0].spendingScriptHash);
  const references: UTxO[] = [];
  // Published after the block setup so the harness nonce UTxO is still
  // unspent when the state-queue block is committed.
  const publishReferences = async () => {
    for (const [index, step] of applied.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `unused-script-witness-step-${(index + 1).toString()}`,
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
        await submitUnusedScriptWitnessInit({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts,
          category: category as never,
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
        await submitUnusedScriptWitnessCancel({
          ...common(threadOutRef, stepIndex),
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
    );
    expect(captured.result.txHash).toMatch(/^[0-9a-f]{64}$/u);
    coverage.cancelledSteps.add(CANCELLABLE_STEPS[stepIndex]!);
    return captured;
  };
  const setupBlock = async (fixture: UnusedScriptWitnessFixture) => {
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
        fraudProofUnusedScriptWitness: entry(applied[0]),
        fraudProofUnusedScriptWitnessStep02: entry(applied[1]),
        fraudProofUnusedScriptWitnessStep03: entry(applied[2]),
        fraudProofUnusedScriptWitnessStep04: entry(applied[3]),
        fraudProofUnusedScriptWitnessStep05: entry(applied[4]),
        fraudProofUnusedScriptWitnessStep06: entry(applied[5]),
      },
    };
  };
  const leaseCoordinator = (token: string) => ({
    acquire: async () => ({
      token,
      source: "emulator" as const,
      renew: async () => undefined,
      release: async () => undefined,
      fail: async () => undefined,
    }),
  });
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
    leaseCoordinator,
  };
};

type Harness = Awaited<ReturnType<typeof makeHarness>>;

/**
 * Builds the maximum-shape fixture: a probe without widening spends reveals
 * the baseline purpose count the machine derives from the shape (one per
 * used script, plus the mint, observer and receive purposes), and the real
 * fixture widens it to exactly `MAXIMUM_PURPOSE_COUNT`.
 */
const buildMaximumFixture = async (
  spec: Omit<
    Parameters<typeof buildUnusedScriptWitnessFixture>[0],
    | "sourceCount"
    | "allPurposeKinds"
    | "extraSpendPurposes"
    | "decoyTransactionCount"
  >,
) => {
  const probe = await buildUnusedScriptWitnessFixture({
    ...spec,
    sourceCount: MAXIMUM_SOURCE_COUNT,
    allPurposeKinds: true,
  });
  expect(probe.purposeCount).toBeLessThan(MAXIMUM_PURPOSE_COUNT);
  const fixture = await buildUnusedScriptWitnessFixture({
    ...spec,
    sourceCount: MAXIMUM_SOURCE_COUNT,
    allPurposeKinds: true,
    extraSpendPurposes: MAXIMUM_PURPOSE_COUNT - probe.purposeCount,
    decoyTransactionCount: MAXIMUM_DECOY_TRANSACTION_COUNT,
  });
  expect(fixture.purposeCount).toBe(MAXIMUM_PURPOSE_COUNT);
  expect(fixture.header.validationTraceCount).toBe(
    BigInt(MAXIMUM_DECOY_TRANSACTION_COUNT + 1),
  );
  return fixture;
};
type Artifact = Awaited<ReturnType<typeof prepareUnusedScriptWitnessArtifact>>;

const shapeLabel = (fixture: UnusedScriptWitnessFixture) =>
  `${fixture.spec.sourceCount.toString()} inline scripts (accused at ${fixture.scriptIndex.toString()}), ${fixture.purposeCount.toString()} purposes, ${fixture.header.validationTraceCount.toString()} validation traces`;

const frontiersOf = (artifact: Artifact) => ({
  source_count: BigInt(artifact.evidence.sources[0]!.membership.frontier.count),
  source_peaks: artifact.evidence.sources[0]!.membership.frontier.peaks.map(
    ({ height, hash }) => ({ height: BigInt(height), hash: hex(hash) }),
  ),
  purpose_count: BigInt(
    artifact.evidence.purposes[0]!.membership.frontier.count,
  ),
  purpose_peaks: artifact.evidence.purposes[0]!.membership.frontier.peaks.map(
    ({ height, hash }) => ({ height: BigInt(height), hash: hex(hash) }),
  ),
});

const sourceOpenings = (artifact: Artifact, start: number, end: number) =>
  artifact.evidence.sources.slice(start, end).map((opening) => ({
    source_index: BigInt(opening.sourceIndex),
    language_tag: BigInt(opening.languageTag),
    script_hash: opening.scriptHashHex,
    total_length: BigInt(opening.scriptTotalLength),
    item_commitment: opening.itemCommitmentHex,
    siblings: opening.membership.siblings.map(hex),
  }));

const purposeOpenings = (artifact: Artifact, start: number, end: number) =>
  artifact.evidence.purposes.slice(start, end).map((opening) => ({
    frontier_index: BigInt(opening.frontierIndex),
    purpose_kind: BigInt(opening.purposeKind),
    purpose_index: BigInt(opening.purposeIndex),
    script_hash: opening.scriptHashHex,
    purpose_subject: opening.purposeSubjectHex,
    siblings: opening.membership.siblings.map(hex),
  }));

/** Every step-02 authentication seam, mutated one at a time against a bound thread. */
const refuseEveryStep02Seam = async (
  h: Harness,
  threadOutRef: string,
  artifact: Artifact,
) => {
  const authentication = artifact.authentication;
  const frontiers = frontiersOf(artifact);
  const attempt = async (
    seam: string,
    mutated: UnusedScriptWitnessAuthentication,
    overrides: Partial<{
      frontiers: typeof frontiers;
      nextStepIndex: number;
    }> = {},
  ) => {
    progress(`step-02 refusal: ${seam}`);
    await expectOnchainRefusal(
      async () =>
        await submitUnusedStep02Raw({
          ...h.common(threadOutRef, 1),
          authentication: mutated,
          frontiers: overrides.frontiers ?? frontiers,
          nextStepIndex: overrides.nextStepIndex,
        }),
    );
    coverage.seams.add(seam);
  };
  const membership = authentication.trace_membership;
  await attempt("validation_traces_root", {
    ...authentication,
    trace_membership: { ...membership, root: "ff".repeat(32) },
  });
  await attempt("trace_descriptor", {
    ...authentication,
    trace_membership: {
      ...membership,
      value: {
        ...membership.value,
        step_count: membership.value.step_count + 1n,
      },
    },
  });
  await attempt("subject_event_key", {
    ...authentication,
    trace_membership: {
      ...membership,
      key: { L2TransactionEventKey: { tx_id: "aa".repeat(32) } },
    },
  });
  await attempt("machine_state", {
    ...authentication,
    machine_state: {
      ...authentication.machine_state,
      prior_ledger_root: "ee".repeat(32),
    },
  });
  expect(authentication.trace_proof.siblings.length).toBeGreaterThan(0);
  await attempt("trace_proof", {
    ...authentication,
    trace_proof: {
      ...authentication.trace_proof,
      siblings: [
        "dd".repeat(32),
        ...authentication.trace_proof.siblings.slice(1),
      ],
    },
  });
  await attempt("retained_control", {
    ...authentication,
    control: {
      witness_cbor: `${authentication.control.witness_cbor.slice(0, -2)}00`,
    },
  });
  await attempt("source_item", {
    ...authentication,
    script_hash: artifact.evidence.sources[0]!.scriptHashHex,
  });
  await attempt("source_language", {
    ...authentication,
    language_tag: authentication.language_tag === 0n ? 3n : 0n,
  });
  await attempt("source_length", {
    ...authentication,
    total_length: authentication.total_length + 1n,
  });
  await attempt("source_commitment", {
    ...authentication,
    item_commitment: "cc".repeat(32),
  });
  expect(authentication.source_siblings.length).toBeGreaterThan(0);
  await attempt("source_membership", {
    ...authentication,
    source_siblings: [
      "99".repeat(32),
      ...authentication.source_siblings.slice(1),
    ],
  });
  await attempt("source_frontier", authentication, {
    frontiers: {
      ...frontiers,
      purpose_peaks: frontiers.purpose_peaks.map((peak) => ({
        ...peak,
        hash: "bb".repeat(32),
      })),
    },
  });
  await attempt("wrong_successor", authentication, { nextStepIndex: 3 });
};

/** Every step-04 seam against a thread that just entered the alternate walk. */
const refuseEveryStep04Seam = async (
  h: Harness,
  threadOutRef: string,
  artifact: Artifact,
) => {
  const state = await readUnusedScanState(h.common(threadOutRef, 3), 3);
  expect(state.alternate_cursor).toBe(0n);
  const budget = UNUSED_SCRIPT_WITNESS_MAXIMUM_SCAN_BATCH;
  const legit = sourceOpenings(artifact, 0, budget);
  expect(legit.length).toBe(budget);
  const next = advanceUnusedScriptWitnessSources({
    state,
    evidence: artifact.evidence,
    itemBudget: budget,
  });
  const attempt = async (
    seam: string,
    input: Partial<Parameters<typeof submitUnusedStep04Raw>[0]>,
  ) => {
    progress(`step-04 refusal: ${seam}`);
    await expectOnchainRefusal(
      async () =>
        await submitUnusedStep04Raw({
          ...h.common(threadOutRef, 3),
          openings: legit,
          itemBudget: BigInt(budget),
          nextState: next,
          nextStepIndex: 3,
          ...input,
        }),
    );
    coverage.seams.add(seam);
  };
  await attempt("alternate_source_item", {
    openings: [
      { ...legit[0]!, script_hash: artifact.evidence.targetScriptHashHex },
      ...legit.slice(1),
    ],
  });
  await attempt("alternate_source_membership", {
    openings: [
      {
        ...legit[0]!,
        siblings: ["77".repeat(32), ...legit[0]!.siblings.slice(1)],
      },
      ...legit.slice(1),
    ],
  });
  await attempt("alternate_source_order", {
    openings: [legit[1]!, legit[0]!, ...legit.slice(2)],
  });
  await attempt("alternate_batch_short", {
    openings: legit.slice(0, budget - 1),
    nextState: advanceUnusedScriptWitnessSources({
      state,
      evidence: artifact.evidence,
      itemBudget: budget - 1,
    }),
  });
  await attempt("alternate_budget_over_bound", {
    openings: sourceOpenings(artifact, 0, budget + 1),
    itemBudget: BigInt(budget + 1),
    nextState: {
      ...advanceUnusedScriptWitnessSources({
        state,
        evidence: artifact.evidence,
        itemBudget: budget,
      }),
      alternate_cursor: BigInt(budget + 1),
    },
  });
};

/** Every step-05 seam against a thread that just entered the reverse match. */
const refuseEveryStep05Seam = async (
  h: Harness,
  threadOutRef: string,
  artifact: Artifact,
) => {
  const state = await readUnusedScanState(h.common(threadOutRef, 4), 4);
  expect(state.purpose_cursor).toBe(0n);
  expect(state.alternate_cursor).toBe(state.witness.bound.script_index);
  const budget = UNUSED_SCRIPT_WITNESS_MAXIMUM_SCAN_BATCH;
  const legit = purposeOpenings(artifact, 0, budget);
  expect(legit.length).toBe(budget);
  const next = advanceUnusedScriptWitnessPurposes({
    state,
    evidence: artifact.evidence,
    itemBudget: budget,
  });
  expect(next.used).toBe(false);
  const attempt = async (
    seam: string,
    input: Partial<Parameters<typeof submitUnusedStep05Raw>[0]>,
  ) => {
    progress(`step-05 refusal: ${seam}`);
    await expectOnchainRefusal(
      async () =>
        await submitUnusedStep05Raw({
          ...h.common(threadOutRef, 4),
          openings: legit,
          itemBudget: BigInt(budget),
          next: { kind: "scan", state: next },
          ...input,
        }),
    );
    coverage.seams.add(seam);
  };
  await attempt("purpose_item", {
    openings: [
      { ...legit[0]!, script_hash: artifact.evidence.targetScriptHashHex },
      ...legit.slice(1),
    ],
  });
  await attempt("purpose_kind", {
    openings: [
      { ...legit[0]!, purpose_kind: (legit[0]!.purpose_kind + 1n) % 4n },
      ...legit.slice(1),
    ],
  });
  await attempt("purpose_membership", {
    openings: [
      {
        ...legit[0]!,
        siblings: ["66".repeat(32), ...legit[0]!.siblings.slice(1)],
      },
      ...legit.slice(1),
    ],
  });
  await attempt("purpose_order", {
    openings: [legit[1]!, legit[0]!, ...legit.slice(2)],
  });
  await attempt("scan_checkpoint", {
    next: {
      kind: "scan",
      state: { ...next, checkpoint_hash: "55".repeat(32) },
    },
  });
  await attempt("scan_batch_short", {
    openings: legit.slice(0, budget - 1),
    next: {
      kind: "scan",
      state: advanceUnusedScriptWitnessPurposes({
        state,
        evidence: artifact.evidence,
        itemBudget: budget - 1,
      }),
    },
  });
  await attempt("scan_budget_over_bound", {
    openings: purposeOpenings(artifact, 0, budget + 1),
    itemBudget: BigInt(budget + 1),
    next: {
      kind: "scan",
      state: { ...next, purpose_cursor: BigInt(budget + 1) },
    },
  });
};

describe("unusedScriptWitness retained lifecycle material", () => {
  const vkey = "11".repeat(28);
  it("selects the first unused inline coordinate from the retained stage-11 audit", async () => {
    const fixture = await buildUnusedScriptWitnessFixture({
      direction: "accepted",
      claimedVerdict: "accepted",
      accusedUnused: true,
      sourceCount: 3,
      inputByte: 0x61,
      operatorVkey: vkey,
      startTime: 1_750_000_000_000n,
    });
    const artifact = await prepareUnusedScriptWitnessArtifact(
      fixture.canonicalBlock("ab".repeat(32)),
    );
    expect(artifact.evidence.finding.scriptIndex).toBe(2);
    expect(artifact.evidence.unused).toBe(true);
    expect(artifact.evidence.sources).toHaveLength(3);
    expect(artifact.evidence.purposes).toHaveLength(2);
    expect(artifact.acceptedInclusion).toBeDefined();
  });

  it("contradicts a forced rejection only through the retained stage-12 terminal", async () => {
    const fixture = await buildUnusedScriptWitnessFixture({
      direction: "forced",
      claimedVerdict: "rejected",
      accusedUnused: false,
      sourceCount: 3,
      inputByte: 0x62,
      operatorVkey: vkey,
      startTime: 1_750_000_000_000n,
    });
    const artifact = await prepareUnusedScriptWitnessArtifact(
      fixture.canonicalBlock("ac".repeat(32)),
    );
    expect(artifact.evidence.unused).toBe(false);
    expect(artifact.evidence.matchedPurposeIndex).toBe(2);
    expect(artifact.forcedMembership).toBeDefined();
    const honest = await buildUnusedScriptWitnessFixture({
      direction: "forced",
      claimedVerdict: "rejected",
      accusedUnused: true,
      sourceCount: 3,
      inputByte: 0x63,
      operatorVkey: vkey,
      startTime: 1_750_000_000_000n,
    });
    await expect(
      prepareUnusedScriptWitnessArtifact(
        honest.canonicalBlock("ad".repeat(32)),
      ),
    ).rejects.toThrow(/no contradiction/u);
  });
});

describe("unusedScriptWitness real lifecycle", () => {
  it("convicts an accepted unused inline script at the maximum shape: cancels every step, refuses every seam and the adjacent coordinate, resumes both scans from their checkpoints, then mints and removes through the actuator", async () => {
    const h = await makeHarness();
    const fixture = await buildMaximumFixture({
      direction: "accepted",
      claimedVerdict: "accepted",
      accusedUnused: true,
      inputByte: 0x64,
      operatorVkey: await funderPaymentKeyHash(h.harness.funderLucid),
      startTime: h.startTime(),
    });
    const setup = await h.setupBlock(fixture);
    const artifact = await prepareUnusedScriptWitnessArtifact(
      fixture.canonicalBlock(setup.headerHash),
    );
    expect(artifact.acceptedInclusion).toBeDefined();
    expect(artifact.forcedMembership).toBeUndefined();
    expect(artifact.evidence.finding.scriptIndex).toBe(
      MAXIMUM_SOURCE_COUNT - 1,
    );
    expect(artifact.evidence.unused).toBe(true);
    expect(artifact.evidence.sources).toHaveLength(MAXIMUM_SOURCE_COUNT);
    expect(artifact.evidence.purposes).toHaveLength(MAXIMUM_PURPOSE_COUNT);
    expect(
      new Set(artifact.evidence.purposes.map((purpose) => purpose.purposeKind)),
    ).toEqual(new Set([0, 1, 2, 3]));
    expect(artifact.evidence.sources[0]!.membership.siblings).toHaveLength(6);
    expect(artifact.evidence.purposes[0]!.membership.siblings).toHaveLength(8);
    expect(
      artifact.authentication.trace_membership.proof.length,
    ).toBeGreaterThan(0);
    const shape = shapeLabel(fixture);
    const scriptIndex = BigInt(fixture.scriptIndex);
    const step01 = async (threadOutRef: string, index = scriptIndex) =>
      await submitUnusedScriptWitnessStep01Accepted({
        ...h.common(threadOutRef, 0),
        blueprint: h.harness.realBlueprint,
        network,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: artifact.acceptedInclusion!,
        header: fixture.header,
        scriptIndex: index,
        witnessReferenceScripts: h.harness.witnessReferenceScripts,
      });
    const step02 = async (threadOutRef: string) =>
      await submitUnusedScriptWitnessStep02({
        ...h.common(threadOutRef, 1),
        evidence: artifact.evidence,
        authentication: artifact.authentication,
      });
    const step03 = async (threadOutRef: string) =>
      await submitUnusedScriptWitnessStep03(h.common(threadOutRef, 2));
    const step04All = async (threadOutRef: string) => {
      let current = threadOutRef;
      let batches = 0;
      for (;;) {
        const result = await submitUnusedScriptWitnessStep04({
          ...h.common(current, 3),
          evidence: artifact.evidence,
        });
        batches += 1;
        current = result.nextThreadOutRef;
        if (result.complete) return { threadOutRef: current, batches };
      }
    };
    const step05All = async (threadOutRef: string) => {
      let current = threadOutRef;
      let batches = 0;
      for (;;) {
        const result = await submitUnusedScriptWitnessStep05({
          ...h.common(current, 4),
          evidence: artifact.evidence,
        });
        batches += 1;
        current = result.nextThreadOutRef;
        if (result.complete) return { threadOutRef: current, batches };
      }
    };
    const fresh = async () => (await h.init(setup)).result.nextThreadOutRef;
    const bound = async () => (await step01(await fresh())).nextThreadOutRef;
    const authenticated = async () =>
      (await step02(await bound())).nextThreadOutRef;
    const walking = async () =>
      (await step03(await authenticated())).nextThreadOutRef;

    progress("cancel at every physical step");
    record(
      "accepted-cancel-step01",
      shape,
      (await h.cancel(await fresh(), 0)).measurement,
    );
    record(
      "accepted-cancel-step02",
      shape,
      (await h.cancel(await bound(), 1)).measurement,
    );
    record(
      "accepted-cancel-step03",
      shape,
      (await h.cancel(await authenticated(), 2)).measurement,
    );
    const walkThread = await walking();
    record(
      "accepted-cancel-step04",
      shape,
      (await h.cancel(walkThread, 3)).measurement,
    );
    const walked = await step04All(await walking());
    expect(walked.batches).toBe(
      Math.ceil(
        (MAXIMUM_SOURCE_COUNT - 1) / UNUSED_SCRIPT_WITNESS_MAXIMUM_SCAN_BATCH,
      ),
    );
    record(
      "accepted-cancel-step05",
      shape,
      (await h.cancel(walked.threadOutRef, 4)).measurement,
    );
    const scanned = await step05All(
      (await step04All(await walking())).threadOutRef,
    );
    expect(scanned.batches).toBe(
      Math.ceil(
        MAXIMUM_PURPOSE_COUNT / UNUSED_SCRIPT_WITNESS_MAXIMUM_SCAN_BATCH,
      ),
    );
    record(
      "accepted-cancel-step06",
      shape,
      (await h.cancel(scanned.threadOutRef, 5)).measurement,
    );

    progress("adjacent-over-bound script index");
    const overBound = await step01(await fresh(), BigInt(MAXIMUM_SOURCE_COUNT));
    await expectOnchainRefusal(
      async () =>
        await submitUnusedStep02Raw({
          ...h.common(overBound.nextThreadOutRef, 1),
          authentication: artifact.authentication,
          frontiers: frontiersOf(artifact),
        }),
    );
    coverage.adjacentOverBoundRefused = true;
    // Coordinate mutation on the accepted side: the thread binds a different
    // in-range coordinate than the one the retained audit authenticates.
    const wrongIndex = await step01(await fresh(), scriptIndex - 1n);
    await expectOnchainRefusal(
      async () =>
        await submitUnusedStep02Raw({
          ...h.common(wrongIndex.nextThreadOutRef, 1),
          authentication: artifact.authentication,
          frontiers: frontiersOf(artifact),
        }),
    );
    coverage.scenarios.add("reason_or_subject_coordinate_mutation");
    await h.cancel(overBound.nextThreadOutRef, 1);
    await h.cancel(wrongIndex.nextThreadOutRef, 1);

    progress("step-02 seam mutations");
    const seamThread02 = await bound();
    await refuseEveryStep02Seam(h, seamThread02, artifact);
    await h.cancel(seamThread02, 1);
    progress("step-03 wrong successor");
    const seamThread03 = await authenticated();
    const authenticatedWitness = await readUnusedAuthenticatedWitness(
      h.common(seamThread03, 2),
    );
    await expectOnchainRefusal(
      async () =>
        await submitUnusedStep03Raw({
          ...h.common(seamThread03, 2),
          nextState:
            initialUnusedScriptWitnessReverseScan(authenticatedWitness),
          nextStepIndex: 4,
        }),
    );
    await h.cancel(seamThread03, 2);
    progress("step-04 seam mutations");
    const seamThread04 = await walking();
    await refuseEveryStep04Seam(h, seamThread04, artifact);
    await h.cancel(seamThread04, 3);
    progress("step-05 seam mutations");
    const seamThread05 = (await step04All(await walking())).threadOutRef;
    await refuseEveryStep05Seam(h, seamThread05, artifact);
    await h.cancel(seamThread05, 4);

    progress("actuator lifecycle");
    const deploymentInfo = await h.removalDeployment();
    const actuatorConfig = {
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
        steps: h.references as unknown as readonly [
          UTxO,
          UTxO,
          UTxO,
          UTxO,
          UTxO,
          UTxO,
        ],
        witnesses: h.harness.witnessReferenceScripts as never,
      },
      stateQueueMutationLeaseCoordinator: h.leaseCoordinator(
        "unused-script-witness-emulator",
      ),
    } as const;
    let actuator = createUnusedScriptWitnessActuator(actuatorConfig);
    const outputAt = (address: string) => async (txHash: string) => {
      const utxo = (await h.harness.proverLucid.utxosAt(address)).find(
        (candidate) => candidate.txHash === txHash,
      );
      return utxo === undefined
        ? null
        : `${utxo.txHash}#${utxo.outputIndex.toString()}`;
    };
    const drive = async (
      label: string,
      action: Parameters<typeof actuator.capture>[0]["action"],
      locate: (txHash: string) => Promise<string | null>,
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
      expect(captured.measurement.l1ByteMargin, label).toBeGreaterThan(0);
      return { ...captured.result, measurement: captured.measurement };
    };
    /**
     * Reproduces the checkpoint a scan thread should carry after
     * `sourceBatches` alternate-source batches and `purposeBatches` purpose
     * batches, from the authenticated witness alone.
     */
    const reproduceScan = (
      witness: Parameters<typeof initialUnusedScriptWitnessReverseScan>[0],
      sourceBatches: number,
      purposeBatches: number,
    ) => {
      let state = initialUnusedScriptWitnessReverseScan(witness);
      for (let batch = 0; batch < sourceBatches; batch += 1)
        state = advanceUnusedScriptWitnessSources({
          state,
          evidence: artifact.evidence,
        });
      for (let batch = 0; batch < purposeBatches; batch += 1)
        state = advanceUnusedScriptWitnessPurposes({
          state,
          evidence: artifact.evidence,
        });
      return state;
    };
    const sourceBatchCount = Math.ceil(
      (MAXIMUM_SOURCE_COUNT - 1) / UNUSED_SCRIPT_WITNESS_MAXIMUM_SCAN_BATCH,
    );
    /**
     * Drives a self-loop until the thread lands on the successor script,
     * recording the first (full) batch and the terminal batch. After the
     * first batch the walk is interrupted: the on-chain checkpoint is
     * reproduced from the authenticated witness off-chain and a fresh
     * actuator resumes from the datum it observes.
     */
    const driveLoop = async (
      label: string,
      stage: "step_04" | "step_05",
      threadOutRef: string,
      ownAddress: string,
      nextAddress: string,
    ) => {
      let current = threadOutRef;
      let batches = 0;
      for (;;) {
        const isFirst = batches === 0;
        const result = await drive(
          `${label} batch ${batches.toString()}`,
          { stage, threadOutRef: current },
          async (txHash) =>
            (await outputAt(nextAddress)(txHash)) ??
            (await outputAt(ownAddress)(txHash)),
        );
        batches += 1;
        if (result.next === null) throw new Error("thread output absent");
        current = result.next;
        if (isFirst) record(`${label}-batch`, shape, result.measurement);
        if ((await outputAt(nextAddress)(result.txHash)) !== null) {
          expect(batches).toBeGreaterThan(1);
          record(`${label}-final`, shape, result.measurement);
          return { threadOutRef: current, batches };
        }
        if (isFirst) {
          const stepIndex = stage === "step_04" ? 3 : 4;
          const observed = await readUnusedScanState(
            h.common(current, stepIndex),
            stepIndex,
          );
          const reproduced =
            stage === "step_04"
              ? reproduceScan(observed.witness, 1, 0)
              : reproduceScan(observed.witness, sourceBatchCount, 1);
          expect(observed.checkpoint_hash).toBe(reproduced.checkpoint_hash);
          expect(observed).toEqual(reproduced);
          actuator = createUnusedScriptWitnessActuator(actuatorConfig);
          coverage.resumedAfterCheckpoint = true;
        }
      }
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
    record("accepted-step01", shape, s1.measurement);
    const s2 = await drive(
      "accepted-step02",
      { stage: "step_02", threadOutRef: s1.next! },
      outputAt(h.applied[2].spendingScriptAddress),
    );
    record("accepted-step02", shape, s2.measurement);
    const s3 = await drive(
      "accepted-step03",
      { stage: "step_03", threadOutRef: s2.next! },
      outputAt(h.applied[3].spendingScriptAddress),
    );
    record("accepted-step03", shape, s3.measurement);
    const s4 = await driveLoop(
      "accepted-step04",
      "step_04",
      s3.next!,
      h.applied[3].spendingScriptAddress,
      h.applied[4].spendingScriptAddress,
    );
    const s5 = await driveLoop(
      "accepted-step05",
      "step_05",
      s4.threadOutRef,
      h.applied[4].spendingScriptAddress,
      h.applied[5].spendingScriptAddress,
    );
    const s6 = await drive(
      "accepted-step06-mint",
      { stage: "step_06", threadOutRef: s5.threadOutRef },
      outputAt(h.harness.contracts.fraudProof.spendingScriptAddress),
    );
    record("accepted-step06-mint", shape, s6.measurement);
    vi.setSystemTime(h.harness.emulator.now());
    const removed = await drive(
      "accepted-remove",
      {
        stage: "remove",
        nextRemovalOutRef: setup.fraudulentBlockOutRef,
        fraudProofOutRef: s6.next!,
      },
      async (txHash) => txHash,
    );
    record("accepted-remove", shape, removed.measurement);
    coverage.reasonArms.add(REASON_ARM);
    coverage.successfulDirections.add("accepted_invalid");
    coverage.scenarios.add("wrongful_acceptance_success");
    coverage.scenarios.add("permanent_proof_token_and_descendant_removal");
    coverage.scenarios.add("maximum_supported_evidence");
  }, 3_600_000);

  it("contradicts a wrongful forced rejection of a used inline script at the maximum shape: refuses every forced-door seam, then mints and removes", async () => {
    const h = await makeHarness();
    const fixture = await buildMaximumFixture({
      direction: "forced",
      claimedVerdict: "rejected",
      accusedUnused: false,
      inputByte: 0x65,
      operatorVkey: await funderPaymentKeyHash(h.harness.funderLucid),
      startTime: h.startTime(),
    });
    const setup = await h.setupBlock(fixture);
    const artifact = await prepareUnusedScriptWitnessArtifact(
      fixture.canonicalBlock(setup.headerHash),
    );
    expect(artifact.forcedMembership).toBeDefined();
    expect(artifact.acceptedInclusion).toBeUndefined();
    expect(artifact.evidence.unused).toBe(false);
    expect(artifact.evidence.sources).toHaveLength(MAXIMUM_SOURCE_COUNT);
    expect(artifact.evidence.purposes).toHaveLength(MAXIMUM_PURPOSE_COUNT);
    const membership = artifact.forcedMembership!;
    const shape = shapeLabel(fixture);
    const scriptIndex = BigInt(fixture.scriptIndex);
    const initialized = await h.init(setup);
    record("forced-init", shape, initialized.measurement);
    const thread = initialized.result.nextThreadOutRef;
    const door = async (
      seam: string,
      patch: Partial<Parameters<typeof submitUnusedStep01ForcedRaw>[0]>,
    ) => {
      progress(`forced-door refusal: ${seam}`);
      await expectOnchainRefusal(
        async () =>
          await submitUnusedStep01ForcedRaw({
            ...h.common(thread, 0),
            header: fixture.header,
            membership,
            scriptIndex,
            direction: 1n,
            ...patch,
          }),
      );
      coverage.seams.add(seam);
    };
    await door("forced_leaf_reason_coordinate", {
      scriptIndex: scriptIndex + 1n,
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
        await submitUnusedScriptWitnessStep01Forced({
          ...h.common(thread, 0),
          header: fixture.header,
          membership,
          scriptIndex,
        }),
    );
    record("forced-step01", shape, bound.measurement);
    progress("forced step 02");
    const authenticated = await captureEmulatorSubmission(
      h.harness.emulator,
      async () =>
        await submitUnusedScriptWitnessStep02({
          ...h.common(bound.result.nextThreadOutRef, 1),
          evidence: artifact.evidence,
          authentication: artifact.authentication,
        }),
    );
    record("forced-step02", shape, authenticated.measurement);
    progress("forced step 03");
    const walking = await captureEmulatorSubmission(
      h.harness.emulator,
      async () =>
        await submitUnusedScriptWitnessStep03(
          h.common(authenticated.result.nextThreadOutRef, 2),
        ),
    );
    record("forced-step03", shape, walking.measurement);
    let current = walking.result.nextThreadOutRef;
    for (let batch = 0; ; batch += 1) {
      progress(`forced step 04 batch ${batch.toString()}`);
      const captured = await captureEmulatorSubmission(
        h.harness.emulator,
        async () =>
          await submitUnusedScriptWitnessStep04({
            ...h.common(current, 3),
            evidence: artifact.evidence,
          }),
      );
      current = captured.result.nextThreadOutRef;
      if (batch === 0)
        record("forced-step04-batch", shape, captured.measurement);
      if (captured.result.complete) {
        expect(batch).toBeGreaterThan(0);
        record("forced-step04-final", shape, captured.measurement);
        break;
      }
    }
    for (let batch = 0; ; batch += 1) {
      progress(`forced step 05 batch ${batch.toString()}`);
      const captured = await captureEmulatorSubmission(
        h.harness.emulator,
        async () =>
          await submitUnusedScriptWitnessStep05({
            ...h.common(current, 4),
            evidence: artifact.evidence,
          }),
      );
      current = captured.result.nextThreadOutRef;
      if (batch === 0)
        record("forced-step05-batch", shape, captured.measurement);
      if (captured.result.complete) {
        expect(batch).toBeGreaterThan(0);
        record("forced-step05-final", shape, captured.measurement);
        break;
      }
    }
    progress("forced step 06");
    const final = await captureEmulatorSubmission(
      h.harness.emulator,
      async () =>
        await submitUnusedScriptWitnessStep06({
          ...h.common(current, 5),
          evidence: artifact.evidence,
          witnessReferenceScripts: h.harness.witnessReferenceScripts,
        }),
    );
    record("forced-step06-mint", shape, final.measurement);
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
    const now = BigInt(h.harness.emulator.now());
    const removal = await captureEmulatorSubmission(
      h.harness.emulator,
      async () =>
        await submitRemoveFraudulentBlock({
          lucid: h.harness.proverLucid,
          blueprint: h.harness.realBlueprint,
          deploymentInfo,
          network,
          signer: h.harness.proverSigner,
          fraudCategory: "unusedScriptWitness",
          fraudulentHeaderHash: setup.headerHash,
          awaitConfirmation: true,
          requireReferenceScripts: true,
          stateQueueMutationLeaseCoordinator: h.leaseCoordinator(
            "unused-script-witness-forced",
          ),
          validFrom: now > 120_000n ? now - 120_000n : 0n,
          validTo: now + 300_000n,
        }),
    );
    record("forced-remove", shape, removal.measurement);
    coverage.reasonArms.add(REASON_ARM);
    coverage.successfulDirections.add("forced_rejection_wrong");
    coverage.scenarios.add("wrongful_forced_rejection_success");
  }, 3_600_000);

  it("refuses to convict an honest accepted block whose accused inline script is used, at the terminal step", async () => {
    const h = await makeHarness();
    // The accused coordinate sits before the end of field 6, so the retained
    // source prefix is shorter than the committed frontier step 02 carries.
    const fixture = await buildUnusedScriptWitnessFixture({
      direction: "accepted",
      claimedVerdict: "accepted",
      accusedUnused: false,
      sourceCount: 2,
      accusedIndex: 0,
      inputByte: 0x66,
      operatorVkey: await funderPaymentKeyHash(h.harness.funderLucid),
      startTime: h.startTime(),
    });
    const setup = await h.setupBlock(fixture);
    const block = fixture.canonicalBlock(setup.headerHash);
    await expect(prepareUnusedScriptWitnessArtifact(block)).rejects.toThrow(
      /no contradiction/u,
    );
    const material = await buildUnusedScriptWitnessMaterialFromRetainedDa({
      block,
      eventKey: fixture.eventKey,
      subject: fixture.subject,
      scriptIndex: 0,
      txCbor: fixture.transaction.txCbor,
    });
    expect(material.evidence.unused).toBe(false);
    expect(material.evidence.sources).toHaveLength(1);
    expect(material.evidence.sources[0]!.membership.frontier.count).toBe(2);
    const initialized = await h.init(setup);
    const bound = await submitUnusedScriptWitnessStep01Accepted({
      ...h.common(initialized.result.nextThreadOutRef, 0),
      blueprint: h.harness.realBlueprint,
      network,
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: fixture.block.txInclusion!,
      header: fixture.header,
      scriptIndex: 0n,
      witnessReferenceScripts: h.harness.witnessReferenceScripts,
    });
    const authenticated = await submitUnusedScriptWitnessStep02({
      ...h.common(bound.nextThreadOutRef, 1),
      evidence: material.evidence,
      authentication: material.authentication,
    });
    const walking = await submitUnusedScriptWitnessStep03(
      h.common(authenticated.nextThreadOutRef, 2),
    );
    const walked = await submitUnusedScriptWitnessStep04({
      ...h.common(walking.nextThreadOutRef, 3),
      evidence: material.evidence,
    });
    expect(walked.complete).toBe(true);
    const scanned = await submitUnusedScriptWitnessStep05({
      ...h.common(walked.nextThreadOutRef, 4),
      evidence: material.evidence,
    });
    expect(scanned.complete).toBe(true);
    await expect(
      submitUnusedScriptWitnessStep06({
        ...h.common(scanned.nextThreadOutRef, 5),
        evidence: material.evidence,
        witnessReferenceScripts: h.harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/retained contradiction/u);
    await expectOnchainRefusal(
      async () =>
        await submitUnusedStep06Raw({
          ...h.common(scanned.nextThreadOutRef, 5),
          witnessReferenceScripts: h.harness.witnessReferenceScripts,
        }),
    );
    await h.cancel(scanned.nextThreadOutRef, 5);
    coverage.scenarios.add("honest_accepted_block_refusal");
  }, 600_000);

  it("refuses to authenticate an honest forced rejection: the machine's stage-11 stop never reaches the stage-12 terminal step 02 demands", async () => {
    const h = await makeHarness();
    const fixture = await buildUnusedScriptWitnessFixture({
      direction: "forced",
      claimedVerdict: "rejected",
      accusedUnused: true,
      sourceCount: 2,
      inputByte: 0x67,
      operatorVkey: await funderPaymentKeyHash(h.harness.funderLucid),
      startTime: h.startTime(),
    });
    const setup = await h.setupBlock(fixture);
    await expect(
      prepareUnusedScriptWitnessArtifact(
        fixture.canonicalBlock(setup.headerHash),
      ),
    ).rejects.toThrow(/no contradiction/u);
    // The only retained ScriptSources seam is the stage-11 audit state at the
    // accused coordinate; hand exactly that to the forced-direction step 02.
    const base = await buildUnusedScriptWitnessDirectionControlFromRetainedDa({
      eventKey: fixture.eventKey,
      transactionId: fixture.nativeTxId,
      direction: 0n,
      scriptIndex: fixture.scriptIndex,
      ...fixture.retainedEntries,
      expectedValidationTracesRoot: fixture.header.validationTracesRoot,
    });
    expect(base.control.stage).toBe(11n);
    const target = base.sources[fixture.scriptIndex]!;
    const authentication: UnusedScriptWitnessAuthentication = {
      trace_membership: base.traceMembership,
      machine_state: base.machineState,
      trace_proof: base.traceProof,
      control: { witness_cbor: base.witnessCbor },
      language_tag: BigInt(target.languageTag),
      script_hash: target.scriptHashHex,
      total_length: BigInt(target.scriptTotalLength),
      item_commitment: target.itemCommitmentHex,
      source_siblings: [...target.siblings],
    };
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: fixture.block.reconstruction,
      eventKey: fixture.eventKey,
    });
    const initialized = await h.init(setup);
    const bound = await submitUnusedScriptWitnessStep01Forced({
      ...h.common(initialized.result.nextThreadOutRef, 0),
      header: fixture.header,
      membership,
      scriptIndex: BigInt(fixture.scriptIndex),
    });
    await expectOnchainRefusal(
      async () =>
        await submitUnusedStep02Raw({
          ...h.common(bound.nextThreadOutRef, 1),
          authentication,
          frontiers: {
            source_count: base.control.source_count,
            source_peaks: base.control.source_peaks,
            purpose_count: base.control.purpose_count,
            purpose_peaks: base.control.purpose_peaks,
          },
        }),
    );
    await h.cancel(bound.nextThreadOutRef, 1);
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
      resumedAfterCheckpoint: coverage.resumedAfterCheckpoint,
      adjacentOverBoundRefused: coverage.adjacentOverBoundRefused,
    };
    assertCompleteLifecycleCoverage({
      coverage: complete,
      expectedReasonArms: [REASON_ARM],
      authenticationSeams: [...AUTHENTICATION_SEAMS],
      cancellablePhysicalSteps: [...CANCELLABLE_STEPS],
      resumable: true,
      hasAdjacentConsensusBound: true,
    });
    const blueprintBytes = readFileSync(realBlueprintPath);
    const preamble = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: "unusedScriptWitness",
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
      "accepted-cancel-step04",
      "accepted-cancel-step05",
      "accepted-cancel-step06",
      "accepted-init",
      "accepted-remove",
      "accepted-step01",
      "accepted-step02",
      "accepted-step03",
      "accepted-step04-batch",
      "accepted-step04-final",
      "accepted-step05-batch",
      "accepted-step05-final",
      "accepted-step06-mint",
      "forced-init",
      "forced-remove",
      "forced-step01",
      "forced-step02",
      "forced-step03",
      "forced-step04-batch",
      "forced-step04-final",
      "forced-step05-batch",
      "forced-step05-final",
      "forced-step06-mint",
      "publish-step01",
      "publish-step02",
      "publish-step03",
      "publish-step04",
      "publish-step05",
      "publish-step06",
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
      console.info(`[unused-script-witness-fit-ledger] wrote ${ledgerPath}`);
    }
    console.info(
      `[unused-script-witness-fit-ledger] ${JSON.stringify(ledger.entries)}`,
    );
  });
});
