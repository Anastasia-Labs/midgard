/**
 * `native-script-decoding` adversarial-prover emulator suite (#635, #633;
 * offchain plan §8.2 suite 7).
 *
 * Every scenario here starts from an HONEST commitment — a header whose
 * decoding classification is correct — and puts an adversarial prover against
 * it. The property under test is the one an operator actually depends on: a
 * wrongful conviction is impossible, and impossible ON CHAIN, not merely
 * inconvenient offchain.
 *
 * So each attack is asserted in BOTH planes, the way the fabricated-deposit
 * valid-block negative is:
 *
 * - the offchain plane: the honest tooling's fail-closed pre-check refuses
 *   the attack before anything is paid for, naming the contradiction; and
 * - the on-chain plane: the same attack, rebuilt past that pre-check through
 *   the test-only raw builders in the support module, is refused by the
 *   validator itself — and the thread is observably no further along.
 *
 * The roads covered: a direction-A conviction over a well-formed payload; a
 * direction-B conviction over a correctly-classed rejection; forged ledger
 * evidence, forged chunk bytes and a forged forced leaf; a third party
 * driving or cancelling somebody else's thread; and a descriptor-contradiction
 * close fired over a tag-0 descriptor that contradicts nothing.
 *
 * The raw builders exist for this file alone. Production code never bypasses
 * a pre-check; an adversary's patched tooling does, which is exactly why the
 * on-chain plane has to be exercised directly.
 *
 * Lives in its own file for the reason its siblings do: `@lucid-evolution/uplc`
 * never reclaims wasm linear memory and vitest isolates per FILE. See
 * tests/support/uplc-heap-guard.ts.
 */
import {
  decodeMidgardLedgerOutputCommitmentV1,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildNativeScriptDecodingChunkProofV1,
  buildNativeScriptDecodingLedgerMembershipV1,
  buildNativeScriptDecodingScanPlanV1,
  buildNativeScriptDecodingStep02EvidenceV1,
  type NativeScriptDecodingContractsV1,
  NativeScriptDecodingPlanRoutesV1,
  nativeScriptDecodingScanArgsEvidenceV1,
  type NativeScriptDecodingScanPlanV1,
  type NativeScriptDecodingVerdictPlanV1,
  nativeScriptDecodingWindowProofsV1,
  requireNativeScriptDecodingThreadUtxoV1,
  submitNativeScriptDecodingCancel,
  submitNativeScriptDecodingInit,
  submitNativeScriptDecodingStep01BindNormal,
  submitNativeScriptDecodingStep01RecordForced,
  submitNativeScriptDecodingStep02,
  submitNativeScriptDecodingStep03AdvanceOrCloseSegment,
  submitNativeScriptDecodingStep03BindDescriptor,
  submitNativeScriptDecodingStep03OpenSubject,
} from "../src/native-script-decoding/index.js";
import type { ResolvedProverSigner } from "../src/runtime.js";
import {
  buildDecodingLedgerFixtureV1,
  DECODING_ACCUSED_TX_ID_V1,
  decodingCanonicalItemV1,
  decodingMalformedMultiChunkItemV1,
  decodingPlutusItemV1,
  type DecodingScenarioV1,
  expectOnchainRefusalV1,
  fundDecodingOutsiderV1,
  makeDecodingEmulatorHarnessV1,
  publishDecodingReferenceScriptsV1,
  setupDecodingScenarioV1,
  submitRawDecodingCancelV1,
  submitRawDecodingStepV1,
} from "./support/native-script-decoding-emulator-v1.js";
import { network } from "./support/submit-init-emulator-shared.js";

type DecodingHarnessV1 = Awaited<
  ReturnType<typeof makeDecodingEmulatorHarnessV1>
>;
type DecodingReferenceScriptsV1 = readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];

/** The §2.4.3(e) rejection direction B disputes, accusing (reference, 0). */
const MALFORMED_REJECTION_VERDICT: SDK.OperatorVerdictV1 = {
  ForcedTxInvalid: {
    reason: {
      ResolvedReferenceScriptMalformed: { source_kind: 1n, input_index: 0n },
    },
  },
};

/** Locates the thread and decodes its step-03 scan state. */
const readStep03Thread = async (
  harness: DecodingHarnessV1,
  threadOutRef: string,
  stepIndex: 2 | 3 | 4 = 4,
): Promise<{
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly state: SDK.NativeScriptDecodingScanThreadStateV1;
}> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxoV1({
      lucid: harness.proverLucid,
      contracts: harness.decoding,
      categoryId: harness.category.categoryId,
      stepIndex,
      threadOutRef,
    });
  const datum = Data.from(
    threadUtxo.datum!,
    SDK.NativeScriptDecodingStep03AdvanceOrCloseDatum,
  );
  if (datum.data === null) {
    throw new Error("step-03 thread carries no scan state");
  }
  return { threadUtxo, threadUnit: threadToken.unit, state: datum.data };
};

const subjectOutpointKeyCbor = (scenario: DecodingScenarioV1): string =>
  Buffer.from(
    SDK.encodeMidgardTxInputCanonicalV1(scenario.subjectFieldInputs[0]!),
  ).toString("hex");

/** Init → step-01 (normal) → step-02: the honest prefix every attack shares. */
const driveNormalThreadToStep03 = async ({
  harness,
  scenario,
  refs,
}: {
  readonly harness: DecodingHarnessV1;
  readonly scenario: DecodingScenarioV1;
  readonly refs: DecodingReferenceScriptsV1;
}): Promise<string> => {
  const { proverLucid, proverSigner, decoding, category, realBlueprint } =
    harness;
  const init = await submitNativeScriptDecodingInit({
    lucid: proverLucid,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: realBlueprint,
    network,
    contracts: decoding,
    category,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    signer: proverSigner,
    fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
  });
  if (scenario.block.txInclusion === null) {
    throw new Error("normal-source fixture carries no tx inclusion");
  }
  const step01 = await submitNativeScriptDecodingStep01BindNormal({
    lucid: proverLucid,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: realBlueprint,
    contracts: decoding,
    categoryId: category.categoryId,
    network,
    signer: proverSigner,
    threadOutRef: init.nextThreadOutRef,
    stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
    txInclusion: scenario.block.txInclusion,
    referenceScriptUtxo: refs[0],
  });
  const step02 = await submitNativeScriptDecodingStep02({
    lucid: proverLucid,
    contracts: decoding,
    categoryId: category.categoryId,
    signer: proverSigner,
    threadOutRef: step01.nextThreadOutRef,
    reconstruction: scenario.block.reconstruction,
    chosenOutpoint: { sourceKind: scenario.accusedSourceKind, cursor: 0n },
    referenceScriptUtxo: refs[1],
  });
  return step02.nextThreadOutRef;
};

/** Init → step-01 (forced, prover-chosen direction) → the step-02 thread. */
const driveForcedThreadToStep02 = async ({
  harness,
  scenario,
  refs,
  direction,
}: {
  readonly harness: DecodingHarnessV1;
  readonly scenario: DecodingScenarioV1;
  readonly refs: DecodingReferenceScriptsV1;
  readonly direction: bigint;
}): Promise<string> => {
  const { proverLucid, proverSigner, decoding, category, realBlueprint } =
    harness;
  const init = await submitNativeScriptDecodingInit({
    lucid: proverLucid,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: realBlueprint,
    network,
    contracts: decoding,
    category,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    signer: proverSigner,
    fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
  });
  const step01 = await submitNativeScriptDecodingStep01RecordForced({
    lucid: proverLucid,
    contracts: decoding,
    categoryId: category.categoryId,
    signer: proverSigner,
    threadOutRef: init.nextThreadOutRef,
    direction,
    referenceScriptUtxo: refs[0],
  });
  return step01.nextThreadOutRef;
};

/** Binds the accused outpoint honestly and runs every planned Scan segment. */
const bindAndScanHonestly = async ({
  harness,
  scenario,
  refs,
  plan,
  item,
  threadOutRef,
}: {
  readonly harness: DecodingHarnessV1;
  readonly scenario: DecodingScenarioV1;
  readonly refs: DecodingReferenceScriptsV1;
  readonly plan: NativeScriptDecodingScanPlanV1;
  readonly item: Uint8Array;
  readonly threadOutRef: string;
}): Promise<string> => {
  const { proverLucid, proverSigner, decoding, category } = harness;
  const opened = await submitNativeScriptDecodingStep03OpenSubject({
    lucid: proverLucid,
    contracts: decoding,
    categoryId: category.categoryId,
    signer: proverSigner,
    threadOutRef,
    nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
    subjectFieldInputs: scenario.subjectFieldInputs,
    referenceScriptUtxo: refs[2],
  });
  const subjectOutpoint = scenario.subjectFieldInputs[0]!;
  const bind = await submitNativeScriptDecodingStep03BindDescriptor({
    lucid: proverLucid,
    contracts: decoding,
    categoryId: category.categoryId,
    signer: proverSigner,
    threadOutRef: opened.nextThreadOutRef,
    outpointKeyCbor: Buffer.from(
      SDK.encodeMidgardTxInputCanonicalV1(subjectOutpoint),
    ).toString("hex"),
    descriptorCbor: scenario.ledger.descriptorCbor,
    ledgerTrie: scenario.ledger.trie,
    plan,
    referenceScriptItemBytes: item,
    referenceScriptUtxo: refs[3],
  });
  let cursor = bind.nextThreadOutRef;
  for (const segment of plan.segments) {
    const scan = await submitNativeScriptDecodingStep03AdvanceOrCloseSegment({
      lucid: proverLucid,
      contracts: decoding,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: cursor,
      segment,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: refs[4],
    });
    cursor = scan.nextThreadOutRef;
  }
  return cursor;
};

/** A raw AdvanceOrClose transition an adversary's patched tooling would submit. */
const submitRawVerdict = async ({
  harness,
  contracts,
  signer,
  threadOutRef,
  controlCbor,
  refusalClass,
  window,
  referenceScriptItemBytes,
  referenceScriptUtxo,
}: {
  readonly harness: DecodingHarnessV1;
  readonly contracts: NativeScriptDecodingContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly controlCbor: string;
  readonly refusalClass: bigint;
  /** Direction A's refusing step reads a chunk window; direction B reads none. */
  readonly window?: NativeScriptDecodingVerdictPlanV1["window"];
  readonly referenceScriptItemBytes?: Uint8Array;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const { threadUtxo, threadUnit, state } = await readStep03Thread(
    harness,
    threadOutRef,
  );
  const proofs =
    window === undefined ||
    window === null ||
    referenceScriptItemBytes === undefined
      ? { chunk_proof: null, next_chunk_proof: null }
      : nativeScriptDecodingWindowProofsV1({
          window,
          fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
          itemIndex: Number(state.output_index),
          itemBytes: referenceScriptItemBytes,
        });
  const nextDatumCbor = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: { ...state, refusal_class: refusalClass },
    },
    SDK.NativeScriptDecodingStep03AdvanceOrCloseDatum,
  );
  return submitRawDecodingStepV1({
    lucid: harness.proverLucid,
    contracts,
    signer,
    stepIndex: 4,
    threadUtxo,
    threadUnit,
    destinationAddress: contracts.steps[5]!.spendingScriptAddress,
    nextDatumCbor,
    buildRedeemer: (layout) =>
      Data.to(
        {
          Continue: [
            {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              control_cbor: controlCbor,
              chunk_proof: proofs.chunk_proof,
              next_chunk_proof: proofs.next_chunk_proof,
              frames: [],
              step_budget: window === undefined ? 0n : 1n,
            },
          ],
        },
        SDK.NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer,
      ),
    referenceScriptUtxo,
  });
};

describe("native-script-decoding adversarial-prover emulator suite", () => {
  it("refuses a direction-A conviction over a well-formed payload", async () => {
    const harness = await makeDecodingEmulatorHarnessV1();
    // The honest commitment: the operator ACCEPTED a transaction whose
    // reference-script payload really does decode canonically.
    const item = decodingCanonicalItemV1();
    const scenario = await setupDecodingScenarioV1({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "normal" },
    });
    const refs = await publishDecodingReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.decoding,
    });

    // Offchain plane: the planner will not stage the fault at all.
    expect(() =>
      buildNativeScriptDecodingScanPlanV1({ itemBytes: item, direction: 0 }),
    ).toThrow(/there is no wrongful\s+acceptance to prove/);

    // On-chain plane. BindDescriptor and scan advances are direction-agnostic — the
    // machine they run is the payload's, not the accusation's — so an
    // adversary can reach the Verdict arm by scanning the honest trace. The
    // fabricated verdict claims the accepting terminal refuses.
    const honestTrace = buildNativeScriptDecodingScanPlanV1({
      itemBytes: item,
      direction: 1,
    });
    const step03OutRef = await driveNormalThreadToStep03({
      harness,
      scenario,
      refs,
    });
    const scanned = await bindAndScanHonestly({
      harness,
      scenario,
      refs,
      plan: honestTrace,
      item,
      threadOutRef: step03OutRef,
    });
    // Bind and every Scan segment LANDED — the thread sits on the payload's
    // real terminal. So whatever the next transaction fails on, it fails on
    // the Verdict arm alone: direction A's verdict must exhibit a refusing
    // step, and this control accepts. (The mirror-image control — the same
    // terminal accepted by a direction-B verdict — is the honest lifecycle in
    // submit-init-emulator-native-script-decoding-direction-b.test.ts, which
    // runs this same canonical payload end to end.)
    const atTerminal = await readStep03Thread(harness, scanned);
    expect(atTerminal.state.machine_state_hash).toBe(
      honestTrace.verdict.control!.hashHex,
    );
    const message = await expectOnchainRefusalV1(() =>
      submitRawVerdict({
        harness,
        contracts: harness.decoding,
        signer: harness.proverSigner,
        threadOutRef: scanned,
        controlCbor: honestTrace.verdict.control!.cborHex,
        refusalClass: 0n,
        referenceScriptUtxo: refs[4],
      }),
    );
    expect(message.length).toBeGreaterThan(0);

    // The thread is exactly where it was: nothing advanced to step-04.
    const after = await readStep03Thread(harness, scanned);
    expect(after.state.refusal_class).toBe(
      SDK.NATIVE_SCRIPT_DECODING_CLASS_PENDING_V1,
    );
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.decoding.steps[5]!.spendingScriptAddress,
        after.threadUnit,
      ),
    ).resolves.toHaveLength(0);
  }, 600_000);

  it("refuses a direction-B conviction over a correctly-classed rejection", async () => {
    const harness = await makeDecodingEmulatorHarnessV1();
    // The honest commitment: the operator REJECTED a forced transaction whose
    // reference-script payload really is malformed, and classed it correctly.
    const item = decodingMalformedMultiChunkItemV1();
    const scenario = await setupDecodingScenarioV1({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "forced", verdict: MALFORMED_REJECTION_VERDICT },
    });
    const refs = await publishDecodingReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.decoding,
    });

    // Offchain plane.
    expect(() =>
      buildNativeScriptDecodingScanPlanV1({ itemBytes: item, direction: 1 }),
    ).toThrow(/there is no wrongful rejection to prove/);

    // On-chain plane: scan the honest refusing trace under a direction-B
    // thread, then claim the refusing step is the canonical terminal.
    const honestTrace = buildNativeScriptDecodingScanPlanV1({
      itemBytes: item,
      direction: 0,
    });
    const step02OutRef = await driveForcedThreadToStep02({
      harness,
      scenario,
      refs,
      direction: 1n,
    });
    const step02 = await submitNativeScriptDecodingStep02({
      lucid: harness.proverLucid,
      contracts: harness.decoding,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step02OutRef,
      reconstruction: scenario.block.reconstruction,
      forcedOrderKey: scenario.block.forcedOrderKey!,
      referenceScriptUtxo: refs[1],
    });
    const scanned = await bindAndScanHonestly({
      harness,
      scenario,
      refs,
      plan: honestTrace,
      item,
      threadOutRef: step02.nextThreadOutRef,
    });
    // Again, bind and every Scan segment landed: the thread sits on the
    // payload's real, REFUSING control. Only the Verdict arm can refuse what
    // follows — direction B's verdict must be the exact canonical terminal.
    // (The mirror-image control is the honest direction-A lifecycle in
    // submit-init-emulator-native-script-decoding-direction-a.test.ts, which
    // closes this same malformed payload over this same control.)
    const atRefusingStep = await readStep03Thread(harness, scanned);
    expect(atRefusingStep.state.machine_state_hash).toBe(
      honestTrace.verdict.control!.hashHex,
    );
    const message = await expectOnchainRefusalV1(() =>
      submitRawVerdict({
        harness,
        contracts: harness.decoding,
        signer: harness.proverSigner,
        threadOutRef: scanned,
        controlCbor: honestTrace.verdict.control!.cborHex,
        refusalClass: SDK.NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED_V1,
        referenceScriptUtxo: refs[4],
      }),
    );
    expect(message.length).toBeGreaterThan(0);
    const after = await readStep03Thread(harness, scanned);
    expect(after.state.refusal_class).toBe(
      SDK.NATIVE_SCRIPT_DECODING_CLASS_PENDING_V1,
    );
  }, 600_000);

  it("refuses a bind whose descriptor comes from a foreign ledger", async () => {
    const harness = await makeDecodingEmulatorHarnessV1();
    // Honest commitment over a canonical payload; the adversary wants the
    // machine to scan a MALFORMED payload instead, so it supplies a foreign
    // descriptor and a proof from the tree that descriptor actually lives in.
    const item = decodingCanonicalItemV1();
    const scenario = await setupDecodingScenarioV1({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "normal" },
    });
    const forgedItem = decodingMalformedMultiChunkItemV1();
    const forged = await buildDecodingLedgerFixtureV1({
      txIdHex: DECODING_ACCUSED_TX_ID_V1,
      outputIndex: 0,
      referenceScriptItemBytes: forgedItem,
      referenceScriptLanguage: 0,
    });
    const forgedPlan = buildNativeScriptDecodingScanPlanV1({
      itemBytes: forgedItem,
      direction: 0,
    });
    const refs = await publishDecodingReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.decoding,
    });
    const step03OutRef = await driveNormalThreadToStep03({
      harness,
      scenario,
      refs,
    });
    const opened = await submitNativeScriptDecodingStep03OpenSubject({
      lucid: harness.proverLucid,
      contracts: harness.decoding,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step03OutRef,
      nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
      subjectFieldInputs: scenario.subjectFieldInputs,
      referenceScriptUtxo: refs[2],
    });

    const bindWith = (trie: typeof scenario.ledger.trie) =>
      submitNativeScriptDecodingStep03BindDescriptor({
        lucid: harness.proverLucid,
        contracts: harness.decoding,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: opened.nextThreadOutRef,
        outpointKeyCbor: subjectOutpointKeyCbor(scenario),
        descriptorCbor: forged.descriptorCbor,
        ledgerTrie: trie,
        plan: forgedPlan,
        referenceScriptItemBytes: forgedItem,
        referenceScriptUtxo: refs[3],
      });

    // Offchain plane: the honest tooling refuses a trie that is not the
    // thread's committed pre-state.
    await expect(bindWith(forged.trie)).rejects.toThrow(
      /is not the thread's prior_ledger_root/,
    );

    // On-chain plane: a LYING handle reports the honest root while proving
    // against the forged tree, so the membership proof reaches the validator.
    const liar = {
      rootHex: scenario.ledger.rootHex,
      prove: forged.trie.prove,
    };
    const message = await expectOnchainRefusalV1(() => bindWith(liar));
    expect(message.length).toBeGreaterThan(0);

    // The thread never bound.
    const after = await readStep03Thread(harness, opened.nextThreadOutRef, 3);
    expect(after.state.machine_state_hash).toBe("");
    expect(after.state.outpoint_key_hash).not.toBe("");

    // Control, so the refusal above is attributable to the forged ledger
    // evidence and nothing else: the SAME bind with the block's own
    // descriptor and its own trie lands. (The plan is the direction-1 trace
    // of the honest payload — BindDescriptor runs the payload's machine, not
    // the accusation's, so the arm is direction-agnostic.)
    const honestBind = await submitNativeScriptDecodingStep03BindDescriptor({
      lucid: harness.proverLucid,
      contracts: harness.decoding,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: opened.nextThreadOutRef,
      outpointKeyCbor: subjectOutpointKeyCbor(scenario),
      descriptorCbor: scenario.ledger.descriptorCbor,
      ledgerTrie: scenario.ledger.trie,
      plan: buildNativeScriptDecodingScanPlanV1({
        itemBytes: item,
        direction: 1,
      }),
      referenceScriptItemBytes: item,
      referenceScriptUtxo: refs[3],
    });
    expect(honestBind.scanState.outpoint_key_hash).not.toBe("");
  }, 600_000);

  it("refuses a Scan whose chunk proof is built over substituted bytes", async () => {
    const harness = await makeDecodingEmulatorHarnessV1();
    const item = decodingMalformedMultiChunkItemV1();
    const scenario = await setupDecodingScenarioV1({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "normal" },
    });
    const plan = buildNativeScriptDecodingScanPlanV1({
      itemBytes: item,
      direction: 0,
    });
    const refs = await publishDecodingReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.decoding,
    });
    const step03OutRef = await driveNormalThreadToStep03({
      harness,
      scenario,
      refs,
    });
    const opened = await submitNativeScriptDecodingStep03OpenSubject({
      lucid: harness.proverLucid,
      contracts: harness.decoding,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step03OutRef,
      nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
      subjectFieldInputs: scenario.subjectFieldInputs,
      referenceScriptUtxo: refs[2],
    });
    const bind = await submitNativeScriptDecodingStep03BindDescriptor({
      lucid: harness.proverLucid,
      contracts: harness.decoding,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: opened.nextThreadOutRef,
      outpointKeyCbor: subjectOutpointKeyCbor(scenario),
      descriptorCbor: scenario.ledger.descriptorCbor,
      ledgerTrie: scenario.ledger.trie,
      plan,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: refs[3],
    });

    // One byte of the first chunk, rewritten. The frozen anchor commits the
    // original, so no proof over these bytes can open under it.
    const substituted = Uint8Array.from(item);
    substituted[7] = substituted[7]! ^ 0xff;
    const segment = plan.segments[0]!;

    // Offchain plane.
    await expect(
      submitNativeScriptDecodingStep03AdvanceOrCloseSegment({
        lucid: harness.proverLucid,
        contracts: harness.decoding,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: bind.nextThreadOutRef,
        segment,
        referenceScriptItemBytes: substituted,
        referenceScriptUtxo: refs[4],
      }),
    ).rejects.toThrow(/do not rebuild the frozen item commitment/);

    // On-chain plane: honest control, honest frames, forged chunk proof.
    const { threadUtxo, threadUnit, state } = await readStep03Thread(
      harness,
      bind.nextThreadOutRef,
    );
    const rawScanWith = (itemBytes: Uint8Array) => {
      const evidence = nativeScriptDecodingScanArgsEvidenceV1({
        segment,
        fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
        itemIndex: Number(state.output_index),
        itemBytes,
      });
      return submitRawDecodingStepV1({
        lucid: harness.proverLucid,
        contracts: harness.decoding,
        signer: harness.proverSigner,
        stepIndex: 4,
        threadUtxo,
        threadUnit,
        destinationAddress: harness.decoding.steps[4]!.spendingScriptAddress,
        nextDatumCbor: Data.to(
          {
            fraud_prover: harness.proverSigner.paymentKeyHash,
            data: {
              ...state,
              machine_state_hash: segment.controlAfter.hashHex,
            },
          },
          SDK.NativeScriptDecodingStep03AdvanceOrCloseDatum,
        ),
        buildRedeemer: (layout) =>
          Data.to(
            {
              Continue: [
                {
                  input_index: layout.inputIndex,
                  output_index: layout.outputIndex,
                  control_cbor: evidence.control_cbor,
                  chunk_proof: evidence.chunk_proof,
                  next_chunk_proof: evidence.next_chunk_proof,
                  frames: [...evidence.frames],
                  step_budget: evidence.step_budget,
                },
              ],
            },
            SDK.NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer,
          ),
        referenceScriptUtxo: refs[4],
      });
    };
    const message = await expectOnchainRefusalV1(() =>
      rawScanWith(substituted),
    );
    expect(message.length).toBeGreaterThan(0);

    // The committed machine did not move.
    const after = await readStep03Thread(harness, bind.nextThreadOutRef);
    expect(after.state.machine_state_hash).toBe(segment.controlBefore.hashHex);

    // Control: the SAME raw transaction, differing only in the bytes the
    // chunk proof is built over, lands. The substituted byte is therefore the
    // whole of the refusal — the chunk proof no longer opens under the anchor
    // the bind froze.
    const controlTxHash = await rawScanWith(item);
    const advanced = await readStep03Thread(harness, `${controlTxHash}#0`);
    expect(advanced.state.machine_state_hash).toBe(
      segment.controlAfter.hashHex,
    );

    // And the raw Verdict builder the two direction attacks above are refused
    // through is proven LANDABLE here, on the honest direction-A verdict this
    // thread has genuinely earned. Its refusals over there are the
    // validator's, not an inability to assemble a Verdict at all.
    const verdictTxHash = await submitRawVerdict({
      harness,
      contracts: harness.decoding,
      signer: harness.proverSigner,
      threadOutRef: `${controlTxHash}#0`,
      controlCbor: plan.verdict.control!.cborHex,
      refusalClass: BigInt(plan.verdict.refusalClass!),
      window: plan.verdict.window,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: refs[4],
    });
    const classed = await harness.proverLucid.utxosByOutRef([
      { txHash: verdictTxHash, outputIndex: 0 },
    ]);
    expect(classed[0]!.address).toBe(
      harness.decoding.steps[5]!.spendingScriptAddress,
    );
    expect(
      Data.from(
        classed[0]!.datum!,
        SDK.NativeScriptDecodingStep03AdvanceOrCloseDatum,
      ).data!.refusal_class,
    ).toBe(0n);
  }, 600_000);

  it("refuses a step-02 that binds a forged forced leaf", async () => {
    const harness = await makeDecodingEmulatorHarnessV1();
    // Honest commitment: the operator ACCEPTED a forced transaction whose
    // payload decodes canonically. The adversary wants a direction-B thread,
    // which needs a rejection leaf the block simply does not contain.
    const item = decodingCanonicalItemV1();
    const scenario = await setupDecodingScenarioV1({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "forced", verdict: "ForcedTxValid" },
    });
    const refs = await publishDecodingReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.decoding,
    });
    const step02OutRef = await driveForcedThreadToStep02({
      harness,
      scenario,
      refs,
      direction: 1n,
    });

    // Offchain plane: the honest tooling reads the real leaf and stops.
    await expect(
      submitNativeScriptDecodingStep02({
        lucid: harness.proverLucid,
        contracts: harness.decoding,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: step02OutRef,
        reconstruction: scenario.block.reconstruction,
        forcedOrderKey: scenario.block.forcedOrderKey!,
        referenceScriptUtxo: refs[1],
      }),
    ).rejects.toThrow(
      /direction B disputes an explicit rejection, but the forced leaf's verdict is an acceptance/,
    );

    // On-chain plane: keep the block's honest membership PROOF, swap the
    // leaf VALUE for the rejection the adversary wishes had been committed.
    // The proof no longer opens under the committed forced-transactions root.
    const evidence = await buildNativeScriptDecodingStep02EvidenceV1({
      reconstruction: scenario.block.reconstruction,
      eventKey: {
        ForcedTransactionEventKey: {
          tx_order_id: scenario.block.forcedOrderKey!,
        },
      },
    });
    const honestLeaf = evidence.forcedMembership!;
    const forgedMembership = {
      ...honestLeaf,
      value: { ...honestLeaf.value, verdict: MALFORMED_REJECTION_VERDICT },
    };
    const scanState = SDK.nativeScriptDecodingPreBindScanStateV1({
      direction: 1n,
      sourceKind: SDK.NATIVE_SCRIPT_DECODING_SOURCE_KIND_FORCED_V1,
      verifiedTxId: honestLeaf.value.tx_id,
      txOrderId: Data.to(honestLeaf.key, SDK.OutputReference),
      scanReasonClass: 0n,
      priorLedgerRoot: evidence.transitionStepMembership.value.pre_utxos_root,
      outpointSourceKind: 1n,
      outpointCursor: 0n,
    });
    const { threadUtxo, threadToken } =
      await requireNativeScriptDecodingThreadUtxoV1({
        lucid: harness.proverLucid,
        contracts: harness.decoding,
        categoryId: harness.category.categoryId,
        stepIndex: 1,
        threadOutRef: step02OutRef,
      });
    const message = await expectOnchainRefusalV1(() =>
      submitRawDecodingStepV1({
        lucid: harness.proverLucid,
        contracts: harness.decoding,
        signer: harness.proverSigner,
        stepIndex: 1,
        threadUtxo,
        threadUnit: threadToken.unit,
        destinationAddress: harness.decoding.steps[2]!.spendingScriptAddress,
        nextDatumCbor: Data.to(
          {
            fraud_prover: harness.proverSigner.paymentKeyHash,
            data: scanState,
          },
          SDK.NativeScriptDecodingStep03OpenSubjectDatum,
        ),
        buildRedeemer: (layout) =>
          Data.to(
            {
              Continue: [
                {
                  input_index: layout.inputIndex,
                  output_index: layout.outputIndex,
                  header: scenario.block.reconstruction.header,
                  event_to_step_membership: evidence.eventToStepMembership,
                  transition_step_membership: evidence.transitionStepMembership,
                  forced_membership: forgedMembership,
                  chosen_outpoint_source_kind: 1n,
                  chosen_outpoint_cursor: 0n,
                },
              ],
            },
            SDK.NativeScriptDecodingStep02SpendRedeemer,
          ),
        referenceScriptUtxo: refs[1],
      }),
    );
    expect(message.length).toBeGreaterThan(0);

    // The thread never left step-02.
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.decoding.steps[2]!.spendingScriptAddress,
        threadToken.unit,
      ),
    ).resolves.toHaveLength(0);

    // Control: the same block and the same three membership proofs, with the
    // leaf value left alone, land. Note that this control varies TWO things
    // at once — the leaf value and the thread's recorded direction — so it
    // establishes only that the evidence assembly is sound. The on-chain
    // refusal above is attributable to step-02's committed-claim opening, but
    // to EITHER of two guards there: the substituted leaf failing to open
    // under the committed forced-transactions root, or the §2.4.3(e)
    // direction↔verdict guard refusing a direction-B thread over an
    // acceptance. Pinning which one rides the follow-up trace work; the
    // security property — an adversary cannot manufacture a rejection leaf
    // the block never committed — holds under either.
    await submitNativeScriptDecodingCancel({
      lucid: harness.proverLucid,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      contracts: harness.decoding,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step02OutRef,
      referenceScriptUtxo: refs[1],
    });
    const honestOutRef = await driveForcedThreadToStep02({
      harness,
      scenario,
      refs,
      direction: 0n,
    });
    const honest = await submitNativeScriptDecodingStep02({
      lucid: harness.proverLucid,
      contracts: harness.decoding,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: honestOutRef,
      reconstruction: scenario.block.reconstruction,
      forcedOrderKey: scenario.block.forcedOrderKey!,
      chosenOutpoint: { sourceKind: 1n, cursor: 0n },
      referenceScriptUtxo: refs[1],
    });
    expect(honest.scanState.direction).toBe(0n);
    expect(honest.scanState.prior_ledger_root).toBe(scenario.ledger.rootHex);
  }, 600_000);

  it("refuses a third party who drives or cancels another prover's thread", async () => {
    const harness = await makeDecodingEmulatorHarnessV1();
    const item = decodingMalformedMultiChunkItemV1();
    const scenario = await setupDecodingScenarioV1({
      harness,
      referenceScriptItemBytes: item,
      source: { kind: "normal" },
    });
    const plan = buildNativeScriptDecodingScanPlanV1({
      itemBytes: item,
      direction: 0,
    });
    await fundDecodingOutsiderV1(harness);
    const refs = await publishDecodingReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.decoding,
    });
    const step03OutRef = await driveNormalThreadToStep03({
      harness,
      scenario,
      refs,
    });
    const opened = await submitNativeScriptDecodingStep03OpenSubject({
      lucid: harness.proverLucid,
      contracts: harness.decoding,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step03OutRef,
      nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
      subjectFieldInputs: scenario.subjectFieldInputs,
      referenceScriptUtxo: refs[2],
    });
    const bind = await submitNativeScriptDecodingStep03BindDescriptor({
      lucid: harness.proverLucid,
      contracts: harness.decoding,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: opened.nextThreadOutRef,
      outpointKeyCbor: subjectOutpointKeyCbor(scenario),
      descriptorCbor: scenario.ledger.descriptorCbor,
      ledgerTrie: scenario.ledger.trie,
      plan,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: refs[3],
    });
    const { outsiderLucid, outsiderSigner: outsider } = harness;
    const segment = plan.segments[0]!;

    // Offchain plane, drive: the tooling refuses to sign for a thread it does
    // not own.
    await expect(
      submitNativeScriptDecodingStep03AdvanceOrCloseSegment({
        lucid: outsiderLucid,
        contracts: harness.decoding,
        categoryId: harness.category.categoryId,
        signer: outsider,
        threadOutRef: bind.nextThreadOutRef,
        segment,
        referenceScriptItemBytes: item,
        referenceScriptUtxo: refs[4],
      }),
    ).rejects.toThrow(/names fraud prover .*, not the signing wallet/);

    // Offchain plane, cancel.
    await expect(
      submitNativeScriptDecodingCancel({
        lucid: outsiderLucid,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        contracts: harness.decoding,
        categoryId: harness.category.categoryId,
        signer: outsider,
        threadOutRef: bind.nextThreadOutRef,
        referenceScriptUtxo: refs[4],
      }),
    ).rejects.toThrow(/only the prover can cancel/);

    // On-chain plane, drive: the theft an adversary would actually attempt is
    // re-datuming the thread to itself, which the shared `continue` helper
    // forbids — the fraud prover may not change during a transition.
    const { threadUtxo, threadUnit, state } = await readStep03Thread(
      harness,
      bind.nextThreadOutRef,
    );
    const evidence = nativeScriptDecodingScanArgsEvidenceV1({
      segment,
      fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
      itemIndex: Number(state.output_index),
      itemBytes: item,
    });
    const rawScanAs = (fraudProver: string) =>
      submitRawDecodingStepV1({
        lucid: outsiderLucid,
        contracts: harness.decoding,
        signer: outsider,
        stepIndex: 4,
        threadUtxo,
        threadUnit,
        destinationAddress: harness.decoding.steps[4]!.spendingScriptAddress,
        nextDatumCbor: Data.to(
          {
            fraud_prover: fraudProver,
            data: {
              ...state,
              machine_state_hash: segment.controlAfter.hashHex,
            },
          },
          SDK.NativeScriptDecodingStep03AdvanceOrCloseDatum,
        ),
        buildRedeemer: (layout) =>
          Data.to(
            {
              Continue: [
                {
                  input_index: layout.inputIndex,
                  output_index: layout.outputIndex,
                  control_cbor: evidence.control_cbor,
                  chunk_proof: evidence.chunk_proof,
                  next_chunk_proof: evidence.next_chunk_proof,
                  frames: [...evidence.frames],
                  step_budget: evidence.step_budget,
                },
              ],
            },
            SDK.NativeScriptDecodingStep03AdvanceOrCloseSpendRedeemer,
          ),
        referenceScriptUtxo: refs[4],
      });
    // The theft: the continuation names the outsider.
    const driveMessage = await expectOnchainRefusalV1(() =>
      rawScanAs(outsider.paymentKeyHash),
    );
    expect(driveMessage.length).toBeGreaterThan(0);

    // On-chain plane, cancel: the validator demands the named prover's
    // signature, which the outsider cannot produce.
    const cancelMessage = await expectOnchainRefusalV1(() =>
      submitRawDecodingCancelV1({
        lucid: outsiderLucid,
        contracts: harness.decoding,
        signer: outsider,
        stepIndex: 4,
        threadUtxo,
        threadUnit,
        threadAssetName: `${harness.category.categoryId}${scenario.setup.headerHash}`,
        referenceScriptUtxo: refs[4],
        computationThreadReferenceUtxo:
          harness.witnessReferenceScripts.computationThreadMint!,
      }),
    );
    expect(cancelMessage.length).toBeGreaterThan(0);

    // The honest prover's thread is untouched by either attempt.
    const after = await readStep03Thread(harness, bind.nextThreadOutRef);
    expect(after.state.machine_state_hash).toBe(segment.controlBefore.hashHex);

    // Control, isolating the refusals to the two checks named above. The same
    // outsider-built, outsider-signed Scan lands the moment the continuation
    // keeps naming the HONEST prover: paying somebody else's thread forward
    // is permitted, taking it over is not. So the drive refusal is the shared
    // `continue` helper's "fraud prover must not change", and the cancel
    // refusal is the step's own demand for the named prover's signature —
    // neither is the outsider merely being unable to build a transaction.
    const controlTxHash = await rawScanAs(harness.proverSigner.paymentKeyHash);
    const advanced = await readStep03Thread(harness, `${controlTxHash}#0`);
    expect(advanced.state.machine_state_hash).toBe(
      segment.controlAfter.hashHex,
    );
    expect(
      Data.from(
        advanced.threadUtxo.datum!,
        SDK.NativeScriptDecodingStep03AdvanceOrCloseDatum,
      ).fraud_prover,
    ).toBe(harness.proverSigner.paymentKeyHash);
  }, 600_000);

  it("refuses a descriptor-contradiction close over a tag-0 descriptor", async () => {
    const harness = await makeDecodingEmulatorHarnessV1();
    // The honest commitment: the operator REJECTED a forced transaction over
    // a malformed payload and classed it correctly. The descriptor the ledger
    // holds for the accused outpoint is an ordinary TAG-0 (native) one.
    const item = decodingMalformedMultiChunkItemV1();
    const scenario = await setupDecodingScenarioV1({
      harness,
      referenceScriptItemBytes: item,
      referenceScriptLanguage: 0,
      source: { kind: "forced", verdict: MALFORMED_REJECTION_VERDICT },
    });
    const refs = await publishDecodingReferenceScriptsV1({
      lucid: harness.funderLucid,
      contracts: harness.decoding,
    });
    const step02OutRef = await driveForcedThreadToStep02({
      harness,
      scenario,
      refs,
      direction: 1n,
    });
    const step02 = await submitNativeScriptDecodingStep02({
      lucid: harness.proverLucid,
      contracts: harness.decoding,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step02OutRef,
      reconstruction: scenario.block.reconstruction,
      forcedOrderKey: scenario.block.forcedOrderKey!,
      referenceScriptUtxo: refs[1],
    });
    const opened = await submitNativeScriptDecodingStep03OpenSubject({
      lucid: harness.proverLucid,
      contracts: harness.decoding,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step02.nextThreadOutRef,
      nativeTxCompactCbor: scenario.block.nativeTxCompactCbor,
      subjectFieldInputs: scenario.subjectFieldInputs,
      referenceScriptUtxo: refs[2],
    });

    // The close the adversary wants: BindDescriptor shaped as the direction-B
    // descriptor contradiction — straight to step-04, class-malformed, no
    // machine ever started — over a descriptor that names no contradiction at
    // all. Firing it here would convict the operator for a payload the
    // canonical decoder reads perfectly well.
    //
    // Offchain plane: the submitter cross-checks the plan's route against the
    // language tag it just bound.
    const contradictionPlan = buildNativeScriptDecodingScanPlanV1({
      itemBytes: decodingPlutusItemV1(),
      direction: 1,
    });
    expect(contradictionPlan.route).toBe(
      NativeScriptDecodingPlanRoutesV1.DescriptorContradiction,
    );
    await expect(
      submitNativeScriptDecodingStep03BindDescriptor({
        lucid: harness.proverLucid,
        contracts: harness.decoding,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: opened.nextThreadOutRef,
        outpointKeyCbor: subjectOutpointKeyCbor(scenario),
        descriptorCbor: scenario.ledger.descriptorCbor,
        ledgerTrie: scenario.ledger.trie,
        plan: contradictionPlan,
        referenceScriptItemBytes: item,
        referenceScriptUtxo: refs[3],
      }),
    ).rejects.toThrow(
      /the plan claims a descriptor contradiction for a tag-0 descriptor/u,
    );

    // On-chain plane: the same close, assembled by hand with the AUTHENTIC
    // tag-0 descriptor and the block's own membership proof, has to die in
    // the validator's language branch.
    const openedThread = await readStep03Thread(
      harness,
      opened.nextThreadOutRef,
      3,
    );
    const outpointKeyCbor = subjectOutpointKeyCbor(scenario);
    const descriptor = decodeMidgardLedgerOutputCommitmentV1(
      Buffer.from(scenario.ledger.descriptorCbor, "hex"),
    );
    const membershipProof = await buildNativeScriptDecodingLedgerMembershipV1({
      trie: scenario.ledger.trie,
      outpointKey: Buffer.from(outpointKeyCbor, "hex"),
      priorLedgerRootHex: openedThread.state.prior_ledger_root,
    });
    const boundState = SDK.nativeScriptDecodingBoundDescriptorStateV1({
      state: openedThread.state,
      referenceScriptLanguage: BigInt(descriptor.referenceScriptLanguage),
      referenceScriptTotalLength: BigInt(descriptor.referenceScriptTotalLength),
      referenceScriptItemCommitment:
        descriptor.referenceScriptItemCommitment.toString("hex"),
    });
    await expectOnchainRefusalV1(() =>
      submitRawDecodingStepV1({
        lucid: harness.proverLucid,
        contracts: harness.decoding,
        signer: harness.proverSigner,
        stepIndex: 3,
        threadUtxo: openedThread.threadUtxo,
        threadUnit: openedThread.threadUnit,
        destinationAddress: harness.decoding.steps[5]!.spendingScriptAddress,
        nextDatumCbor: Data.to(
          {
            fraud_prover: harness.proverSigner.paymentKeyHash,
            data: {
              ...boundState,
              refusal_class:
                SDK.NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED_V1,
            },
          },
          SDK.NativeScriptDecodingStep03BindDescriptorDatum,
        ),
        buildRedeemer: (layout) =>
          Data.to(
            {
              Continue: [
                {
                  input_index: layout.inputIndex,
                  output_index: layout.outputIndex,
                  outpoint_key_cbor: outpointKeyCbor,
                  descriptor_cbor: scenario.ledger.descriptorCbor,
                  ledger_membership_proof: membershipProof,
                  first_chunk_proof: buildNativeScriptDecodingChunkProofV1({
                    fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
                    itemIndex: descriptor.outputIndex,
                    itemBytes: item,
                    chunkIndex: 0,
                  }),
                },
              ],
            },
            SDK.NativeScriptDecodingStep03BindDescriptorSpendRedeemer,
          ),
        referenceScriptUtxo: refs[3],
      }),
    );
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.decoding.steps[5]!.spendingScriptAddress,
        toUnit(
          harness.decoding.computationThread.policyId,
          `${harness.category.categoryId}${scenario.setup.headerHash}`,
        ),
      ),
    ).resolves.toHaveLength(0);

    // Control: every piece of evidence that close carried — the §8.8 field
    // opening, the ledger membership proof, the descriptor — is accepted the
    // moment the SHAPE is right. The same thread binds into the machine, so
    // the refusal above is the close shape and nothing else. (The mirror
    // image, a contradiction close that LANDS over a genuinely non-tag-0
    // descriptor, is the honest lifecycle in
    // submit-init-emulator-native-script-decoding-direction-b.test.ts.)
    const machinePlan = buildNativeScriptDecodingScanPlanV1({
      itemBytes: item,
      direction: 0,
    });
    const bound = await submitNativeScriptDecodingStep03BindDescriptor({
      lucid: harness.proverLucid,
      contracts: harness.decoding,
      categoryId: harness.category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: opened.nextThreadOutRef,
      outpointKeyCbor,
      descriptorCbor: scenario.ledger.descriptorCbor,
      ledgerTrie: scenario.ledger.trie,
      plan: machinePlan,
      referenceScriptItemBytes: item,
      referenceScriptUtxo: refs[3],
    });
    expect(bound.scanState.machine_state_hash).toBe(
      machinePlan.segments[0]!.controlBefore.hashHex,
    );
    expect(bound.scanState.refusal_class).toBe(
      SDK.NATIVE_SCRIPT_DECODING_CLASS_PENDING_V1,
    );
  }, 600_000);
});
