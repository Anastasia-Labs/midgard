import { MIDGARD_LEDGER_OUTPUT_FIELD_INDEX } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import {
  type NativeScriptDecodingContracts,
  type NativeScriptDecodingScanPlan,
  type NativeScriptDecodingVerdictPlan,
  nativeScriptDecodingWindowProofs,
  requireNativeScriptDecodingThreadUtxo,
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
  type DecodingScenario,
  makeDecodingEmulatorHarness,
  submitRawDecodingStep,
} from "./support/native-script-decoding-emulator.js";
import { network } from "./support/submit-init-emulator-shared.js";

type DecodingHarness = Awaited<ReturnType<typeof makeDecodingEmulatorHarness>>;

type DecodingReferenceScripts = readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];

/** The §2.4.3(e) rejection direction B disputes, accusing (reference, 0). */
export const MALFORMED_REJECTION_VERDICT: SDK.OperatorVerdict = {
  ForcedTxInvalid: {
    reason: {
      ResolvedReferenceScriptMalformed: { source_kind: 1n, input_index: 0n },
    },
  },
};

/** Locates the thread and decodes its step-03 scan state. */
export const readStep03Thread = async (
  harness: DecodingHarness,
  threadOutRef: string,
  stepIndex: 2 | 3 | 4 = 4,
): Promise<{
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly state: SDK.NativeScriptDecodingScanThreadState;
}> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxo({
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

export const subjectOutpointKeyCbor = (scenario: DecodingScenario): string =>
  Buffer.from(
    SDK.encodeMidgardTxInputCanonical(scenario.subjectFieldInputs[0]!),
  ).toString("hex");

/** Init → step-01 (normal) → step-02: the honest prefix every attack shares. */
export const driveNormalThreadToStep03 = async ({
  harness,
  scenario,
  refs,
}: {
  readonly harness: DecodingHarness;
  readonly scenario: DecodingScenario;
  readonly refs: DecodingReferenceScripts;
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
export const driveForcedThreadToStep02 = async ({
  harness,
  scenario,
  refs,
  direction,
}: {
  readonly harness: DecodingHarness;
  readonly scenario: DecodingScenario;
  readonly refs: DecodingReferenceScripts;
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
export const bindAndScanHonestly = async ({
  harness,
  scenario,
  refs,
  plan,
  item,
  threadOutRef,
}: {
  readonly harness: DecodingHarness;
  readonly scenario: DecodingScenario;
  readonly refs: DecodingReferenceScripts;
  readonly plan: NativeScriptDecodingScanPlan;
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
      SDK.encodeMidgardTxInputCanonical(subjectOutpoint),
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
export const submitRawVerdict = async ({
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
  readonly harness: DecodingHarness;
  readonly contracts: NativeScriptDecodingContracts;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly controlCbor: string;
  readonly refusalClass: bigint;
  /** Direction A's refusing step reads a chunk window; direction B reads none. */
  readonly window?: NativeScriptDecodingVerdictPlan["window"];
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
      : nativeScriptDecodingWindowProofs({
          window,
          fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
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
  return submitRawDecodingStep({
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
