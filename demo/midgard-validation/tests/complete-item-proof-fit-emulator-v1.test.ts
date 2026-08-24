import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  hashMidgardValidationMachineStateV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
} from "@al-ft/midgard-core";
import {
  encodeMidgardFieldPreimageV1,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
} from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  AuthenticatedCanonicalDecodeItemDatumV1,
  buildUnsignedValidationProofItemPublicationV1Program,
  buildValidationTraceDisputeFaultProofContracts,
  deriveValidationProofItemPublicationV1,
  minimumLovelaceForValidationProofItemPublicationV1,
  ObservedCanonicalDecodeItemDatumV1,
  parseFaultProofBlueprint,
  PreparedCanonicalDecodeItemDatumV1,
  PreparedValidationResolutionDatumV1,
  type PreparedValidationResolutionDatumV1 as PreparedValidationResolutionDatumV1Data,
  requireInputIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  validationMachineStateDataFromCore,
  ValidationOneStepWitnessV1,
  type ValidationOneStepWitnessV1 as ValidationOneStepWitnessV1Data,
  ValidationProofItemDatumV1,
  type ValidationProofItemPublicationV1,
  type ValidationTraceDisputeFaultProofContracts,
  VerifiedCanonicalDecodeItemDatumV1,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  CML,
  Constr,
  credentialToAddress,
  Data,
  Emulator,
  Lucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  deriveCanonicalDecodeItemStageDataV1,
  validationOneStepEvidenceHashV1,
} from "../../midgard-fault-proofs/src/validation-dispute/submit.js";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOpV1,
  buildValidationMachineLedgerMutationSteps,
  buildValidationOneStepArgumentV1,
  type DeterministicValidationMachineTrace,
  type ValidationOneStepArgumentV1,
} from "../src/index.js";
import {
  fundingLovelaceForOutputsV1,
  makeMinAdaFundedExactSizeOutputItemV1,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
} from "./validation-fixtures.js";

const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");
const blueprintJson = JSON.parse(readFileSync(blueprintPath, "utf8"));

const HUB_ORACLE_POLICY_ID = "11".repeat(28);
const FRAUD_PROOF_CATALOGUE_POLICY_ID = "22".repeat(28);
const THREAD_ASSET_NAME = "aa".repeat(32);

const SIGNING_KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 7));
const SIGNER_HASH = Buffer.from(
  SIGNING_KEY.to_public().hash().to_raw_bytes(),
).toString("hex");

// §3.3 execution reserve: at or below the compiled protocol floors with a
// 20% reserve (docs/consensus-profile-v1.md §10).
const RESERVED_MEMORY_UNITS = Math.floor(
  MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits * 0.8,
);
const RESERVED_CPU_UNITS = Math.floor(
  MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxCpuUnits * 0.8,
);
const MAX_L1_PROOF_TX_BYTES =
  MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes;

/**
 * RE-AUTHORED, NOT SUPPRESSED (#618 ruling 1; R8 of decision 0005). This file
 * used to carry its own copy of the exact-size item builder, producing
 * 10-lovelace items that the ValueAndMint output-descriptor scan now convicts
 * with `E_MIN_ADA`. The shared builder funds each item at its own minimum-Ada
 * floor without moving its length, so every carriage measurement below
 * measures the same number of bytes it did before the wiring.
 */
const makeExactSizeOutputItem = makeMinAdaFundedExactSizeOutputItemV1;

/** The §5.1 outputs field, which is the field every case here carries. */
const OUTPUT_FIELD_INDEX = 2;

/**
 * `NoAuxiliaryWitness` as the committed evidence names it — the Option B
 * (#620) auxiliary half of the canonical-decode resolver's `evidence_hash`.
 * Same literal as `complete-item-route-adversarial-emulator-v1.test.ts` and
 * `complete-item-carriage-tiers-emulator-v1.test.ts`.
 */
const NO_AUXILIARY_WITNESS_CBOR = Buffer.from("d87980", "hex");

/**
 * The largest complete item this **tier-1** harness can carry.
 *
 * §8.4 partitions on the field's §5.1 preimage, and a single-item field-2
 * envelope is `81 ‖ 59 LLLL ‖ item` — four bytes — so the tier-1 ceiling of
 * `MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1` (14,336) admits an item of at
 * most 14,332 bytes. Anything larger resolves as tier-2 `RawUtxo`, which
 * `buildCanonicalDecodeItemCase` refuses by design.
 *
 * **#580 NOTE — the 64-byte overhang.** The applied publication cap
 * `MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes` is 14,396,
 * which is 64 bytes ABOVE this ceiling: items in (14,332, 14,396] are publishable
 * but cannot be carried inline. Before 2026-08-14 this suite hid that, because it
 * selected its complete-item witness by `(phase, kind)` alone and so measured
 * field 0's few-dozen-byte preimage while claiming to run at the cap. The
 * publication-maximum row moved to
 * `complete-item-carriage-tiers-emulator-v1.test.ts` (tier-2) under the same
 * owner ruling, and **#580 owns re-measuring the overhang**. Retargeting this
 * harness does not resolve it.
 */
/**
 * The four bytes a single-item field-2 §5.1 envelope costs: `81 ‖ 59 LLLL`.
 * Named so the derivation below states the arithmetic instead of restating its
 * result — 14,332 is not an independent measurement, it is the tier-1 ceiling
 * minus this envelope, and a change to that ceiling must move it.
 */
const SINGLE_ITEM_FIELD_ENVELOPE_BYTES = 4;

const TIER1_MAX_COMPLETE_ITEM_BYTES =
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1 -
  SINGLE_ITEM_FIELD_ENVELOPE_BYTES;

const traceContext = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
  eventKeyCbor: Buffer.from("d8799f4100ff", "hex"),
  sourceKind: "normal" as const,
  blockEndTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};

const buildTraceWithOutputs = async (
  outputs: readonly Buffer[],
): Promise<DeterministicValidationMachineTrace> => {
  const spent = outRefFromByte(0x11);
  // The resolved input has to fund every produced output now that each is
  // funded at its own minimum-Ada floor, or stage five would convict this
  // trace with `E_VALUE_NOT_PRESERVED` instead of accepting it. The fee is
  // zero, so the sum is exact.
  const spentOutput = makeOutput(fundingLovelaceForOutputsV1(outputs));
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    outputs,
  });
  const expectedLedgerOps = [
    { type: "delete" as const, key: spent },
    ...outputs.map((output, index) =>
      buildValidationMachineLedgerInsertOpV1({
        key: outRefFromTxId(transaction.txId, BigInt(index)),
        outputCbor: output,
      }),
    ),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output: spentOutput }],
    operations: expectedLedgerOps,
  });
  return Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      ...traceContext,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
      postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
      ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: "accepted",
      expectedRejectionCode: null,
    }),
  );
};

type CanonicalDecodeItemCase = {
  readonly trace: DeterministicValidationMachineTrace;
  readonly stateIndex: number;
  readonly itemBytes: number;
  readonly argument: ValidationOneStepArgumentV1;
  readonly transitionData: Data;
  /**
   * #597. `TransactionFieldItemWitness` carries a `FieldCarriageV1` now, and the
   * producer emits tier-1 `Inline`, so the whole wire surface is the field's
   * §5.1 preimage. `carriageData` is that carriage as the redeemer names it;
   * `fieldPreimageHex` is the bytes inside it, which is what a publication
   * holds.
   */
  readonly carriageData: Data;
  readonly fieldPreimageHex: string;
  readonly evidenceHash: string;
  readonly preState: ReturnType<typeof validationMachineStateDataFromCore>;
  readonly claimedSuccessorHash: string;
  /**
   * The four staged datums the chain hands on, derived by
   * `deriveCanonicalDecodeItemStageDataV1` — the same producer
   * `submitValidationDisputeSemanticResolution` uses. Nothing in this file
   * hand-builds a stage datum any more: post-Option-B the observe stage is
   * the size-bearing one, and its datum is not a local restatement of the
   * authenticate stage's.
   */
  readonly preparedThreadDatum: string;
  readonly authenticatedDatum: string;
  readonly preparedDatum: string;
  readonly observedDatum: string;
  readonly verifiedDatum: string;
};

const buildCanonicalDecodeItemCase = async (
  itemBytes: number,
): Promise<CanonicalDecodeItemCase> => {
  const item = makeExactSizeOutputItem(itemBytes);
  const trace = await buildTraceWithOutputs([item]);
  const expectedPreimageBytes = encodeMidgardFieldPreimageV1([item]).length;
  let stateIndex = -1;
  for (let index = 0; index < trace.witnesses.length; index += 1) {
    const witness = trace.witnesses[index]!;
    // This harness runs entirely inside §8.3's tier-1 domain — its probes are
    // the measured publication frontiers, all below the 14,336-byte cap — so the
    // default resolver's `Inline` is the carriage every case here uses (#600).
    //
    // #579: selecting on `(phase, kind)` alone is NOT enough, and the difference
    // is not cosmetic. The canonicalDecode walk emits a complete-item witness
    // for field 0 first, whose preimage is a few dozen bytes, so a first-match
    // selector silently returns field 0 and every measurement below becomes a
    // measurement of the wrong field — `itemBytes` then bears no relation to the
    // bytes actually carried. That is what made the publication-maximum row
    // measure ~363 signed bytes and made the substitution row write past the end
    // of a ~40-byte buffer, i.e. mutate nothing at all. Locate the step by the
    // field it opened AND the bytes it read, the discipline
    // `complete-item-carriage-tiers-emulator-v1.test.ts` and
    // `complete-item-proof-fit-v1.test.ts` already keep.
    if (
      witness.phase === "canonicalDecode" &&
      witness.auxiliary?.kind === "transactionFieldItem" &&
      witness.auxiliary.fieldIndex === OUTPUT_FIELD_INDEX &&
      witness.auxiliary.fieldPreimage.length === expectedPreimageBytes
    ) {
      stateIndex = index;
      break;
    }
  }
  if (stateIndex < 0) {
    throw new Error(
      `trace has no canonicalDecode field-${OUTPUT_FIELD_INDEX.toString()} complete-item witness of ${expectedPreimageBytes.toString()} preimage bytes`,
    );
  }
  const argument = buildValidationOneStepArgumentV1({ trace, stateIndex });
  if (argument.resolverIndex !== 0 || argument.semanticResolverIndex !== 1) {
    throw new Error("complete-item case selected an unexpected resolver");
  }
  const auxiliary = Data.from(argument.auxiliaryCbor.toString("hex"));
  if (
    !(auxiliary instanceof Constr) ||
    auxiliary.index !== 30 ||
    auxiliary.fields.length !== 1
  ) {
    throw new Error("complete-item auxiliary witness has an unexpected shape");
  }
  const carriageData = auxiliary.fields[0]!;
  if (
    !(carriageData instanceof Constr) ||
    carriageData.index !== 0 ||
    carriageData.fields.length !== 1 ||
    typeof carriageData.fields[0] !== "string"
  ) {
    throw new Error("complete-item carriage is not tier-1 Inline");
  }
  const fieldPreimageHex = carriageData.fields[0];
  // Option B (#620): the canonical-decode resolver commits to the transition
  // ALONE — `NoAuxiliaryWitness` is the auxiliary half of `evidence_hash`,
  // whatever carriage the auxiliary witness names, because the carriage is
  // dereferenced and content-checked only at the observe stage's §8.8 door.
  const evidenceHash = validationOneStepEvidenceHashV1({
    transitionCbor: argument.transitionCbor,
    auxiliaryCbor: NO_AUXILIARY_WITNESS_CBOR,
  });
  const preState = validationMachineStateDataFromCore(
    trace.states[stateIndex]!,
  );
  const claimedSuccessorHash = hashMidgardValidationMachineStateV1(
    trace.states[stateIndex + 1]!,
  ).toString("hex");
  const preparedThreadDatum = Data.to(
    {
      fraud_prover: SIGNER_HASH,
      data: {
        version: 1n,
        resolution: {
          version: 1n,
          pre_state: preState,
          operator_successor_hash: claimedSuccessorHash,
          challenger_successor_hash: claimedSuccessorHash,
        },
        evidence_hash: evidenceHash,
      },
    },
    PreparedValidationResolutionDatumV1,
  );
  const preparedResolution = (
    Data.from(
      preparedThreadDatum,
      PreparedValidationResolutionDatumV1,
    ) as PreparedValidationResolutionDatumV1Data
  ).data;
  if (preparedResolution === null) {
    throw new Error("prepared thread datum is missing its state");
  }
  const stageData = deriveCanonicalDecodeItemStageDataV1({
    preparedResolution,
    transition: Data.from(
      argument.transitionCbor.toString("hex"),
      ValidationOneStepWitnessV1,
    ) as ValidationOneStepWitnessV1Data,
    fieldPreimage: fieldPreimageHex,
  });
  return {
    trace,
    stateIndex,
    itemBytes,
    argument,
    transitionData: Data.from(argument.transitionCbor.toString("hex")),
    carriageData,
    fieldPreimageHex,
    evidenceHash,
    preState,
    claimedSuccessorHash,
    preparedThreadDatum,
    authenticatedDatum: Data.to(
      { fraud_prover: SIGNER_HASH, data: stageData.authenticated },
      AuthenticatedCanonicalDecodeItemDatumV1,
    ),
    preparedDatum: Data.to(
      { fraud_prover: SIGNER_HASH, data: stageData.prepared },
      PreparedCanonicalDecodeItemDatumV1,
    ),
    observedDatum: Data.to(
      { fraud_prover: SIGNER_HASH, data: stageData.observed },
      ObservedCanonicalDecodeItemDatumV1,
    ),
    verifiedDatum: Data.to(
      { fraud_prover: SIGNER_HASH, data: stageData.verified },
      VerifiedCanonicalDecodeItemDatumV1,
    ),
  };
};

type CompleteSignedTransactionMeasurement = {
  readonly completeSignedBytes: number;
  readonly l1ByteMargin: number;
  readonly fee: bigint;
  readonly executionMemory: bigint;
  readonly executionSteps: bigint;
  readonly inputCount: number;
  readonly referenceInputCount: number;
  readonly outputCount: number;
  readonly redeemerCount: number;
};

/**
 * CML normalizes Plutus datums (definite-length arrays) when it frames the
 * transaction, while lucid's `Data.to` emits Aiken-style indefinite arrays.
 * The deployed validators compare parsed Data values, so datum equality here
 * must also be value-level.
 */
const sameDatumValue = (left: string, right: string): boolean =>
  left === right || Data.to(Data.from(left)) === Data.to(Data.from(right));

const measureCompleteSignedTransaction = (
  transactionCbor: string,
): CompleteSignedTransactionMeasurement => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  const body = transaction.body();
  const redeemers = transaction
    .witness_set()
    .redeemers()
    ?.as_arr_legacy_redeemer();
  let executionMemory = 0n;
  let executionSteps = 0n;
  let redeemerCount = 0;
  if (redeemers !== undefined) {
    redeemerCount = redeemers.len();
    for (let index = 0; index < redeemers.len(); index += 1) {
      const units = redeemers.get(index).ex_units();
      executionMemory += units.mem();
      executionSteps += units.steps();
    }
  }
  const completeSignedBytes = transactionCbor.length / 2;
  return {
    completeSignedBytes,
    l1ByteMargin: MAX_L1_PROOF_TX_BYTES - completeSignedBytes,
    fee: body.fee(),
    executionMemory,
    executionSteps,
    inputCount: body.inputs().len(),
    referenceInputCount: body.reference_inputs()?.len() ?? 0,
    outputCount: body.outputs().len(),
    redeemerCount,
  };
};

type StageContract = {
  readonly spendingScriptAddress: string;
  readonly spendingScript: Script;
};

type EmulatorHarness = {
  readonly lucid: LucidEvolution;
  readonly emulator: Emulator;
  readonly contracts: ValidationTraceDisputeFaultProofContracts;
  readonly signerHash: string;
  readonly threadUnit: string;
  readonly semanticAddress: string;
  readonly itemSourceAddress: string;
  readonly semanticScript: Script;
  readonly threadUtxos: readonly UTxO[];
  /** The four staged contracts the complete-item chain walks. */
  readonly stages: ValidationTraceDisputeFaultProofContracts["validationTraceDispute"]["canonicalDecodeItemStages"];
};

let cachedContracts: ValidationTraceDisputeFaultProofContracts | undefined;
const loadContracts =
  async (): Promise<ValidationTraceDisputeFaultProofContracts> => {
    cachedContracts ??= await Effect.runPromise(
      buildValidationTraceDisputeFaultProofContracts({
        blueprint: parseFaultProofBlueprint(blueprintJson),
        network: "Custom",
        hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
        fraudProofCataloguePolicyId: FRAUD_PROOF_CATALOGUE_POLICY_ID,
      }),
    );
    return cachedContracts;
  };

const setupEmulator = async (
  threadDatums: readonly string[],
): Promise<EmulatorHarness> => {
  const contracts = await loadContracts();
  const semanticContract =
    contracts.validationTraceDispute.semanticResolvers[1];
  if (semanticContract === undefined) {
    throw new Error("canonical-decode item semantic resolver is missing");
  }
  const itemSourceAddress =
    contracts.validationTraceDispute.canonicalDecodeItemStages.source
      .spendingScriptAddress;
  const signingKey = SIGNING_KEY;
  const signerHash = SIGNER_HASH;
  const walletAddress = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(signingKey.to_public().hash()),
  )
    .to_address()
    .to_bech32();
  const threadUnit = toUnit(
    contracts.computationThread.policyId,
    THREAD_ASSET_NAME,
  );
  const emulator = new Emulator(
    [
      {
        seedPhrase: "",
        privateKey: signingKey.to_bech32(),
        address: walletAddress,
        assets: { lovelace: 100_000_000_000n },
      },
      ...threadDatums.map((datum) => ({
        seedPhrase: "",
        privateKey: "",
        address: semanticContract.spendingScriptAddress,
        assets: { lovelace: 60_000_000n, [threadUnit]: 1n },
        outputData: { inline: datum },
      })),
    ],
    { ...PROTOCOL_PARAMETERS_DEFAULT, maxTxSize: 65_536 },
  );
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromPrivateKey(signingKey.to_bech32());

  const threadUtxos = (
    await lucid.utxosAt(semanticContract.spendingScriptAddress)
  ).sort((left, right) => left.outputIndex - right.outputIndex);
  if (threadUtxos.length !== threadDatums.length) {
    throw new Error("emulator thread seeding mismatch");
  }
  return {
    lucid,
    emulator,
    contracts,
    signerHash,
    threadUnit,
    semanticAddress: semanticContract.spendingScriptAddress,
    itemSourceAddress,
    semanticScript: semanticContract.spendingScript,
    threadUtxos,
    stages: contracts.validationTraceDispute.canonicalDecodeItemStages,
  };
};

// FLIPPED TO THE OPTION B PIPELINE (#617 wave sign-off, item 1; #620/#621/#622).
// This harness used to speak the wire #620 retired — the four-field `Verify`,
// the `VerifyReference` arm, and the two-part evidence hash — and to measure
// the AUTHENTICATE stage, because that was the stage the §5.1 preimage rode.
// Since Option B the preimage rides the observe stage's §8.8 door, `Verify` is
// `(input_index, output_index, transition)`, the semantic reference arm is
// gone, and `evidence_hash` commits to `(transition, NoAuxiliaryWitness)`. So
// the size-bearing rows below measure the OBSERVE transaction, and the harness
// walks the real staged chain (authenticate -> source -> observe) to reach it
// rather than stopping at the first stage.
//
// Framing note (#622 caveat 1, carried deliberately): the byte counts this
// harness measures are its OWN framing's, not the production journey's. The
// consensus-profile rows are pinned from the production journey
// (`demo/midgard-fault-proofs/tests/submit-init-emulator-option-b-*.test.ts`);
// every assertion here is therefore a RELATION (fits / does not fit / smaller
// than) against those pins, never an equality restating them.

type StageSubmission = {
  readonly nextThreadUtxo: UTxO;
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly outputDatum: string;
  readonly signedCbor: string;
};

const feeInputFor = async (harness: EmulatorHarness): Promise<UTxO> => {
  const candidates = (await harness.lucid.wallet().getUtxos()).filter(
    (utxo) => utxo.assets[harness.threadUnit] === undefined,
  );
  return candidates.reduce((left, right) =>
    (left.assets.lovelace ?? 0n) >= (right.assets.lovelace ?? 0n)
      ? left
      : right,
  );
};

/**
 * Publishes a validator as a plain reference script, parked at a salted script
 * address so several parked scripts stay individually addressable and none of
 * them is reachable by coin selection. The parking transaction rides the raised
 * emulator ceiling and is not part of any measurement.
 */
const publishReferenceScript = async (
  harness: EmulatorHarness,
  script: Script,
  salt: string,
): Promise<UTxO> => {
  const parkAddress = credentialToAddress(
    "Custom",
    scriptHashToCredential(salt.repeat(28)),
  );
  const unsigned = await harness.lucid
    .newTx()
    .pay.ToAddressWithData(
      parkAddress,
      undefined,
      { lovelace: 60_000_000n },
      script,
    )
    .complete();
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await harness.lucid.awaitTx(txHash);
  const utxo = (await harness.lucid.utxosAt(parkAddress)).find(
    (candidate) => candidate.txHash === txHash && candidate.scriptRef != null,
  );
  if (utxo === undefined) {
    throw new Error("reference script failed to park");
  }
  return utxo;
};

/**
 * One stage of the staged canonical-decode chain, built, signed and submitted
 * against the applied validators. The thread token and its lovelace are handed
 * on unchanged, exactly as the production submitter hands them on.
 */
const submitStage = async ({
  harness,
  inputUtxo,
  inputContract,
  outputContract,
  outputDatum,
  label,
  encode,
  scriptReference,
  extraReferences,
}: {
  readonly harness: EmulatorHarness;
  readonly inputUtxo: UTxO;
  readonly inputContract: StageContract;
  readonly outputContract: StageContract;
  readonly outputDatum: string;
  readonly label: string;
  readonly encode: (layout: {
    readonly inputIndex: bigint;
    readonly outputIndex: bigint;
    readonly referenceInputIndex: (utxo: UTxO) => bigint;
  }) => string;
  readonly scriptReference?: UTxO;
  readonly extraReferences?: readonly UTxO[];
}): Promise<StageSubmission> => {
  const makeRedeemer: BuildTxWithRedeemer = (ctx) =>
    encode({
      inputIndex: requireInputIndex(ctx, inputUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        (output) =>
          output.address === outputContract.spendingScriptAddress &&
          output.datum != null &&
          sameDatumValue(output.datum, outputDatum) &&
          output.assets[harness.threadUnit] === 1n,
        label,
      ),
      referenceInputIndex: (utxo) =>
        requireReferenceInputIndex(ctx, utxo, label),
    });
  let tx = harness.lucid
    .newTx()
    .collectFrom([await feeInputFor(harness)])
    .collectFrom([inputUtxo], makeRedeemer);
  if (scriptReference !== undefined) {
    tx = tx.readFrom([scriptReference]);
  }
  if (extraReferences !== undefined && extraReferences.length > 0) {
    tx = tx.readFrom([...extraReferences]);
  }
  tx = tx.pay
    .ToContract(
      outputContract.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      {
        lovelace: inputUtxo.assets.lovelace ?? 0n,
        [harness.threadUnit]: 1n,
      },
    )
    .addSignerKey(harness.signerHash);
  if (scriptReference === undefined) {
    tx = tx.attach.SpendingValidator(inputContract.spendingScript);
  }
  let unsigned;
  try {
    unsigned = await tx.complete({ localUPLCEval: true });
  } catch (cause) {
    throw new Error(
      `${label} local evaluation failed: ${
        cause instanceof Error ? cause.message : String(cause)
      }`,
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const txHash = await signed.submit();
  await harness.lucid.awaitTx(txHash);
  const nextThreadUtxo = (
    await harness.lucid.utxosAt(outputContract.spendingScriptAddress)
  ).find(
    (utxo) => utxo.txHash === txHash && utxo.assets[harness.threadUnit] === 1n,
  );
  if (nextThreadUtxo === undefined) {
    throw new Error(`${label} did not hand the thread on`);
  }
  return {
    nextThreadUtxo,
    measurement: measureCompleteSignedTransaction(signedCbor),
    outputDatum,
    signedCbor,
  };
};

/** The `Verify` redeemer, Option B shape. */
const verifyRedeemer = (
  transition: Data,
  layout: {
    readonly inputIndex: bigint;
    readonly outputIndex: bigint;
  },
): string =>
  Data.to(
    new Constr(1, [
      new Constr(0, [layout.inputIndex, layout.outputIndex, transition]),
    ]),
  );

/** The stage-2 `Continue` redeemer (source binding takes indices only). */
const indicesRedeemer = (layout: {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
}): string =>
  Data.to(
    new Constr(1, [new Constr(0, [layout.inputIndex, layout.outputIndex])]),
  );

/**
 * How the §8.8 door is handed the field's §5.1 preimage: inline in the observe
 * redeemer (tier-1 `Inline`, the only tier this harness builds), or by
 * reference to a published proof item.
 */
type ObserveDelivery =
  | { readonly kind: "inline"; readonly fieldPreimageHex?: string }
  | { readonly kind: "reference"; readonly publication: UTxO };

type CompleteItemJourney = {
  readonly harness: EmulatorHarness;
  readonly itemCase: CanonicalDecodeItemCase;
  readonly authenticate: StageSubmission;
  readonly source: StageSubmission;
  readonly observe: StageSubmission;
};

/**
 * Walks one thread from the semantic resolver to the observe door. Both
 * validators that carry a body worth referencing — the semantic resolver and
 * the observe validator — are sourced from parked reference scripts, the
 * production basis since the #617 reference-script wiring; embedding either
 * would put a validator body inside the transaction whose size is the
 * measurement.
 */
const runJourneyToObserve = async ({
  harness,
  itemCase,
  threadUtxo,
  delivery,
  observeScriptReference,
  semanticScriptReference,
}: {
  readonly harness: EmulatorHarness;
  readonly itemCase: CanonicalDecodeItemCase;
  readonly threadUtxo: UTxO;
  readonly delivery: ObserveDelivery;
  /**
   * Omitted only by the embedded-basis probe, which attaches the observe
   * validator to its own transaction instead of reading the published copy.
   */
  readonly observeScriptReference?: UTxO;
  readonly semanticScriptReference: UTxO;
}): Promise<CompleteItemJourney> => {
  const semanticContract = {
    spendingScriptAddress: harness.semanticAddress,
    spendingScript: harness.semanticScript,
  };
  const authenticate = await submitStage({
    harness,
    inputUtxo: threadUtxo,
    inputContract: semanticContract,
    outputContract: harness.stages.source,
    outputDatum: itemCase.authenticatedDatum,
    label: "canonical item authentication",
    scriptReference: semanticScriptReference,
    encode: (layout) => verifyRedeemer(itemCase.transitionData, layout),
  });
  const source = await submitStage({
    harness,
    inputUtxo: authenticate.nextThreadUtxo,
    inputContract: harness.stages.source,
    outputContract: harness.stages.observe,
    outputDatum: itemCase.preparedDatum,
    label: "canonical item source binding",
    encode: indicesRedeemer,
  });
  const observe = await submitStage({
    harness,
    inputUtxo: source.nextThreadUtxo,
    inputContract: harness.stages.observe,
    outputContract: harness.stages.proof,
    outputDatum: itemCase.observedDatum,
    label: "canonical item observation",
    ...(observeScriptReference === undefined
      ? {}
      : { scriptReference: observeScriptReference }),
    ...(delivery.kind === "reference"
      ? { extraReferences: [delivery.publication] }
      : {}),
    encode: ({ inputIndex, outputIndex, referenceInputIndex }) =>
      delivery.kind === "inline"
        ? Data.to(
            new Constr(1, [
              new Constr(0, [
                inputIndex,
                outputIndex,
                new Constr(0, [
                  delivery.fieldPreimageHex ?? itemCase.fieldPreimageHex,
                ]),
              ]),
            ]),
          )
        : Data.to(
            new Constr(1, [
              new Constr(1, [
                inputIndex,
                outputIndex,
                referenceInputIndex(delivery.publication),
              ]),
            ]),
          ),
  });
  return { harness, itemCase, authenticate, source, observe };
};

type PublishedProofItem = {
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly utxo: UTxO;
  readonly datumCbor: string;
  readonly minAdaLovelace: bigint;
};

const publishProofItemPublication = async ({
  harness,
  publication,
}: {
  readonly harness: EmulatorHarness;
  readonly publication: ValidationProofItemPublicationV1;
}): Promise<PublishedProofItem> => {
  const minAdaLovelace = minimumLovelaceForValidationProofItemPublicationV1({
    contracts: harness.contracts,
    publication,
    coinsPerUtxoByte: BigInt(PROTOCOL_PARAMETERS_DEFAULT.coinsPerUtxoByte),
  });
  const unsigned = await Effect.runPromise(
    buildUnsignedValidationProofItemPublicationV1Program(
      harness.lucid,
      harness.contracts,
      publication,
    ),
  );
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const txHash = await signed.submit();
  await harness.lucid.awaitTx(txHash);
  const utxo = (
    await harness.lucid.utxosAt(
      harness.contracts.validationTraceDispute.proofItem.spendingScriptAddress,
    )
  ).find(
    (candidate) =>
      candidate.txHash === txHash &&
      candidate.datum != null &&
      sameDatumValue(candidate.datum, publication.datumCbor),
  );
  if (utxo === undefined) {
    throw new Error("published proof item was not found");
  }
  return {
    measurement: measureCompleteSignedTransaction(signedCbor),
    utxo,
    datumCbor: publication.datumCbor,
    minAdaLovelace,
  };
};

const publishProofItem = async ({
  harness,
  itemCase,
  fieldPreimageHexOverride,
}: {
  readonly harness: EmulatorHarness;
  readonly itemCase: CanonicalDecodeItemCase;
  readonly fieldPreimageHexOverride?: string;
}): Promise<PublishedProofItem> => {
  const preState = itemCase.preState;
  const publication = deriveValidationProofItemPublicationV1({
    transactionId: preState.transaction_id,
    transactionCommitment: preState.transaction_commitment,
    fieldPreimage: fieldPreimageHexOverride ?? itemCase.fieldPreimageHex,
  });
  return await publishProofItemPublication({ harness, publication });
};

const buildRawProofItemPublicationForNegativeControl = ({
  itemCase,
  fieldPreimage,
}: {
  readonly itemCase: CanonicalDecodeItemCase;
  readonly fieldPreimage: string;
}): ValidationProofItemPublicationV1 => {
  const datum: ValidationProofItemPublicationV1["datum"] = {
    version: 1n,
    transaction_id: itemCase.preState.transaction_id,
    transaction_commitment: itemCase.preState.transaction_commitment,
    field_preimage: fieldPreimage,
  };
  return {
    datum,
    datumCbor: Data.to(datum, ValidationProofItemDatumV1),
  };
};

const publishRawProofItemForNegativeControl = async ({
  harness,
  itemCase,
  fieldPreimage,
}: {
  readonly harness: EmulatorHarness;
  readonly itemCase: CanonicalDecodeItemCase;
  readonly fieldPreimage: string;
}): Promise<PublishedProofItem> =>
  await publishProofItemPublication({
    harness,
    publication: buildRawProofItemPublicationForNegativeControl({
      itemCase,
      fieldPreimage,
    }),
  });

/**
 * The size-bearing measurement, post-Option-B: one journey from the semantic
 * resolver to the observe door with the §5.1 preimage delivered INLINE, which
 * is where the item now rides. Both reference scripts are parked first, so the
 * measured transactions carry indices rather than validator bodies — the
 * production basis since the #617 reference-script wiring.
 *
 * The whole journey is returned, not just the observe row: the authenticate
 * and source rows are what make "every non-observe stage is item-size
 * independent" (#622's finding, the precondition of the lane-level re-pins)
 * checkable here rather than merely quoted.
 */
const measureObserveAt = async (
  itemBytes: number,
  options: { readonly embedObserveValidator?: boolean } = {},
): Promise<CompleteItemJourney> => {
  const itemCase = await buildCanonicalDecodeItemCase(itemBytes);
  const harness = await setupEmulator([itemCase.preparedThreadDatum]);
  const semanticScriptReference = await publishReferenceScript(
    harness,
    harness.semanticScript,
    "2f",
  );
  const observeScriptReference =
    options.embedObserveValidator === true
      ? undefined
      : await publishReferenceScript(
          harness,
          harness.stages.observe.spendingScript,
          "3f",
        );
  return await runJourneyToObserve({
    harness,
    itemCase,
    threadUtxo: harness.threadUtxos[0]!,
    delivery: { kind: "inline" },
    ...(observeScriptReference === undefined ? {} : { observeScriptReference }),
    semanticScriptReference,
  });
};

// `measureReferenceAt` lived here and went with the publication-maximum row it
// was the only caller of — see the removal note in the describe block below.
// Reference-carriage consumption is still measured in this file by the
// "reaches the identical terminal state through direct and reference carriage"
// row, which resolves a published item at a tier-1 size.

const measurePublicationFrontierAt = async (
  itemByteCandidates: readonly number[],
): Promise<
  readonly {
    readonly itemBytes: number;
    readonly publication: Awaited<ReturnType<typeof publishProofItem>>;
  }[]
> => {
  // Retargeted 2026-08-14 (owner ruling) from
  // `maxSinglePublicationCompleteItemBytes` (14,396) to the tier-1 maximum. The
  // base case only supplies the thread datum and the transaction identity — every
  // probe below overrides the field preimage outright — but it still has to be a
  // case this tier-1-only harness can build, and a 14,396-byte item's field-2
  // preimage is 14,400 bytes, i.e. tier-2. See TIER1_MAX_COMPLETE_ITEM_BYTES.
  const itemCase = await buildCanonicalDecodeItemCase(
    TIER1_MAX_COMPLETE_ITEM_BYTES,
  );
  const harness = await setupEmulator([itemCase.preparedThreadDatum]);
  const measurements = [];
  for (const itemBytes of itemByteCandidates) {
    measurements.push({
      itemBytes,
      publication: await publishProofItem({
        harness,
        itemCase,
        fieldPreimageHexOverride: encodeMidgardFieldPreimageV1([
          makeExactSizeOutputItem(itemBytes),
        ]).toString("hex"),
      }),
    });
  }
  return measurements;
};

/**
 * **RESOLVED 2026-08-14: `5 passed (5)`.** The handoff below is the historical
 * record of the pre-#579 freeze it describes; it is no longer the suite's state.
 * #579 regenerated the blueprint, the applied §3.2 resolver hash was re-pinned
 * from the producer, and two further things that the freeze had been masking
 * came out with it:
 *
 * - The complete-item witness selector matched on `(phase, kind)` alone, so it
 *   silently measured **field 0**'s few-dozen-byte preimage instead of field 2's.
 *   The publication rows were measuring the wrong field, and the substitution row
 *   was writing its flipped byte past the end of a ~40-byte buffer — mutating
 *   nothing, and so rejecting nothing. Both are fixed and both now assert that
 *   what they claim to exercise is what they exercise.
 * - The row at the applied publication maximum MOVED to
 *   `complete-item-carriage-tiers-emulator-v1.test.ts`, because 14,396 bytes is
 *   tier-2 under §8.4 and this harness is tier-1 only (owner ruling). See
 *   `TIER1_MAX_COMPLETE_ITEM_BYTES` for the 64-byte overhang that exposed, which
 *   **#580 owns**.
 *
 * ---
 *
 * **HANDOFF TO #579, measured 2026-08-12: `5 failed | 1 passed (6)`.**
 *
 * This suite applies the validators compiled into the **committed**
 * `onchain/aiken/plutus.json`, which #592 deliberately left byte-identical at
 * the now-SUPERSEDED md5 `c52589df225145ad74c8f444d500dfe5` — blueprints move
 * once, in #579's single regeneration pass (#587's precedent). That pass has
 * since landed: the blueprint's current md5 is
 * `b20c9a14a8fe445cdddbe5305b3857c1`, so the freeze described here is the
 * pre-#579 condition and the digest above must not be read as current.
 *
 * #592 reshaped `canonical_decode_item_semantic_v1`'s `Verify` from
 * `(input_index, output_index, transition, collection_proof, item_cbor)` to
 * `(input_index, output_index, transition, carriage)` in the Aiken source, and
 * #597 moved the TypeScript half to match. So the redeemers this suite builds
 * are the four-field form while the frozen compiled validator still expects
 * five, and every row that submits one fails inside the script with
 * `failed script execution Spend[1] unexpected empty list` — the frozen
 * validator destructuring a field that is no longer there.
 *
 * It is the same blueprint-freeze blind spot already recorded for
 * `sdk-aiken-schema-parity` and `validation-resolver-applied-hashes`, and it
 * clears with the regeneration rather than with any change here: nothing in this
 * file can make a stale compiled validator accept the current wire format. The
 * sixth row (`pins the applied §3.2 necessity identities`) was already red
 * before #597 for the same freeze, on applied hashes rather than on shape.
 *
 * Before #597 this suite measured `1 failed | 5 passed (6)`.
 */
describe("complete-item proof fit V1 (emulator, applied validators)", () => {
  // #546: the six shared §3.2 necessity artifacts
  // (transaction-field-chunk, ledger-output-incremental-proof,
  // mint-fold-asset, native-script-traversal, redeemer-item-traversal,
  // script-source-hash-block) bind exactly these two identities on this
  // measurement deployment. The C21-AUDIT (#484) pass inferred the applied
  // hash from the unchanged unapplied script instead of measuring it; this
  // selector measures it, so the artifacts' "any change invalidates" clause
  // is now gated rather than argued. Measured unchanged under the current
  // tree's stock testnet blueprint
  // 605c8b8dca1f01e2cde5219138a1f81e69214f9a182c10b73c20341187ddc2dc
  // (391 validators, aiken v1.1.22+39d6b04).
  // Re-pinned 2026-08-14 (#579): the regeneration
  // (`onchain/aiken/plutus.json` md5 b20c9a14a8fe445cdddbe5305b3857c1, 398
  // validators, aiken v1.1.23+2a78108) recompiled the canonical-decode item
  // semantic resolver, and #594 gave it a trailing
  // `field_preimage_certificate_policy_id` parameter that the SDK applies from
  // the blueprint, so its applied hash moves: 983051b4… -> f492660e….
  // Re-pinned 2026-08-16 (#606): the E2 repair regeneration
  // (`onchain/aiken/plutus.json` md5 5e38d7c6ccb7987d0aca710307dcaea7, 398
  // validators, same fork) moved the certificate policy id
  // (c3682abd… -> f030476f…) and recompiled the canonical-decode resolver
  // against the welded-`field_hash` door, so the applied hash moves again:
  // f492660e… -> 69fd502b…. The resolver COUNT (76) and the proof-item hash
  // below are measured unchanged.
  // Re-pinned 2026-08-23 (#617 IG1, the wave's one sanctioned blueprint
  // regeneration, `onchain/aiken/plutus.json` md5
  // 66426a3af44236a2ad3ded1b03f8fdcf, 444 validators, same fork). Two of the
  // three identities move, for two different reasons, and neither is the
  // regeneration itself:
  //   - COUNT 76 -> 91. Lane A's cek/ValueAndMint prepare+semantic split
  //     appends fifteen semantic resolvers (cek 4, ValueAndMint 11). They are
  //     APPENDED, so index 1 still names the canonical-decode item semantic
  //     resolver -- checked, not assumed, because a positional pin silently
  //     re-pointed at another validator would be absorbed by this very
  //     re-pin: the C53 sweep measures
  //     `canonical_decode_item_semantic_v1.main.spend` at applied hash
  //     81b42c84... through its own emulator run, and that is the value this
  //     row receives at index 1.
  //   - resolver[1] applied hash 69fd502b... -> 81b42c84.... This is #620
  //     (7da3eba0) retiring the direct-resolver premise -- it changed the
  //     resolver body and dropped a parameter -- and the move reproduces at
  //     02365110, i.e. BEFORE Lane A. It is neither the regeneration's nor
  //     Lane A's.
  // The proof-item hash below is measured UNCHANGED, and independently: it is
  // the same value `inspect-contracts.test.ts` prints as
  // `q13AppliedIdentities.fraudProofSpendingScriptHash`.
  // Measured by `loadContracts()` here, the same producer this row asserts on.
  it("pins the applied §3.2 necessity identities on the measurement deployment", async () => {
    const contracts = await loadContracts();
    expect(contracts.validationTraceDispute.semanticResolvers).toHaveLength(91);
    expect(
      contracts.validationTraceDispute.semanticResolvers[1]!.spendingScriptHash,
    ).toBe("81b42c84e294a579b20b4a635a135f12dea7893e4e202632ff6c2976");
    expect(contracts.validationTraceDispute.proofItem.spendingScriptHash).toBe(
      "22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab",
    );
  }, 120_000);

  it("measures the applied observe door at the staged reliability boundary", async () => {
    // FLIPPED ONTO THE OBSERVE STAGE (#617 sign-off item 1). Through the
    // counted era and the #597 wiring era this row measured the AUTHENTICATE
    // stage, because that was the stage the §5.1 preimage rode: it double-
    // carried the item and was therefore the direct route's binder (owner-
    // signed reserve 12,810 / exact 13,294). Option B (#620) moved the
    // preimage to the observe stage's §8.8 door and left authenticate
    // item-size-independent, so the binder moved with it and the owner-signed
    // frontier rebound to the measured post-change pair (13,522 / 14,004,
    // #622 ruling (b)). This row now walks the real staged chain to that door
    // and measures the transaction that actually grows with the item.
    //
    // What is asserted is a RELATION, not a restatement: the production
    // journey's byte table is pinned in
    // `demo/midgard-fault-proofs/tests/submit-init-emulator-option-b-*.test.ts`,
    // and #622's caveat 1 says those numbers are framing-relative. This
    // harness has its own framing, so it asserts that the applied validators
    // carry the reserve-frontier item through the door inside the reliability
    // budget, that the door is the binder, and that the §3.3 execution
    // reserve holds — every one of which is falsifiable here.
    const reliableItemBytes =
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
    const journey = await measureObserveAt(reliableItemBytes);
    const observe = journey.observe.measurement;

    // One spend redeemer; one reference input, and it is the parked observe
    // validator — the preimage rides inline, so nothing else is referenced.
    expect(observe.redeemerCount).toBe(1);
    expect(observe.referenceInputCount).toBe(1);
    expect(observe.completeSignedBytes).toBeLessThanOrEqual(
      MAX_L1_PROOF_TX_BYTES -
        MIDGARD_V1_ENVELOPE_MEASUREMENTS.proofItemEnvelopeReliabilityReserveBytes,
    );
    expect(Number(observe.executionMemory)).toBeLessThanOrEqual(
      RESERVED_MEMORY_UNITS,
    );
    expect(Number(observe.executionSteps)).toBeLessThanOrEqual(
      RESERVED_CPU_UNITS,
    );

    // The door is the binder: it carries the item, the two stages before it do
    // not, and every stage of the chain fits the real L1 envelope.
    const authenticate = journey.authenticate.measurement;
    const source = journey.source.measurement;
    expect(observe.completeSignedBytes).toBeGreaterThan(
      authenticate.completeSignedBytes,
    );
    expect(observe.completeSignedBytes).toBeGreaterThan(
      source.completeSignedBytes,
    );
    expect(authenticate.completeSignedBytes).toBeLessThan(reliableItemBytes);
    for (const [stage, measurement] of [
      ["authenticate", authenticate],
      ["source", source],
      ["observe", observe],
    ] as const) {
      expect(measurement.completeSignedBytes, stage).toBeLessThanOrEqual(
        MAX_L1_PROOF_TX_BYTES,
      );
      expect(Number(measurement.executionMemory), stage).toBeLessThanOrEqual(
        RESERVED_MEMORY_UNITS,
      );
      expect(Number(measurement.executionSteps), stage).toBeLessThanOrEqual(
        RESERVED_CPU_UNITS,
      );
    }

    // The door wrote the observation the off-chain staging derived — the row
    // measures a journey that really completed, not one that merely balanced.
    expect(
      sameDatumValue(
        journey.observe.nextThreadUtxo.datum ?? "",
        journey.itemCase.observedDatum,
      ),
    ).toBe(true);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      const contracts = await loadContracts();
      console.info(
        JSON.stringify(
          {
            completeItemDirectProofFitV1: {
              appliedSemanticScriptHash:
                contracts.validationTraceDispute.semanticResolvers[1]!
                  .spendingScriptHash,
              reliableDirectItemBytes: reliableItemBytes,
              authenticateTransaction: authenticate,
              sourceTransaction: source,
              observeTransaction: observe,
              reservedMemoryUnits: RESERVED_MEMORY_UNITS,
              reservedCpuUnits: RESERVED_CPU_UNITS,
            },
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ),
      );
    }
  }, 600_000);

  it("measures the complete signed tier-1 step transaction at the 14,336-byte preimage cap", async () => {
    // #611 (#557 M2): #580 measured the at-cap ONE-STEP EVIDENCE — 15,848
    // bytes over the 14,336-byte preimage, 536 unspent inside the 16,383-byte
    // evidence envelope — but no suite built the complete SIGNED step
    // transaction around it, so M2 was narrowed rather than closed. This row
    // builds and submits that transaction.
    //
    // MOVED ONTO THE OBSERVE STAGE (#617 sign-off item 1). The at-cap inline
    // preimage used to ride the authenticate stage's redeemer; since Option B
    // (#620) it rides the observe stage's §8.8 door, so the worst-case inline
    // step transaction is the observe one and the M2 reading has to be taken
    // there. The production route still cannot produce it — build-time
    // routing (#621) demotes items far below the cap to publication, and the
    // pre-sign envelope gate refuses the rest — so this hand-driven journey
    // remains the only producer of the shape an adversarial prover is free to
    // attempt, which is exactly the shape the tier-1 bound must hold for.
    const atCap = await measureObserveAt(TIER1_MAX_COMPLETE_ITEM_BYTES);
    const byReference = atCap.observe.measurement;
    // The carried §5.1 preimage really is the whole tier-1 domain — cap
    // bytes, carried Inline (the case builder throws on any other carriage).
    expect(Buffer.from(atCap.itemCase.fieldPreimageHex, "hex").length).toBe(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
    );
    expect(byReference.redeemerCount).toBe(1);
    expect(byReference.referenceInputCount).toBe(1);
    expect(Number(byReference.executionMemory)).toBeLessThanOrEqual(
      RESERVED_MEMORY_UNITS,
    );
    expect(Number(byReference.executionSteps)).toBeLessThanOrEqual(
      RESERVED_CPU_UNITS,
    );
    // #557 M2, MEASURED AND FALSIFIED (#611, 2026-08-17; re-measured on the
    // observe stage 2026-08-23): the complete signed step transaction at the
    // cap does NOT fit maxTxSize even on the deployed route. The
    // evidence-layer reading (15,848 bytes, 536 unspent in the 16,383-byte
    // evidence envelope) never included a stage's protocol framing — thread
    // input, continuation output and datum, required signer, reference input,
    // change — which the production shape cannot shed. This assertion pins the
    // measured overflow so the row flips of its own accord when the owner
    // reprices the tier-1 bound; it does NOT accept the state as correct —
    // repricing is parameter churn and rides the #611 escalation. It is also
    // the same reading #622 recorded from the production journey: the
    // contiguous inline frontier ends at 14,004, below the 14,336 cap, and
    // items past it auto-demote to publication.
    expect(byReference.completeSignedBytes).toBeGreaterThan(
      MAX_L1_PROOF_TX_BYTES,
    );

    // Embedded basis, measured for the record: a prover who attaches the
    // observe validator instead of referencing the published copy adds the
    // whole validator body on top — route waste on the prover's side, but it
    // pins that the published reference script is load-bearing for step
    // liveness anywhere near the cap.
    const embedded = await measureObserveAt(TIER1_MAX_COMPLETE_ITEM_BYTES, {
      embedObserveValidator: true,
    });
    expect(embedded.observe.measurement.referenceInputCount).toBe(0);
    expect(embedded.observe.measurement.completeSignedBytes).toBeGreaterThan(
      byReference.completeSignedBytes,
    );

    // The actual fitting frontier, bisected on the deployed route: the largest
    // complete item whose signed OBSERVE transaction fits maxTxSize.
    // `maxReliableDirectCompleteItemBytes` is a known fitting floor (the row
    // above measures it inside the reliability budget); the cap is the
    // measured overflow above. This is the number a repricing decision needs.
    // Not every exact item size is constructible — the datum filler's chunk
    // headers make the size ladder skip a byte or two at chunk boundaries —
    // so the bisect walks the nearest constructible size and the frontier is
    // exact at constructible-size resolution.
    const constructibleNear = (target: number): number | undefined => {
      for (let offset = 0; offset <= 4; offset += 1) {
        for (const candidate of offset === 0
          ? [target]
          : [target - offset, target + offset]) {
          try {
            makeExactSizeOutputItem(candidate);
            return candidate;
          } catch {
            // Not constructible; keep looking.
          }
        }
      }
      return undefined;
    };
    let fittingItemBytes: number =
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
    let overflowingItemBytes: number = TIER1_MAX_COMPLETE_ITEM_BYTES;
    const probes: Record<string, number> = {
      [TIER1_MAX_COMPLETE_ITEM_BYTES.toString()]:
        byReference.completeSignedBytes,
    };
    while (overflowingItemBytes - fittingItemBytes > 1) {
      const midpoint = Math.floor(
        (fittingItemBytes + overflowingItemBytes) / 2,
      );
      const candidate = constructibleNear(midpoint);
      if (
        candidate === undefined ||
        candidate <= fittingItemBytes ||
        candidate >= overflowingItemBytes
      ) {
        break;
      }
      const probe = await measureObserveAt(candidate);
      probes[candidate.toString()] =
        probe.observe.measurement.completeSignedBytes;
      if (
        probe.observe.measurement.completeSignedBytes <= MAX_L1_PROOF_TX_BYTES
      ) {
        fittingItemBytes = candidate;
      } else {
        overflowingItemBytes = candidate;
      }
    }
    if (probes[fittingItemBytes.toString()] === undefined) {
      const floorProbe = await measureObserveAt(fittingItemBytes);
      probes[fittingItemBytes.toString()] =
        floorProbe.observe.measurement.completeSignedBytes;
    }
    // The frontier is tight to within the constructible-size ladder's gaps.
    expect(overflowingItemBytes - fittingItemBytes).toBeLessThanOrEqual(3);
    expect(probes[fittingItemBytes.toString()]).toBeLessThanOrEqual(
      MAX_L1_PROOF_TX_BYTES,
    );
    expect(probes[overflowingItemBytes.toString()]).toBeGreaterThan(
      MAX_L1_PROOF_TX_BYTES,
    );

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            tier1CapSignedStepTransactionV1: {
              tier1PreimageCapBytes:
                MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
              itemBytes: TIER1_MAX_COMPLETE_ITEM_BYTES,
              evidenceBytes: atCap.itemCase.argument.evidenceCbor.length,
              byReferenceObserveTransaction: byReference,
              embeddedObserveTransaction: embedded.observe.measurement,
              fittingFrontierItemBytes: fittingItemBytes,
              fittingFrontierSignedBytes: probes[fittingItemBytes.toString()],
              overflowSignedBytes: probes[overflowingItemBytes.toString()],
              probes,
              maxL1ProofTxBytes: MAX_L1_PROOF_TX_BYTES,
            },
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ),
      );
    }
  }, 900_000);

  it("pins the exact applied publication frontiers and reliability reserve", async () => {
    const reliable =
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableCompleteItemPublicationBytes;
    const exact =
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxExactCompleteItemPublicationBytes;
    const [reliableFit, reliableOverflow, exactFit, exactOverflow] =
      await measurePublicationFrontierAt([
        reliable,
        reliable + 1,
        exact,
        exact + 1,
      ]);
    expect(reliableFit!.publication.measurement.completeSignedBytes).toBe(
      MAX_L1_PROOF_TX_BYTES -
        MIDGARD_V1_ENVELOPE_MEASUREMENTS.proofItemEnvelopeReliabilityReserveBytes,
    );
    expect(
      reliableOverflow!.publication.measurement.completeSignedBytes,
    ).toBeGreaterThan(
      MAX_L1_PROOF_TX_BYTES -
        MIDGARD_V1_ENVELOPE_MEASUREMENTS.proofItemEnvelopeReliabilityReserveBytes,
    );
    expect(exactFit!.publication.measurement.completeSignedBytes).toBe(
      MAX_L1_PROOF_TX_BYTES,
    );
    expect(
      exactOverflow!.publication.measurement.completeSignedBytes,
    ).toBeGreaterThan(MAX_L1_PROOF_TX_BYTES);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            completeItemPublicationFrontierV1: Object.fromEntries(
              [reliableFit, reliableOverflow, exactFit, exactOverflow].map(
                ({ itemBytes, publication }) => [
                  itemBytes.toString(),
                  {
                    ...publication.measurement,
                    datumBytes: publication.datumCbor.length / 2,
                    minAdaLovelace: publication.minAdaLovelace,
                  },
                ],
              ),
            ),
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ),
      );
    }
  }, 600_000);

  // REMOVED 2026-08-14 (owner ruling): "measures inline-datum publication plus
  // reference-input consumption at the publication maximum" MOVED to
  // `complete-item-carriage-tiers-emulator-v1.test.ts` as "carries one complete
  // item at the applied publication maximum through the tier-2 door".
  //
  // It cannot run here. The publication maximum is 14,396 bytes, whose field-2
  // preimage is 14,400 — tier-2 `RawUtxo` under §8.4, and this harness is
  // tier-1 `Inline` only. The row only appeared to run because the witness
  // selector matched field 0 instead of field 2, so it measured a ~40-byte
  // preimage and a 363-byte "publication". See TIER1_MAX_COMPLETE_ITEM_BYTES
  // for the 64-byte overhang this exposed, which #580 owns.

  it("reaches the identical observed state through inline and reference delivery of the same item", async () => {
    // Same complete item, both deliveries, one emulator: the applied door must
    // accept each and write the byte-identical observation. MOVED ONTO THE
    // OBSERVE DOOR (#617 sign-off item 1): before Option B the two deliveries
    // were two arms of the authenticate stage's `Verify`; #620 retired the
    // reference arm there and made the observe stage the sole content gate, so
    // route determinism is a property of that door now. Both legs park and
    // read the same reference scripts, the production basis since the #617
    // wiring.
    const itemBytes =
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
    const itemCase = await buildCanonicalDecodeItemCase(itemBytes);
    const harness = await setupEmulator([
      itemCase.preparedThreadDatum,
      itemCase.preparedThreadDatum,
    ]);
    const [inlineThread, referenceThread] = harness.threadUtxos;
    const semanticScriptReference = await publishReferenceScript(
      harness,
      harness.semanticScript,
      "2f",
    );
    const observeScriptReference = await publishReferenceScript(
      harness,
      harness.stages.observe.spendingScript,
      "3f",
    );

    const inline = await runJourneyToObserve({
      harness,
      itemCase,
      threadUtxo: inlineThread!,
      delivery: { kind: "inline" },
      observeScriptReference,
      semanticScriptReference,
    });
    const publication = await publishProofItem({ harness, itemCase });
    const reference = await runJourneyToObserve({
      harness,
      itemCase,
      threadUtxo: referenceThread!,
      delivery: { kind: "reference", publication: publication.utxo },
      observeScriptReference,
      semanticScriptReference,
    });

    // Both doors wrote the same observation, and both transactions fit the
    // real L1 envelope.
    expect(
      sameDatumValue(
        inline.observe.nextThreadUtxo.datum ?? "",
        reference.observe.nextThreadUtxo.datum ?? "",
      ),
    ).toBe(true);
    expect(
      sameDatumValue(
        inline.observe.nextThreadUtxo.datum ?? "",
        itemCase.observedDatum,
      ),
    ).toBe(true);
    expect(inline.observe.measurement.completeSignedBytes).toBeLessThanOrEqual(
      MAX_L1_PROOF_TX_BYTES,
    );
    expect(
      reference.observe.measurement.completeSignedBytes,
    ).toBeLessThanOrEqual(MAX_L1_PROOF_TX_BYTES);

    // The whole point of the reference delivery: the item is named, not
    // serialized again, so its door transaction is far smaller than the inline
    // one at the same item size.
    expect(reference.observe.measurement.completeSignedBytes).toBeLessThan(
      inline.observe.measurement.completeSignedBytes - itemBytes / 2,
    );
    expect(reference.observe.measurement.referenceInputCount).toBe(2);

    const terminal = (
      await harness.lucid.utxosAt(harness.stages.proof.spendingScriptAddress)
    ).map((utxo) => utxo.datum ?? "");
    expect(terminal).toHaveLength(2);
    expect(new Set(terminal).size).toBe(1);
  }, 900_000);

  it("rejects substituted and trailing-byte items at the observe reference door, and accepts the honest one", async () => {
    // REWRITTEN ONTO THE OBSERVE DOOR (#617 sign-off item 1). This row used to
    // drive the authenticate stage's retired `VerifyReference` arm. On the
    // Option B wire that arm does not exist, so every submission it made —
    // hostile or honest — was refused for the wrong reason: the row was
    // VACUOUS, rejecting the honest publication too, which is exactly what an
    // unfalsifiable negative control looks like. The honest leg below is the
    // control that keeps it honest: the same machinery, the same door, the
    // unmutated publication, GREEN. A refusal only counts because that leg
    // passes.
    const itemBytes = 12_000;
    const itemCase = await buildCanonicalDecodeItemCase(itemBytes);
    const harness = await setupEmulator([
      itemCase.preparedThreadDatum,
      itemCase.preparedThreadDatum,
      itemCase.preparedThreadDatum,
    ]);
    const semanticScriptReference = await publishReferenceScript(
      harness,
      harness.semanticScript,
      "2f",
    );
    const observeScriptReference = await publishReferenceScript(
      harness,
      harness.stages.observe.spendingScript,
      "3f",
    );
    const observeWith = async (
      threadUtxo: UTxO,
      publication: UTxO,
    ): Promise<CompleteItemJourney> =>
      await runJourneyToObserve({
        harness,
        itemCase,
        threadUtxo,
        delivery: { kind: "reference", publication },
        observeScriptReference,
        semanticScriptReference,
      });

    // Substitution: same length, one flipped byte deep inside the published
    // field preimage. The door hashes the whole preimage against the committed
    // field commitment, so a single flipped byte anywhere inside it fails
    // closed.
    //
    // #579: the offset is taken from the preimage's OWN length. It used to be
    // `itemBytes - 100`, an index into a buffer that a selection defect had
    // made ~40 bytes long — the write landed past the end, `Buffer` swallowed
    // it, and the "substituted" preimage was byte-identical to the original.
    // The equality guard below is what keeps that from being silent again: a
    // test that mutates a buffer must show the mutation took before it can
    // claim the mutation was rejected.
    const original = Buffer.from(itemCase.fieldPreimageHex, "hex");
    const substituted = Buffer.from(original);
    const flipOffset = substituted.length - 100;
    expect(flipOffset).toBeGreaterThan(0);
    substituted[flipOffset] = substituted[flipOffset]! ^ 0x01;
    expect(substituted.length).toBe(original.length);
    expect(substituted.equals(original)).toBe(false);
    const substitutedPublication = await publishRawProofItemForNegativeControl({
      harness,
      itemCase,
      fieldPreimage: substituted.toString("hex"),
    });
    await expect(
      observeWith(harness.threadUtxos[0]!, substitutedPublication.utxo),
    ).rejects.toThrow(/canonical item observation local evaluation failed/u);

    // Trailing data: the exact item plus one extra byte.
    const trailingPublication = await publishRawProofItemForNegativeControl({
      harness,
      itemCase,
      fieldPreimage: `${itemCase.fieldPreimageHex}00`,
    });
    await expect(
      observeWith(harness.threadUtxos[1]!, trailingPublication.utxo),
    ).rejects.toThrow(/canonical item observation local evaluation failed/u);

    // The control: the honest publication, through the same door, on the same
    // blueprint. If this leg ever goes red the two refusals above stop meaning
    // anything, and this row says so instead of staying quietly green.
    const honestPublication = await publishProofItem({ harness, itemCase });
    const honest = await observeWith(
      harness.threadUtxos[2]!,
      honestPublication.utxo,
    );
    expect(
      sameDatumValue(
        honest.observe.nextThreadUtxo.datum ?? "",
        itemCase.observedDatum,
      ),
    ).toBe(true);
  }, 900_000);
});
