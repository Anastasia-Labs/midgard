import { hashMidgardValidationMachineState } from "@al-ft/midgard-core";
import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  deriveCanonicalDecodeItemStageData,
  validationOneStepEvidenceHash,
} from "@al-ft/midgard-fault-proofs";
import {
  AuthenticatedCanonicalDecodeItemDatum,
  buildValidationTraceDisputeFaultProofContracts,
  ObservedCanonicalDecodeItemDatum,
  parseFaultProofBlueprint,
  PreparedCanonicalDecodeItemDatum,
  PreparedValidationResolutionDatum,
  type PreparedValidationResolutionDatum as PreparedValidationResolutionDatumData,
  validationMachineStateDataFromCore,
  ValidationOneStepWitness,
  type ValidationOneStepWitness as ValidationOneStepWitnessData,
  type ValidationTraceDisputeFaultProofContracts,
  VerifiedCanonicalDecodeItemDatum,
} from "@al-ft/midgard-sdk";
import {
  CML,
  Constr,
  Data,
  Emulator,
  Lucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { buildValidationOneStepArgument } from "../src/index.js";
import {
  blueprintJson,
  buildTraceWithOutputs,
  type CanonicalDecodeItemCase,
  FRAUD_PROOF_CATALOGUE_POLICY_ID,
  HUB_ORACLE_POLICY_ID,
  makeExactSizeOutputItem,
  MAX_L1_PROOF_TX_BYTES,
  NO_AUXILIARY_WITNESS_CBOR,
  OUTPUT_FIELD_INDEX,
  SIGNER_HASH,
  SIGNING_KEY,
  THREAD_ASSET_NAME,
} from "./complete-item-proof-fit-emulator.build-trace-with-outputs.js";

export const buildCanonicalDecodeItemCase = async (
  itemBytes: number,
): Promise<CanonicalDecodeItemCase> => {
  const item = makeExactSizeOutputItem(itemBytes);
  const trace = await buildTraceWithOutputs([item]);
  const expectedPreimageBytes = encodeMidgardFieldPreimage([item]).length;
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
    // `complete-item-carriage-tiers-emulator.test.ts` and
    // `complete-item-proof-fit.test.ts` already keep.
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
  const argument = buildValidationOneStepArgument({ trace, stateIndex });
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
  const evidenceHash = validationOneStepEvidenceHash({
    transitionCbor: argument.transitionCbor,
    auxiliaryCbor: NO_AUXILIARY_WITNESS_CBOR,
  });
  const preState = validationMachineStateDataFromCore(
    trace.states[stateIndex]!,
  );
  const claimedSuccessorHash = hashMidgardValidationMachineState(
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
    PreparedValidationResolutionDatum,
  );
  const preparedResolution = (
    Data.from(
      preparedThreadDatum,
      PreparedValidationResolutionDatum,
    ) as PreparedValidationResolutionDatumData
  ).data;
  if (preparedResolution === null) {
    throw new Error("prepared thread datum is missing its state");
  }
  const stageData = deriveCanonicalDecodeItemStageData({
    preparedResolution,
    transition: Data.from(
      argument.transitionCbor.toString("hex"),
      ValidationOneStepWitness,
    ) as ValidationOneStepWitnessData,
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
      AuthenticatedCanonicalDecodeItemDatum,
    ),
    preparedDatum: Data.to(
      { fraud_prover: SIGNER_HASH, data: stageData.prepared },
      PreparedCanonicalDecodeItemDatum,
    ),
    observedDatum: Data.to(
      { fraud_prover: SIGNER_HASH, data: stageData.observed },
      ObservedCanonicalDecodeItemDatum,
    ),
    verifiedDatum: Data.to(
      { fraud_prover: SIGNER_HASH, data: stageData.verified },
      VerifiedCanonicalDecodeItemDatum,
    ),
  };
};

export type CompleteSignedTransactionMeasurement = {
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
export const sameDatumValue = (left: string, right: string): boolean =>
  left === right || Data.to(Data.from(left)) === Data.to(Data.from(right));

export const measureCompleteSignedTransaction = (
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

export type StageContract = {
  readonly spendingScriptAddress: string;
  readonly spendingScript: Script;
};

export type EmulatorHarness = {
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

export let cachedContracts:
  | ValidationTraceDisputeFaultProofContracts
  | undefined;

export const loadContracts =
  async (): Promise<ValidationTraceDisputeFaultProofContracts> => {
    cachedContracts ??= await Effect.runPromise(
      buildValidationTraceDisputeFaultProofContracts({
        blueprint: parseFaultProofBlueprint(blueprintJson),
        network: "Custom",
        hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
        fraudProofCataloguePolicyId: FRAUD_PROOF_CATALOGUE_POLICY_ID,
        referenceScriptAuthPolicyId: "33".repeat(28),
      }),
    );
    return cachedContracts;
  };

export const setupEmulator = async (
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
