import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  hashMidgardValidationMachineStateV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
} from "@al-ft/midgard-core";
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
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
  parseFaultProofBlueprint,
  PreparedValidationResolutionDatumV1,
  requireInputIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  validationMachineStateDataFromCore,
  ValidationProofItemDatumV1,
  type ValidationProofItemPublicationV1,
  type ValidationTraceDisputeFaultProofContracts,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
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
import { describe, expect, it } from "vitest";

import { validationOneStepEvidenceHashV1 } from "../../midgard-fault-proofs/src/validation-dispute/submit.js";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOpV1,
  buildValidationMachineLedgerMutationSteps,
  buildValidationOneStepArgumentV1,
  type DeterministicValidationMachineTrace,
  type ValidationOneStepArgumentV1,
} from "../src/index.js";
import {
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
  TEST_ADDRESS_BYTES,
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

const encodeBoundedBytesItem = (payload: Buffer): Buffer => {
  if (payload.length < 24) {
    return Buffer.concat([Buffer.from([0x40 + payload.length]), payload]);
  }
  if (payload.length <= 0xff) {
    return Buffer.concat([Buffer.from([0x58, payload.length]), payload]);
  }
  throw new Error("bounded datum chunk must stay below 256 bytes");
};

const canonicalDatumFiller = (payloadBytes: number): Buffer => {
  if (payloadBytes <= 64) {
    return encodeBoundedBytesItem(Buffer.alloc(payloadBytes, 0xa5));
  }
  const items: Buffer[] = [];
  let remaining = payloadBytes;
  while (remaining > 0) {
    const take = Math.min(remaining, 64);
    items.push(encodeBoundedBytesItem(Buffer.alloc(take, 0xa5)));
    remaining -= take;
  }
  return Buffer.concat([Buffer.from([0x9f]), ...items, Buffer.from([0xff])]);
};

const makeExactSizeOutputItem = (targetItemBytes: number): Buffer => {
  const probe = (payloadBytes: number): Buffer =>
    encodeMidgardTxOutput({
      address: TEST_ADDRESS_BYTES,
      value: { lovelace: 10n, assets: new Map() },
      datum: { kind: "inline", cbor: canonicalDatumFiller(payloadBytes) },
    });
  let payload = Math.max(0, targetItemBytes - probe(0).length);
  for (let attempt = 0; attempt < 12; attempt += 1) {
    const candidate = probe(payload);
    const delta = targetItemBytes - candidate.length;
    if (delta === 0) {
      return candidate;
    }
    payload += delta;
  }
  throw new Error(
    `could not converge on an exact ${targetItemBytes.toString()}-byte output item`,
  );
};

/** The §5.1 outputs field, which is the field every case here carries. */
const OUTPUT_FIELD_INDEX = 2;

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
  const spentOutput = makeOutput(10n);
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
  return {
    trace,
    stateIndex,
    itemBytes,
    argument,
    transitionData: Data.from(argument.transitionCbor.toString("hex")),
    carriageData,
    fieldPreimageHex: carriageData.fields[0],
    evidenceHash: validationOneStepEvidenceHashV1({
      transitionCbor: argument.transitionCbor,
      auxiliaryCbor: argument.auxiliaryCbor,
    }),
    preState: validationMachineStateDataFromCore(trace.states[stateIndex]!),
    claimedSuccessorHash: hashMidgardValidationMachineStateV1(
      trace.states[stateIndex + 1]!,
    ).toString("hex"),
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
};

const preparedThreadDatumCbor = (
  itemCase: CanonicalDecodeItemCase,
  signerHash: string,
): string =>
  Data.to(
    {
      fraud_prover: signerHash,
      data: {
        version: 1n,
        resolution: {
          version: 1n,
          pre_state: itemCase.preState,
          operator_successor_hash: itemCase.claimedSuccessorHash,
          challenger_successor_hash: itemCase.claimedSuccessorHash,
        },
        evidence_hash: itemCase.evidenceHash,
      },
    },
    PreparedValidationResolutionDatumV1,
  );

const authenticatedOutputDatumCbor = (
  itemCase: CanonicalDecodeItemCase,
  signerHash: string,
): string => {
  const prepared = Data.from(
    preparedThreadDatumCbor(itemCase, signerHash),
    PreparedValidationResolutionDatumV1,
  );
  if (prepared.data === null) {
    throw new Error("prepared thread datum is missing its state");
  }
  return Data.to(
    {
      fraud_prover: signerHash,
      data: {
        version: 1n,
        base: prepared.data,
        transition: {
          work_witness_cbor: workWitnessCborHex(itemCase),
          claimed_successor: validationMachineStateDataFromCore(
            itemCase.trace.states[itemCase.stateIndex + 1]!,
          ),
        },
      },
    },
    AuthenticatedCanonicalDecodeItemDatumV1,
  );
};

const workWitnessCborHex = (itemCase: CanonicalDecodeItemCase): string => {
  const transition = itemCase.transitionData;
  if (!(transition instanceof Constr) || transition.fields.length !== 2) {
    throw new Error("one-step transition has an unexpected shape");
  }
  const workWitness = transition.fields[0];
  if (typeof workWitness !== "string") {
    throw new Error("one-step transition work witness must be bytes");
  }
  return workWitness;
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
  };
};

const submitSemanticProof = async ({
  harness,
  itemCase,
  threadUtxo,
  proofItemReferenceUtxo,
}: {
  readonly harness: EmulatorHarness;
  readonly itemCase: CanonicalDecodeItemCase;
  readonly threadUtxo: UTxO;
  readonly proofItemReferenceUtxo?: UTxO;
}): Promise<{
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly outputDatum: string;
  readonly signedCbor: string;
}> => {
  const outputDatum = authenticatedOutputDatumCbor(
    itemCase,
    harness.signerHash,
  );
  const makeRedeemer: BuildTxWithRedeemer = (ctx) => {
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "complete-item semantic proof",
    );
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      (output) =>
        output.address === harness.itemSourceAddress &&
        output.datum != null &&
        sameDatumValue(output.datum, outputDatum) &&
        output.assets[harness.threadUnit] === 1n,
      "complete-item semantic proof",
    );
    if (proofItemReferenceUtxo !== undefined) {
      return Data.to(
        new Constr(1, [
          new Constr(1, [
            inputIndex,
            outputIndex,
            itemCase.transitionData,
            requireReferenceInputIndex(
              ctx,
              proofItemReferenceUtxo,
              "complete-item semantic proof",
            ),
          ]),
        ]),
      );
    }
    return Data.to(
      new Constr(1, [
        new Constr(0, [
          inputIndex,
          outputIndex,
          itemCase.transitionData,
          itemCase.carriageData,
        ]),
      ]),
    );
  };
  const walletUtxos = (await harness.lucid.wallet().getUtxos()).filter(
    (utxo) => utxo.assets[harness.threadUnit] === undefined,
  );
  const feeInput = walletUtxos.reduce((left, right) =>
    (left.assets.lovelace ?? 0n) >= (right.assets.lovelace ?? 0n)
      ? left
      : right,
  );
  let tx = harness.lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], makeRedeemer);
  if (proofItemReferenceUtxo !== undefined) {
    tx = tx.readFrom([proofItemReferenceUtxo]);
  }
  tx = tx.pay
    .ToContract(
      harness.itemSourceAddress,
      { kind: "inline", value: outputDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [harness.threadUnit]: 1n,
      },
    )
    .addSignerKey(harness.signerHash)
    .attach.SpendingValidator(harness.semanticScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const txHash = await signed.submit();
  await harness.lucid.awaitTx(txHash);
  return {
    measurement: measureCompleteSignedTransaction(signedCbor),
    outputDatum,
    signedCbor,
  };
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

const measureDirectAt = async (
  itemBytes: number,
): Promise<{
  readonly itemCase: CanonicalDecodeItemCase;
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly outputDatum: string;
}> => {
  const itemCase = await buildCanonicalDecodeItemCase(itemBytes);
  const harness = await setupEmulator([
    preparedThreadDatumCbor(itemCase, SIGNER_HASH),
  ]);
  const result = await submitSemanticProof({
    harness,
    itemCase,
    threadUtxo: harness.threadUtxos[0]!,
  });
  return { itemCase, ...result };
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
  const harness = await setupEmulator([
    preparedThreadDatumCbor(itemCase, SIGNER_HASH),
  ]);
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
  // Measured by `loadContracts()` here, the same producer this row asserts on.
  it("pins the applied §3.2 necessity identities on the measurement deployment", async () => {
    const contracts = await loadContracts();
    expect(contracts.validationTraceDispute.semanticResolvers).toHaveLength(76);
    expect(
      contracts.validationTraceDispute.semanticResolvers[1]!.spendingScriptHash,
    ).toBe("69fd502baf0a512c3275671515da07aaa6c049cec2fd430e55d61cc9");
    expect(contracts.validationTraceDispute.proofItem.spendingScriptHash).toBe(
      "22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab",
    );
  }, 120_000);

  it("measures applied direct authentication at the staged reliability boundary", async () => {
    // The observation stage is the limiting direct-carriage transaction and
    // is pinned by the full five-stage fault-proof emulator lifecycle. This
    // narrower validator test proves that the preceding authentication stage
    // also fits when it embeds the applied script and exact boundary item.
    const reliableItemBytes =
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
    const authentication = await measureDirectAt(reliableItemBytes);
    expect(authentication.measurement.redeemerCount).toBe(1);
    expect(authentication.measurement.completeSignedBytes).toBeLessThanOrEqual(
      MAX_L1_PROOF_TX_BYTES -
        MIDGARD_V1_ENVELOPE_MEASUREMENTS.proofItemEnvelopeReliabilityReserveBytes,
    );
    expect(
      Number(authentication.measurement.executionMemory),
    ).toBeLessThanOrEqual(RESERVED_MEMORY_UNITS);
    expect(
      Number(authentication.measurement.executionSteps),
    ).toBeLessThanOrEqual(RESERVED_CPU_UNITS);
    expect(authentication.measurement.referenceInputCount).toBe(0);
    expect(authentication.measurement.completeSignedBytes).toBeLessThanOrEqual(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemAuthenticationTransactionBytes,
    );

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
              authenticationTransaction: authentication.measurement,
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

  it("reaches the identical terminal state through direct and reference carriage of the same item", async () => {
    // Same complete item, both representations, one emulator: the deployed
    // validator must accept both and bind the identical continuation state.
    const itemBytes =
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
    const itemCase = await buildCanonicalDecodeItemCase(itemBytes);
    const threadDatum = preparedThreadDatumCbor(itemCase, SIGNER_HASH);
    const harness = await setupEmulator([threadDatum, threadDatum]);
    const [directThread, referenceThread] = harness.threadUtxos;

    const direct = await submitSemanticProof({
      harness,
      itemCase,
      threadUtxo: directThread!,
    });
    const publication = await publishProofItem({ harness, itemCase });
    const reference = await submitSemanticProof({
      harness,
      itemCase,
      threadUtxo: referenceThread!,
      proofItemReferenceUtxo: publication.utxo,
    });

    expect(direct.outputDatum).toBe(reference.outputDatum);
    expect(direct.measurement.completeSignedBytes).toBeLessThanOrEqual(
      MAX_L1_PROOF_TX_BYTES,
    );
    expect(reference.measurement.completeSignedBytes).toBeLessThanOrEqual(
      MAX_L1_PROOF_TX_BYTES,
    );
    const directTerminal = (
      await harness.lucid.utxosAt(harness.itemSourceAddress)
    ).map((utxo) => utxo.datum ?? "");
    expect(directTerminal).toHaveLength(2);
    expect(new Set(directTerminal).size).toBe(1);
  }, 600_000);

  it("rejects substituted and trailing-byte items through the deployed reference route", async () => {
    const itemBytes = 12_000;
    const itemCase = await buildCanonicalDecodeItemCase(itemBytes);
    const threadDatum = preparedThreadDatumCbor(itemCase, SIGNER_HASH);
    const harness = await setupEmulator([threadDatum, threadDatum]);

    // Substitution: same length, one flipped byte deep inside the published
    // field preimage. The door hashes the whole preimage against the committed
    // field hash, so a single flipped byte anywhere inside it fails closed.
    //
    // #579: the offset is taken from the preimage's OWN length. It used to be
    // `itemBytes - 100`, an index into a buffer that the selection defect above
    // had made ~40 bytes long — the write landed past the end, `Buffer` swallowed
    // it, and the "substituted" preimage was byte-identical to the original. The
    // row then proved nothing and resolved. The equality guard below is what
    // keeps that from being silent again: a test that mutates a buffer must show
    // the mutation took before it can claim the mutation was rejected.
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
      submitSemanticProof({
        harness,
        itemCase,
        threadUtxo: harness.threadUtxos[0]!,
        proofItemReferenceUtxo: substitutedPublication.utxo,
      }),
    ).rejects.toThrow();

    // Trailing data: the exact item plus one extra byte.
    const trailingPublication = await publishRawProofItemForNegativeControl({
      harness,
      itemCase,
      fieldPreimage: `${itemCase.fieldPreimageHex}00`,
    });
    await expect(
      submitSemanticProof({
        harness,
        itemCase,
        threadUtxo: harness.threadUtxos[1]!,
        proofItemReferenceUtxo: trailingPublication.utxo,
      }),
    ).rejects.toThrow();
  }, 600_000);
});
