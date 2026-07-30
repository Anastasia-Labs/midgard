import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  hashMidgardValidationMachineStateV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
} from "@al-ft/midgard-core";
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  AuthenticatedCanonicalDecodeItemDatumV1,
  BoundedCollectionItemProofV1,
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
const MAX_L1_PROOF_TX_BYTES = MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes;

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
  readonly collectionProofData: Data;
  readonly itemCborHex: string;
  readonly evidenceHash: string;
  readonly preState: ReturnType<typeof validationMachineStateDataFromCore>;
  readonly claimedSuccessorHash: string;
};

const buildCanonicalDecodeItemCase = async (
  itemBytes: number,
): Promise<CanonicalDecodeItemCase> => {
  const item = makeExactSizeOutputItem(itemBytes);
  const trace = await buildTraceWithOutputs([item]);
  let stateIndex = -1;
  for (let index = 0; index < trace.witnesses.length; index += 1) {
    const witness = trace.witnesses[index]!;
    if (
      witness.phase === "canonicalDecode" &&
      witness.auxiliary?.kind === "transactionFieldItem" &&
      witness.auxiliary.itemCbor.length === itemBytes
    ) {
      stateIndex = index;
      break;
    }
  }
  if (stateIndex < 0) {
    throw new Error(
      `trace has no canonicalDecode complete-item witness of ${itemBytes.toString()} bytes`,
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
    auxiliary.fields.length !== 2 ||
    typeof auxiliary.fields[1] !== "string"
  ) {
    throw new Error("complete-item auxiliary witness has an unexpected shape");
  }
  return {
    trace,
    stateIndex,
    itemBytes,
    argument,
    transitionData: Data.from(argument.transitionCbor.toString("hex")),
    collectionProofData: auxiliary.fields[0]!,
    itemCborHex: auxiliary.fields[1],
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
  left === right ||
  Data.to(Data.from(left)) === Data.to(Data.from(right));

const measureCompleteSignedTransaction = (
  transactionCbor: string,
): CompleteSignedTransactionMeasurement => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  const body = transaction.body();
  const redeemers = transaction.witness_set().redeemers()?.as_arr_legacy_redeemer();
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
  const semanticContract = contracts.validationTraceDispute.semanticResolvers[1];
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
  const outputDatum = authenticatedOutputDatumCbor(itemCase, harness.signerHash);
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
          itemCase.collectionProofData,
          itemCase.itemCborHex,
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
  const tx = harness.lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], makeRedeemer)
    .readFrom(
      proofItemReferenceUtxo === undefined
        ? []
        : [proofItemReferenceUtxo],
    )
    .pay.ToContract(
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

const publishProofItem = async ({
  harness,
  itemCase,
  itemCborHexOverride,
}: {
  readonly harness: EmulatorHarness;
  readonly itemCase: CanonicalDecodeItemCase;
  readonly itemCborHexOverride?: string;
}): Promise<{
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly utxo: UTxO;
  readonly datumCbor: string;
  readonly minAdaLovelace: bigint;
}> => {
  const collectionProof = Data.from(
    Data.to(itemCase.collectionProofData),
    BoundedCollectionItemProofV1,
  );
  const preState = itemCase.preState;
  const publication = deriveValidationProofItemPublicationV1({
    transactionId: preState.transaction_id,
    transactionCommitment: preState.transaction_commitment,
    collectionProof,
    itemCbor: itemCborHexOverride ?? itemCase.itemCborHex,
  });
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

const measureReferenceAt = async (
  itemBytes: number,
): Promise<{
  readonly itemCase: CanonicalDecodeItemCase;
  readonly publication: Awaited<ReturnType<typeof publishProofItem>>;
  readonly consumption: {
    readonly measurement: CompleteSignedTransactionMeasurement;
    readonly outputDatum: string;
    readonly signedCbor: string;
  };
}> => {
  const itemCase = await buildCanonicalDecodeItemCase(itemBytes);
  const harness = await setupEmulator([
    preparedThreadDatumCbor(itemCase, SIGNER_HASH),
  ]);
  const publication = await publishProofItem({ harness, itemCase });
  const consumption = await submitSemanticProof({
    harness,
    itemCase,
    threadUtxo: harness.threadUtxos[0]!,
    proofItemReferenceUtxo: publication.utxo,
  });
  return { itemCase, publication, consumption };
};

describe("complete-item proof fit V1 (emulator, applied validators)", () => {
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
    ).toBeLessThanOrEqual(
      RESERVED_MEMORY_UNITS,
    );
    expect(
      Number(authentication.measurement.executionSteps),
    ).toBeLessThanOrEqual(
      RESERVED_CPU_UNITS,
    );
    expect(authentication.measurement.referenceInputCount).toBe(0);
    expect(authentication.measurement.completeSignedBytes).toBe(
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

  it("measures inline-datum publication plus reference-input consumption at the publication maximum", async () => {
    const publicationMax =
      MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes;
    const { itemCase, publication, consumption } =
      await measureReferenceAt(publicationMax);
    expect(itemCase.itemBytes).toBe(publicationMax);

    expect(publication.measurement.redeemerCount).toBe(0);
    expect(
      (publication.utxo.assets.lovelace ?? 0n) >= publication.minAdaLovelace,
    ).toBe(true);

    expect(consumption.measurement.referenceInputCount).toBe(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.referenceCompleteItemProofReferenceInputCount,
    );
    expect(consumption.measurement.completeSignedBytes).toBeLessThanOrEqual(
      MAX_L1_PROOF_TX_BYTES,
    );
    expect(Number(consumption.measurement.executionMemory)).toBeLessThanOrEqual(
      RESERVED_MEMORY_UNITS,
    );
    expect(Number(consumption.measurement.executionSteps)).toBeLessThanOrEqual(
      RESERVED_CPU_UNITS,
    );
    // The consuming transaction resolves the item from the UTxO set instead
    // of serializing it again.
    expect(consumption.measurement.completeSignedBytes).toBeLessThan(
      publication.measurement.completeSignedBytes / 4,
    );

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      const contracts = await loadContracts();
      console.info(
        JSON.stringify(
          {
            completeItemPublicationProofFitV1: {
              proofItemScriptHash:
                contracts.validationTraceDispute.proofItem.spendingScriptHash,
              publicationItemBytes: publicationMax,
              publication: {
                ...publication.measurement,
                datumBytes: publication.datumCbor.length / 2,
                minAdaLovelace: publication.minAdaLovelace,
              },
              referenceConsumption: consumption.measurement,
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

    // Substitution: same length, one flipped byte deep inside the item.
    const substituted = Buffer.from(itemCase.itemCborHex, "hex");
    substituted[itemBytes - 100] = substituted[itemBytes - 100]! ^ 0x01;
    const substitutedPublication = await publishProofItem({
      harness,
      itemCase,
      itemCborHexOverride: substituted.toString("hex"),
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
    const trailingPublication = await publishProofItem({
      harness,
      itemCase,
      itemCborHexOverride: `${itemCase.itemCborHex}00`,
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
