import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  asArray,
  asBytes,
  decodeSingleCbor,
  hashMidgardValidationMachineState,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import {
  type MidgardFieldCarriagePlan,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core/codec/native-tx-carriage";
import {
  encodeMidgardFieldPreimage,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  type MidgardFieldCarriage,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  deriveCanonicalDecodeItemStageData,
  validationOneStepEvidenceHash,
} from "@al-ft/midgard-fault-proofs";
import {
  assertMidgardFieldCarriageResolvesAtDoor,
  AuthenticatedCanonicalDecodeItemDatum,
  buildUnsignedFieldPreimageCertificationProgram,
  buildUnsignedFieldPreimagePublicationProgram,
  buildValidationTraceDisputeFaultProofContracts,
  fieldPreimagePublicationOutputs,
  ObservedCanonicalDecodeItemDatum,
  parseFaultProofBlueprint,
  PreparedCanonicalDecodeItemDatum,
  PreparedValidationResolutionDatum,
  type PreparedValidationResolutionDatum as PreparedValidationResolutionDatumData,
  requireInputIndex,
  requireUniqueOutputIndex,
  resolveMidgardFieldCarriageAgainstReferenceInputs,
  validationMachineStateDataFromCore,
  ValidationOneStepWitness,
  type ValidationOneStepWitness as ValidationOneStepWitnessData,
  type ValidationTraceDisputeFaultProofContracts,
  VerifiedCanonicalDecodeItemDatum,
} from "@al-ft/midgard-sdk";
import {
  applyDoubleCborEncoding,
  applyParamsToScript,
  type BuildTxWithRedeemer,
  CML,
  Constr,
  credentialToAddress,
  Data,
  Emulator,
  Lucid,
  type LucidEvolution,
  type MintingPolicy,
  mintingPolicyToId,
  PROTOCOL_PARAMETERS_DEFAULT,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  buildValidationOneStepArgument,
  type DeterministicValidationMachineTrace,
  encodeValidationOneStepWitnessCbor,
  type ValidationMachineFieldCarriageResolver,
} from "../src/index.js";
import {
  fundingLovelaceForOutputs,
  makeMinAdaFundedExactSizeOutputItem,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
} from "./validation-fixtures.js";

/**
 * **The tiers-2/3 leg of the canonical-decode staged chain, against the applied
 * validators** — `docs/spec/midgard-tx.md` §8, issues #600 and #579.
 *
 * #600 landed the producer and cross-language-vector halves of the §8.4
 * carriage seam and stopped there on the orchestrator's Ruling 2: a tiers-2/3
 * emulator row built against the then-frozen blueprint would have failed with
 * the same `Spend[1] unexpected empty list` signature as five unrelated rows,
 * whatever its own correctness, and an unfalsifiable red proves nothing. #579's
 * regeneration removed that obstacle. This file is the row that was owed.
 *
 * **What is actually exercised.** `canonical_decode_item_observe_v1` is the one
 * stage of the complete-item chain that *dereferences* the carriage — the
 * semantic and source stages only hash it into `evidence_hash` — so it is the
 * only place a tier-2 or tier-3 index means anything, and it is the only stage
 * carrying the §8.6 `field_preimage_certificate_policy_id` parameter. Each row
 * below therefore drives the whole chain (`Verify` → source → `Observe` → proof
 * → settlement) over a field-2 preimage §8.4 places above the 14,336-byte
 * tier-1 cap, with the carriage published to, and resolved back out of, a real
 * emulator ledger.
 *
 * **Nothing here is hand-written.** The preimage comes from the machine's own
 * trace, the tier from `selectMidgardFieldCarriageTier`, the plan from
 * `planMidgardFieldCarriage`, the publications and the §8.6 certification
 * from the SDK builders, the indices from
 * `resolveMidgardFieldCarriageAgainstReferenceInputs` resolving by content
 * against the door transaction's own reference-input set, the committed
 * evidence from `buildValidationOneStepArgument`, and the staged datums from
 * `deriveCanonicalDecodeItemStageData` — the same function the production
 * submitter uses. There is no CBOR literal and no copied hash in this file.
 *
 * **The certificate is the compiled one.** Tier 3's door checks the named
 * reference input for one unit of the policy the observe validator was
 * *parameterised* with, so a stand-in policy cannot reach it: the certificate is
 * minted by `field_preimage_certificate.field_preimage_certificate.mint` out of
 * the same blueprint, and the parameterisation is pinned by rebuilding the
 * observe validator from that policy id and comparing script hashes.
 */

const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");
const blueprintJson = JSON.parse(readFileSync(blueprintPath, "utf8")) as {
  readonly validators: readonly {
    readonly title: string;
    readonly compiledCode: string;
  }[];
};

const compiledScript = (title: string): string => {
  const validator = blueprintJson.validators.find(
    (candidate) => candidate.title === title,
  );
  if (validator === undefined) {
    throw new Error(`Validator with title "${title}" not found`);
  }
  return applyDoubleCborEncoding(validator.compiledCode);
};

const NETWORK = "Custom" as const;
const HUB_ORACLE_POLICY_ID = "11".repeat(28);
const FRAUD_PROOF_CATALOGUE_POLICY_ID = "22".repeat(28);
const THREAD_ASSET_NAME = "aa".repeat(32);

const SIGNING_KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 7));
const SIGNER_HASH = Buffer.from(
  SIGNING_KEY.to_public().hash().to_raw_bytes(),
).toString("hex");

const MAX_L1_TX_BYTES = MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes;

/** §2.5 field 2 — the outputs field, the one these traces read complete items of. */
const OUTPUT_FIELD_INDEX = 2;

/**
 * `NoAuxiliaryWitness` as the committed evidence names it — the Option B
 * (#620) auxiliary half of the canonical-decode resolver's `evidence_hash`.
 * Same literal as `complete-item-route-adversarial-emulator.test.ts`.
 */
const NO_AUXILIARY_WITNESS_CBOR = Buffer.from("d87980", "hex");

// ## The disputed transaction, and the step that reads its field 2

/**
 * RE-AUTHORED, NOT SUPPRESSED (#618 ruling 1; R8 of decision 0005). This file
 * used to carry its own copy of the exact-size item builder, producing
 * 10-lovelace items that the ValueAndMint output-descriptor scan now convicts
 * with `E_MIN_ADA`. The shared builder funds each item at its own minimum-Ada
 * floor without moving its length, so every carriage measurement below
 * measures the same number of bytes it did before the wiring.
 */
const makeExactSizeOutputItem = makeMinAdaFundedExactSizeOutputItem;

const traceContext = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  eventKeyCbor: Buffer.from("d8799f4100ff", "hex"),
  sourceKind: "normal" as const,
  blockEndTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};

/**
 * The two outputs whose §5.1 field-2 envelope is **exactly** `targetBytes`.
 *
 * Two rather than one, and that is the whole reason this helper exists. §8.4
 * partitions on the *field*'s preimage while the trace producer routes on the
 * *item*: an output past
 * `MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes` is decoded
 * by canonicalDecode's chunked route and emits no complete-item witness at all,
 * so a single-output field cannot reach tier 3 through this chain — its
 * complete-item step stops existing several thousand bytes below `chunk_bytes_k`.
 * Splitting the same byte count across two admissible items keeps every item on
 * the complete-item route while the field they belong to climbs the ladder,
 * which is the ordinary shape of a large transaction anyway.
 *
 * The envelope is `82 ‖ 59 LLLL ‖ item0 ‖ 59 LLLL ‖ item1` — seven bytes of
 * framing — and the result is checked against `encodeMidgardFieldPreimage`
 * rather than trusted.
 */
const outputsForFieldTwoPreimageBytes = (
  targetBytes: number,
): readonly Buffer[] => {
  const payload = targetBytes - 7;
  const first = Math.floor(payload / 2);
  const outputs = [
    makeExactSizeOutputItem(first),
    makeExactSizeOutputItem(payload - first),
  ];
  const measured = encodeMidgardFieldPreimage(outputs).length;
  if (measured !== targetBytes) {
    throw new Error(
      `two-output field-2 envelope measured ${measured.toString()} bytes, wanted ${targetBytes.toString()}`,
    );
  }
  for (const output of outputs) {
    if (
      output.length >
      MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes
    ) {
      throw new Error(
        "output item is past the producer's complete-item threshold and would be chunked",
      );
    }
  }
  return outputs;
};

const buildTraceWithOutputs = async (
  outputs: readonly Buffer[],
): Promise<DeterministicValidationMachineTrace> => {
  const spent = outRefFromByte(0x11);
  // Every generated item pays 10 lovelace, so the one input has to fund all of
  // them or the machine rejects with `E_VALUE_NOT_PRESERVED` before it ever
  // reaches canonicalDecode.
  // The resolved input has to fund every produced output now that each is
  // funded at its own minimum-Ada floor, or stage five would convict this
  // trace with `E_VALUE_NOT_PRESERVED` instead of accepting it. The fee is
  // zero, so the sum is exact.
  const spentOutput = makeOutput(fundingLovelaceForOutputs(outputs));
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    outputs: [...outputs],
  });
  const expectedLedgerOps = [
    { type: "delete" as const, key: spent },
    ...outputs.map((output, index) =>
      buildValidationMachineLedgerInsertOp({
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

/**
 * The `canonicalDecode` complete-item step for **field 2**, located by the bytes
 * it read.
 *
 * Selecting on `(phase, kind)` alone is not enough and the difference is not
 * cosmetic: the canonicalDecode walk emits a complete-item witness for field 0
 * first, whose preimage is a few dozen bytes, so a first-match selector silently
 * measures the wrong field — every tier assertion below would then be about a
 * tier-1 preimage. Matching on the preimage length is the same discipline
 * `complete-item-proof-fit.test.ts` keeps.
 */
const findFieldTwoCompleteItemStep = (
  trace: DeterministicValidationMachineTrace,
  expectedPreimageBytes: number,
): { readonly stateIndex: number; readonly fieldPreimage: Buffer } => {
  for (let index = 0; index < trace.witnesses.length; index += 1) {
    const witness = trace.witnesses[index]!;
    if (
      witness.phase !== "canonicalDecode" ||
      witness.auxiliary?.kind !== "transactionFieldItem" ||
      witness.auxiliary.fieldIndex !== OUTPUT_FIELD_INDEX ||
      witness.auxiliary.fieldPreimage.length !== expectedPreimageBytes
    ) {
      continue;
    }
    return {
      stateIndex: index,
      fieldPreimage: witness.auxiliary.fieldPreimage,
    };
  }
  throw new Error(
    `trace has no canonicalDecode field-2 complete-item witness of ${expectedPreimageBytes.toString()} preimage bytes`,
  );
};

// ## The deployed contracts, and the §8.6 policy the door is parameterised by

const OBSERVE_TITLE =
  "fraud_proofs/validation_trace/canonical_decode_item_observe_v1.main.spend";
const CERTIFICATE_MINT_TITLE =
  "field_preimage_certificate.field_preimage_certificate.mint";

let cachedContracts: ValidationTraceDisputeFaultProofContracts | undefined;
const loadContracts =
  async (): Promise<ValidationTraceDisputeFaultProofContracts> => {
    cachedContracts ??= await Effect.runPromise(
      buildValidationTraceDisputeFaultProofContracts({
        blueprint: parseFaultProofBlueprint(
          JSON.parse(JSON.stringify(blueprintJson)),
        ),
        network: NETWORK,
        hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
        fraudProofCataloguePolicyId: FRAUD_PROOF_CATALOGUE_POLICY_ID,
      }),
    );
    return cachedContracts;
  };

/**
 * The compiled §8.6 certificate policy — mint and spend are handlers of one
 * validator, so the policy id *is* the address's payment credential, and the
 * mint handler refuses any output carrying a stake credential.
 */
const certificatePolicy = (): {
  readonly script: MintingPolicy;
  readonly policyId: string;
  readonly address: string;
} => {
  const script: MintingPolicy = {
    type: "PlutusV3",
    script: compiledScript(CERTIFICATE_MINT_TITLE),
  };
  const policyId = mintingPolicyToId(script);
  return {
    script,
    policyId,
    address: credentialToAddress(NETWORK, scriptHashToCredential(policyId)),
  };
};

// ## Emulator harness

type Harness = {
  readonly lucid: LucidEvolution;
  readonly contracts: ValidationTraceDisputeFaultProofContracts;
  readonly threadUnit: string;
  readonly walletAddress: string;
};

const WALLET_ADDRESS = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(SIGNING_KEY.to_public().hash()),
)
  .to_address()
  .to_bech32();

const setupEmulator = async (): Promise<Harness> => {
  const contracts = await loadContracts();
  const threadUnit = toUnit(
    contracts.computationThread.policyId,
    THREAD_ASSET_NAME,
  );
  const emulator = new Emulator(
    [
      {
        seedPhrase: "",
        privateKey: SIGNING_KEY.to_bech32(),
        address: WALLET_ADDRESS,
        assets: { lovelace: 500_000_000_000n },
      },
      {
        // The computation-thread token, held by the prover until the chain
        // starts. Minting it would need the real thread policy and a hub
        // oracle; what this file is about is the carriage, so the token is
        // seeded and the thread's own authenticity is out of scope here (it is
        // covered by the fault-proofs lifecycle suites).
        seedPhrase: "",
        privateKey: SIGNING_KEY.to_bech32(),
        address: WALLET_ADDRESS,
        assets: { lovelace: 100_000_000n, [threadUnit]: 1n },
      },
    ],
    { ...PROTOCOL_PARAMETERS_DEFAULT, maxTxSize: MAX_L1_TX_BYTES },
  );
  const lucid = await Lucid(emulator, NETWORK);
  lucid.selectWallet.fromPrivateKey(SIGNING_KEY.to_bech32());
  return { lucid, contracts, threadUnit, walletAddress: WALLET_ADDRESS };
};

/**
 * CML normalises Plutus datums when it frames a transaction while lucid's
 * `Data.to` emits Aiken-style indefinite arrays; the validators compare parsed
 * values, so datum equality here is value-level too.
 */
const sameDatumValue = (left: string, right: string): boolean =>
  left === right || Data.to(Data.from(left)) === Data.to(Data.from(right));

const signedByteCount = (transactionCbor: string): number =>
  transactionCbor.length / 2;

const feeInputFor = async (harness: Harness): Promise<UTxO> => {
  const candidates = (await harness.lucid.wallet().getUtxos()).filter(
    (utxo) => utxo.assets[harness.threadUnit] === undefined,
  );
  const selected = candidates.reduce((left, right) =>
    (left.assets.lovelace ?? 0n) >= (right.assets.lovelace ?? 0n)
      ? left
      : right,
  );
  return selected;
};

const submitAndAwait = async (
  harness: Harness,
  unsigned: Awaited<
    ReturnType<ReturnType<LucidEvolution["newTx"]>["complete"]>
  >,
): Promise<{ readonly txHash: string; readonly signedCbor: string }> => {
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const txHash = await signed.submit();
  await harness.lucid.awaitTx(txHash);
  return { txHash, signedCbor };
};

/** Publishes a validator as a plain reference script, parked out of reach of coin selection. */
const publishReferenceScript = async (
  harness: Harness,
  script: Script,
): Promise<UTxO> => {
  const parkAddress = credentialToAddress(
    NETWORK,
    scriptHashToCredential("2f".repeat(28)),
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
  const { txHash, signedCbor } = await submitAndAwait(harness, unsigned);
  expect(signedByteCount(signedCbor)).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
  const outputs = CML.Transaction.from_cbor_hex(signedCbor).body().outputs();
  let scriptRefOutputIndex = -1;
  for (let index = 0; index < outputs.len(); index += 1) {
    if (outputs.get(index).script_ref() !== undefined) {
      scriptRefOutputIndex = index;
      break;
    }
  }
  if (scriptRefOutputIndex < 0) {
    throw new Error(
      "reference-script publication omitted its script-ref output",
    );
  }
  const published = await harness.lucid.utxosByOutRef([
    { txHash, outputIndex: scriptRefOutputIndex },
  ]);
  const utxo = published[0];
  if (published.length !== 1 || utxo === undefined || utxo.scriptRef == null) {
    throw new Error("published reference script was not found");
  }
  return utxo;
};

// ## The carriage, published to and resolved back out of the ledger

type PublishedCarriage = {
  readonly plan: MidgardFieldCarriagePlan;
  readonly chunkUtxos: readonly UTxO[];
  readonly certificateUtxo: UTxO | undefined;
  readonly certificatePolicyId: string;
  readonly publicationBytes: readonly number[];
  readonly certificationBytes: number | undefined;
};

const publishCarriage = async ({
  harness,
  plan,
  compactCbor,
  witnessSetCompactCbor,
}: {
  readonly harness: Harness;
  readonly plan: MidgardFieldCarriagePlan;
  readonly compactCbor: string;
  readonly witnessSetCompactCbor: string;
}): Promise<PublishedCarriage> => {
  const policy = certificatePolicy();
  const chunkUtxos: UTxO[] = [];
  const publicationBytes: number[] = [];
  for (const publication of fieldPreimagePublicationOutputs(plan)) {
    const unsigned = await Effect.runPromise(
      buildUnsignedFieldPreimagePublicationProgram(harness.lucid, {
        publication,
        publisherAddress: harness.walletAddress,
      }),
    );
    const { txHash, signedCbor } = await submitAndAwait(harness, unsigned);
    publicationBytes.push(signedByteCount(signedCbor));
    const published = (await harness.lucid.utxosAt(harness.walletAddress)).find(
      (utxo) =>
        utxo.txHash === txHash &&
        utxo.datum != null &&
        sameDatumValue(utxo.datum, publication.datumCbor),
    );
    if (published === undefined) {
      throw new Error(
        `published carriage chunk ${publication.chunkIndex.toString()} was not found`,
      );
    }
    chunkUtxos.push(published);
  }
  if (plan.tier !== "Certified") {
    return {
      plan,
      chunkUtxos,
      certificateUtxo: undefined,
      certificatePolicyId: policy.policyId,
      publicationBytes,
      certificationBytes: undefined,
    };
  }
  const unsigned = await Effect.runPromise(
    buildUnsignedFieldPreimageCertificationProgram(harness.lucid, {
      plan,
      certificatePolicyId: policy.policyId,
      certificateAddress: policy.address,
      certificateWitness: {
        kind: "inline_emulator_only",
        certificateScript: policy.script,
      },
      // Handed in reverse ledger order on purpose: the redeemer's indices must
      // be indices into the canonically-sorted reference-input list, not into
      // the order the builder was given.
      chunkUtxos: [...chunkUtxos].reverse(),
      compactCbor,
      witnessSetCompactCbor,
    }),
  );
  const { txHash, signedCbor } = await submitAndAwait(harness, unsigned);
  const certificateUtxo = (await harness.lucid.utxosAt(policy.address)).find(
    (utxo) => utxo.txHash === txHash,
  );
  if (certificateUtxo === undefined) {
    throw new Error("minted §8.6 certificate was not found");
  }
  return {
    plan,
    chunkUtxos,
    certificateUtxo,
    certificatePolicyId: policy.policyId,
    publicationBytes,
    certificationBytes: signedByteCount(signedCbor),
  };
};

// ## The staged chain

type StageContract = {
  readonly spendingScriptAddress: string;
  readonly spendingScript: Script;
};

type StageResult = {
  readonly nextThreadUtxo: UTxO;
  readonly signedBytes: number;
  readonly outputDatum: string;
};

const submitStage = async ({
  harness,
  inputUtxo,
  inputContract,
  outputContract,
  outputDatum,
  label,
  encode,
  scriptReference,
  carriageReferences,
}: {
  readonly harness: Harness;
  readonly inputUtxo: UTxO;
  readonly inputContract: StageContract;
  readonly outputContract: StageContract;
  readonly outputDatum: string;
  readonly label: string;
  readonly encode: (layout: {
    readonly inputIndex: bigint;
    readonly outputIndex: bigint;
  }) => string;
  readonly scriptReference?: UTxO;
  readonly carriageReferences?: readonly UTxO[];
}): Promise<StageResult> => {
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
    });
  let tx = harness.lucid
    .newTx()
    .collectFrom([await feeInputFor(harness)])
    .collectFrom([inputUtxo], makeRedeemer);
  if (scriptReference !== undefined) {
    tx = tx.readFrom([scriptReference]);
  }
  if (carriageReferences !== undefined && carriageReferences.length > 0) {
    tx = tx.readFrom([...carriageReferences]);
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
    .addSignerKey(SIGNER_HASH);
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
  const { txHash, signedCbor } = await submitAndAwait(harness, unsigned);
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
    signedBytes: signedByteCount(signedCbor),
    outputDatum,
  };
};

// ## One journey per tier

type TierJourney = {
  readonly tier: MidgardFieldCarriage["carriage"];
  readonly preimageBytes: number;
  readonly committedCarriage: MidgardFieldCarriage;
  readonly auxiliaryBytes: number;
  readonly doorReferenceInputs: readonly UTxO[];
  readonly published: PublishedCarriage;
  readonly plan: MidgardFieldCarriagePlan;
  readonly observedDatum: string;
  readonly observedOnLedger: string;
  readonly stageBytes: Readonly<Record<string, number>>;
  readonly observation: {
    readonly itemCount: bigint;
    readonly itemLength: bigint;
  };
  readonly reobserve: (
    doorReferenceInputs: readonly UTxO[],
  ) => MidgardFieldCarriage;
};

const runTierJourney = async (
  fieldPreimageBytes: number,
  options: {
    readonly withholdCarriageAtDoor?: boolean;
    /**
     * Override the field's items. Only the publication-maximum row uses this,
     * because that row's whole claim is about **one** complete item at the cap
     * rather than about a byte count reached however is convenient.
     */
    readonly outputs?: readonly Buffer[];
  } = {},
): Promise<TierJourney> => {
  const outputs =
    options.outputs ?? outputsForFieldTwoPreimageBytes(fieldPreimageBytes);
  const trace = await buildTraceWithOutputs(outputs);
  const { stateIndex, fieldPreimage } = findFieldTwoCompleteItemStep(
    trace,
    fieldPreimageBytes,
  );
  const harness = await setupEmulator();
  const stages =
    harness.contracts.validationTraceDispute.canonicalDecodeItemStages;
  const semanticContract =
    harness.contracts.validationTraceDispute.semanticResolvers[1];
  if (semanticContract === undefined) {
    throw new Error("canonical-decode item semantic resolver is missing");
  }

  // The observe validator arrives by `readFrom`, exactly as a real dispute step
  // sources it. It is also what makes this leg's indices non-degenerate: a
  // non-carriage entry sorts into the same reference-input list, so a resolver
  // that counted only carriage UTxOs would be right here and wrong on L1.
  const observeScriptReference = await publishReferenceScript(
    harness,
    stages.observe.spendingScript,
  );

  const preState = validationMachineStateDataFromCore(
    trace.states[stateIndex]!,
  );
  const plan = planMidgardFieldCarriage({
    owner: Buffer.from(SIGNER_HASH, "hex"),
    txId: Buffer.from(preState.transaction_id, "hex"),
    fieldIndex: OUTPUT_FIELD_INDEX,
    preimage: fieldPreimage,
  });
  expect(plan.tier).toBe(selectMidgardFieldCarriageTier(fieldPreimage.length));

  // The §8.6 mint re-derives the field commitment from the disputed
  // transaction's own compact structures, so they come off the step's own work
  // witness rather than being invented for the certification. The transition is
  // encoded on its own here — it carries no carriage and therefore needs no
  // resolver, which is what lets the certification be built *before* the
  // carriage exists to resolve against.
  const transitionCbor = encodeValidationOneStepWitnessCbor({
    witness: trace.witnesses[stateIndex]!,
    claimedSuccessor: trace.states[stateIndex + 1]!,
  });
  const transition = Data.from(
    transitionCbor.toString("hex"),
    ValidationOneStepWitness,
  ) as ValidationOneStepWitnessData;
  const control = asArray(
    decodeSingleCbor(Buffer.from(transition.work_witness_cbor, "hex")),
    "canonical_decode_item.control",
  );
  if (control.length !== 9) {
    throw new Error("canonical-decode control must carry nine fields");
  }
  const compactCbor = asBytes(
    control[0],
    "canonical_decode_item.compact",
  ).toString("hex");
  const witnessSetCompactCbor = asBytes(
    control[1],
    "canonical_decode_item.witness_set",
  ).toString("hex");

  const published = await publishCarriage({
    harness,
    plan,
    compactCbor,
    witnessSetCompactCbor,
  });

  // **The door transaction's reference-input set**, in the order the builder
  // hands it to the ledger. Both the resolver and the guard sort it canonically
  // themselves, which is the property that makes the indices ledger-truth
  // rather than builder-order.
  const doorReferenceInputs: readonly UTxO[] = [
    observeScriptReference,
    ...published.chunkUtxos,
    ...(published.certificateUtxo === undefined
      ? []
      : [published.certificateUtxo]),
  ];
  const resolveAgainst = (
    referenceInputs: readonly UTxO[],
  ): MidgardFieldCarriage =>
    resolveMidgardFieldCarriageAgainstReferenceInputs({
      plan,
      referenceInputs,
      certificatePolicyId: published.certificatePolicyId,
    });
  const resolveFieldCarriage: ValidationMachineFieldCarriageResolver = ({
    fieldIndex,
    fieldPreimage: preimage,
  }) => {
    if (fieldIndex !== plan.fieldIndex || !preimage.equals(fieldPreimage)) {
      throw new Error(
        "carriage resolver was asked about a field this journey did not plan",
      );
    }
    return resolveAgainst(doorReferenceInputs);
  };

  const argument = buildValidationOneStepArgument({
    trace,
    stateIndex,
    resolveFieldCarriage,
  });
  expect(argument.resolverIndex).toBe(0);
  expect(argument.semanticResolverIndex).toBe(1);
  const auxiliary = Data.from(argument.auxiliaryCbor.toString("hex"));
  if (
    !(auxiliary instanceof Constr) ||
    auxiliary.index !== 30 ||
    auxiliary.fields.length !== 1
  ) {
    throw new Error("complete-item auxiliary witness has an unexpected shape");
  }
  const carriageData = auxiliary.fields[0]!;
  const committedCarriage = resolveAgainst(doorReferenceInputs);

  // The guard, over the material the door is about to be handed. It is the
  // production call site's own check (`submitValidationDisputeSemanticResolution`
  // calls it immediately before this stage), run here against real ledger UTxOs.
  assertMidgardFieldCarriageResolvesAtDoor({
    carriage: committedCarriage,
    plan,
    doorReferenceInputs,
    certificatePolicyId: published.certificatePolicyId,
    label: `field ${OUTPUT_FIELD_INDEX.toString()}`,
  });

  // Option B (#620, #617 wave regeneration): the canonical-decode resolver
  // commits to the transition ALONE. The auxiliary hashed into `evidence_hash`
  // is `NoAuxiliaryWitness` whatever carriage the auxiliary witness names,
  // because the carriage is dereferenced — and content-checked — only at the
  // observe stage's §8.8 door. `submit.ts` computes exactly this for
  // `resolverIndex === 0`; this file states it as the same constant the #621
  // adversarial matrix uses, so the two harnesses cannot drift apart silently.
  const evidenceHash = validationOneStepEvidenceHash({
    transitionCbor: argument.transitionCbor,
    auxiliaryCbor: NO_AUXILIARY_WITNESS_CBOR,
  });
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
    transition,
    fieldPreimage: fieldPreimage.toString("hex"),
  });
  const authenticatedDatum = Data.to(
    { fraud_prover: SIGNER_HASH, data: stageData.authenticated },
    AuthenticatedCanonicalDecodeItemDatum,
  );
  const preparedDatum = Data.to(
    { fraud_prover: SIGNER_HASH, data: stageData.prepared },
    PreparedCanonicalDecodeItemDatum,
  );
  const observedDatum = Data.to(
    { fraud_prover: SIGNER_HASH, data: stageData.observed },
    ObservedCanonicalDecodeItemDatum,
  );
  const verifiedDatum = Data.to(
    { fraud_prover: SIGNER_HASH, data: stageData.verified },
    VerifiedCanonicalDecodeItemDatum,
  );

  // Hand the seeded computation-thread token to the semantic resolver with the
  // PrepareSelected datum the chain starts from.
  const threadSeed = (await harness.lucid.wallet().getUtxos()).find(
    (utxo) => utxo.assets[harness.threadUnit] === 1n,
  );
  if (threadSeed === undefined) {
    throw new Error("thread token seed was not found");
  }
  const seedUnsigned = await harness.lucid
    .newTx()
    .collectFrom([threadSeed])
    .pay.ToContract(
      semanticContract.spendingScriptAddress,
      { kind: "inline", value: preparedThreadDatum },
      { lovelace: 80_000_000n, [harness.threadUnit]: 1n },
    )
    .complete();
  const seed = await submitAndAwait(harness, seedUnsigned);
  const threadUtxo = (
    await harness.lucid.utxosAt(semanticContract.spendingScriptAddress)
  ).find(
    (utxo) =>
      utxo.txHash === seed.txHash && utxo.assets[harness.threadUnit] === 1n,
  );
  if (threadUtxo === undefined) {
    throw new Error("seeded thread UTxO was not found");
  }

  const authenticate = await submitStage({
    harness,
    inputUtxo: threadUtxo,
    inputContract: semanticContract,
    outputContract: stages.source,
    outputDatum: authenticatedDatum,
    label: "canonical item authentication",
    // Option B (#620): `Verify(input_index, output_index, transition)`. The
    // retired four-field form appended the carriage here; it is now a refusal
    // (pinned as such by `complete-item-route-adversarial-emulator.test.ts`),
    // and the carriage reaches the chain only at the observe door below.
    encode: ({ inputIndex, outputIndex }) =>
      Data.to(
        new Constr(1, [
          new Constr(0, [
            inputIndex,
            outputIndex,
            Data.from(argument.transitionCbor.toString("hex")),
          ]),
        ]),
      ),
  });
  const source = await submitStage({
    harness,
    inputUtxo: authenticate.nextThreadUtxo,
    inputContract: stages.source,
    outputContract: stages.observe,
    outputDatum: preparedDatum,
    label: "canonical item source binding",
    encode: ({ inputIndex, outputIndex }) =>
      Data.to(new Constr(1, [new Constr(0, [inputIndex, outputIndex])])),
  });
  const observe = await submitStage({
    harness,
    inputUtxo: source.nextThreadUtxo,
    inputContract: stages.observe,
    outputContract: stages.proof,
    outputDatum: observedDatum,
    label: "canonical item observation",
    scriptReference: observeScriptReference,
    // `withholdCarriageAtDoor` submits the *same* committed redeemer with the
    // carriage left out of the reference-input set. It is the falsifier for
    // every green row above: if the door did not really dereference these
    // indices, withholding what they name would change nothing.
    carriageReferences: options.withholdCarriageAtDoor
      ? []
      : [
          ...published.chunkUtxos,
          ...(published.certificateUtxo === undefined
            ? []
            : [published.certificateUtxo]),
        ],
    // §592's `Observe(input_index, output_index, carriage)`.
    encode: ({ inputIndex, outputIndex }) =>
      Data.to(
        new Constr(1, [new Constr(0, [inputIndex, outputIndex, carriageData])]),
      ),
  });
  const proof = await submitStage({
    harness,
    inputUtxo: observe.nextThreadUtxo,
    inputContract: stages.proof,
    outputContract: stages.settlement,
    outputDatum: verifiedDatum,
    label: "canonical item proof verification",
    encode: ({ inputIndex, outputIndex }) =>
      Data.to(new Constr(1, [new Constr(0, [inputIndex, outputIndex])])),
  });

  return {
    tier: committedCarriage.carriage,
    preimageBytes: fieldPreimage.length,
    committedCarriage,
    auxiliaryBytes: argument.auxiliaryCbor.length,
    doorReferenceInputs,
    published,
    plan,
    observedDatum,
    observedOnLedger: observe.nextThreadUtxo.datum ?? "",
    observation: {
      itemCount: stageData.observed.observation.item_count,
      itemLength: stageData.observed.observation.item_length,
    },
    stageBytes: {
      authenticate: authenticate.signedBytes,
      source: source.signedBytes,
      observe: observe.signedBytes,
      proof: proof.signedBytes,
    },
    reobserve: resolveAgainst,
  };
};

/**
 * §8.4's partition at the two field-2 preimage sizes #600 measured — the old
 * C21 frontier and the exact maximum admissible output — carried here by two
 * items apiece rather than by one, for the reason
 * {@link outputsForFieldTwoPreimageBytes} states.
 */
const TIER2_PREIMAGE_BYTES = 14_778;
const TIER3_PREIMAGE_BYTES = 16_388;

/**
 * The single output at the applied publication cap, and the field-2 envelope it
 * produces: `81 ‖ 59 LLLL ‖ item` is four bytes of framing, so
 * 14,396 -> 14,400.
 *
 * **This is where the publication-maximum case lives now** (owner ruling,
 * 2026-08-14). It used to sit in
 * `complete-item-proof-fit-emulator.test.ts`, whose harness is tier-1 only.
 * That suite selected its complete-item step by `(phase, kind)` alone, so it
 * silently measured field 0's few-dozen-byte preimage instead of field 2's; the
 * row appeared to run at the cap while never carrying anything near it. Once the
 * selector was corrected the row could not run there at all, because 14,400
 * bytes is tier-2 `RawUtxo` and that harness refuses anything but tier-1
 * `Inline`. A >tier-1 publication belongs in this suite.
 *
 * **#580 NOTE — the 64-byte overhang.** `maxSinglePublicationCompleteItemBytes`
 * is 14,396, but §8.4's tier-1 ceiling of
 * `MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES` = 14,336 admits an item of at
 * most **14,332** bytes once the 4-byte field-2 envelope is counted. The applied
 * publication cap therefore sits **64 bytes above** the largest item whose field
 * can be carried inline, so items in (14,332, 14,396] are publishable but not
 * inline-carriable. That is a real gap, not an artefact of this retarget, and
 * **#580 has now measured and dispositioned it**: the band selects tier 2
 * (`RawUtxo`) rather than failing, so the gap is a carriage-tier transition and
 * not a hole — see §8.3 of `docs/spec/midgard-tx.md`. The assertion below is
 * unchanged and stays as an anti-conflation guard, so a future change that
 * silently equates the publication cap with the inline ceiling goes red.
 * Recorded in prose at §8.10 of `docs/spec/midgard-tx.md` too.
 */
const PUBLICATION_MAXIMUM_ITEM_BYTES =
  MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes;
const PUBLICATION_MAXIMUM_PREIMAGE_BYTES = 14_400;
/**
 * The four bytes a single-item field-2 §5.1 envelope costs: `81 ‖ 59 LLLL`.
 * Named so the derivation below states the arithmetic instead of restating its
 * result — 14,332 is not an independent measurement, it is the tier-1 ceiling
 * minus this envelope, and a change to that ceiling must move it.
 */
const SINGLE_ITEM_FIELD_ENVELOPE_BYTES = 4;

const TIER1_ADMISSIBLE_ITEM_BYTES =
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES - SINGLE_ITEM_FIELD_ENVELOPE_BYTES;
const PUBLICATION_MAXIMUM_TIER1_OVERHANG_BYTES = 64;

describe("complete-item §8 carriage tiers 2 and 3 (emulator, applied validators)", () => {
  it("pins the §8.6 certificate policy the observe door is parameterised by", async () => {
    // The door's tier-3 branch checks the named reference input for one unit of
    // *this* policy, so if the policy id the certification mints under is not
    // the one baked into the observe validator, tier 3 cannot pass — and would
    // look like a carriage defect. Rebuilding the validator from the computed
    // policy id and matching script hashes is what turns that into a stated
    // identity rather than an inference from a green row.
    const contracts = await loadContracts();
    const policy = certificatePolicy();
    const rebuilt = validatorToScriptHash({
      type: "PlutusV3",
      script: applyParamsToScript(compiledScript(OBSERVE_TITLE), [
        contracts.validationTraceDispute.canonicalDecodeItemStages.proof
          .spendingScriptHash,
        contracts.computationThread.policyId,
        contracts.validationTraceDispute.proofItem.spendingScriptHash,
        policy.policyId,
      ]),
    });
    expect(rebuilt).toBe(
      contracts.validationTraceDispute.canonicalDecodeItemStages.observe
        .spendingScriptHash,
    );
    // The certificate address is the policy's own script credential with no
    // stake part — the mint handler refuses anything else.
    expect(policy.address).toBe(
      validatorToAddress(NETWORK, {
        type: "PlutusV3",
        script: compiledScript(CERTIFICATE_MINT_TITLE),
      }),
    );
  }, 120_000);

  it("carries a tier-2 RawUtxo field preimage through the applied door", async () => {
    const journey = await runTierJourney(TIER2_PREIMAGE_BYTES);
    expect(journey.tier).toBe("RawUtxo");
    expect(journey.preimageBytes).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    expect(journey.published.chunkUtxos).toHaveLength(1);
    expect(journey.published.certificateUtxo).toBeUndefined();

    // The committed index is not `0`: the published observe validator sorts
    // into the same list, so an index that agreed with itself trivially would
    // prove nothing about the resolution.
    if (journey.committedCarriage.carriage !== "RawUtxo") {
      throw new Error("tier-2 journey did not commit RawUtxo carriage");
    }
    expect(journey.committedCarriage.refInputIndex).toBeGreaterThanOrEqual(0);
    expect(journey.doorReferenceInputs).toHaveLength(2);

    // The auxiliary is indices, so it is O(1) in the output it stands for —
    // 14,778 bytes of preimage reach the door in a handful of redeemer bytes.
    expect(journey.auxiliaryBytes).toBeLessThan(128);

    // The door wrote the observation the off-chain staging predicted.
    expect(
      sameDatumValue(journey.observedOnLedger, journey.observedDatum),
    ).toBe(true);
    expect(journey.observation.itemCount).toBe(2n);
    expect(journey.observation.itemLength).toBe(
      BigInt(outputsForFieldTwoPreimageBytes(TIER2_PREIMAGE_BYTES)[0]!.length),
    );

    for (const [stage, bytes] of Object.entries(journey.stageBytes)) {
      expect(bytes, stage).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
    }
    for (const bytes of journey.published.publicationBytes) {
      expect(bytes).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
    }
  }, 900_000);

  it("carries one complete item at the applied publication maximum through the tier-2 door", async () => {
    // Moved here from `complete-item-proof-fit-emulator.test.ts` (owner
    // ruling, 2026-08-14). One item at the cap, not a byte count split across
    // two: the claim is about the largest single complete item the applied
    // policy admits for publication.
    const item = makeExactSizeOutputItem(PUBLICATION_MAXIMUM_ITEM_BYTES);
    expect(item.length).toBe(PUBLICATION_MAXIMUM_ITEM_BYTES);
    expect(encodeMidgardFieldPreimage([item]).length).toBe(
      PUBLICATION_MAXIMUM_PREIMAGE_BYTES,
    );

    // The overhang, asserted rather than described. #580 measured and
    // dispositioned it — the band selects tier 2, it is not a hole — and this
    // assertion stays as the anti-conflation guard: if a future change closes
    // the overhang or equates the two ceilings, this row fails and says so.
    expect(PUBLICATION_MAXIMUM_ITEM_BYTES - TIER1_ADMISSIBLE_ITEM_BYTES).toBe(
      PUBLICATION_MAXIMUM_TIER1_OVERHANG_BYTES,
    );
    expect(
      encodeMidgardFieldPreimage([
        makeExactSizeOutputItem(TIER1_ADMISSIBLE_ITEM_BYTES),
      ]).length,
    ).toBe(MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES);

    const journey = await runTierJourney(PUBLICATION_MAXIMUM_PREIMAGE_BYTES, {
      outputs: [item],
    });

    // The cap is past tier 1, which is the whole reason this row is in this
    // suite rather than the tier-1 proof-fit harness.
    expect(journey.tier).toBe("RawUtxo");
    expect(journey.preimageBytes).toBeGreaterThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    expect(journey.observation.itemCount).toBe(1n);
    expect(journey.observation.itemLength).toBe(
      BigInt(PUBLICATION_MAXIMUM_ITEM_BYTES),
    );

    // "Inline-datum publication at the publication maximum" — the item is
    // published whole, in one publication, inside the L1 envelope.
    expect(journey.published.chunkUtxos).toHaveLength(1);
    expect(journey.published.certificateUtxo).toBeUndefined();
    for (const bytes of journey.published.publicationBytes) {
      expect(bytes).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
    }

    // "...plus reference-input consumption": the door resolves the item from
    // the UTxO set instead of serializing it again. The old row asserted the
    // consuming transaction was under half the publication; indices are O(1) in
    // the item they stand for, which is strictly stronger.
    if (journey.committedCarriage.carriage !== "RawUtxo") {
      throw new Error("publication-maximum journey did not commit RawUtxo");
    }
    expect(journey.committedCarriage.refInputIndex).toBeGreaterThanOrEqual(0);
    expect(journey.auxiliaryBytes).toBeLessThan(128);
    expect(
      sameDatumValue(journey.observedOnLedger, journey.observedDatum),
    ).toBe(true);
    for (const [stage, bytes] of Object.entries(journey.stageBytes)) {
      expect(bytes, stage).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
    }
  }, 900_000);

  it("carries a tier-3 Certified field preimage through the applied door", async () => {
    const journey = await runTierJourney(TIER3_PREIMAGE_BYTES);
    expect(journey.tier).toBe("Certified");
    expect(journey.plan.publications.length).toBeGreaterThan(1);
    expect(journey.published.certificateUtxo).toBeDefined();
    if (journey.committedCarriage.carriage !== "Certified") {
      throw new Error("tier-3 journey did not commit Certified carriage");
    }
    expect(journey.committedCarriage.chunkRefInputIndices).toHaveLength(
      journey.plan.publications.length,
    );
    // Every index is distinct and inside the door's reference-input list.
    const named = [
      journey.committedCarriage.certRefInputIndex,
      ...journey.committedCarriage.chunkRefInputIndices,
    ];
    expect(new Set(named).size).toBe(named.length);
    for (const index of named) {
      expect(index).toBeGreaterThanOrEqual(0);
      expect(index).toBeLessThan(journey.doorReferenceInputs.length);
    }

    expect(journey.auxiliaryBytes).toBeLessThan(128);
    expect(
      sameDatumValue(journey.observedOnLedger, journey.observedDatum),
    ).toBe(true);
    expect(journey.observation.itemCount).toBe(2n);
    expect(journey.observation.itemLength).toBe(
      BigInt(outputsForFieldTwoPreimageBytes(TIER3_PREIMAGE_BYTES)[0]!.length),
    );

    // Every transaction on the ladder — the full-`K` chunk publication
    // included — clears the real 16,384-byte envelope. That is §8.3 erratum
    // E1's repair, measured against applied validators rather than asserted.
    for (const [stage, bytes] of Object.entries(journey.stageBytes)) {
      expect(bytes, stage).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
    }
    for (const bytes of journey.published.publicationBytes) {
      expect(bytes).toBeLessThanOrEqual(MAX_L1_TX_BYTES);
    }
    expect(journey.published.certificationBytes ?? 0).toBeLessThanOrEqual(
      MAX_L1_TX_BYTES,
    );
  }, 900_000);

  it("refuses a door whose reference-input set moved under the committed indices", async () => {
    // Ruling D3-A, over ledger-resolved material: the indices were resolved
    // against one list and the door would see another. Catching that off chain
    // is the guard's whole job, because the committed `evidence_hash` binds the
    // indices and a failed submission has already spent the staged evidence.
    const journey = await runTierJourney(TIER3_PREIMAGE_BYTES);

    // The perturbation is the cheapest realistic one and the only deterministic
    // one: an entry that sorts ahead of every emulator UTxO — no blake2b tx id
    // is all-zero — shifts the whole canonically-sorted list by one, which is
    // exactly "the door transaction acquired a reference input the indices did
    // not count".
    const acquiredAhead: UTxO = {
      txHash: "00".repeat(32),
      outputIndex: 0,
      address: journey.doorReferenceInputs[0]!.address,
      assets: { lovelace: 5_000_000n },
    };
    const movedDoor = [acquiredAhead, ...journey.doorReferenceInputs];
    expect(() =>
      assertMidgardFieldCarriageResolvesAtDoor({
        carriage: journey.committedCarriage,
        plan: journey.plan,
        doorReferenceInputs: movedDoor,
        certificatePolicyId: journey.published.certificatePolicyId,
        label: `field ${OUTPUT_FIELD_INDEX.toString()}`,
      }),
    ).toThrowError(
      /§8 carriage does not resolve at the door-running transaction/u,
    );
    // The disagreement is a real re-resolution rather than an artefact of the
    // comparison: every index moved by exactly one.
    const movedCarriage = journey.reobserve(movedDoor);
    expect(movedCarriage).not.toStrictEqual(journey.committedCarriage);
    if (
      movedCarriage.carriage !== "Certified" ||
      journey.committedCarriage.carriage !== "Certified"
    ) {
      throw new Error("tier-3 journey did not commit Certified carriage");
    }
    expect(movedCarriage.certRefInputIndex).toBe(
      journey.committedCarriage.certRefInputIndex + 1,
    );
    expect(movedCarriage.chunkRefInputIndices).toStrictEqual(
      journey.committedCarriage.chunkRefInputIndices.map((index) => index + 1),
    );
    // And the guard is not policing something the door would have shrugged off:
    // the same committed redeemer, submitted without the carriage the indices
    // name, is refused by the applied validator.
    await expect(
      runTierJourney(TIER3_PREIMAGE_BYTES, { withholdCarriageAtDoor: true }),
    ).rejects.toThrow(/canonical item observation local evaluation failed/u);
  }, 900_000);
});
