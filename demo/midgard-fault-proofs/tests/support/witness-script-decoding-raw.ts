import {
  decodeMidgardNativeTxCompact,
  encodeMidgardForcedTxCompact,
} from "@al-ft/midgard-core";
/**
 * Shapes and raw submitters for the `witnessScriptDecoding` lifecycle. The
 * raw submitters skip the off-chain closure and state guards so an honest
 * verdict or a mutated authentication seam reaches the applied validator and
 * is refused there, never by a builder.
 */
import {
  deriveMidgardNativeTxWitnessSetCompact,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardFieldCarriagePlan,
  midgardFieldCommitment,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  type FieldOpening,
  type ForcedInclusionTxV1,
  type Header,
  MIDGARD_FIELD_INDEX,
  type NativeTxWitnessSetCompact,
  type OutputReference,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type RootMembershipProof,
  type WitnessScriptDecodingBound,
  type WitnessScriptDecodingScanState,
  WitnessScriptDecodingStep01RedeemerSchema,
  WitnessScriptDecodingStep02DatumSchema,
  WitnessScriptDecodingStep02RedeemerSchema,
  WitnessScriptDecodingStep03DatumSchema,
  WitnessScriptDecodingStep03RedeemerSchema,
  WitnessScriptDecodingStep04DatumSchema,
  WitnessScriptDecodingStep04RedeemerSchema,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
} from "../../src/field-opening.js";
import {
  requireLinearFaultInitialDatum,
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";
import type { WitnessScriptDecodingContracts } from "../../src/witness-script-decoding/contracts.js";
import type { WitnessScriptDecodingScanArgs } from "../../src/witness-script-decoding/submit-step-03.js";
import {
  DECODING_SIGNER_KEY,
  decodingItemFromPayload,
} from "./native-script-decoding-emulator.js";

const FAMILY = "witness-script-decoding";

// ---------------------------------------------------------------------------
// Shapes
// ---------------------------------------------------------------------------

/** The consensus bound on one retained field preimage (`[items]`). */
export const MAXIMUM_FIELD_BYTES = 32_768;
/**
 * A single-item field is `81 59 llll <item>`: the widest item the bound
 * admits is four bytes narrower than the field.
 */
export const MAXIMUM_ITEM_BYTES = MAXIMUM_FIELD_BYTES - 4;
/** The item's own wrapper `82 0t 59 llll` costs five more bytes. */
const MAXIMUM_PAYLOAD_BYTES = MAXIMUM_ITEM_BYTES - 5;

/** `[0, sig-node]`: canonical, two primitive steps. */
export const SIGNATURE_NODE = Buffer.from(
  `8200581c${DECODING_SIGNER_KEY.toString("hex")}`,
  "hex",
);
/** `after 0`: the narrowest canonical leaf token (3 bytes). */
const AFTER_ZERO_NODE = Buffer.from("820400", "hex");

/** `[0, h'']`: the wrapper decodes, the empty payload is the body fault. */
export const emptyPayloadItem = (): Buffer => Buffer.from("820040", "hex");

/** `[1, h'0a']`: language 1 is outside {0, 3, 128}, the header fault. */
export const headerMalformedItem = (): Buffer => Buffer.from("8201410a", "hex");

/**
 * The header fault at the maximum item shape: `[1, <32,759 bytes>]`, a
 * 32,764-byte item in a 32,768-byte field whose nine bounded-item chunks
 * step 02 commits before the wrapper is refused.
 */
export const headerMalformedMaximumItem = (): Buffer => {
  const payload = Buffer.alloc(MAXIMUM_PAYLOAD_BYTES, 0x0a);
  return Buffer.concat([
    Buffer.from("820159", "hex"),
    u16(payload.length),
    payload,
  ]);
};

/** One byte past the aggregate field bound: the adjacent refusal shape. */
export const ADJACENT_FIELD_BYTES = MAXIMUM_FIELD_BYTES + 1;

/**
 * `[1, <32,760 bytes>]`: the same wrapper as the maximum shape in a
 * 32,769-byte field, which no canonical transaction may carry. The field
 * commits like any other, so its chunks publish; the certificate policy
 * refuses `total_length` on chain, and no door can open it.
 */
export const headerMalformedAdjacentItem = (): Buffer => {
  const payload = Buffer.alloc(MAXIMUM_PAYLOAD_BYTES + 1, 0x0a);
  return Buffer.concat([
    Buffer.from("820159", "hex"),
    u16(payload.length),
    payload,
  ]);
};

/**
 * The tier-3 carriage plan an adversarial publisher would build for a field
 * preimage past the aggregate bound: the deterministic §8.4 split, the
 * digest vector and the certificate datum, exactly as `planMidgardFieldCarriage`
 * derives them, minus the off-chain bound the core codec enforces. The chunks
 * are real publications; only the certificate mint can refuse the length, and
 * the lifecycle proves it does so on chain.
 */
export const overBoundFieldCarriagePlan = ({
  owner,
  txId,
  preimage,
}: {
  readonly owner: string;
  readonly txId: string;
  readonly preimage: Buffer;
}): MidgardFieldCarriagePlan => {
  if (preimage.length <= MAXIMUM_FIELD_BYTES)
    throw new Error("the over-bound plan is only for a field past the bound");
  const chunks: Buffer[] = [];
  for (
    let offset = 0;
    offset < preimage.length;
    offset += MIDGARD_CHUNK_BYTES_K
  )
    chunks.push(preimage.subarray(offset, offset + MIDGARD_CHUNK_BYTES_K));
  const commitment = midgardFieldCommitment(preimage);
  const chunkDigests = chunks.map((chunk) => midgardFieldCommitment(chunk));
  return {
    tier: "Certified",
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    txId: Buffer.from(txId, "hex"),
    totalLength: preimage.length,
    commitment,
    inlinePreimage: null,
    publications: chunks.map((bytes, chunkIndex) => ({
      chunkIndex,
      bytes,
      digest: chunkDigests[chunkIndex]!,
    })),
    certificate: {
      owner: Buffer.from(owner, "hex"),
      txId: Buffer.from(txId, "hex"),
      fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
      fieldHash: commitment,
      totalLength: preimage.length,
      chunkDigests,
    },
    certificateAssetName: Buffer.from(
      MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
    ),
  };
};

const u16 = (value: number): Buffer => {
  const out = Buffer.alloc(2);
  out.writeUInt16BE(value, 0);
  return out;
};

const cborArrayHeader = (count: number): Buffer =>
  count < 24
    ? Buffer.from([0x80 + count])
    : count < 256
      ? Buffer.from([0x98, count])
      : Buffer.concat([Buffer.from([0x99]), u16(count)]);

/**
 * The widest canonical shape the field bound admits: one `all` container
 * over 1,020 signature nodes and 38 `after 0` leaves, 1,059 nodes in a
 * 32,764-byte item that fills the 32,768-byte field. Every node is a
 * primitive token step followed by a frame step (2,119 steps with the
 * finalize), so the scan resumes through 133 sixteen-step segments, most of
 * which open on a frame step.
 */
export const wideCanonicalMaximumItem = (): Buffer => {
  const children = [
    ...Array.from({ length: 1_020 }, () => SIGNATURE_NODE),
    ...Array.from({ length: 38 }, () => AFTER_ZERO_NODE),
  ];
  const payload = Buffer.concat([
    Buffer.from("8201", "hex"),
    cborArrayHeader(children.length),
    ...children,
  ]);
  return decodingItemFromPayload(payload);
};

/**
 * The deepest canonical shape the field bound admits: `depth` nested
 * single-child `all` containers over one signature leaf. At the default
 * depth the field is exactly 32,768 bytes and the stack reaches 10,909,
 * two thirds of the 16,384 consensus depth bound, which no field-6 item can
 * reach (a container costs three bytes per level).
 */
export const DEEP_MAXIMUM_DEPTH = 10_909;
export const deepCanonicalItem = (depth = DEEP_MAXIMUM_DEPTH): Buffer => {
  const level = Buffer.from("820181", "hex");
  const payload = Buffer.concat([
    ...Array.from({ length: depth }, () => level),
    SIGNATURE_NODE,
  ]);
  return decodingItemFromPayload(payload);
};

/** A small canonical `all[sig]` item: four primitive steps, one chunk. */
export const smallCanonicalItem = (): Buffer =>
  decodingItemFromPayload(
    Buffer.concat([Buffer.from("820181", "hex"), SIGNATURE_NODE]),
  );

/** Field 6 as the retained preimage `[items]`. */
export const scriptWitnessField = (items: readonly Buffer[]): Buffer =>
  encodeCbor(items);

/**
 * A native transaction whose only non-empty field is the script-witness
 * field. `fee` separates otherwise identical transactions by id.
 */
export const nativeTxWithScriptWitnesses = (
  items: readonly Buffer[],
  fee: bigint,
): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: scriptWitnessField(items),
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

export type WitnessSetCarriage = {
  readonly witnessSet: NativeTxWitnessSetCompact;
  readonly witnessSetHash: string;
  readonly witnessSetCompactCbor: string;
  readonly compactCbor: string;
};

/** The compact witness-set view every step-02 opening anchors to. */
export const witnessSetCarriageOf = (
  nativeTx: MidgardNativeTxFull,
): WitnessSetCarriage => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet);
  return {
    witnessSet: {
      addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
      script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString(
        "hex",
      ),
      redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
        "hex",
      ),
    },
    witnessSetHash: nativeTx.compact.transactionWitnessSetHash.toString("hex"),
    witnessSetCompactCbor:
      encodeMidgardNativeTxWitnessSetCompact(compact).toString("hex"),
    compactCbor: encodeMidgardNativeTxCompact(nativeTx.compact).toString("hex"),
  };
};

// ---------------------------------------------------------------------------
// Raw submitters
// ---------------------------------------------------------------------------

/**
 * Forced step 01 with every bound value exposed: the header, membership,
 * direction, witness-set hash, coordinate, accused class and successor are
 * carried exactly as given, so a substitution at any of them is refused by
 * the applied validator.
 */
export const submitWitnessScriptDecodingStep01ForcedRaw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  header,
  membership,
  direction,
  subject,
  witnessSetHash,
  scriptIndex,
  accusedClass,
  referenceScriptUtxo,
  nextStepIndex = 1,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WitnessScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly header: Header;
  readonly membership: RootMembershipProof<
    OutputReference,
    ForcedInclusionTxV1
  >;
  readonly direction: bigint;
  readonly subject: WitnessScriptDecodingBound["subject"];
  readonly witnessSetHash: string;
  readonly scriptIndex: bigint;
  readonly accusedClass: bigint;
  readonly referenceScriptUtxo: UTxO;
  readonly nextStepIndex?: 0 | 1 | 2 | 3;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex: 0,
    threadOutRef,
  });
  requireLinearFaultInitialDatum({ threadUtxo, signer, family: FAMILY });
  const bound: WitnessScriptDecodingBound = {
    subject,
    witness_set_hash: witnessSetHash,
    script_index: scriptIndex,
    accused_class: accusedClass,
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: bound } as never,
    WitnessScriptDecodingStep02DatumSchema as never,
  );
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: FAMILY,
    stepIndex: 0,
  });
  const nextStep = contracts.steps[nextStepIndex];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} raw forced step 01`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, FAMILY);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} raw forced step 01`,
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                input_index: inputIndex,
                output_index: outputIndex,
                header,
                membership,
                direction,
              },
            },
            script_index: scriptIndex,
          },
        ],
      } as never,
      WitnessScriptDecodingStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: `${FAMILY} raw forced step 01`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    awaitConfirmation: true,
  });
  if (outputIndex === undefined) throw new Error(`${FAMILY}: raw layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

/**
 * Step 02 without the off-chain bound-subject guard and with the opening and
 * successor state exposed. Carriage must already be published (and certified
 * when the tier requires it); `mutateOpening` rewrites the redeemer's opening
 * after the off-chain planner has passed, and `nextState` replaces the exact
 * successor the validator recomputes.
 */
export const submitWitnessScriptDecodingStep02Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  anchorTxId,
  anchorWitnessSetHash,
  carriage,
  scriptWitnessItems,
  carriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  nextState,
  mutateOpening = (opening) => opening,
  nextStepIndex = 2,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WitnessScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly anchorTxId: string;
  readonly anchorWitnessSetHash: string;
  readonly carriage: WitnessSetCarriage;
  readonly scriptWitnessItems: readonly Uint8Array[];
  readonly carriageUtxos: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly nextState: WitnessScriptDecodingScanState;
  readonly mutateOpening?: (
    opening: FieldOpening,
    referenceInputs: readonly UTxO[],
  ) => FieldOpening;
  readonly nextStepIndex?: 0 | 1 | 2 | 3;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const bound = requireLinearFaultStepState<WitnessScriptDecodingBound>({
    threadUtxo,
    signer,
    schema: WitnessScriptDecodingStep02DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: bound.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    anchorTxId,
    nativeTxCompactCbor:
      bound.subject.source_kind === 1n
        ? encodeMidgardForcedTxCompact(
            decodeMidgardNativeTxCompact(
              Buffer.from(carriage.compactCbor, "hex"),
            ),
          ).toString("hex")
        : carriage.compactCbor,
    itemCbors: scriptWitnessItems,
    owner: signer.paymentKeyHash,
    publish: carriageUtxos.length > 0,
    witnessSet: carriage.witnessSet,
    anchorWitnessSetHash,
    label: `${FAMILY} raw step 02 field 6`,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const referenceInputs = [
    ...carriageUtxos,
    stepReference,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
  ];
  const opening = mutateOpening(
    faultProofFieldOpening({
      planned,
      referenceInputs,
      certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
      label: `${FAMILY} raw step 02 field 6`,
    }),
    referenceInputs,
  );
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    WitnessScriptDecodingStep03DatumSchema as never,
  );
  const nextStep = contracts.steps[nextStepIndex];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} raw step 02`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, FAMILY);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} raw step 02`,
    );
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, opening },
        ],
      } as never,
      WitnessScriptDecodingStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: `${FAMILY} raw step 02`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
    awaitConfirmation: true,
  });
  if (outputIndex === undefined) throw new Error(`${FAMILY}: raw layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

/**
 * Step 03 with the redeemer arguments, successor state and successor script
 * carried exactly as given: a substituted chunk, control, frame, budget,
 * checkpoint or successor is refused by the applied validator.
 */
export const submitWitnessScriptDecodingStep03Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  args,
  nextState,
  nextStepIndex,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WitnessScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly args: WitnessScriptDecodingScanArgs;
  readonly nextState: WitnessScriptDecodingScanState;
  readonly nextStepIndex: 0 | 1 | 2 | 3;
  readonly referenceScriptUtxo: UTxO;
}) => {
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    (nextStepIndex === 3
      ? WitnessScriptDecodingStep04DatumSchema
      : WitnessScriptDecodingStep03DatumSchema) as never,
  );
  const nextStep = contracts.steps[nextStepIndex];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} raw step 03`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, FAMILY);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} raw step 03`,
    );
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, ...args },
        ],
      } as never,
      WitnessScriptDecodingStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: `${FAMILY} raw step 03`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    awaitConfirmation: true,
  });
  if (outputIndex === undefined) throw new Error(`${FAMILY}: raw layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

/**
 * Step 04 without the off-chain closure guard: an honest verdict's closed
 * state reaches `terminal_contradiction_v1` and is refused there.
 */
export const submitWitnessScriptDecodingStep04Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WitnessScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  requireLinearFaultStepState<WitnessScriptDecodingScanState>({
    threadUtxo,
    signer,
    schema: WitnessScriptDecodingStep04DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  return await submitLinearFaultFinalize({
    lucid,
    family: FAMILY,
    stepIndex,
    step: contracts.steps[stepIndex],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: WitnessScriptDecodingStep04RedeemerSchema,
    buildFamilyArgs: (layout) => ({
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
};

// ---------------------------------------------------------------------------
// Opening mutations (field 6 openings are `WitnessFieldOpening`)
// ---------------------------------------------------------------------------

const witnessOpeningOf = (opening: FieldOpening) => {
  if (!("WitnessFieldOpening" in opening))
    throw new Error(`${FAMILY}: witness opening expected`);
  return opening.WitnessFieldOpening;
};

/** Patch a certified carriage's reference-input coordinates. */
export const mutateWitnessCertifiedCarriage = (
  opening: FieldOpening,
  patch: (carriage: {
    cert_ref_input_index: bigint;
    chunk_ref_input_indices: bigint[];
  }) => {
    cert_ref_input_index: bigint;
    chunk_ref_input_indices: bigint[];
  },
): FieldOpening => {
  const witness = witnessOpeningOf(opening);
  if (!("Certified" in witness.carriage))
    throw new Error(`${FAMILY}: certified carriage expected`);
  return {
    WitnessFieldOpening: {
      ...witness,
      carriage: {
        Certified: patch({
          cert_ref_input_index: witness.carriage.Certified.cert_ref_input_index,
          chunk_ref_input_indices: [
            ...witness.carriage.Certified.chunk_ref_input_indices,
          ],
        }),
      },
    },
  };
};

/** Point a published (RawUtxo) carriage at a different reference input. */
export const mutateWitnessRawUtxoCarriage = (
  opening: FieldOpening,
  offset: bigint,
): FieldOpening => {
  const witness = witnessOpeningOf(opening);
  if (!("RawUtxo" in witness.carriage))
    throw new Error(`${FAMILY}: raw-utxo carriage expected`);
  return {
    WitnessFieldOpening: {
      ...witness,
      carriage: {
        RawUtxo: {
          ref_input_index: witness.carriage.RawUtxo.ref_input_index + offset,
        },
      },
    },
  };
};

/** Replace the compact transaction bytes the opening is anchored to. */
export const mutateWitnessCompactSource = (
  opening: FieldOpening,
  nativeTxCompactCbor: string,
): FieldOpening => ({
  WitnessFieldOpening: {
    ...witnessOpeningOf(opening),
    native_tx_compact_cbor: nativeTxCompactCbor,
  },
});

/** Replace the compact witness set the opening claims for the anchor. */
export const mutateWitnessSet = (
  opening: FieldOpening,
  witnessSet: NativeTxWitnessSetCompact,
): FieldOpening => ({
  WitnessFieldOpening: {
    ...witnessOpeningOf(opening),
    witness_set: witnessSet,
  },
});
