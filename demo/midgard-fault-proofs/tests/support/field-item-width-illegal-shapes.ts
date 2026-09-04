/**
 * Field shapes, block fixtures, and raw (guard-free) submitters for the
 * `fieldItemWidthIllegal` lifecycle suite.
 *
 * The family's rule is one comparison per field: a field-2 output item is
 * illegal strictly above `max_serialized_output_preimage_bytes` (16,384
 * payload bytes) and a field-5 mint item is illegal only when empty. The
 * shapes below pin both rules at both boundaries in both directions, and pin
 * the field-2 shapes at the §5.4 aggregate ceiling (a 32,768-byte field opened
 * through three certified chunks) so the fit ledger measures the most
 * expensive carriage the family can be asked to open.
 *
 * The raw submitters exist so a negative reaches local UPLC evaluation. The
 * production builders refuse an honest verdict and a mutated opening before
 * building; a lifecycle refusal has to come from the validator, so these
 * bypass exactly those off-chain guards and nothing else.
 */

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxProofSource,
  deriveMidgardNativeTxWitnessSetCompact,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  DA_PAYLOAD_VERSION,
  EMPTY_MERKLE_TREE_ROOT,
  encodeDaPayload,
  EventKeySchema,
  EventToStepValueSchema,
  type FieldOpening,
  ForcedInclusionTxV1Schema,
  hashBlockHeader,
  OutputReference,
  Proof,
  type RejectionReason,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  ROOT_DOMAINS,
  TransitionStepSchema,
  ValidationTraceDescriptorSchema,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { FieldItemWidthIllegalContracts } from "../../src/field-item-width-illegal/contracts.js";
import type { FieldItemWidthEvidence } from "../../src/field-item-width-illegal/field-item-width-illegal.js";
import {
  FieldItemWidthStep02DatumSchema,
  FieldItemWidthStep02RedeemerSchema,
  FieldItemWidthStep03DatumSchema,
  FieldItemWidthStep03RedeemerSchema,
} from "../../src/field-item-width-illegal/schemas.js";
import {
  certifyFaultProofFieldCarriage,
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import {
  nativeTxFromCoreCompact,
  type SubmitStep01TxInclusion,
} from "../../src/submit-step-01.js";
import {
  buildCountedRoot,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import { reconstructDaPayload } from "../../src/transition-trace/reconstruct.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";
import { h32, makeHeader } from "./emulator/header-fixtures.js";
import { l2TransactionSourceCbor } from "./emulator/native-tx.js";
import {
  outputReferenceCbor,
  sortedDaEntries,
  transitionTraceRawEntry,
} from "./submit-init-emulator-fixtures.js";
import {
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "./submit-init-emulator-shared.js";

// ---------------------------------------------------------------------------
// Shapes
// ---------------------------------------------------------------------------

/** §5.4 aggregate field ceiling; the largest field the carriage can open. */
export const MAXIMUM_FIELD_BYTES = 32_768;
/** `max_serialized_output_preimage_bytes`: the field-2 width bound. */
export const MAXIMUM_LEGAL_OUTPUT_BYTES = 16_384;

/**
 * A field-2 item is wrapped `59 LLLL` above 255 bytes and the one-item field
 * header is one byte, so the widest single output a 32,768-byte field can
 * carry is 32,764 payload bytes.
 */
export const MAXIMUM_OUTPUT_ITEM_BYTES = MAXIMUM_FIELD_BYTES - 1 - 3;

/**
 * Two items, both `59 LLLL`-wrapped, under a one-byte header: the selected
 * item sits second so opening it walks past the first and reads across the
 * chunk boundary. `16,376 + 16,384` payload bytes fills the field to 32,767.
 */
export const FORCED_FILLER_OUTPUT_BYTES =
  MAXIMUM_FIELD_BYTES - 1 - 3 - 3 - MAXIMUM_LEGAL_OUTPUT_BYTES - 1;

export const widthNativeTx = ({
  outputItems = [],
  mintItems = [],
}: {
  readonly outputItems?: readonly Buffer[];
  readonly mintItems?: readonly Buffer[];
}): MidgardNativeTxFull =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: encodeCbor([...outputItems]),
      fee: 7n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: encodeCbor([...mintItems]),
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

export type WidthShape = {
  readonly label: string;
  readonly nativeTx: MidgardNativeTxFull;
  readonly fieldIndex: 2 | 5;
  readonly itemIndex: number;
  readonly fieldPreimage: Buffer;
  /** What the canonical rule says about the selected item. */
  readonly illegal: boolean;
};

const shape = (
  label: string,
  nativeTx: MidgardNativeTxFull,
  fieldIndex: 2 | 5,
  itemIndex: number,
  illegal: boolean,
): WidthShape => {
  const fieldPreimage = Buffer.from(
    fieldIndex === 2
      ? nativeTx.body.outputsPreimageCbor
      : nativeTx.body.mintPreimageCbor,
  );
  const item = decodeMidgardFieldPreimage(fieldPreimage)[itemIndex];
  if (item === undefined) throw new Error(`${label}: item absent`);
  const actual =
    fieldIndex === 2
      ? item.length > MAXIMUM_LEGAL_OUTPUT_BYTES
      : item.length === 0;
  if (actual !== illegal) throw new Error(`${label}: shape polarity wrong`);
  return { label, nativeTx, fieldIndex, itemIndex, fieldPreimage, illegal };
};

/** Accepted, illegal: the widest output a maximum field can carry. */
export const maximumIllegalOutputShape = (): WidthShape =>
  shape(
    "maximum 32,768-byte output field, one 32,764-byte item (illegal)",
    widthNativeTx({
      outputItems: [Buffer.alloc(MAXIMUM_OUTPUT_ITEM_BYTES, 0xa1)],
    }),
    2,
    0,
    true,
  );

/** Legal at the bound: exactly 16,384 payload bytes, certified carriage. */
export const boundaryLegalOutputShape = (): WidthShape =>
  shape(
    "one 16,384-byte output item at the width bound (legal)",
    widthNativeTx({
      outputItems: [Buffer.alloc(MAXIMUM_LEGAL_OUTPUT_BYTES, 0xb2)],
    }),
    2,
    0,
    false,
  );

/**
 * Forced maximum: a 32,767-byte field whose second item is exactly at the
 * bound. Selecting index 1 walks the whole field under certified carriage.
 */
export const forcedMaximumLegalOutputShape = (): WidthShape =>
  shape(
    "maximum 32,767-byte output field, selected 16,384-byte second item (legal)",
    widthNativeTx({
      outputItems: [
        Buffer.alloc(FORCED_FILLER_OUTPUT_BYTES, 0xc3),
        Buffer.alloc(MAXIMUM_LEGAL_OUTPUT_BYTES, 0xc4),
      ],
    }),
    2,
    1,
    false,
  );

/** Forced honest: an output one byte over the bound, correctly rejected. */
export const forcedIllegalOutputShape = (): WidthShape =>
  shape(
    "one 16,385-byte output item one byte over the bound (illegal)",
    widthNativeTx({
      outputItems: [Buffer.alloc(MAXIMUM_LEGAL_OUTPUT_BYTES + 1, 0xd5)],
    }),
    2,
    0,
    true,
  );

/** Field 5 with a real policy item first and the empty item second. */
export const mintFieldTx = (): MidgardNativeTxFull =>
  widthNativeTx({
    mintItems: [Buffer.alloc(29, 0xe6), Buffer.alloc(0)],
  });

export const emptyMintItemShape = (nativeTx = mintFieldTx()): WidthShape =>
  shape(
    "two-item mint field, empty second item (illegal)",
    nativeTx,
    5,
    1,
    true,
  );

export const nonEmptyMintItemShape = (nativeTx = mintFieldTx()): WidthShape =>
  shape(
    "two-item mint field, 29-byte first item (legal)",
    nativeTx,
    5,
    0,
    false,
  );

export const compactCborHex = (nativeTx: MidgardNativeTxFull): string =>
  encodeMidgardNativeTxCompact(nativeTx.compact).toString("hex");

export const witnessSetCompactCborHex = (
  nativeTx: MidgardNativeTxFull,
): string =>
  encodeMidgardNativeTxWitnessSetCompact(
    deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet),
  ).toString("hex");

// ---------------------------------------------------------------------------
// Accepted block: one transactions root over every committed transaction
// ---------------------------------------------------------------------------

export const buildAcceptedWidthInclusions = async (
  transactions: readonly MidgardNativeTxFull[],
): Promise<{
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly inclusions: readonly SubmitStep01TxInclusion[];
}> => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  const ids = transactions.map((nativeTx) =>
    computeMidgardNativeTxId(nativeTx).toString("hex"),
  );
  for (const [index, nativeTx] of transactions.entries()) {
    await trie.insert(
      Buffer.from(ids[index]!, "hex"),
      Buffer.from(l2TransactionSourceCbor(nativeTx), "hex"),
    );
  }
  const transactionsRoot = Buffer.from(trie.hash).toString("hex");
  const inclusions: SubmitStep01TxInclusion[] = [];
  for (const [index, nativeTx] of transactions.entries()) {
    const proof = await trie.prove(Buffer.from(ids[index]!, "hex"));
    inclusions.push({
      nativeTxId: ids[index]!,
      nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
      nativeTxCompactCbor: compactCborHex(nativeTx),
      l2TransactionSourceCbor: l2TransactionSourceCbor(nativeTx),
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(proof.toCBOR().toString("hex"), Proof),
      txMembershipProofCbor: proof.toCBOR().toString("hex"),
    });
  }
  return {
    transactionsRoot,
    l2TransactionCount: BigInt(transactions.length),
    inclusions,
  };
};

// ---------------------------------------------------------------------------
// Forced block: one rejected forced leaf carrying the family's typed reason
// ---------------------------------------------------------------------------

export const buildWidthForcedFixture = async ({
  operatorVkey,
  now,
  nativeTx,
  rejectionReason,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly nativeTx: MidgardNativeTxFull;
  readonly rejectionReason: RejectionReason;
}) => {
  const txOrderId = transitionTraceOutRef("f1");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const finalUtxo = transitionTraceRawEntry(
    outputReferenceCbor({ transactionId: h32("01"), outputIndex: 0n }).toString(
      "hex",
    ),
    "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
  );
  const descriptor = buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef: Buffer.from(finalUtxo[0], "hex"),
    outputCbor: Buffer.from(finalUtxo[1], "hex"),
  }).descriptorCbor;
  const finalRoot = await keyValuePhasRootWithCount([
    { key: Buffer.from(finalUtxo[0], "hex"), value: descriptor },
  ]);
  const source = deriveMidgardNativeTxProofSource(
    adjudicateMidgardNativeTxFullValidity(nativeTx, "TxIsInvalid"),
  );
  const transaction = {
    tx_id: computeMidgardNativeTxId(nativeTx).toString("hex"),
    source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: { ForcedTxInvalid: { reason: rejectionReason } },
  } as const;
  const forcedEntries = [
    transitionTraceDaEntry({
      key: txOrderId,
      keySchema: OutputReference as never,
      value: transaction,
      valueSchema: ForcedInclusionTxV1Schema,
    }),
  ];
  const transitionEntries = [
    transitionTraceDaEntry({
      key: 0n,
      keySchema: Data.Integer() as never,
      value: {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "ForcedTransaction",
        pre_utxos_root: EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: finalRoot.root,
      },
      valueSchema: TransitionStepSchema,
    }),
  ];
  const eventEntries = [
    transitionTraceDaEntry({
      key: eventKey,
      keySchema: EventKeySchema,
      value: { step_index: 0n, phase: "ForcedTransaction" },
      valueSchema: EventToStepValueSchema,
    }),
  ];
  const validationEntries = [
    transitionTraceDaEntry({
      key: eventKey,
      keySchema: EventKeySchema,
      value: {
        schema_version: 1n,
        machine_version: 1n,
        trace_root: h32("c1"),
        step_count: 1n,
        initial_state_hash: h32("c2"),
        terminal_state_hash: h32("c3"),
        verdict: "Rejected",
        rejection_code_hash: h32("c4"),
      },
      valueSchema: ValidationTraceDescriptorSchema,
    }),
  ];
  const counted = async (
    domain: Parameters<typeof buildCountedRoot>[0],
    entries: readonly (readonly [string, string])[],
  ) =>
    await buildCountedRoot(
      domain,
      entries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const [forcedRoot, transitionRoot, eventRoot, validationRoot] =
    await Promise.all([
      counted(ROOT_DOMAINS.forcedTransactionsV1, forcedEntries),
      counted(ROOT_DOMAINS.transitionTrace, transitionEntries),
      counted(ROOT_DOMAINS.eventToStep, eventEntries),
      counted(ROOT_DOMAINS.validationTraces, validationEntries),
    ]);
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 1n,
  };
  const header = {
    ...makeHeader(operatorVkey, now),
    utxosRoot: finalRoot.root,
    forcedTransactionsRoot: forcedRoot.root,
    transitionTraceRoot: transitionRoot.root,
    eventToStepRoot: eventRoot.root,
    validationTracesRoot: validationRoot.root,
    ...counts,
  };
  const headerHash = await Effect.runPromise(hashBlockHeader(header));
  const payloadEnvelopeCbor = await wrapDaPayload(
    encodeDaPayload({
      version: DA_PAYLOAD_VERSION,
      block_body: {
        header_hash: headerHash,
        header,
        utxos: sortedDaEntries([finalUtxo]),
        withdrawals: [],
        forced_transactions: sortedDaEntries(forcedEntries),
        transactions: [],
        deposits: [],
        transition_trace: sortedDaEntries(transitionEntries),
        event_to_step: sortedDaEntries(eventEntries),
        transaction_preimages: [],
        forced_transaction_preimages: sortedDaEntries([
          transitionTraceRawEntry(
            forcedEntries[0]![0],
            encodeMidgardNativeTxCanonical(nativeTx).toString("hex"),
          ),
        ]),
        cek_program_material: [],
        validation_traces: sortedDaEntries(validationEntries),
        validation_trace_witnesses: [],
        counts,
      },
    }),
    { mode: "identity" },
  );
  return {
    header,
    reconstruction: await reconstructDaPayload({
      payloadEnvelopeCbor,
      expectedHeaderHash: headerHash,
      committedHeader: header,
    }),
    eventKey,
    nativeTx,
    transaction,
    rejectionReason,
  };
};

// ---------------------------------------------------------------------------
// Raw submitters
// ---------------------------------------------------------------------------

/**
 * Step 02 with the opening exposed. Carriage is published and certified the
 * way the production builder does it; `mutateOpening` then rewrites the
 * redeemer's opening after every off-chain check has passed, and
 * `nextStepIndex` names a successor other than the one the validator was
 * applied with. Both reach the validator, which is the point.
 */
export const submitWidthStep02Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  referenceScriptUtxo,
  certificateReferenceScriptUtxo,
  mutateOpening = (opening) => opening,
  nextStepIndex = 2,
  publishedCarriageUtxos,
  certificateUtxo: suppliedCertificateUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: FieldItemWidthIllegalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: FieldItemWidthEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly mutateOpening?: (
    opening: FieldOpening,
    referenceInputs: readonly UTxO[],
  ) => FieldOpening;
  readonly nextStepIndex?: 1 | 2;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "field-item-width-illegal",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    field_index: bigint;
    item_index: bigint;
  }>({
    threadUtxo,
    signer,
    schema: FieldItemWidthStep02DatumSchema as never,
    family: "field-item-width-illegal",
    stepIndex,
  });
  const items = decodeMidgardFieldPreimage(
    Buffer.from(evidence.fieldPreimageHex, "hex"),
  );
  const planned = planFaultProofFieldOpening({
    fieldIndex: evidence.fieldIndex,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: false,
    label: "field-item-width-illegal raw field opening",
  });
  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: "field-item-width-illegal raw field opening",
    }));
  const certificateUtxo =
    suppliedCertificateUtxo ??
    (planned.plan.tier === "Certified"
      ? (
          await certifyFaultProofFieldCarriage({
            lucid,
            network: lucid.config().network!,
            signer,
            planned,
            certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
            certificateMintingScript:
              contracts.fieldPreimageCertificateMintingScript,
            certificateReferenceScriptUtxo:
              certificateReferenceScriptUtxo ??
              (() => {
                throw new Error(
                  "field-item-width-illegal raw: certified opening requires certificate reference script",
                );
              })(),
            chunkUtxos: carriageUtxos,
            compactCbor: nativeTxCompactCbor,
            witnessSetCompactCbor,
          })
        ).certificateUtxo
      : undefined);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "field-item-width-illegal",
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
      label: "field-item-width-illegal raw field opening",
    }),
    referenceInputs,
  );
  // The successor datum carries what the thread bound, not what the evidence
  // says: a coordinate mutation is bound at step 01 and must be refused here
  // by the door, not masked by an off-chain equality check.
  const item = items[Number(state.item_index)];
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: evidence.subject,
        field_index: state.field_index,
        item_index: state.item_index,
        item_width: BigInt(item?.length ?? 0),
      },
    } as never,
    FieldItemWidthStep03DatumSchema as never,
  );
  const nextStep = contracts.steps[nextStepIndex];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "field-item-width-illegal raw step-02",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "field-item-width-illegal raw",
    );
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "field-item-width-illegal raw step-02 output",
    );
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, opening },
        ],
      } as never,
      FieldItemWidthStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  return await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[1].spendingScript,
    stepRole: "field-item-width-illegal raw step-02",
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
    awaitConfirmation: true,
  });
};

/**
 * Step 03 without the off-chain `fieldItemWidthEvidenceCloses` guard, so an
 * honest verdict reaches `terminal_contradiction_v1` and is refused there.
 */
export const submitWidthStep03Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: FieldItemWidthIllegalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "field-item-width-illegal",
    stepIndex,
    threadOutRef,
  });
  return await submitLinearFaultFinalize({
    lucid,
    family: "field-item-width-illegal",
    stepIndex,
    step: contracts.steps[2],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: FieldItemWidthStep03RedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
};
