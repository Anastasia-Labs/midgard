import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  encodeMidgardNativeTxCanonical,
  initialMidgardLedgerOutputScanControl,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  DA_PAYLOAD_VERSION,
  EMPTY_MERKLE_TREE_ROOT,
  encodeDaPayload,
  EventKeySchema,
  EventToStepValueSchema,
  type FieldOpening,
  fieldOpeningForField,
  ForcedInclusionTxV1Schema,
  forcedVerdictSubject,
  hashBlockHeader,
  type Header,
  OutputReference,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  ROOT_DOMAINS,
  type RootMembershipProof,
  TransitionStepSchema,
  ValidationTraceDescriptorSchema,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  certifyFaultProofFieldCarriage,
  faultProofFieldCarriage,
  type FaultProofFieldOpeningPlan,
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
import type { TransactionOutputNonCanonicalContracts } from "../../src/transaction-output-non-canonical/contracts.js";
import {
  TransactionOutputStep01RedeemerSchema,
  TransactionOutputStep02DatumSchema,
  TransactionOutputStep02RedeemerSchema,
  TransactionOutputStep03DatumSchema,
  TransactionOutputStep03RedeemerSchema,
  TransactionOutputStep04DatumSchema,
  TransactionOutputStep04RedeemerSchema,
} from "../../src/transaction-output-non-canonical/schemas.js";
import {
  type TransactionOutputEvidence,
  transactionOutputScanControlData,
} from "../../src/transaction-output-non-canonical/transaction-output-non-canonical.js";
import {
  buildCountedRoot,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import { reconstructDaPayload } from "../../src/transition-trace/reconstruct.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";
import { makeNativeTx } from "./emulator/native-tx.js";
import {
  outputReferenceCbor,
  sortedDaEntries,
  transitionTraceRawEntry,
} from "./submit-init-emulator-fixtures.js";
import {
  h32,
  makeHeader,
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "./submit-init-emulator-shared.js";

const FAMILY = "transaction-output-non-canonical";

export const TRANSACTION_OUTPUT_SCAN_CHUNK_BYTES = 4_095;
export const TRANSACTION_OUTPUT_SCAN_WINDOW_BYTES = 8_190;

/** The output shape the rule's own maximum selector scans, sized to `total` bytes. */
export const canonicalOutputOfLength = (total: number): Buffer => {
  // a3 | 00 <address> | 01 <value> | 02 <inline datum bytes>
  const head = Buffer.from(
    "a300581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a002",
    "hex",
  );
  const payloadLength = total - head.length - 3;
  if (payloadLength < 0 || payloadLength > 0xffff)
    throw new Error(
      "canonical output length is outside the two-byte datum form",
    );
  const datumHeader = Buffer.alloc(3);
  datumHeader[0] = 0x59;
  datumHeader.writeUInt16BE(payloadLength, 1);
  const output = Buffer.concat([
    head,
    datumHeader,
    Buffer.alloc(payloadLength),
  ]);
  if (output.length !== total)
    throw new Error("canonical output sizing drifted");
  return output;
};

/** The rule selectors' malformed twin: a leading `b8` tag where the output map must start. */
export const MALFORMED_OUTPUT = Buffer.from(
  "b80200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0",
  "hex",
);

export type Common = Readonly<{
  lucid: LucidEvolution;
  contracts: TransactionOutputNonCanonicalContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  referenceScriptUtxo: UTxO;
}>;

/**
 * A raw continuation: the exact datum, redeemer and successor the test asks
 * for reach the validator, so every substitution is refused on chain rather
 * than by an off-chain builder guard.
 */
const continueRaw = async ({
  common,
  stepIndex,
  nextAddress,
  nextDatum,
  redeemerSchema,
  args,
  carriageUtxos = [],
  extraReferenceInputs = [],
}: {
  readonly common: Common;
  readonly stepIndex: number;
  readonly nextAddress: string;
  readonly nextDatum: string;
  readonly redeemerSchema: unknown;
  readonly args: (
    inputIndex: bigint,
    outputIndex: bigint,
  ) => Record<string, unknown>;
  readonly carriageUtxos?: readonly UTxO[];
  readonly extraReferenceInputs?: readonly UTxO[];
}) => {
  const { lucid, contracts, categoryId, signer, threadOutRef } = common;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const role = `raw step ${(stepIndex + 1).toString().padStart(2, "0")}`;
  const stepReference = requireLinearFaultReferenceScript({
    utxo: common.referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex]!.spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const outputMatches = computationThreadOutputPredicate({
    address: nextAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, role);
    const inputIndex = requireInputIndex(ctx, threadUtxo, role);
    outputIndex = requireUniqueOutputIndex(ctx.outputs, outputMatches, role);
    return Data.to(
      { Continue: [args(inputIndex, outputIndex)] } as never,
      redeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex]!.spendingScript,
    stepRole: role,
    nextAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs,
    awaitConfirmation: true,
  });
  if (outputIndex === undefined) throw new Error(`${role}: no layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

const datumOf = (common: Common, data: unknown, schema: unknown) =>
  Data.to(
    { fraud_prover: common.signer.paymentKeyHash, data } as never,
    schema as never,
  );

export type OutputScanStateData = ReturnType<typeof scanStateOf>;

/** The step-03/04 thread state at trace position `controlIndex` with the given outcome. */
export const scanStateOf = (
  evidence: TransactionOutputEvidence,
  controlIndex: number,
  outcome: bigint,
) => ({
  subject: evidence.subject,
  output_index: BigInt(evidence.itemIndex),
  item_length: BigInt(evidence.itemLength),
  item_hash: evidence.itemHash,
  chunk_hashes: evidence.chunkHashes,
  control: transactionOutputScanControlData(
    evidence.scanControls[controlIndex]!,
  ),
  outcome,
});

/** The initial scan state over raw item bytes, computed without the family's off-chain width guard. */
export const initialScanStateOfItem = ({
  subject,
  itemIndex,
  item,
}: {
  readonly subject: VerdictSubject;
  readonly itemIndex: number;
  readonly item: Buffer;
}) => ({
  subject,
  output_index: BigInt(itemIndex),
  item_length: BigInt(item.length),
  item_hash: computeHash32(item).toString("hex"),
  chunk_hashes: Array.from(
    { length: Math.ceil(item.length / TRANSACTION_OUTPUT_SCAN_CHUNK_BYTES) },
    (_, index) =>
      computeHash32(
        item.subarray(
          index * TRANSACTION_OUTPUT_SCAN_CHUNK_BYTES,
          (index + 1) * TRANSACTION_OUTPUT_SCAN_CHUNK_BYTES,
        ),
      ).toString("hex"),
  ),
  control: transactionOutputScanControlData(
    initialMidgardLedgerOutputScanControl(),
  ),
  outcome: 0n,
});

/** The scan window the step-03 builder derives for a checkpoint at `cursor`/`stage`. */
export const scanWindowAt = (
  item: Buffer,
  control: { readonly cursor: bigint; readonly stage: bigint },
): Buffer => {
  const chunkStart =
    Math.floor(Number(control.cursor) / TRANSACTION_OUTPUT_SCAN_CHUNK_BYTES) *
    TRANSACTION_OUTPUT_SCAN_CHUNK_BYTES;
  return item.subarray(
    chunkStart,
    chunkStart +
      (control.stage <= 4n
        ? TRANSACTION_OUTPUT_SCAN_WINDOW_BYTES
        : TRANSACTION_OUTPUT_SCAN_CHUNK_BYTES),
  );
};

export const readOutputScanState = async (common: Common, stepIndex: 2 | 3) => {
  const { threadUtxo } = await requireLinearFaultThreadUtxo({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef: common.threadOutRef,
  });
  return requireLinearFaultStepState<{
    subject: unknown;
    output_index: bigint;
    item_length: bigint;
    item_hash: string;
    chunk_hashes: readonly string[];
    control: { readonly cursor: bigint; readonly stage: bigint };
    outcome: bigint;
  }>({
    threadUtxo,
    signer: common.signer,
    schema: (stepIndex === 2
      ? TransactionOutputStep03DatumSchema
      : TransactionOutputStep04DatumSchema) as never,
    family: FAMILY,
    stepIndex,
  });
};

/** Step 01 over a forced leaf with the source, direction and coordinates handed to the validator verbatim. */
export const submitOutputStep01ForcedRaw = async ({
  header,
  membership,
  direction,
  outputIndex,
  boundIndex = outputIndex,
  ...common
}: Common & {
  readonly header: Header;
  readonly membership: RootMembershipProof<OutputReference, unknown>;
  readonly direction: bigint;
  /** The redeemer's output coordinate. */
  readonly outputIndex: bigint;
  /** The coordinate the next thread state claims; defaults to the redeemer's. */
  readonly boundIndex?: bigint;
}) => {
  const leaf = membership.value as {
    readonly tx_id: string;
    readonly verdict: "ForcedTxValid" | { ForcedTxInvalid: { reason: never } };
  };
  const subject = {
    ...forcedVerdictSubject({
      transactionId: leaf.tx_id,
      sourceKey: membership.key,
      rejectionReason:
        leaf.verdict === "ForcedTxValid"
          ? null
          : leaf.verdict.ForcedTxInvalid.reason,
    }),
    direction,
  };
  return await continueRaw({
    common,
    stepIndex: 0,
    nextAddress: common.contracts.steps[1].spendingScriptAddress,
    nextDatum: datumOf(
      common,
      { subject, output_index: boundIndex },
      TransactionOutputStep02DatumSchema,
    ),
    redeemerSchema: TransactionOutputStep01RedeemerSchema,
    args: (input_index, output_index) => ({
      source: {
        ForcedSource: {
          input_index,
          output_index,
          header,
          membership,
          direction,
        },
      },
      output_index: outputIndex,
    }),
  });
};

/** Step 02 with the opening and the next scan state supplied verbatim. */
export const submitOutputStep02Raw = async ({
  opening,
  nextState,
  nextStepIndex = 2,
  carriageUtxos,
  extraReferenceInputs,
  ...common
}: Common & {
  readonly opening: FieldOpening;
  readonly nextState: OutputScanStateData;
  readonly nextStepIndex?: 2 | 3;
  readonly carriageUtxos?: readonly UTxO[];
  readonly extraReferenceInputs?: readonly UTxO[];
}) =>
  await continueRaw({
    common,
    stepIndex: 1,
    nextAddress: common.contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum: datumOf(common, nextState, TransactionOutputStep03DatumSchema),
    redeemerSchema: TransactionOutputStep02RedeemerSchema,
    args: (input_index, output_index) => ({
      input_index,
      output_index,
      opening,
    }),
    carriageUtxos,
    extraReferenceInputs,
  });

/** Step 03 with the window, successor and next checkpoint supplied verbatim. */
export const submitOutputStep03Raw = async ({
  window,
  nextState,
  nextStepIndex,
  ...common
}: Common & {
  readonly window: Buffer;
  readonly nextState: OutputScanStateData;
  /** 2 keeps the scan self-loop, 3 hands over to step 04. */
  readonly nextStepIndex: 2 | 3;
}) =>
  await continueRaw({
    common,
    stepIndex: 2,
    nextAddress: common.contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum: datumOf(common, nextState, TransactionOutputStep03DatumSchema),
    redeemerSchema: TransactionOutputStep03RedeemerSchema,
    args: (input_index, output_index) => ({
      input_index,
      output_index,
      window: window.toString("hex"),
    }),
  });

/** Step 04 without the off-chain contradiction guard, so an honest terminal reaches the validator. */
export const submitOutputStep04Raw = async ({
  witnessReferenceScripts,
  ...common
}: Common & {
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: FAMILY,
    stepIndex: 3,
    threadOutRef: common.threadOutRef,
  });
  return await submitLinearFaultFinalize({
    lucid: common.lucid,
    family: FAMILY,
    stepIndex: 3,
    step: common.contracts.steps[3],
    computationThread: common.contracts.computationThread,
    fraudProof: common.contracts.fraudProof,
    signer: common.signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: TransactionOutputStep04RedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo: common.referenceScriptUtxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
};

export type PublishedOutputFieldCarriage = {
  readonly planned: FaultProofFieldOpeningPlan;
  readonly carriageUtxos: readonly UTxO[];
  readonly certificateUtxo: UTxO | undefined;
};

/**
 * Publishes (and under tier 3 certifies) the field-2 carriage of one
 * transaction so a test can hand the step-02 door either the genuine opening
 * or this carriage under another transaction's anchor.
 */
export const publishOutputFieldCarriage = async ({
  lucid,
  network,
  signer,
  contracts,
  anchorTxId,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  items,
  certificateReferenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly contracts: TransactionOutputNonCanonicalContracts;
  readonly anchorTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly items: readonly Buffer[];
  readonly certificateReferenceScriptUtxo: UTxO;
}): Promise<PublishedOutputFieldCarriage> => {
  const planned = planFaultProofFieldOpening({
    fieldIndex: 2,
    anchorTxId,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: false,
    label: "transaction-output-non-canonical test carriage",
  });
  signer.selectWallet(lucid);
  const carriageUtxos = await publishFaultProofFieldCarriage({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "transaction-output-non-canonical test carriage",
  });
  const certificateUtxo =
    planned.plan.tier === "Certified"
      ? (
          await certifyFaultProofFieldCarriage({
            lucid,
            network,
            signer,
            planned,
            certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
            certificateMintingScript:
              contracts.fieldPreimageCertificateMintingScript,
            certificateReferenceScriptUtxo,
            chunkUtxos: carriageUtxos,
            compactCbor: nativeTxCompactCbor,
            witnessSetCompactCbor,
          })
        ).certificateUtxo
      : undefined;
  return { planned, carriageUtxos, certificateUtxo };
};

/** The reference inputs a step-02 transaction reads for a published carriage. */
export const carriageReferenceInputs = (
  carriage: PublishedOutputFieldCarriage,
  stepReference: UTxO,
): readonly UTxO[] => [
  ...carriage.carriageUtxos,
  stepReference,
  ...(carriage.certificateUtxo === undefined ? [] : [carriage.certificateUtxo]),
];

/** A body-field opening carrying `carriage` under `anchorCompactCbor`'s anchor. */
export const outputFieldOpening = ({
  anchorCompactCbor,
  carriage,
  stepReference,
  certificatePolicyId,
}: {
  readonly anchorCompactCbor: string;
  readonly carriage: PublishedOutputFieldCarriage;
  readonly stepReference: UTxO;
  readonly certificatePolicyId: string;
}): FieldOpening =>
  fieldOpeningForField({
    fieldIndex: 2,
    nativeTxCompactCbor: anchorCompactCbor,
    carriage: faultProofFieldCarriage({
      planned: carriage.planned,
      referenceInputs: carriageReferenceInputs(carriage, stepReference),
      certificatePolicyId,
      label: "transaction-output-non-canonical test opening",
    }),
  });

export type ForcedOutputFixture = Awaited<
  ReturnType<typeof buildForcedOutputFixture>
>;

/**
 * A block whose single forced transaction carries `outputCbor` at output 0 and
 * was rejected for `OutputNonCanonical { output_index }`; the retained DA is
 * reconstructed so the forced leaf, its membership proof and the transaction
 * preimage all come from the committed payload.
 */
export const buildForcedOutputFixture = async ({
  operatorVkey,
  now,
  outputCbor,
  outputIndex = 0n,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly outputCbor: Buffer;
  readonly outputIndex?: bigint;
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
  const nativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    referenceByte: "b1",
    outputCbors: [outputCbor],
    witnessByte: "b8",
  });
  const source = deriveMidgardNativeTxProofSource(
    adjudicateMidgardNativeTxFullValidity(nativeTx, "TxIsInvalid"),
  );
  const rejectionReason = {
    OutputNonCanonical: { output_index: outputIndex },
  } as const;
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
