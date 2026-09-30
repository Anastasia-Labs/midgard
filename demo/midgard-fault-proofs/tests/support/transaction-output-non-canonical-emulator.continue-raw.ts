import { initialMidgardLedgerOutputScanControl } from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  forcedVerdictSubject,
  type Header,
  OutputReference,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type RootMembershipProof,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import type { TransactionOutputNonCanonicalContracts } from "../../src/transaction-output-non-canonical/contracts.js";
import {
  TransactionOutputStep01RedeemerSchema,
  TransactionOutputStep02DatumSchema,
  TransactionOutputStep03DatumSchema,
  TransactionOutputStep04DatumSchema,
} from "../../src/transaction-output-non-canonical/schemas.js";
import {
  type TransactionOutputEvidence,
  transactionOutputScanControlData,
} from "../../src/transaction-output-non-canonical/transaction-output-non-canonical.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";

export const FAMILY = "transaction-output-non-canonical";

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
export const continueRaw = async ({
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

export const datumOf = (common: Common, data: unknown, schema: unknown) =>
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
