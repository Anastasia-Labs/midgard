import { buildMidgardBoundedItem } from "@al-ft/midgard-core";
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
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
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { MissingScriptSourceContracts } from "./contracts.js";
import type { MissingScriptSourceEvidence } from "./family.js";
import {
  ExecutionSourceScanStateSchema,
  ExecutionSourceStep05DatumSchema,
  ExecutionSourceStep05RedeemerSchema,
  ExecutionSourceStep06DatumSchema,
  SourceDescriptorSchema,
} from "./schemas.js";
import {
  MISSING_SCRIPT_SOURCE_SCAN_BUDGET,
  missingScriptSourceOnchainCheckpoint,
} from "./universe-scan.js";

const FAMILY = "missing-script-source";

/** Advances one bounded universal-source batch, self-looping until complete. */
export const submitMissingScriptSourceStep05 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: MissingScriptSourceContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: MissingScriptSourceEvidence;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 4;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<
    Data.Static<typeof ExecutionSourceScanStateSchema>
  >({
    threadUtxo,
    signer,
    schema: ExecutionSourceStep05DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const cursor = Number(state.cursor);
  if (
    !Number.isSafeInteger(cursor) ||
    cursor < 0 ||
    cursor > evidence.sources.length ||
    state.authenticated.purpose.scan_limit !== BigInt(evidence.sources.length)
  )
    throw new Error(`${FAMILY}: scan cursor/source frontier changed`);
  const batch = evidence.sources.slice(
    cursor,
    cursor + MISSING_SCRIPT_SOURCE_SCAN_BUDGET,
  );
  const sources: Data.Static<typeof SourceDescriptorSchema>[] = batch.map(
    (source, offset) => {
      if (source.sourceIndex !== cursor + offset)
        throw new Error(`${FAMILY}: scan batch is not consensus ordered`);
      const itemBytes = Buffer.from(source.scriptItemHex, "hex");
      const itemIndex =
        source.originKind === 0
          ? source.sourceIndex
          : Buffer.from(source.sourceKeyHex, "hex").readUInt16BE(36);
      const bounded =
        itemBytes.length === 0
          ? null
          : buildMidgardBoundedItem({
              fieldIndex: source.originKind === 0 ? 6 : 2,
              itemIndex,
              bytes: itemBytes,
            });
      const totalLength = source.scriptTotalLength ?? itemBytes.length;
      const itemCommitment =
        source.scriptItemCommitmentHex ?? bounded?.commitment.toString("hex");
      if (
        itemCommitment === undefined ||
        (bounded !== null &&
          (totalLength !== itemBytes.length ||
            itemCommitment !== bounded.commitment.toString("hex")))
      )
        throw new Error(`${FAMILY}: source item commitment changed`);
      return {
        source_index: BigInt(source.sourceIndex),
        location_kind: BigInt(source.originKind),
        source_key: source.sourceKeyHex,
        language_tag: BigInt(source.languageTag),
        script_hash: source.scriptHashHex,
        total_length: BigInt(totalLength),
        item_commitment: itemCommitment,
        siblings: source.sourceMembership.siblings.map((sibling) =>
          Buffer.from(sibling).toString("hex"),
        ),
      };
    },
  );
  const nextCursor = cursor + sources.length;
  const found =
    state.found ||
    batch.some(
      ({ scriptHashHex }) =>
        scriptHashHex === state.authenticated.purpose.required_script_hash,
    );
  const closed = nextCursor === evidence.sources.length;
  if (!closed && sources.length === 0)
    throw new Error(`${FAMILY}: empty scan batch cannot make progress`);
  const nextExpectedScriptHash =
    contracts.steps[closed ? 5 : 4].spendingScriptHash;
  const nextState: Data.Static<typeof ExecutionSourceScanStateSchema> = {
    ...state,
    cursor: BigInt(nextCursor),
    found,
    next_expected_script_hash: nextExpectedScriptHash,
    checkpoint_hash: missingScriptSourceOnchainCheckpoint({
      sourceIdentityHex: state.authenticated.source_identity_hash,
      cursor: BigInt(nextCursor),
      found,
      nextExpectedScriptHashHex: nextExpectedScriptHash,
    }),
  };
  const nextSchema = closed
    ? ExecutionSourceStep06DatumSchema
    : ExecutionSourceStep05DatumSchema;
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    nextSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[closed ? 5 : 4].spendingScriptAddress,
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
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 05`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step 05`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step 05`,
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            sources,
            item_budget: BigInt(MISSING_SCRIPT_SOURCE_SCAN_BUDGET),
          },
        ],
      } as never,
      ExecutionSourceStep05RedeemerSchema as never,
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
    stepRole: `${FAMILY} step 05`,
    nextAddress: contracts.steps[closed ? 5 : 4].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    closed,
  };
};
