import { buildMidgardBoundedItemV1 } from "@al-ft/midgard-core";
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
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { MissingScriptSourceContractsV1 } from "./contracts-v1.js";
import type { MissingScriptSourceEvidenceV1 } from "./family-v1.js";
import {
  ExecutionSourceScanStateV1Schema,
  ExecutionSourceStep05DatumV1Schema,
  ExecutionSourceStep05RedeemerV1Schema,
  ExecutionSourceStep06DatumV1Schema,
  SourceDescriptorV1Schema,
} from "./schemas-v1.js";
import {
  MISSING_SCRIPT_SOURCE_SCAN_BUDGET_V1,
  missingScriptSourceOnchainCheckpointV1,
} from "./universe-scan-v1.js";

const FAMILY = "missing-script-source";

/** Advances one bounded universal-source batch, self-looping until complete. */
export const submitMissingScriptSourceStep05V1 = async ({
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
  contracts: MissingScriptSourceContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: MissingScriptSourceEvidenceV1;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 4;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<
    Data.Static<typeof ExecutionSourceScanStateV1Schema>
  >({
    threadUtxo,
    signer,
    schema: ExecutionSourceStep05DatumV1Schema as never,
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
    cursor + MISSING_SCRIPT_SOURCE_SCAN_BUDGET_V1,
  );
  const sources: Data.Static<typeof SourceDescriptorV1Schema>[] = batch.map(
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
          : buildMidgardBoundedItemV1({
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
  const nextState: Data.Static<typeof ExecutionSourceScanStateV1Schema> = {
    ...state,
    cursor: BigInt(nextCursor),
    found,
    next_expected_script_hash: nextExpectedScriptHash,
    checkpoint_hash: missingScriptSourceOnchainCheckpointV1({
      sourceIdentityHex: state.authenticated.source_identity_hash,
      cursor: BigInt(nextCursor),
      found,
      nextExpectedScriptHashHex: nextExpectedScriptHash,
    }),
  };
  const nextSchema = closed
    ? ExecutionSourceStep06DatumV1Schema
    : ExecutionSourceStep05DatumV1Schema;
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    nextSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[closed ? 5 : 4].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScriptV1({
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
            item_budget: BigInt(MISSING_SCRIPT_SOURCE_SCAN_BUDGET_V1),
          },
        ],
      } as never,
      ExecutionSourceStep05RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinueV1({
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
