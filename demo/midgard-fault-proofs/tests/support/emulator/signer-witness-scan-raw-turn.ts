import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk/core";
import { type FieldOpening } from "@al-ft/midgard-sdk/fraud-proof";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type RedeemerContext,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type LinearFaultContracts,
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../../../src/linear-fault-family.js";
import { buildLinearFaultContinue } from "../../../src/linear-fault-submit.js";
import type { ResolvedProverSigner } from "../../../src/runtime.js";
import { SignerWitnessScanRedeemerSchema } from "../../../src/signer-witness-scan/schemas.js";
import {
  SIGNER_WITNESS_SCAN_STEP_INDEX,
  SIGNER_WITNESS_TERMINAL_STEP_INDEX,
} from "../../../src/signer-witness-scan/submit-scan.js";
import { computationThreadOutputPredicate } from "../../../src/tx-layout.js";
import { buildAndSubmitTransaction } from "../submit-steps.js";

/**
 * One turn of the shared signer-witness scan carrying exactly the caller's
 * next datum, opening and checkpoint, so a refusal comes from the scan
 * validator rather than from the production builder's guards.
 */
export const submitRawSignerWitnessScanTurn = async ({
  lucid,
  family,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  terminal,
  nextDatum,
  openingFor,
  checkpointCbor,
  referenceScriptUtxo,
  carriageReferenceInputs,
}: {
  readonly lucid: LucidEvolution;
  readonly family: string;
  readonly contracts: LinearFaultContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Writes the next datum to the terminal instead of the scan. */
  readonly terminal: boolean;
  readonly nextDatum: string;
  /** The opening, resolved against the built transaction's reference inputs. */
  readonly openingFor: (ctx: RedeemerContext) => FieldOpening;
  /** `null` on the opening turn. */
  readonly checkpointCbor: string | null;
  readonly referenceScriptUtxo: UTxO;
  /** Carriage chunks and certificate; the step reference is added here. */
  readonly carriageReferenceInputs: readonly UTxO[];
}) => {
  const stepIndex = SIGNER_WITNESS_SCAN_STEP_INDEX;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family,
    stepIndex,
    threadOutRef,
  });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex]!.spendingScriptHash,
    family,
    stepIndex,
  });
  const nextStep =
    contracts.steps[terminal ? SIGNER_WITNESS_TERMINAL_STEP_INDEX : stepIndex]!;
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const role = `${family} raw scan`;
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, role);
    const inputIndex = requireInputIndex(ctx, threadUtxo, role);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${role} output`,
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            witnesses_opening: openingFor(ctx),
            checkpoint_cbor: checkpointCbor,
          },
        ],
      } as never,
      SignerWitnessScanRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await buildAndSubmitTransaction(buildLinearFaultContinue, {
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex]!.spendingScript,
    stepRole: role,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: carriageReferenceInputs.filter(
      (utxo) =>
        utxo.txHash !== stepReference.txHash ||
        utxo.outputIndex !== stepReference.outputIndex,
    ),
    awaitConfirmation: true,
  });
  if (outputIndex === undefined)
    throw new Error(`${family}: raw scan layout unresolved`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
