import {
  type FieldOpening,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type Data as PlutusData,
  type LucidEvolution,
  type RedeemerContext,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { buildLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { ProtectedOutputSignerMissingContracts } from "./contracts.js";

export const buildProtectedOutputSignerOpeningTransition = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  stepIndex,
  nextStepIndex,
  nextDatum,
  openingFor,
  coordinateOutOfRange,
  grammarRefInputIndex,
  referenceScriptUtxo,
  carriageReferenceInputs,
  redeemerSchema,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ProtectedOutputSignerMissingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stepIndex: 1;
  /** Step 02 continues at the witness scan (2) or the terminal (3). */
  readonly nextStepIndex: 2 | 3;
  readonly nextDatum: string;
  /** The opening, resolved against the built transaction's reference inputs. */
  readonly openingFor: (ctx: RedeemerContext) => FieldOpening;
  /** Names the out-of-range direct arm in the redeemer. */
  readonly coordinateOutOfRange?: boolean;
  /** The field grammar certificate's slot, or null to walk. */
  readonly grammarRefInputIndex?: (
    referenceInputs: readonly UTxO[],
  ) => bigint | null;
  readonly referenceScriptUtxo: UTxO;
  readonly carriageReferenceInputs: readonly UTxO[];
  readonly redeemerSchema: PlutusData;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "protected-output-signer-missing",
    stepIndex,
    threadOutRef,
  });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: "protected-output-signer-missing",
    stepIndex,
  });
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      `protected-output-signer-missing step-${(stepIndex + 1).toString().padStart(2, "0")}`,
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "protected-output-signer-missing",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "protected-output-signer-missing next output",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            opening: openingFor(ctx),
            coordinate_out_of_range: coordinateOutOfRange ?? false,
            grammar_ref_input_index:
              grammarRefInputIndex?.(ctx.referenceInputs) ?? null,
          },
        ],
      } as never,
      redeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const transaction = await buildLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: `protected-output-signer-missing step-${(stepIndex + 1).toString().padStart(2, "0")}`,
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: carriageReferenceInputs.filter(
      (utxo) =>
        utxo.txHash !== stepReference.txHash ||
        utxo.outputIndex !== stepReference.outputIndex,
    ),
  });
  if (outputIndex === undefined)
    throw new Error("protected-output-signer-missing: layout unresolved");
  return {
    transaction,
    nextThreadOutRef: `${transaction.txHash}#${outputIndex.toString()}`,
  };
};
