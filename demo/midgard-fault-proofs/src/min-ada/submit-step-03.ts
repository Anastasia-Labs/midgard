import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { MinAdaOutputScanControlSchema } from "@al-ft/midgard-sdk";
import {
  MinAdaStep03DatumSchema,
  MinAdaStep03SpendRedeemerSchema,
  MinAdaStep04DatumSchema,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  MIDGARD_COINS_PER_UTXO_BYTE,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  linearFaultStepLabel,
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  MIN_ADA_CATEGORY_LABEL as FAMILY,
  type MinAdaContracts,
} from "./contracts.js";
import {
  minAdaOutputScanEvidence,
  transactionOutputScanControlData,
} from "./scan.js";

type State = NonNullable<Data.Static<typeof MinAdaStep03DatumSchema>["data"]>;
type Step03Datum = Data.Static<typeof MinAdaStep03DatumSchema>;
const Step03Datum = asDataType<Step03Datum>(MinAdaStep03DatumSchema);
type Step04Datum = Data.Static<typeof MinAdaStep04DatumSchema>;
const Step04Datum = asDataType<Step04Datum>(MinAdaStep04DatumSchema);
type Redeemer = Data.Static<typeof MinAdaStep03SpendRedeemerSchema>;
const Redeemer = asDataType<Redeemer>(MinAdaStep03SpendRedeemerSchema);

/**
 * Whether the authenticated descriptor carries the fault step 03 adjudicates in
 * its direction: an accepted output below the floor (direction 0), or a
 * rejected one that meets it (direction 1). The floor is Midgard's L2 rate,
 * the deployment constant step 03 compiles (`env.coins_per_utxo_byte`), never
 * the L1 protocol parameter.
 */
export const minAdaDescriptorIsFault = (facts: {
  readonly direction: bigint;
  readonly total_length: bigint;
  readonly lovelace: bigint;
}): boolean =>
  outputMeetsMinAda(
    MIDGARD_COINS_PER_UTXO_BYTE,
    facts.total_length,
    facts.lovelace,
  ) ===
  (facts.direction === 1n);

/** Applies the exact release-bound min-Ada predicate to the authenticated descriptor. */
export const submitMinAdaStep03 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  outputItemCbors,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
  unsafeSkipLocalViolationCheckForTest = false,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MinAdaContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly outputItemCbors?: readonly string[];
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  readonly unsafeSkipLocalViolationCheckForTest?: boolean;
}): Promise<{
  txHash: string;
  nextThreadOutRef: string;
  nextStepIndex: number;
}> => {
  const stepIndex = 2;
  const label = linearFaultStepLabel(FAMILY, stepIndex);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<State>({
    threadUtxo,
    signer,
    schema: Step03Datum,
    family: FAMILY,
    stepIndex,
  });
  if ("MinAdaTxScan" in state) {
    if (!outputItemCbors)
      throw new Error("min-ada output scan requires retained output preimage");
    const scan = state.MinAdaTxScan.scan;
    const evidence = minAdaOutputScanEvidence(
      scan.subject.transaction_id,
      scan.output_index,
      outputItemCbors,
    );
    if (
      evidence.itemHash !== scan.item_hash ||
      BigInt(evidence.itemLength) !== scan.item_length
    )
      throw new Error("min-ada output scan item changed");
    let cursor = threadOutRef;
    let scanState = state.MinAdaTxScan;
    while (true) {
      const index = evidence.scanControls.findIndex(
        (control) =>
          Data.to(
            transactionOutputScanControlData(control) as never,
            MinAdaOutputScanControlSchema as never,
          ) ===
          Data.to(
            scanState.scan.control as never,
            MinAdaOutputScanControlSchema as never,
          ),
      );
      if (index < 0) throw new Error("min-ada output scan checkpoint changed");
      let end = index;
      for (let count = 0; count < 4; count++) {
        const prior = evidence.scanControls[end]!;
        const next = evidence.scanControls[end + 1];
        if (!next) throw new Error("min-ada output scan is noncanonical");
        end++;
        if (
          end === evidence.scanControls.length - 1 ||
          Math.floor(prior.cursor / 4095) !== Math.floor(next.cursor / 4095) ||
          prior.stage <= 4 !== next.stage <= 4
        )
          break;
      }
      const control = evidence.scanControls[end]!;
      const terminal = end === evidence.scanControls.length - 1;
      const nextState: State = terminal
        ? {
            MinAdaTxDescriptor: {
              direction: scanState.direction,
              total_length: scan.item_length,
              lovelace: control.lovelace,
            },
          }
        : {
            MinAdaTxScan: {
              direction: scanState.direction,
              scan: {
                ...scanState.scan,
                control: transactionOutputScanControlData(control),
                outcome: 0n,
              },
            },
          };
      const current = evidence.scanControls[index]!;
      const start = Math.floor(current.cursor / 4095) * 4095;
      const bytes = Buffer.from(evidence.itemHex, "hex");
      const window = bytes
        .subarray(start, start + (current.stage <= 4 ? 8190 : 4095))
        .toString("hex");
      const active = await requireLinearFaultThreadUtxo({
        lucid,
        contracts,
        categoryId,
        family: FAMILY,
        stepIndex,
        threadOutRef: cursor,
      });
      const reference = requireLinearFaultReferenceScript({
        utxo: referenceScriptUtxo,
        expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
        family: FAMILY,
        stepIndex,
      });
      const nextDatum = Data.to(
        { fraud_prover: signer.paymentKeyHash, data: nextState },
        Step03Datum,
      );
      const matches = computationThreadOutputPredicate({
        address: contracts.steps[stepIndex].spendingScriptAddress,
        datum: nextDatum,
        unit: active.threadToken.unit,
      });
      let outputIndex: bigint | undefined;
      const redeemer = ((ctx) => {
        requireOwnSpendPurpose(ctx, active.threadUtxo, label);
        outputIndex = requireUniqueOutputIndex(ctx.outputs, matches, label);
        return Data.to(
          {
            Continue: [
              {
                window,
                input_index: requireInputIndex(ctx, active.threadUtxo, label),
                output_index: outputIndex,
              },
            ],
          },
          Redeemer,
        );
      }) satisfies BuildTxWithRedeemer;
      signer.selectWallet(lucid);
      const txHash = await submitLinearFaultContinue({
        lucid,
        signerPaymentKeyHash: signer.paymentKeyHash,
        threadUtxo: active.threadUtxo,
        threadUnit: active.threadToken.unit,
        stepReference: reference,
        stepScript: contracts.steps[stepIndex].spendingScript,
        stepRole: label,
        nextAddress: contracts.steps[stepIndex].spendingScriptAddress,
        nextDatum,
        redeemer,
        preSubmitBoundary,
        awaitConfirmation,
      });
      if (outputIndex === undefined)
        throw new Error("min-ada scan layout unresolved");
      cursor = `${txHash}#${outputIndex}`;
      if (!awaitConfirmation)
        return { txHash, nextThreadOutRef: cursor, nextStepIndex: 2 };
      if (terminal)
        return submitMinAdaStep03({
          lucid,
          contracts,
          categoryId,
          signer,
          threadOutRef: cursor,
          referenceScriptUtxo,
          preSubmitBoundary,
          awaitConfirmation,
          unsafeSkipLocalViolationCheckForTest,
        });
      if (!("MinAdaTxScan" in nextState))
        throw new Error("min-ada scan state changed");
      scanState = nextState.MinAdaTxScan;
    }
  }
  const facts = state.MinAdaTxDescriptor;
  if (
    !unsafeSkipLocalViolationCheckForTest &&
    !minAdaDescriptorIsFault(facts)
  ) {
    throw new Error(
      `${label}: authenticated descriptor does not violate min-Ada`,
    );
  }
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: "PredicateAuthenticated" },
    Step04Datum,
  );
  const nextStepIndex = 3;
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const inputIndex = requireInputIndex(ctx, threadUtxo, label);
    outputIndex = requireUniqueOutputIndex(ctx.outputs, outputMatches, label);
    return Data.to(
      {
        Continue: [
          { window: "", input_index: inputIndex, output_index: outputIndex },
        ],
      },
      Redeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: label,
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined) throw new Error(`${label}: unresolved layout`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    nextStepIndex,
  };
};
