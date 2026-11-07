import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Constr,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { createDirectFieldPlan } from "../../src/direct-script-derivation/index.js";
import { faultProofFieldOpening } from "../../src/field-opening.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import {
  expectOnchainRefusal,
  measureCompleteSignedTransaction,
} from "./submit-init-emulator-shared.js";

/** One physical receive-lifecycle transition per call: every refused datum is
 * rejected on-chain first, then the honest successor is signed, measured,
 * submitted and becomes the next thread input. */
export const createDirectReceiveStepper = ({
  lucid,
  prover,
  unit,
  initialThread,
  references,
  carriageReferences,
  plan,
  sourcePlan,
  certificatePolicyId,
}: {
  readonly lucid: LucidEvolution;
  readonly prover: string;
  readonly unit: string;
  readonly initialThread: UTxO;
  readonly references: ReadonlyMap<string, UTxO>;
  readonly carriageReferences: readonly UTxO[];
  readonly plan: ReturnType<typeof createDirectFieldPlan>;
  readonly sourcePlan: ReturnType<typeof createDirectFieldPlan>;
  readonly certificatePolicyId: string;
}) => {
  let thread = initialThread;
  const measurements: {
    label: string;
    bytes: number;
    memory: string;
    cpu: string;
    transactionHash: string;
  }[] = [];
  const step = async (
    label: string,
    role: SDK.SpendingValidator,
    address: string,
    nextDatum: string,
    redeemer: (indices: {
      input_index: bigint;
      output_index: bigint;
      opening: SDK.FieldOpening;
    }) => string,
    {
      fieldIndex = 2,
      refusedDatums = [],
    }: {
      readonly fieldIndex?: 2 | 6;
      readonly refusedDatums?: readonly string[];
    } = {},
  ) => {
    let outputIndex: bigint | undefined;
    const build = (outputDatum: string) => {
      const spend = ((ctx) => {
        SDK.requireOwnSpendPurpose(ctx, thread, label);
        outputIndex = SDK.requireUniqueOutputIndex(
          ctx.outputs,
          computationThreadOutputPredicate({
            address,
            datum: outputDatum,
            unit,
          }),
          label,
        );
        return redeemer({
          input_index: SDK.requireInputIndex(ctx, thread, label),
          output_index: outputIndex,
          opening: faultProofFieldOpening({
            planned:
              fieldIndex === 2 ? plan.openingPlan : sourcePlan.openingPlan,
            referenceInputs: ctx.referenceInputs,
            certificatePolicyId,
            label: "N6 actual receive field",
          }),
        });
      }) satisfies BuildTxWithRedeemer;
      return lucid
        .newTx()
        .collectFrom([thread], spend)
        .readFrom([
          references.get(role.spendingScriptHash)!,
          ...(fieldIndex === 2 ? carriageReferences : []),
        ])
        .pay.ToContract(
          address,
          { kind: "inline", value: outputDatum },
          thread.assets,
        )
        .addSignerKey(prover);
    };
    const badDatum = Data.to(
      new Constr(0, [prover, new Constr(0, [new Constr(0, [])])]),
    );
    for (const refusedDatum of [badDatum, ...refusedDatums])
      await expectOnchainRefusal(() =>
        build(refusedDatum).complete({ localUPLCEval: true }),
      );
    const unsigned = await build(nextDatum).complete({ localUPLCEval: true });
    const signed = await unsigned.sign.withWallet().complete();
    const measured = measureCompleteSignedTransaction(signed.toCBOR());
    expect(measured.completeSignedBytes).toBeLessThanOrEqual(15872);
    expect(measured.executionMemory).toBeLessThanOrEqual(13_200_000n);
    expect(measured.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
    measurements.push({
      label,
      bytes: measured.completeSignedBytes,
      memory: measured.executionMemory.toString(),
      cpu: measured.executionSteps.toString(),
      transactionHash: signed.toHash(),
    });
    const hash = await signed.submit();
    await lucid.awaitTx(hash);
    if (outputIndex === undefined)
      throw new Error(`${label}: completed output index absent`);
    thread = (
      await lucid.utxosByOutRef([
        { txHash: hash, outputIndex: Number(outputIndex) },
      ])
    )[0]!;
    expect(Data.from(thread.datum!)).toEqual(Data.from(nextDatum));
  };
  return { step, measurements };
};
