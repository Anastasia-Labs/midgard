import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  CML,
  Data,
  getAddressDetails,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import {
  expectOnchainRefusal,
  measureCompleteSignedTransaction,
} from "./submit-init-emulator-shared.js";

type ComponentTransition = {
  readonly label: string;
  readonly role: SDK.SpendingValidator;
  readonly destination: string;
  readonly datum: string;
  readonly fields?: readonly number[];
  readonly redeemer: (
    indices: { input_index: bigint; output_index: bigint },
    open: (field: number) => SDK.FieldOpening,
  ) => string;
};

/** Real completed indices, reference scripts and full signed measurements for
 * component regressions. The seeded origin remains the caller's explicit scope. */
export const createDirectComponentStepper = ({
  initialThread,
  unit,
  references,
  carriage = new Map(),
  opening,
}: {
  readonly initialThread: UTxO;
  readonly unit: string;
  readonly references: ReadonlyMap<string, UTxO>;
  readonly carriage?: ReadonlyMap<number, readonly UTxO[]>;
  readonly opening: (
    field: number,
    references: readonly UTxO[],
  ) => SDK.FieldOpening;
}) => {
  let thread = initialThread;
  let outputIndex: bigint | undefined;
  const measurements: {
    label: string;
    bytes: number;
    memory: string;
    cpu: string;
    fee: string;
  }[] = [];
  const build = async (
    transition: ComponentTransition,
    lucid: LucidEvolution,
  ) => {
    const signer = getAddressDetails(await lucid.wallet().address())
      .paymentCredential!.hash;
    const spend = ((ctx) => {
      SDK.requireOwnSpendPurpose(ctx, thread, transition.label);
      outputIndex = SDK.requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: transition.destination,
          datum: transition.datum,
          unit,
        }),
        transition.label,
      );
      return transition.redeemer(
        {
          input_index: SDK.requireInputIndex(ctx, thread, transition.label),
          output_index: outputIndex,
        },
        (field) => opening(field, ctx.referenceInputs),
      );
    }) satisfies BuildTxWithRedeemer;
    return lucid
      .newTx()
      .collectFrom([thread], spend)
      .readFrom([
        references.get(transition.role.spendingScriptHash)!,
        ...(transition.fields ?? []).flatMap(
          (field) => carriage.get(field) ?? [],
        ),
      ])
      .pay.ToContract(
        transition.destination,
        { kind: "inline", value: transition.datum },
        thread.assets,
      )
      .addSignerKey(signer);
  };
  const complete = async (
    transition: ComponentTransition,
    lucid: LucidEvolution,
  ) => (await build(transition, lucid)).complete({ localUPLCEval: true });
  const accept = async (
    transition: ComponentTransition,
    lucid: LucidEvolution,
  ) => {
    const signed = await (await complete(transition, lucid)).sign
      .withWallet()
      .complete();
    const cbor = signed.toCBOR();
    const measured = measureCompleteSignedTransaction(cbor);
    expect(measured.completeSignedBytes).toBeLessThanOrEqual(15872);
    expect(measured.executionMemory).toBeLessThanOrEqual(13_200_000n);
    expect(measured.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
    const hash = await signed.submit();
    await lucid.awaitTx(hash);
    if (outputIndex === undefined)
      throw new Error(`${transition.label}: output index missing`);
    thread = (
      await lucid.utxosByOutRef([
        { txHash: hash, outputIndex: Number(outputIndex) },
      ])
    )[0]!;
    expect(Data.from(thread.datum!)).toEqual(Data.from(transition.datum));
    expect(thread.assets).toEqual(initialThread.assets);
    measurements.push({
      label: transition.label,
      bytes: measured.completeSignedBytes,
      memory: measured.executionMemory.toString(),
      cpu: measured.executionSteps.toString(),
      fee: CML.Transaction.from_cbor_hex(cbor).body().fee().toString(),
    });
  };
  const refuse = async (
    transition: ComponentTransition,
    lucid: LucidEvolution,
  ) => expectOnchainRefusal(() => complete(transition, lucid));
  return { accept, refuse, complete, measurements, thread: () => thread };
};
