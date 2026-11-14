import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  CML,
  Constr,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

type SourceRedeemerBuilder = (
  indices: { input_index: bigint; output_index: bigint },
  open: (field: number) => SDK.FieldOpening,
) => string;

import { createDirectFieldPlan } from "../../src/direct-script-derivation/index.js";
import { faultProofFieldOpening } from "../../src/field-opening.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import {
  expectOnchainRefusal,
  measureCompleteSignedTransaction,
} from "./submit-init-emulator-shared.js";

export const createDirectSourceStepper = ({
  lucid,
  prover,
  unit,
  initialThread,
  references,
  carriage,
  plan,
  certificatePolicyId,
}: {
  readonly lucid: LucidEvolution;
  readonly prover: string;
  readonly unit: string;
  readonly initialThread: UTxO;
  readonly references: ReadonlyMap<string, UTxO>;
  readonly carriage: ReadonlyMap<number, readonly UTxO[]>;
  readonly plan: (field: number) => ReturnType<typeof createDirectFieldPlan>;
  readonly certificatePolicyId: string;
}) => {
  let thread = initialThread;
  const measurements: {
    label: string;
    bytes: number;
    memory: string;
    cpu: string;
    fee: string;
    elapsedMs: number;
  }[] = [];
  const step = async (
    label: string,
    role: SDK.SpendingValidator,
    address: string,
    nextDatum: string,
    fields: readonly number[],
    redeemer: SourceRedeemerBuilder,
    badDatums: readonly string[] = [],
    badRedeemers: readonly SourceRedeemerBuilder[] = [],
    badDestinations: readonly string[] = [],
  ) => {
    let outputIndex: bigint | undefined;
    const build = (
      outputDatum: string,
      selectedRedeemer = redeemer,
      destination = address,
    ) => {
      const spend = ((ctx) => {
        SDK.requireOwnSpendPurpose(ctx, thread, label);
        outputIndex = SDK.requireUniqueOutputIndex(
          ctx.outputs,
          computationThreadOutputPredicate({
            address: destination,
            datum: outputDatum,
            unit,
          }),
          label,
        );
        return selectedRedeemer(
          {
            input_index: SDK.requireInputIndex(ctx, thread, label),
            output_index: outputIndex,
          },
          (field) =>
            faultProofFieldOpening({
              planned: plan(field).openingPlan,
              referenceInputs: ctx.referenceInputs,
              certificatePolicyId,
              label,
            }),
        );
      }) satisfies BuildTxWithRedeemer;
      return lucid
        .newTx()
        .collectFrom([thread], spend)
        .readFrom([
          references.get(role.spendingScriptHash)!,
          ...fields.flatMap((field) => carriage.get(field)!),
        ])
        .pay.ToContract(
          destination,
          { kind: "inline", value: outputDatum },
          thread.assets,
        )
        .addSignerKey(prover);
    };
    const badDatum = Data.to(new Constr(0, [prover, new Constr(0, [0n])]));
    for (const changed of [badDatum, ...badDatums])
      await expectOnchainRefusal(() =>
        build(changed).complete({ localUPLCEval: true }),
      );
    for (const changed of badRedeemers)
      await expectOnchainRefusal(() =>
        build(nextDatum, changed).complete({ localUPLCEval: true }),
      );
    for (const changed of badDestinations)
      await expectOnchainRefusal(() =>
        build(nextDatum, redeemer, changed).complete({ localUPLCEval: true }),
      );
    const started = performance.now();
    const unsigned = await build(nextDatum).complete({ localUPLCEval: true });
    const signed = await unsigned.sign.withWallet().complete();
    const cbor = signed.toCBOR();
    const measured = measureCompleteSignedTransaction(cbor);
    expect(measured.completeSignedBytes).toBeLessThanOrEqual(15872);
    expect(measured.executionMemory).toBeLessThanOrEqual(13_200_000n);
    expect(measured.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
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
    expect(thread.assets).toEqual({ lovelace: 30_000_000n, [unit]: 1n });
    measurements.push({
      label,
      bytes: measured.completeSignedBytes,
      memory: measured.executionMemory.toString(),
      cpu: measured.executionSteps.toString(),
      fee: CML.Transaction.from_cbor_hex(cbor).body().fee().toString(),
      elapsedMs: performance.now() - started,
    });
  };
  const funding = async (initialWalletLovelace: bigint) => {
    const walletUtxos = await lucid.wallet().getUtxos();
    const remainingWalletLovelace = walletUtxos.reduce(
      (sum, item) => sum + item.assets.lovelace,
      0n,
    );
    const walletOutRefs = new Set(
      walletUtxos.map((item) => `${item.txHash}#${item.outputIndex}`),
    );
    const sourceReferenceLovelace = [...references.values()].reduce(
      (sum, item) => sum + item.assets.lovelace,
      0n,
    );
    const uniqueCarriage = new Map(
      [...carriage.values()]
        .flat()
        .map((item) => [`${item.txHash}#${item.outputIndex}`, item]),
    );
    const liveCarriage = await lucid.utxosByOutRef(
      [...uniqueCarriage.values()].map(({ txHash, outputIndex }) => ({
        txHash,
        outputIndex,
      })),
    );
    const carriageLovelace = liveCarriage.reduce(
      (sum, item) => sum + item.assets.lovelace,
      0n,
    );
    const walletCarriageLovelace = liveCarriage.reduce(
      (sum, item) =>
        sum +
        (walletOutRefs.has(`${item.txHash}#${item.outputIndex}`)
          ? item.assets.lovelace
          : 0n),
      0n,
    );
    const externalCarriageLovelace = carriageLovelace - walletCarriageLovelace;
    const stepFees = measurements.reduce(
      (sum, item) => sum + BigInt(item.fee),
      0n,
    );
    const walletSpent = initialWalletLovelace - remainingWalletLovelace;
    return {
      initialWalletLovelace: initialWalletLovelace.toString(),
      remainingWalletLovelace: remainingWalletLovelace.toString(),
      walletSpentLovelace: walletSpent.toString(),
      sourceReferenceLovelace: sourceReferenceLovelace.toString(),
      liveCarriageLovelace: carriageLovelace.toString(),
      carriageIncludedInWalletLovelace: walletCarriageLovelace.toString(),
      carriageOutsideWalletLovelace: externalCarriageLovelace.toString(),
      actualStepFeesLovelace: stepFees.toString(),
      otherPublicationAndCertificationCostLovelace: (
        walletSpent -
        sourceReferenceLovelace -
        externalCarriageLovelace -
        stepFees
      ).toString(),
      retainedThreadLovelace: thread.assets.lovelace.toString(),
      qualification:
        "Seeded bound prefix; excludes CT Init, family terminal, proof mint, state-queue removal and live latency. Live carriage at the wallet remains in its balance and is not subtracted twice. Other cost includes publication/certificate fees and any certificate script reference omitted from returned carriage references.",
    };
  };
  return { step, measurements, funding };
};
