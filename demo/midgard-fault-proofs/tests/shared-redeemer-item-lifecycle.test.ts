import {
  asLucidDataValue,
  buildMidgardRedeemerItemProofTrace,
  computeHash32,
  encodeCbor,
  MidgardRedeemerItemProofModes,
} from "@al-ft/midgard-core";
import {
  buildCekRedeemerItemStages,
  parseFaultProofBlueprint,
  requireInputIndex,
} from "@al-ft/midgard-sdk";
import {
  redeemerItemControlData,
  redeemerItemProofWitnessData,
} from "@al-ft/midgard-validation";
import {
  type BuildTxWithRedeemer,
  Constr,
  credentialToAddress,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import {
  deriveCekRedeemerItemPlan,
  redeemerItemExecutor,
} from "../src/redeemer-item-plan.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  expectOnchainRefusal,
  measureCompleteSignedTransaction,
  network,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

it("executes every shared item executor through the exact CEK Pending to Verified wire", async () => {
  const deploymentId = "71".repeat(32),
    policy = "72".repeat(28),
    returnHash = "73".repeat(28);
  const stages = buildCekRedeemerItemStages({
    blueprint: parseFaultProofBlueprint(readBlueprint(realBlueprintPath)),
    network,
    computationThreadPolicyId: policy,
    deploymentId,
    returnScriptHash: returnHash,
  });
  const traces = [
    "00",
    Data.to(-(2n ** 90n)),
    Data.to("ab".repeat(4500)),
    "9f0001ff",
    "a200010203",
    "d8799f0102ff",
    "d8668218809f01ff",
  ].map((value) =>
    buildMidgardRedeemerItemProofTrace({
      itemIndex: 0,
      itemCount: 1,
      itemBytes: encodeCbor([0n, 0n, Buffer.from(value, "hex"), [10n, 20n]]),
      mode: MidgardRedeemerItemProofModes.Data,
    }),
  );
  const selected = new Map<number, (typeof traces)[number]["steps"][number]>();
  for (const trace of traces)
    for (const step of trace.steps) {
      const index = redeemerItemExecutor(step.control, step.witness).index;
      if (!selected.has(index)) selected.set(index, step);
    }
  expect([...selected.keys()].sort((a, b) => a - b)).toEqual(
    Array.from({ length: 17 }, (_, i) => i),
  );
  const account = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover = getAddressDetails(account.address).paymentCredential!.hash;
  const datum = (state: Data) =>
    Data.to(new Constr(0, [prover, new Constr(0, [state])]));
  const cases = [...selected].map(([index, step]) => {
    const witness = Data.from(
      Data.to(asLucidDataValue(redeemerItemProofWitnessData(step.witness))),
    );
    const pending = new Constr(0, [
      new Constr(0, [BigInt(index)]),
      Data.from(
        Data.to(asLucidDataValue(redeemerItemControlData(step.control))),
      ),
      computeHash32(Buffer.from(Data.to(witness), "hex")).toString("hex"),
      Data.from(Data.to(asLucidDataValue(redeemerItemControlData(step.next)))),
    ]);
    return {
      index,
      unit: policy + index.toString(16).padStart(64, "0"),
      plan: deriveCekRedeemerItemPlan({
        pending,
        witness,
        stages,
        deploymentId,
      }),
      pending,
    };
  });
  const emulator = new Emulator(
    [
      account,
      ...cases.map((c) => ({
        ...account,
        address: stages.entry.spendingScriptAddress,
        assets: { lovelace: 30_000_000n, [c.unit]: 1n },
        outputData: { inline: datum(c.pending) },
      })),
    ],
    { ...EMULATOR_PROTOCOL_PARAMETERS, maxTxSize: 16384 },
  );
  const lucid = await Lucid(emulator, network);
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const refs = new Map<string, UTxO>();
  for (const role of [
    stages.entry,
    stages.traversalNormalizer,
    stages.outerNormalizer,
    stages.sourceAuthenticator,
    ...stages.executors,
    stages.settlement,
  ]) {
    const pub = await publishPlainReferenceScriptUtxo({
      lucid,
      script: role.spendingScript,
      label: "shared item",
    });
    refs.set(role.spendingScriptHash, pub.utxo);
  }
  for (const c of cases) {
    let thread = (await lucid.utxosAt(stages.entry.spendingScriptAddress)).find(
      (u) => u.assets[c.unit] === 1n,
    )!;
    for (let i = 0; i < c.plan.length; i++) {
      const binding = c.plan[i]!;
      const nextAddress =
        c.plan[i + 1]?.validator.spendingScriptAddress ??
        credentialToAddress(network, { type: "Script", hash: returnHash });
      const spend = ((ctx) =>
        Data.to(
          binding.spendRedeemer(
            requireInputIndex(ctx, thread, "shared item"),
            0n,
          ),
        )) satisfies BuildTxWithRedeemer;
      await expectOnchainRefusal(() =>
        lucid
          .newTx()
          .collectFrom([thread], spend)
          .readFrom([refs.get(binding.validator.spendingScriptHash)!])
          .pay.ToContract(
            nextAddress,
            { kind: "inline", value: datum(new Constr(0, [0n])) },
            thread.assets,
          )
          .addSignerKey(prover)
          .complete(),
      );
      const unsigned = await lucid
        .newTx()
        .collectFrom([thread], spend)
        .readFrom([refs.get(binding.validator.spendingScriptHash)!])
        .pay.ToContract(
          nextAddress,
          { kind: "inline", value: datum(binding.outputState) },
          thread.assets,
        )
        .addSignerKey(prover)
        .complete()
        .catch((cause: unknown) => {
          throw new Error(
            `executor ${c.index} ${binding.key}: ${String(cause)}`,
          );
        });
      const signed = await unsigned.sign.withWallet().complete();
      const measured = measureCompleteSignedTransaction(signed.toCBOR());
      expect(measured.completeSignedBytes).toBeLessThanOrEqual(15872);
      expect(measured.executionMemory).toBeLessThanOrEqual(13_200_000n);
      expect(measured.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
      const hash = await signed.submit();
      await lucid.awaitTx(hash);
      thread = (
        await lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }])
      )[0]!;
      expect(Data.from(thread.datum!)).toEqual(
        Data.from(datum(binding.outputState)),
      );
    }
  }
}, 1_800_000);
