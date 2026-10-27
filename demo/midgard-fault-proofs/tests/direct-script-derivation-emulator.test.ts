import { computeHash28 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
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
import { Effect } from "effect";
import { expect, it } from "vitest";

import {
  directBindEventRedeemer,
  directBindingStates,
  directBindPreStateRedeemer,
  prepareDirectScriptMaterial,
} from "../src/direct-script-derivation/index.js";
import { carriedCountedMembership } from "../src/input-existence/counted-membership-carriage.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.js";
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "./helpers/canonical-block-evidence-fixture.js";
import { driveDirectReceiveLifecycle } from "./support/direct-receive-lifecycle.js";
import { maximumDirectReceiveOutputs } from "./support/direct-receive-max-fixture.js";
import { driveDirectSourceLifecycle } from "./support/direct-source-lifecycle.js";
import { prepareDirectSourceResourceFixture } from "./support/direct-source-resource-fixture.js";
import { measureDirectSourceResourcePath } from "./support/direct-source-resource-measurement.js";
import { nativeDecodingFixture } from "./support/native-script-decoding-retained.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  expectOnchainRefusal,
  measureCompleteSignedTransaction,
  network,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
} from "./support/submit-init-emulator-shared.js";

const materialFor = async (direction: "A-N" | "A-F" | "R-F") => {
  if (direction !== "A-N") {
    const fixture = await nativeDecodingFixture({
      direction: direction === "A-F" ? 0 : 1,
    });
    return prepareDirectScriptMaterial({
      current: fixture.evidence.reconstruction,
      eventKey: {
        ForcedTransactionEventKey: {
          tx_order_id:
            fixture.evidence.reconstruction.forcedTransactions[0]!.key,
        },
      },
      predecessor: completeCanonicalReplayPredecessorLedger({
        evidence: fixture.evidence,
        context: fixture.context,
      }),
    });
  }
  const tx = buildFixtureTransaction({ spendInputs: [], fee: 0n });
  const eventKey: SDK.EventKey = { L2TransactionEventKey: { tx_id: tx.txId } };
  const fixture = await buildCanonicalBlockFixture({
    transactions: [tx],
    transitionSteps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "L2Transaction",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
    ],
  });
  return prepareDirectScriptMaterial({
    current: await reconstructDaPayload({
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    }),
    eventKey,
  });
};

it.each(["A-N", "A-F", "R-F"] as const)(
  "executes separate direct event and pre-state custody steps for %s",
  async (direction) => {
    const material = await materialFor(direction);
    const states = directBindingStates(material);
    const blueprint = readBlueprint(realBlueprintPath);
    const policy = "cc".repeat(28),
      returnHash = "ee".repeat(28);
    const roles = await Effect.runPromise(
      SDK.buildDirectBindingContracts({
        blueprint,
        network,
        computationThreadPolicyId: policy,
        nextStepScriptHash: returnHash,
      }),
    );
    const account = generateEmulatorAccount({ lovelace: 40_000_000_000n });
    const prover = getAddressDetails(account.address).paymentCredential!.hash;
    const unit =
      policy +
      "0000002b" +
      computeHash28(SDK.encodeHeaderCbor(material.sourceBound.header)).toString(
        "hex",
      );
    // A seeded, already carried source tests this prefix's real spending/yield
    // composition; full source inclusion/Init/family terminal is a separate gate.
    const startDatum = Data.to(
      { fraud_prover: prover, data: states.source },
      SDK.DirectBindEventDatum,
    );
    const emulator = new Emulator(
      [
        account,
        {
          ...account,
          address: roles.event.spendingScriptAddress,
          assets: { lovelace: 30_000_000n, [unit]: 1n },
          outputData: { inline: startDatum },
        },
      ],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const lucid = await Lucid(emulator, network);
    lucid.selectWallet.fromSeed(account.seedPhrase);
    await registerPhasMembershipRewardAccount(lucid, blueprint);
    const phas = SDK.phasMembershipWithdrawalScriptFromBlueprint(
      SDK.parsePhasMembershipBlueprint(blueprint),
    );
    const reward = SDK.phasMembershipRewardAddress(network, phas);
    const refs: UTxO[] = [];
    for (const role of [roles.event, roles.preState])
      refs.push(
        (
          await publishPlainReferenceScriptUtxo({
            lucid,
            script: role.spendingScript,
            label: "N6 custody",
          })
        ).utxo,
      );
    let thread = (await lucid.utxosAt(roles.event.spendingScriptAddress)).find(
      (u) => u.assets[unit] === 1n,
    )!;
    for (const stage of [0, 1] as const) {
      const nextAddress =
        stage === 0
          ? roles.preState.spendingScriptAddress
          : credentialToAddress(network, { type: "Script", hash: returnHash });
      const datum =
        stage === 0
          ? Data.to(
              { fraud_prover: prover, data: states.selected },
              SDK.DirectBindPreStateDatum,
            )
          : Data.to(
              { fraud_prover: prover, data: states.bound },
              SDK.DirectScriptBoundDatum,
            );
      const proof =
        stage === 0 ? material.eventMembership : material.transitionMembership;
      const key =
        stage === 0
          ? Data.to(material.eventMembership.key, SDK.EventKey)
          : Data.to(material.transitionMembership.key);
      const value =
        stage === 0
          ? Data.to(material.eventMembership.value, SDK.EventToStepValue)
          : Data.to(material.transitionMembership.value, SDK.TransitionStep);
      const membership = Data.to([
        proof.phas_root,
        key,
        value,
        Data.from(Data.to(proof.proof, SDK.Proof)),
      ]);
      const spend = ((ctx) =>
        (stage === 0 ? directBindEventRedeemer : directBindPreStateRedeemer)({
          material,
          fraudProver: prover,
          ctx,
          thread,
          unit,
          nextAddress,
          carriage: { directRewardAddress: reward },
        })) satisfies BuildTxWithRedeemer;
      const badDatum = Data.to(
        new Constr(0, [prover, new Constr(0, [new Constr(0, [])])]),
      );
      // An adversary uses the real completed input/withdrawal slots, but asks for
      // an unrelated output state. It must reach the actual spending refusal.
      const adversarial = ((ctx) => {
        SDK.requireOwnSpendPurpose(ctx, thread, "N6 adversary");
        const indices = {
          input_index: SDK.requireInputIndex(ctx, thread, "N6 adversary"),
          output_index: SDK.requireUniqueOutputIndex(
            ctx.outputs,
            computationThreadOutputPredicate({
              address: nextAddress,
              datum: badDatum,
              unit,
            }),
            "N6 adversary output",
          ),
        };
        return stage === 0
          ? Data.to(
              {
                Continue: [
                  {
                    ...indices,
                    event_opening: carriedCountedMembership({
                      witness: material.eventMembership,
                      ctx,
                      directRewardAddress: reward,
                      label: "N6 event adversary",
                    }),
                  },
                ],
              },
              SDK.DirectBindEventRedeemer,
            )
          : Data.to(
              {
                Continue: [
                  {
                    ...indices,
                    transition_opening: carriedCountedMembership({
                      witness: material.transitionMembership,
                      ctx,
                      directRewardAddress: reward,
                      label: "N6 transition adversary",
                    }),
                  },
                ],
              },
              SDK.DirectBindPreStateRedeemer,
            );
      }) satisfies BuildTxWithRedeemer;
      const build = (redeemer: BuildTxWithRedeemer, output: string) =>
        lucid
          .newTx()
          .collectFrom([thread], redeemer)
          .readFrom([refs[stage]!])
          .withdraw(reward, 0n, membership)
          .attach.WithdrawalValidator(phas)
          .pay.ToContract(
            nextAddress,
            { kind: "inline", value: output },
            thread.assets,
          )
          .addSignerKey(prover);
      await expectOnchainRefusal(() =>
        build(adversarial, badDatum).complete({ localUPLCEval: true }),
      );
      const unsigned = await build(spend, datum).complete({
        localUPLCEval: true,
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
      expect(Data.from(thread.datum!)).toEqual(Data.from(datum));
    }
  },
  300_000,
);

it("publishes every physical direct receive stage within the unchanged L1 envelope", async () => {
  const blueprint = readBlueprint(realBlueprintPath);
  const roles = await Effect.runPromise(
    SDK.buildDirectReceiveContracts({
      blueprint,
      network,
      computationThreadPolicyId: "cc".repeat(28),
      fieldPreimageCertificatePolicyId: "77".repeat(28),
      nextStepScriptHash: "ee".repeat(28),
    }),
  );
  const account = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const lucid = await Lucid(
    new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS),
    network,
  );
  lucid.selectWallet.fromSeed(account.seedPhrase);
  for (const [name, role] of Object.entries(roles)) {
    expect(role.spendingScript.script.length / 2).toBeLessThanOrEqual(16384);
    const { publicationMeasurement } = await publishPlainReferenceScriptUtxo({
      lucid,
      script: role.spendingScript,
      label: `N6 receive ${name}`,
    });
    console.log(
      "N6 actual signed receive publication",
      JSON.stringify({
        name,
        appliedBytes: role.spendingScript.script.length / 2,
        signedBytes: publicationMeasurement.completeSignedBytes,
      }),
    );
    expect(publicationMeasurement.completeSignedBytes).toBeLessThanOrEqual(
      15872,
    );
  }
}, 300_000);

it("executes bounded receive batches and sorted ordinal custody without trace states", async () => {
  const measurements = await driveDirectReceiveLifecycle();
  expect(measurements).toHaveLength(10);
  console.log(
    "N6 ordinary receive four outputs/two credentials, two output batches",
    JSON.stringify(measurements),
  );
}, 300_000);

it.each(["Base", "Enterprise"] as const)(
  "executes certified maximum receive field batches with %s wallet and exact sorted credential custody",
  async (walletAddressType) => {
    const outputs = maximumDirectReceiveOutputs();
    const measurements = await driveDirectReceiveLifecycle({
      outputs,
      batchSize: 8,
      targetOrdinal: 17,
      walletAddressType,
    });
    expect(
      measurements.filter((m) => m.label.startsWith("outputs ")),
    ).toHaveLength(Math.ceil(outputs.length / 8));
    expect(
      measurements.filter((m) => m.label.startsWith("sorted exact")),
    ).toHaveLength(8);
    console.log(
      "N6 maximum receive sixty-four credentials",
      JSON.stringify(measurements),
    );
  },
  900_000,
);

// This physical publication check catches an applied source worker whose script
// or complete signed publication exceeds the protocol envelope. It shares the
// existing direct worker emulator setup and uses the public SDK factory.
it.each(["Base", "Enterprise"] as const)(
  "publishes every physical direct source stage with %s wallet within the unchanged L1 envelope",
  async (walletAddressType) => {
    const blueprint = readBlueprint(realBlueprintPath);
    const roles = await Effect.runPromise(
      SDK.buildDirectSourceContracts({
        blueprint,
        network,
        computationThreadPolicyId: "cc".repeat(28),
        fieldPreimageCertificatePolicyId: "77".repeat(28),
        nextStepScriptHash: "ee".repeat(28),
      }),
    );
    const generatedAccount = generateEmulatorAccount({
      lovelace: 40_000_000_000n,
    });
    const prover = getAddressDetails(generatedAccount.address)
      .paymentCredential!.hash;
    const account = {
      ...generatedAccount,
      address:
        walletAddressType === "Enterprise"
          ? credentialToAddress(network, { type: "Key", hash: prover })
          : generatedAccount.address,
    };
    const lucid = await Lucid(
      new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS),
      network,
    );
    lucid.selectWallet.fromSeed(account.seedPhrase, {
      addressType: walletAddressType,
    });
    const measurements: {
      name: string;
      appliedBytes: number;
      signedBytes: number;
    }[] = [];
    for (const [name, role] of Object.entries(roles)) {
      const appliedBytes = role.spendingScript.script.length / 2;
      console.log(
        "N6 actual applied source",
        walletAddressType,
        name,
        appliedBytes,
      );
      expect(appliedBytes).toBeLessThanOrEqual(16384);
      const { publicationMeasurement } = await publishPlainReferenceScriptUtxo({
        lucid,
        script: role.spendingScript,
        label: `N6 source ${name} ${walletAddressType}`,
      });
      measurements.push({
        name,
        appliedBytes,
        signedBytes: publicationMeasurement.completeSignedBytes,
      });
      expect(publicationMeasurement.completeSignedBytes).toBeLessThanOrEqual(
        15872,
      );
    }
    expect(measurements).toHaveLength(8);
    console.log(
      "N6 actual signed source publications",
      walletAddressType,
      JSON.stringify(measurements),
    );
  },
  300_000,
);

it("executes source field-authentication-map custody in ordinary eight-item batches", async () => {
  const result = await driveDirectSourceLifecycle();
  expect(result.inputCount).toBe(16);
  expect(result.witnessCount).toBe(8);
  expect(result.index.count).toBe(8n);
  expect(result.measurements).toHaveLength(16);
  console.log(
    "N6 actual ordinary source prefix",
    JSON.stringify(result, (_, value: unknown) =>
      typeof value === "bigint" ? value.toString() : value,
    ),
  );
}, 300_000);

it("source batch serviceability drains 100 genuine descriptors with budget1", async () => {
  const fixture = await prepareDirectSourceResourceFixture();
  const result = await measureDirectSourceResourcePath(fixture, 1);
  const json = JSON.stringify(result, (_, value: unknown) =>
    typeof value === "bigint" ? value.toString() : value,
  );
  console.log("N6 F2 accepted-selection resource measurement", json);
  expect(result.protocol).toEqual({
    maxTxSize: 16_384,
    maxTxExMem: "16500000",
    maxTxExSteps: "10000000000",
  });
  expect(result.outcome, `F2 budget1: ${json}`).toBe("serviceable");
  expect(result.processed).toBe(100);
  expect(result.finalState.complete).toBe(true);
  expect(result.finalState.source_index).toEqual(fixture.evidence.index);
  expect(result.finalState.source_index.count).toBe(100n);
  const accepted = result.attempts.filter(
    (attempt) => attempt.status === "accepted",
  );
  expect(accepted).toHaveLength(301);
  for (const attempt of accepted)
    expect(attempt.signed?.withinLedger).toBe(true);
}, 900_000);

it.each(["Base", "Enterprise"] as const)(
  "executes certified maximum source field with %s wallet without shrinking admission",
  async (walletAddressType) => {
    const result = await driveDirectSourceLifecycle({
      maximumWitnessField: true,
      walletAddressType,
    });
    expect(result.inputCount).toBe(16);
    expect(result.witnessCount).toBe(1);
    expect(result.index.count).toBe(8n);
    expect(result.measurements).toHaveLength(16);
    console.log(
      "N6 actual maximum source prefix",
      walletAddressType,
      JSON.stringify(result, (_, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      ),
    );
  },
  300_000,
);
