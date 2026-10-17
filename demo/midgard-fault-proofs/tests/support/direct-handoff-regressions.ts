import { computeHash28 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  Emulator,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { createDirectFieldPlan } from "../../src/direct-script-derivation/index.js";
import { faultProofFieldOpening } from "../../src/field-opening.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import {
  directHandoffContext,
  directHandoffInitialThread,
  directHandoffMaterial,
} from "./direct-handoff-fixture.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  expectOnchainRefusal,
  measureCompleteSignedTransaction,
  network,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./submit-init-emulator-shared.js";

/** Actual applied scripts, independent wallets and authentic door transitions.
 * The component prefix is seeded; this is not Init/proof-mint/SQ acceptance. */
export const driveDirectHandoffRegression = async ({
  family,
  empty,
  selection = false,
  selectionRefusal = "both",
}: {
  readonly family: "input" | "witness" | "receive";
  readonly empty: boolean;
  readonly selection?: boolean;
  readonly selectionRefusal?: "unrelated" | "none" | "both";
}) => {
  const material = await directHandoffMaterial(family, empty);
  const {
    owner,
    stranger,
    prover,
    strangerHash,
    source,
    receive,
    policy,
    certificate,
  } = await directHandoffContext(readBlueprint(realBlueprintPath));
  expect(strangerHash).not.toBe(prover);
  const field = family === "input" ? 0 : family === "witness" ? 6 : 2;
  const plans = new Map(
    [0, 1, 2, 6].map((index) => [
      index,
      createDirectFieldPlan(material, index, prover),
    ]),
  );
  const plan = (index: number) => plans.get(index)!;
  const initialCursor = plan(field).position(
    field === 0 ? "Walking" : "Certifying",
    0,
  ).cursor;
  let sourceState: SDK.DirectSourceScanState = {
    cursor: initialCursor,
    source_index: { root: SDK.EMPTY_MERKLE_TREE_ROOT, count: 0n },
    complete: false,
  };
  let outputState: SDK.DirectReceiveOutputState = {
    cursor: initialCursor,
    source_index: sourceState.source_index,
    credential_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    credential_count: 0n,
    complete: false,
  };
  const sourceDatum = (data: SDK.DirectSourceScanState) =>
    Data.to({ fraud_prover: prover, data }, SDK.DirectSourceScanDatum);
  const outputDatum = (data: SDK.DirectReceiveOutputState) =>
    Data.to({ fraud_prover: prover, data }, SDK.DirectReceiveOutputDatum);
  const { initial, initialDatum } = directHandoffInitialThread({
    family,
    material,
    prover,
    source,
    receive,
    sourceState,
  });
  const unit =
    policy +
    "0000002b" +
    computeHash28(SDK.encodeHeaderCbor(material.sourceBound.header)).toString(
      "hex",
    );
  const emulator = new Emulator(
    [
      owner,
      stranger,
      {
        ...owner,
        address: initial.spendingScriptAddress,
        assets: { lovelace: 30_000_000n, [unit]: 1n },
        outputData: { inline: initialDatum },
      },
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const ownerLucid = await Lucid(emulator, network);
  ownerLucid.selectWallet.fromSeed(owner.seedPhrase);
  const strangerLucid = await Lucid(emulator, network);
  strangerLucid.selectWallet.fromSeed(stranger.seedPhrase);
  const roles = family === "receive" ? receive : source;
  const refs = new Map<string, UTxO>();
  for (const role of Object.values(roles))
    refs.set(
      role.spendingScriptHash,
      (
        await publishPlainReferenceScriptUtxo({
          lucid: ownerLucid,
          script: role.spendingScript,
          label: "N6 handoff regression",
        })
      ).utxo,
    );
  let thread = (await ownerLucid.utxosAt(initial.spendingScriptAddress)).find(
    (item) => item.assets[unit] === 1n,
  )!;
  let accepted = 0,
    refused = 0;
  const measured: { bytes: number; memory: string; cpu: string }[] = [];
  const execute = async (
    role: SDK.SpendingValidator,
    destination: string,
    datum: string,
    redeemer: (
      indices: { input_index: bigint; output_index: bigint },
      open: (index: number) => SDK.FieldOpening,
    ) => string,
    asProver = false,
    mustRefuse = false,
  ) => {
    const lucid = asProver ? ownerLucid : strangerLucid;
    let outputIndex: bigint | undefined;
    const spend = ((ctx) => {
      SDK.requireOwnSpendPurpose(ctx, thread, "N6 handoff");
      outputIndex = SDK.requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({ address: destination, datum, unit }),
        "N6 handoff output",
      );
      return redeemer(
        {
          input_index: SDK.requireInputIndex(ctx, thread, "N6 handoff input"),
          output_index: outputIndex,
        },
        (index) =>
          faultProofFieldOpening({
            planned: plan(index).openingPlan,
            referenceInputs: ctx.referenceInputs,
            certificatePolicyId: certificate,
            label: "N6 handoff opening",
          }),
      );
    }) satisfies BuildTxWithRedeemer;
    const build = () =>
      lucid
        .newTx()
        .collectFrom([thread], spend)
        .readFrom([refs.get(role.spendingScriptHash)!])
        .pay.ToContract(
          destination,
          { kind: "inline", value: datum },
          thread.assets,
        )
        .addSignerKey(asProver ? prover : strangerHash);
    if (mustRefuse) {
      await expectOnchainRefusal(() =>
        build().complete({ localUPLCEval: true }),
      );
      refused++;
      return;
    }
    const signed = await (await build().complete({ localUPLCEval: true })).sign
      .withWallet()
      .complete();
    const measurement = measureCompleteSignedTransaction(signed.toCBOR());
    expect(measurement.completeSignedBytes).toBeLessThanOrEqual(15872);
    expect(measurement.executionMemory).toBeLessThanOrEqual(13_200_000n);
    expect(measurement.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
    const hash = await signed.submit();
    await lucid.awaitTx(hash);
    if (outputIndex === undefined) throw new Error("N6 handoff output absent");
    thread = (
      await lucid.utxosByOutRef([
        { txHash: hash, outputIndex: Number(outputIndex) },
      ])
    )[0]!;
    expect(Data.from(thread.datum!)).toEqual(Data.from(datum));
    expect(thread.assets).toEqual({ lovelace: 30_000_000n, [unit]: 1n });
    measured.push({
      bytes: measurement.completeSignedBytes,
      memory: measurement.executionMemory.toString(),
      cpu: measurement.executionSteps.toString(),
    });
    accepted++;
  };
  const count = plan(field).items.length;
  if (family === "input") {
    sourceState = { ...sourceState, complete: count === 0 };
    await execute(
      source.start,
      source.resolved.spendingScriptAddress,
      sourceDatum(sourceState),
      (indices, open) =>
        Data.to(
          { Continue: [{ ...indices, opening: open(0) }] },
          SDK.DirectSourceStartRedeemer,
        ),
    );
  } else if (family === "witness") {
    sourceState = {
      ...sourceState,
      cursor: plan(6).position("Walking", 0).cursor,
      complete: count === 0,
    };
    await execute(
      source.grammar,
      source.witnesses.spendingScriptAddress,
      sourceDatum(sourceState),
      (indices, open) =>
        Data.to(
          {
            Continue: [
              {
                ...indices,
                opening: open(6),
                checkpoint: plan(6).position("Certifying", 0).checkpointBytes,
                budget: BigInt(Math.max(1, count)),
              },
            ],
          },
          SDK.DirectSourceGrammarRedeemer,
        ),
    );
  } else {
    await execute(
      receive.start,
      receive.grammar.spendingScriptAddress,
      outputDatum(outputState),
      (indices, open) =>
        Data.to(
          { Continue: [{ ...indices, outputs_opening: open(2) }] },
          SDK.DirectReceiveStartRedeemer,
        ),
    );
    outputState = {
      ...outputState,
      cursor: plan(2).position("Walking", 0).cursor,
      complete: count === 0,
    };
    await execute(
      receive.grammar,
      receive.outputs.spendingScriptAddress,
      outputDatum(outputState),
      (indices, open) =>
        Data.to(
          {
            Continue: [
              {
                ...indices,
                outputs_opening: open(2),
                checkpoint_bytes: plan(2).position("Certifying", 0)
                  .checkpointBytes,
                budget: BigInt(Math.max(1, count)),
              },
            ],
          },
          SDK.DirectReceiveGrammarRedeemer,
        ),
    );
    if (selection) {
      outputState = {
        ...outputState,
        cursor: plan(2).position("Walking", count).cursor,
        complete: true,
      };
      await execute(
        receive.outputs,
        receive.outputs.spendingScriptAddress,
        outputDatum(outputState),
        (indices, open) =>
          Data.to(
            {
              Continue: [
                {
                  Scan: {
                    ...indices,
                    outputs_opening: open(2),
                    checkpoint_bytes: plan(2).position("Walking", 0)
                      .checkpointBytes,
                    witnesses: plan(2).items.map(() => "NoCredential" as const),
                  },
                },
              ],
            },
            SDK.DirectReceiveOutputRedeemer,
          ),
      );
    }
  }
  if (family === "input") {
    const resume = Data.to(
      {
        fraud_prover: prover,
        data: {
          resume_script_hash: source.resolved.spendingScriptHash,
          state: sourceState,
        },
      },
      SDK.DirectSourceResumeDatum,
    );
    await execute(
      source.resolved,
      source.inputFinish.spendingScriptAddress,
      resume,
      (indices) =>
        Data.to(
          { Continue: [{ Advance: indices }] },
          SDK.DirectSourceResolveRedeemer,
        ),
      false,
      !empty,
    );
    if (empty) {
      sourceState = {
        ...sourceState,
        cursor: plan(1).position("Walking", 0).cursor,
        complete: true,
      };
      await execute(
        source.inputFinish,
        source.resolved.spendingScriptAddress,
        sourceDatum(sourceState),
        (indices, open) =>
          Data.to(
            {
              Continue: [
                {
                  ...indices,
                  opening: open(0),
                  checkpoint: plan(0).position("Walking", 0).checkpointBytes,
                  next_opening: open(1),
                },
              ],
            },
            SDK.DirectSourceInputFinishRedeemer,
          ),
      );
    }
  } else if (family === "witness") {
    await execute(
      source.witnesses,
      source.finish.spendingScriptAddress,
      sourceDatum(sourceState),
      (indices) =>
        Data.to(
          { Continue: [{ Finish: indices }] },
          SDK.DirectSourceWitnessRedeemer,
        ),
      false,
      !empty,
    );
    if (empty)
      await execute(
        source.finish,
        receive.start.spendingScriptAddress,
        Data.to(
          {
            fraud_prover: prover,
            data: {
              bound: material.bound,
              source_index: sourceState.source_index,
            },
          },
          SDK.DirectSourceBoundIndexDatum,
        ),
        (indices, open) =>
          Data.to(
            {
              Continue: [
                {
                  ...indices,
                  opening: open(6),
                  checkpoint: plan(6).position("Walking", 0).checkpointBytes,
                },
              ],
            },
            SDK.DirectSourceFinishRedeemer,
          ),
      );
  } else {
    await execute(
      receive.outputs,
      receive.finish.spendingScriptAddress,
      outputDatum(outputState),
      (indices) =>
        Data.to(
          { Continue: [{ Finish: indices }] },
          SDK.DirectReceiveOutputRedeemer,
        ),
      false,
      !empty && !selection,
    );
    if (empty || selection) {
      const declaration = (
        target: bigint | null,
      ): SDK.DirectReceiveDeclarationState => ({
        bound: material.bound,
        source_index: outputState.source_index,
        output_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        expected_count: 0n,
        declared_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        declared_count: 0n,
        previous_credential: "",
        target_ordinal: target,
        selected: null,
      });
      const choose =
        (target: bigint | null) =>
        (
          indices: { input_index: bigint; output_index: bigint },
          open: (index: number) => SDK.FieldOpening,
        ) =>
          Data.to(
            {
              Continue: [
                {
                  ...indices,
                  outputs_opening: open(2),
                  checkpoint_bytes: plan(2).position("Walking", count)
                    .checkpointBytes,
                  target_ordinal: target,
                },
              ],
            },
            SDK.DirectReceiveFinishRedeemer,
          );
      const selectedDatum = (target: bigint | null) =>
        Data.to(
          { fraud_prover: prover, data: declaration(target) },
          SDK.DirectReceiveDeclarationDatum,
        );
      if (selectionRefusal !== "none")
        await execute(
          receive.finish,
          receive.declaration.spendingScriptAddress,
          selectedDatum(0n),
          choose(0n),
          false,
          true,
        );
      if (selectionRefusal !== "unrelated")
        await execute(
          receive.finish,
          receive.declaration.spendingScriptAddress,
          selectedDatum(null),
          choose(null),
          true,
          true,
        );
      await execute(
        receive.finish,
        receive.declaration.spendingScriptAddress,
        selectedDatum(0n),
        choose(0n),
        true,
      );
    }
  }
  console.log(
    "N6 actual handoff regression",
    JSON.stringify({
      family,
      empty,
      selection,
      accepted,
      refused,
      unrelatedKey: strangerHash,
      prover,
      measured,
    }),
  );
  return { accepted, refused };
};
