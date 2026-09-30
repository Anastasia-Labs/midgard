import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type RedeemerContext,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  type AuthenticatedValidator,
  buildSchedulerRefreshTx,
  buildUnsignedSchedulerRefreshTxProgram,
  type SchedulerRefreshWitnessSelection,
  SchedulerSpendRedeemer,
} from "../src/index.js";
import {
  assembledIndices,
  canonicalOutRefOrder,
  outRefKey,
  requireIndex,
  setupSchedulerScene,
} from "./scheduler-refresh.setup-scheduler-scene.js";

describe("scheduler refresh SDK builder on the Lucid emulator", () => {
  it("derives Advance indices from the transaction Lucid assembled and the ledger", async () => {
    const scene = await setupSchedulerScene();
    const result = await Effect.runPromise(
      buildUnsignedSchedulerRefreshTxProgram({
        ...scene.baseConfig,
        selection: { kind: "Advance", activeNode: { utxo: scene.activeTail } },
      }),
    );
    const assembled = assembledIndices(result.tx);
    const advanced = result.layout as Extract<
      typeof result.layout,
      { kind: "Advance" }
    >;
    expect(assembled.inputs).toEqual(canonicalOutRefOrder(assembled.inputs));
    expect(assembled.referenceInputs).toEqual(
      canonicalOutRefOrder(assembled.referenceInputs),
    );
    expect(assembled.inputs.length).toBeGreaterThan(1);
    expect(assembled.referenceInputs).toHaveLength(2);

    const signed = await result.tx.sign.withWallet().complete();
    const txHash = await signed.submit();
    await scene.lucid.awaitTx(txHash);
    scene.emulator.awaitBlock(1);
    const settled = await scene.lucid.utxosByOutRef([
      { txHash, outputIndex: Number(advanced.schedulerOutputIndex) },
    ]);
    expect(settled).toHaveLength(1);
    expect(settled[0]?.assets[scene.schedulerUnit]).toBe(1n);
    expect(settled[0]?.datum).toBe(result.refreshedDatumCbor);
    expect(settled[0]?.address).toBe(scene.scheduler.spendingScriptAddress);

    expect(result.layout).toEqual({
      kind: "Advance",
      schedulerInputIndex: requireIndex(assembled.inputs, scene.schedulerInput),
      schedulerOutputIndex: advanced.schedulerOutputIndex,
      activeNodeRefInputIndex: requireIndex(
        assembled.referenceInputs,
        scene.activeTail,
      ),
    });
    expect(
      Data.from(result.schedulerSpendRedeemerCbor, SchedulerSpendRedeemer),
    ).toEqual({
      scheduler_input_index: advanced.schedulerInputIndex,
      scheduler_output_index: advanced.schedulerOutputIndex,
      advancing_approach: {
        GoToNextDueToEndOfShift: {
          new_shifts_operator_node_ref_input_index:
            advanced.activeNodeRefInputIndex,
        },
      },
    });
  }, 300_000);

  it("derives AppointFirst reference indices from the assembled reference-input set", async () => {
    const scene = await setupSchedulerScene();
    const result = await Effect.runPromise(
      buildUnsignedSchedulerRefreshTxProgram({
        ...scene.baseConfig,
        selection: {
          kind: "AppointFirst",
          activeNode: { utxo: scene.activeTail },
          registeredWitnessNode: { utxo: scene.registeredWitness },
        },
      }),
    );
    const assembled = assembledIndices(result.tx);
    const appointed = result.layout as Extract<
      typeof result.layout,
      { kind: "AppointFirst" }
    >;
    expect(assembled.referenceInputs).toEqual(
      canonicalOutRefOrder(assembled.referenceInputs),
    );
    expect(assembled.referenceInputs).toHaveLength(3);
    expect(result.layout).toEqual({
      kind: "AppointFirst",
      schedulerInputIndex: requireIndex(assembled.inputs, scene.schedulerInput),
      schedulerOutputIndex: appointed.schedulerOutputIndex,
      activeNodeRefInputIndex: requireIndex(
        assembled.referenceInputs,
        scene.activeTail,
      ),
      registeredWitnessRefInputIndex: requireIndex(
        assembled.referenceInputs,
        scene.registeredWitness,
      ),
    });
    expect(
      Data.from(result.schedulerSpendRedeemerCbor, SchedulerSpendRedeemer),
    ).toEqual({
      scheduler_input_index: appointed.schedulerInputIndex,
      scheduler_output_index: appointed.schedulerOutputIndex,
      advancing_approach: {
        AppointFirstOperator: {
          new_shifts_operator_node_ref_input_index:
            appointed.activeNodeRefInputIndex,
          registered_element_ref_input_index:
            appointed.registeredWitnessRefInputIndex,
        },
      },
    });
  }, 300_000);

  it("derives Rewind's three reference indices from the assembled reference-input set", async () => {
    const scene = await setupSchedulerScene();
    const result = await Effect.runPromise(
      buildUnsignedSchedulerRefreshTxProgram({
        ...scene.baseConfig,
        selection: {
          kind: "Rewind",
          activeNode: { utxo: scene.activeTail },
          activeRootNode: { utxo: scene.activeRoot },
          registeredWitnessNode: { utxo: scene.registeredWitness },
        },
      }),
    );
    const assembled = assembledIndices(result.tx);
    const rewind = result.layout as Extract<
      typeof result.layout,
      { kind: "Rewind" }
    >;
    expect(assembled.referenceInputs).toEqual(
      canonicalOutRefOrder(assembled.referenceInputs),
    );
    expect(assembled.referenceInputs).toHaveLength(4);
    expect(result.layout).toEqual({
      kind: "Rewind",
      schedulerInputIndex: requireIndex(assembled.inputs, scene.schedulerInput),
      schedulerOutputIndex: rewind.schedulerOutputIndex,
      activeRootRefInputIndex: requireIndex(
        assembled.referenceInputs,
        scene.activeRoot,
      ),
      activeTailRefInputIndex: requireIndex(
        assembled.referenceInputs,
        scene.activeTail,
      ),
      registeredWitnessRefInputIndex: requireIndex(
        assembled.referenceInputs,
        scene.registeredWitness,
      ),
    });
    // The three witness reference indices are distinct positions of one sorted
    // set, so a builder that resolved them all through the same lookup would
    // not survive this.
    expect(
      new Set([
        rewind.activeRootRefInputIndex,
        rewind.activeTailRefInputIndex,
        rewind.registeredWitnessRefInputIndex,
      ]).size,
    ).toBe(3);
  }, 300_000);

  it("carries the scheduler script by reference, or attaches it when none is published", async () => {
    const scene = await setupSchedulerScene();
    const selection: SchedulerRefreshWitnessSelection = {
      kind: "Advance",
      activeNode: { utxo: scene.activeTail },
    };
    const withReference = await buildSchedulerRefreshTx(
      { ...scene.baseConfig, selection },
      "00",
    ).complete({ localUPLCEval: false });
    const referenced = assembledIndices(withReference);
    expect(referenced.referenceInputs).toContain(
      outRefKey(scene.schedulerScriptRef),
    );
    expect(
      withReference.toTransaction().witness_set().plutus_v3_scripts()?.len() ??
        0,
    ).toBe(0);

    const withoutReference = await buildSchedulerRefreshTx(
      {
        ...scene.baseConfig,
        schedulerSpendingScriptRef: undefined,
        selection,
      },
      "00",
    ).complete({ localUPLCEval: false });
    const attached = assembledIndices(withoutReference);
    expect(attached.referenceInputs).not.toContain(
      outRefKey(scene.schedulerScriptRef),
    );
    expect(
      withoutReference
        .toTransaction()
        .witness_set()
        .plutus_v3_scripts()
        ?.len() ?? 0,
    ).toBe(1);

    // Causal negative: a reference input that carries no script cannot stand in
    // for the scheduler validator, so the builder must not silently produce an
    // unwitnessed spend when it is handed the wrong UTxO.
    await expect(
      buildSchedulerRefreshTx(
        {
          ...scene.baseConfig,
          schedulerSpendingScriptRef: scene.activeTail,
          selection,
        },
        "00",
      ).complete({ localUPLCEval: false }),
    ).rejects.toThrow();
  }, 300_000);

  it("rejects Lucid validity times outside the safe number range", async () => {
    const scene = await setupSchedulerScene();
    expect(() =>
      buildSchedulerRefreshTx(
        {
          ...scene.baseConfig,
          validFrom: BigInt(Number.MAX_SAFE_INTEGER) + 1n,
          selection: {
            kind: "Advance",
            activeNode: { utxo: scene.activeTail },
          },
        },
        "00",
      ),
    ).toThrow("validFrom");
  }, 300_000);
});

/**
 * `BuildTxWithRedeemer` is a Lucid callback that the completion pass may invoke
 * more than once; the builder refuses to publish a redeemer when two
 * resolutions disagree. The emulator resolves consistently by construction, so
 * this leg drives the callback directly with contexts the builder cannot
 * distinguish from Lucid's own.
 */
export const makeCallbackProbeLucid = (
  contexts: readonly RedeemerContext[],
): LucidEvolution => {
  const tx = {
    validFrom: () => tx,
    validTo: () => tx,
    collectFrom: (_inputs: readonly UTxO[], redeemer?: unknown) => {
      if (typeof redeemer === "function") {
        resolutions.push(redeemer as BuildTxWithRedeemer);
      }
      return tx;
    },
    readFrom: () => tx,
    pay: { ToContract: () => tx },
    addSignerKey: () => tx,
    attach: { Script: () => tx },
    complete: async () => {
      for (const resolve of resolutions) {
        for (const context of contexts) {
          resolve(context);
        }
      }
      return { toTransaction: () => ({}) } as unknown as TxSignBuilder;
    },
  };
  const resolutions: BuildTxWithRedeemer[] = [];
  return { newTx: () => tx } as unknown as LucidEvolution;
};

export const probeContext = (
  scheduler: AuthenticatedValidator,
  schedulerInput: UTxO,
  refreshedDatumCbor: string,
  schedulerUnit: string,
  inputIndex: bigint,
  referenceInputs: readonly UTxO[],
): RedeemerContext =>
  ({
    ownPurpose: { tag: "spend", input: schedulerInput },
    redeemers: [{ tag: "spend", input: schedulerInput }],
    referenceInputs,
    outputs: [
      {
        address: scheduler.spendingScriptAddress,
        datum: refreshedDatumCbor,
        assets: { lovelace: 20_000_000n, [schedulerUnit]: 1n },
      },
    ],
    inputIndex: () => inputIndex,
    redeemerIndex: () => 0n,
  }) as unknown as RedeemerContext;
