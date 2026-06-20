import {
  type Assets,
  type BuildTxWithRedeemer,
  Data,
  type RedeemerContext,
  type Script,
  toUnit,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  type AuthenticatedValidator,
  buildSchedulerRefreshTx,
  type BuildSchedulerRefreshTxConfig,
  buildUnsignedSchedulerRefreshTxProgram,
  encodeSchedulerDatumForChain,
  SCHEDULER_ASSET_NAME,
  type SchedulerRefreshWitnessSelection,
  SchedulerSpendRedeemer,
} from "../src/index.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);

type RecordedPayment = {
  readonly address: string;
  readonly datum: { readonly kind: "inline"; readonly value: string };
  readonly assets: Assets;
};

type RecordedTx = {
  readonly reads: UTxO[][];
  readonly collects: { readonly inputs: UTxO[]; readonly redeemer?: unknown }[];
  readonly payments: RecordedPayment[];
  readonly signerKeys: string[];
  readonly attachedScripts: Script[];
  readonly completeOptions: unknown[];
  redeemerCallbackCalls: number;
  validFrom?: number;
  validTo?: number;
};

const sameOutRef = (
  left: Pick<UTxO, "txHash" | "outputIndex">,
  right: Pick<UTxO, "txHash" | "outputIndex">,
): boolean =>
  left.txHash === right.txHash && left.outputIndex === right.outputIndex;

const mkUtxo = (
  byte: string,
  outputIndex: number,
  assets: Assets = { lovelace: 5_000_000n },
  datum: string | null = null,
): UTxO =>
  ({
    txHash: h32(byte),
    outputIndex,
    address: `addr_test_${byte}`,
    assets,
    datum,
  }) as UTxO;

const schedulerValidator = (): AuthenticatedValidator =>
  ({
    policyId: h28("aa"),
    spendingScriptAddress: "addr_scheduler",
    spendingScriptHash: h28("bb"),
    spendingScriptCBOR: "",
    mintingScriptCBOR: "",
    spendingScript: { type: "PlutusV3", script: "" },
    mintingScript: { type: "PlutusV3", script: "" },
  }) as AuthenticatedValidator;

const makeRedeemerContext = (
  record: RecordedTx,
  ownInput: UTxO,
): RedeemerContext => {
  const collectedInputs = record.collects.flatMap(({ inputs }) => inputs);
  return {
    ownPurpose: { tag: "spend", input: ownInput },
    redeemers: [{ tag: "spend", input: ownInput }],
    referenceInputs: record.reads.flat(),
    outputs: record.payments.map((payment) => ({
      address: payment.address,
      datum: payment.datum.value,
      assets: payment.assets,
    })),
    inputIndex: (input: UTxO) => {
      const index = collectedInputs.findIndex((candidate) =>
        sameOutRef(candidate, input),
      );
      return index < 0 ? undefined : BigInt(index);
    },
    redeemerIndex: () => 0n,
  } as unknown as RedeemerContext;
};

const makeRecordingLucid = (
  options: { readonly redeemerCallbackInvocations?: number } = {},
): {
  readonly lucid: BuildSchedulerRefreshTxConfig["lucid"];
  readonly txs: RecordedTx[];
} => {
  const redeemerCallbackInvocations = options.redeemerCallbackInvocations ?? 1;
  const txs: RecordedTx[] = [];
  const lucid = {
    newTx: () => {
      const record: RecordedTx = {
        reads: [],
        collects: [],
        payments: [],
        signerKeys: [],
        attachedScripts: [],
        completeOptions: [],
        redeemerCallbackCalls: 0,
      };
      txs.push(record);
      const tx = {
        validFrom: (value: number) => {
          record.validFrom = value;
          return tx;
        },
        validTo: (value: number) => {
          record.validTo = value;
          return tx;
        },
        collectFrom: (inputs: UTxO[], redeemer?: unknown) => {
          record.collects.push({ inputs, redeemer });
          return tx;
        },
        readFrom: (inputs: UTxO[]) => {
          record.reads.push(inputs);
          return tx;
        },
        pay: {
          ToContract: (
            address: string,
            datum: RecordedPayment["datum"],
            assets: Assets,
          ) => {
            record.payments.push({ address, datum, assets });
            return tx;
          },
        },
        addSignerKey: (keyHash: string) => {
          record.signerKeys.push(keyHash);
          return tx;
        },
        attach: {
          Script: (script: Script) => {
            record.attachedScripts.push(script);
            return tx;
          },
        },
        complete: async (completeOptions?: unknown) => {
          record.completeOptions.push(completeOptions);
          for (const collect of record.collects) {
            if (typeof collect.redeemer === "function") {
              for (let i = 0; i < redeemerCallbackInvocations; i += 1) {
                record.redeemerCallbackCalls += 1;
                (collect.redeemer as BuildTxWithRedeemer)(
                  makeRedeemerContext(record, collect.inputs[0]!),
                );
              }
            }
          }
          return {
            toTransaction: () => ({}),
          } as TxSignBuilder;
        },
      };
      return tx;
    },
  } as unknown as BuildSchedulerRefreshTxConfig["lucid"];
  return { lucid, txs };
};

const makeFixture = () => {
  const scheduler = schedulerValidator();
  const schedulerUnit = toUnit(scheduler.policyId, SCHEDULER_ASSET_NAME);
  const schedulerInput = mkUtxo("10", 0, {
    lovelace: 5_000_000n,
    [schedulerUnit]: 1n,
  });
  const feeInput = mkUtxo("01", 0);
  const activeRoot = mkUtxo("20", 0);
  const activeTail = mkUtxo("30", 0);
  const registeredWitness = mkUtxo("40", 0);
  const schedulerScriptRef = mkUtxo("50", 0);
  return {
    scheduler,
    schedulerInput,
    feeInput,
    activeRoot,
    activeTail,
    registeredWitness,
    schedulerScriptRef,
    baseConfig: {
      scheduler,
      operatorKeyHash: h28("99"),
      feeInput,
      presetWalletInputs: [feeInput],
      schedulerInput,
      refreshedDatum: {
        ActiveOperator: {
          operator: h28("99"),
          start_time: 42n,
        },
      },
      validFrom: 1_000n,
      validTo: 2_000n,
    },
  };
};

const completeWithSelection = async (
  selection: SchedulerRefreshWitnessSelection,
) => {
  const fixture = makeFixture();
  const { lucid, txs } = makeRecordingLucid();
  const result = await Effect.runPromise(
    buildUnsignedSchedulerRefreshTxProgram({
      ...fixture.baseConfig,
      lucid,
      selection,
      schedulerSpendingScriptRef: fixture.schedulerScriptRef,
    }),
  );
  return { fixture, result, txs };
};

describe("scheduler refresh SDK builder", () => {
  it("encodes scheduler datums with a definite root array for deployed validators", () => {
    expect(
      encodeSchedulerDatumForChain({
        ActiveOperator: {
          operator: "aa",
          start_time: 42n,
        },
      }),
    ).toBe("d87a8241aa182a");
  });

  it("derives Advance layout and rebuilds with a static redeemer", async () => {
    const { fixture, result, txs } = await completeWithSelection({
      kind: "Advance",
      activeNode: { utxo: makeFixture().activeTail },
    });

    expect(result.layout).toEqual({
      kind: "Advance",
      schedulerInputIndex: 1n,
      schedulerOutputIndex: 0n,
      activeNodeRefInputIndex: 0n,
    });
    expect(
      Data.from(result.schedulerSpendRedeemerCbor, SchedulerSpendRedeemer),
    ).toEqual({
      scheduler_input_index: 1n,
      scheduler_output_index: 0n,
      advancing_approach: {
        GoToNextDueToEndOfShift: {
          new_shifts_operator_node_ref_input_index: 0n,
        },
      },
    });
    expect(txs[0]?.reads).toEqual([
      [fixture.activeTail, fixture.schedulerScriptRef],
    ]);
    expect(txs[1]?.collects[1]?.redeemer).toBe(
      result.schedulerSpendRedeemerCbor,
    );
    expect(txs[0]?.completeOptions[0]).toEqual({
      localUPLCEval: true,
      presetWalletInputs: [fixture.feeInput],
    });
    expect(txs[1]?.completeOptions[0]).toEqual({
      localUPLCEval: true,
      presetWalletInputs: [fixture.feeInput],
    });
  });

  it("accepts repeated consistent redeemer callback resolutions", async () => {
    const fixture = makeFixture();
    const { lucid, txs } = makeRecordingLucid({
      redeemerCallbackInvocations: 3,
    });

    const result = await Effect.runPromise(
      buildUnsignedSchedulerRefreshTxProgram({
        ...fixture.baseConfig,
        lucid,
        selection: {
          kind: "Advance",
          activeNode: { utxo: fixture.activeTail },
        },
        schedulerSpendingScriptRef: fixture.schedulerScriptRef,
      }),
    );

    expect(txs[0]?.redeemerCallbackCalls).toBe(3);
    expect(result.layout).toEqual({
      kind: "Advance",
      schedulerInputIndex: 1n,
      schedulerOutputIndex: 0n,
      activeNodeRefInputIndex: 0n,
    });
    expect(txs[1]?.collects[1]?.redeemer).toBe(
      result.schedulerSpendRedeemerCbor,
    );
  });

  it("derives AppointFirst and Rewind reference indexes from final context", async () => {
    const appoint = await completeWithSelection({
      kind: "AppointFirst",
      activeNode: { utxo: makeFixture().activeTail },
      registeredWitnessNode: { utxo: makeFixture().registeredWitness },
    });
    expect(appoint.result.layout).toEqual({
      kind: "AppointFirst",
      schedulerInputIndex: 1n,
      schedulerOutputIndex: 0n,
      activeNodeRefInputIndex: 0n,
      registeredWitnessRefInputIndex: 1n,
    });

    const rewindFixture = makeFixture();
    const rewind = await completeWithSelection({
      kind: "Rewind",
      activeNode: { utxo: rewindFixture.activeTail },
      activeRootNode: { utxo: rewindFixture.activeRoot },
      registeredWitnessNode: { utxo: rewindFixture.registeredWitness },
    });
    expect(rewind.result.layout).toEqual({
      kind: "Rewind",
      schedulerInputIndex: 1n,
      schedulerOutputIndex: 0n,
      activeRootRefInputIndex: 0n,
      activeTailRefInputIndex: 1n,
      registeredWitnessRefInputIndex: 2n,
    });
  });

  it("attaches the scheduler spending script only when no script reference is supplied", () => {
    const fixture = makeFixture();
    const withReference = makeRecordingLucid();
    buildSchedulerRefreshTx(
      {
        ...fixture.baseConfig,
        lucid: withReference.lucid,
        selection: {
          kind: "Advance",
          activeNode: { utxo: fixture.activeTail },
        },
        schedulerSpendingScriptRef: fixture.schedulerScriptRef,
      },
      "00",
    );
    expect(withReference.txs[0]?.reads).toEqual([
      [fixture.activeTail, fixture.schedulerScriptRef],
    ]);
    expect(withReference.txs[0]?.attachedScripts).toEqual([]);

    const withoutReference = makeRecordingLucid();
    buildSchedulerRefreshTx(
      {
        ...fixture.baseConfig,
        lucid: withoutReference.lucid,
        selection: {
          kind: "Advance",
          activeNode: { utxo: fixture.activeTail },
        },
      },
      "00",
    );
    expect(withoutReference.txs[0]?.reads).toEqual([[fixture.activeTail]]);
    expect(withoutReference.txs[0]?.attachedScripts).toEqual([
      fixture.scheduler.spendingScript,
    ]);
  });

  it("rejects Lucid validity times outside the safe number range", () => {
    const fixture = makeFixture();
    const { lucid } = makeRecordingLucid();
    expect(() =>
      buildSchedulerRefreshTx(
        {
          ...fixture.baseConfig,
          lucid,
          validFrom: BigInt(Number.MAX_SAFE_INTEGER) + 1n,
          selection: {
            kind: "Advance",
            activeNode: { utxo: fixture.activeTail },
          },
        },
        "00",
      ),
    ).toThrow("validFrom");
  });
});
