import {
  buildMidgardRedeemerItemProofTrace,
  encodeCbor,
} from "@al-ft/midgard-core";
import {
  PreparedValidationResolutionDatum,
  type ValidationMachineState,
  ValidationOneStepWitness,
} from "@al-ft/midgard-sdk";
import {
  redeemerItemControlData,
  redeemerItemProofWitnessData,
} from "@al-ft/midgard-validation";
import {
  Constr,
  credentialToAddress,
  Data,
  type UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  recoverStagedRoutePreparationFromHistory,
  stagedRoutePreparationInput,
  ValidationStagedPreparationUnrecoverableError,
  withStagedPreparation,
} from "../src/validation-dispute/workflow-staged-route-preparation.js";

const threadUnit = `${"44".repeat(28)}00000006${"55".repeat(28)}`;
const address = (byte: string) =>
  credentialToAddress("Custom", { type: "Script", hash: byte.repeat(28) });
const semanticResolver = address("a1");
const firstStage = address("b1");
const secondStage = address("b2");

const state: ValidationMachineState = {
  machine_version: 1n,
  event_key_hash: "01".repeat(32),
  transaction_id: "02".repeat(32),
  transaction_commitment: "03".repeat(32),
  validation_context_hash: "04".repeat(32),
  source_kind: "Forced",
  prior_ledger_root: "05".repeat(32),
  phase: "ScriptSources",
  program_counter: 0n,
  work_root: "06".repeat(32),
  execution_cpu: 0n,
  execution_memory: 0n,
  verdict: "Pending",
  rejection_code_hash: "00".repeat(32),
  ledger_delta_root: "07".repeat(32),
};
const prepared = (operatorSuccessor: string) =>
  Data.to(
    {
      fraud_prover: "11".repeat(28),
      data: {
        version: 1n,
        resolution: {
          version: 1n,
          pre_state: state,
          operator_successor_hash: operatorSuccessor.repeat(32),
          challenger_successor_hash: "33".repeat(32),
        },
        evidence_hash: "88".repeat(32),
      },
    },
    PreparedValidationResolutionDatum,
  );
const preparation = prepared("22");
const stageDatum = Data.to(new Constr(0, ["11".repeat(28)]));

const output = (
  txHash: string,
  outputIndex: number,
  at: string,
  datum: string,
): UTxO => ({
  txHash: txHash.repeat(32),
  outputIndex,
  address: at,
  assets: { lovelace: 5_000_000n, [threadUnit]: 1n },
  datum,
});
const raw = (utxo: UTxO) => ({
  outRef: `${utxo.txHash}#${utxo.outputIndex.toString()}`,
  outputCbor: utxoToCore(utxo).output().to_canonical_cbor_hex(),
  datumCbor: utxo.datum ?? null,
  referenceScriptCbor: null,
});

// prepare_selected (aa) leaves the thread at the semantic resolver; the entry
// stage (bb) spends it to the first stage, the next stage (cc) to the second.
const atResolver = output("aa", 0, semanticResolver, preparation);
const atFirstStage = output("bb", 1, firstStage, stageDatum);
const live = output("cc", 0, secondStage, stageDatum);
const history = [
  {
    txHash: "bb".repeat(32),
    resolvedInputs: [
      raw({ ...atResolver, assets: { lovelace: 2_000_000n } }),
      raw(atResolver),
    ].reverse(),
  },
  { txHash: "cc".repeat(32), resolvedInputs: [raw(atFirstStage)] },
];
const recover = (
  thread: UTxO,
  transactions: Parameters<
    typeof recoverStagedRoutePreparationFromHistory
  >[0]["snapshot"]["transactions"] = history,
) =>
  recoverStagedRoutePreparationFromHistory({
    snapshot: { transactions },
    thread,
    threadUnit,
    preparationAddresses: new Set([semanticResolver]),
  });

describe("validationTraceDispute staged-route preparation recovery", () => {
  it("walks the authenticated thread history back to the preparation the route consumed", () => {
    expect(recover(live)).toBe(preparation);
    // A thread still at its semantic resolver holds the preparation itself.
    expect(recover(atResolver)).toBe(preparation);
  });

  it("names why the history cannot recompute the preparation", () => {
    expect(() => recover(live, history.slice(1))).toThrow(
      new ValidationStagedPreparationUnrecoverableError(
        `the authenticated thread history does not include transaction ${"bb".repeat(32)}`,
      ),
    );
    expect(() =>
      recover(live, [
        history[0]!,
        { txHash: "cc".repeat(32), resolvedInputs: [] },
      ]),
    ).toThrow(
      `transaction ${"cc".repeat(32)} does not spend exactly one thread output`,
    );
    expect(() =>
      recover({ ...atResolver, datum: stageDatum }, history),
    ).toThrow("holds no prepared resolution");
    expect(() =>
      recover({ ...live, assets: { lovelace: 5_000_000n } }),
    ).toThrow("does not carry the thread token");
  });

  it("captures against a usable retained preparation without reading the history", async () => {
    const source = { recover: vi.fn(async () => preparation) };
    const capture = vi.fn(async (value: string) => value);
    await expect(
      withStagedPreparation({
        retained: preparation,
        thread: live,
        source,
        use: capture,
      }),
    ).resolves.toBe(preparation);
    expect(source.recover).not.toHaveBeenCalled();
    expect(capture).toHaveBeenCalledTimes(1);
  });

  it.each(["", "zz", "d87980", stageDatum])(
    "discards a retained preparation that does not decode (%j) and recomputes it from the history",
    async (corrupt) => {
      const source = { recover: vi.fn(async () => recover(live)) };
      const capture = vi.fn(async (value: string) => value);
      await expect(
        withStagedPreparation({
          retained: corrupt,
          thread: live,
          source,
          use: capture,
        }),
      ).resolves.toBe(preparation);
      expect(source.recover).toHaveBeenCalledWith(live);
      expect(capture.mock.calls).toEqual([[preparation]]);
    },
  );

  it("recaptures once against the recomputed preparation when the stage refuses the retained one", async () => {
    const wrong = prepared("99");
    const capture = vi.fn(async (value: string) => {
      if (value === wrong) throw new Error("stage refused its preparation");
      return value;
    });
    await expect(
      withStagedPreparation({
        retained: wrong,
        thread: live,
        source: { recover: async () => recover(live) },
        use: capture,
      }),
    ).resolves.toBe(preparation);
    expect(capture.mock.calls).toEqual([[wrong], [preparation]]);
  });

  it("keeps a failure the preparation did not cause", async () => {
    const failure = new Error("provider unavailable");
    const capture = vi.fn(async () => {
      throw failure;
    });
    // The history recomputes the same preparation: the stage's own error
    // stands, and the stage is not captured twice.
    await expect(
      withStagedPreparation({
        retained: preparation,
        thread: live,
        source: { recover: async () => preparation },
        use: capture,
      }),
    ).rejects.toBe(failure);
    expect(capture).toHaveBeenCalledTimes(1);
    // So it does when the history cannot recompute one.
    await expect(
      withStagedPreparation({
        retained: preparation,
        thread: live,
        source: { recover: async () => recover(live, []) },
        use: capture,
      }),
    ).rejects.toBe(failure);
    // A history read that fails keeps its own error too.
    const unavailable = new Error("raw L1 snapshot unavailable");
    await expect(
      withStagedPreparation({
        retained: "zz",
        thread: live,
        source: {
          recover: async () => {
            throw unavailable;
          },
        },
        use: capture,
      }),
    ).rejects.toBe(unavailable);
  });

  it("fails closed with a named reason when a corrupt preparation cannot be recomputed", async () => {
    const capture = vi.fn(async (value: string) => value);
    const refused = withStagedPreparation({
      retained: "zz",
      thread: live,
      source: { recover: async () => recover(live, []) },
      use: capture,
    });
    await expect(refused).rejects.toBeInstanceOf(
      ValidationStagedPreparationUnrecoverableError,
    );
    await expect(refused).rejects.toThrow(
      `validationTraceDispute cannot recompute its staged-route preparation: the retained preparation does not decode and the authenticated thread history does not include transaction ${"cc".repeat(32)}`,
    );
    expect(capture).not.toHaveBeenCalled();
  });
});

describe("validationTraceDispute staged-route preparation journaling", () => {
  const transitionCbor = Buffer.from(
    Data.to(
      {
        work_witness_cbor: "8100",
        claimed_successor: { ...state, program_counter: 1n },
      },
      ValidationOneStepWitness,
    ),
    "hex",
  );
  const stepProof = buildMidgardRedeemerItemProofTrace({
    itemIndex: 0,
    itemCount: 1,
    itemBytes: encodeCbor([0n, 0n, Buffer.from("00", "hex"), [10n, 20n]]),
    mode: 1,
  }).steps[0]!;
  const itemStep = new Constr(18, [
    new Constr(1, []),
    Data.from(Data.to<unknown>(redeemerItemControlData(stepProof.control))),
    Data.from(
      Data.to<unknown>(redeemerItemProofWitnessData(stepProof.witness)),
    ),
  ]);
  const scriptSources = (auxiliary: Data, semanticResolverIndex = 28) => ({
    resolverIndex: 8,
    semanticResolverIndex,
    transitionCbor,
    auxiliaryCbor: Buffer.from(Data.to(auxiliary), "hex"),
  });

  it("journals the preparation a split ScriptSources item route's entry stage consumes", () => {
    expect(
      stagedRoutePreparationInput({
        argument: scriptSources(itemStep),
        input: atResolver,
      }),
    ).toEqual({ scriptSourcesItemPreparedCbor: preparation });
    // A resumed stage journals the preparation it resumed against.
    expect(
      stagedRoutePreparationInput({
        argument: scriptSources(itemStep),
        input: live,
        resumedPreparation: preparation,
      }),
    ).toEqual({ scriptSourcesItemPreparedCbor: preparation });
  });

  it("journals nothing for a single-transaction ScriptSources resolution", () => {
    const begin = new Constr(29, [new Constr(0, ["00"])]);
    expect(
      stagedRoutePreparationInput({
        argument: scriptSources(begin, 15),
        input: atResolver,
      }),
    ).toEqual({});
  });
});
