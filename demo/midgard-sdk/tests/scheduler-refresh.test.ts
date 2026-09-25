/**
 * Scheduler-refresh builder, exercised against a real Lucid Emulator.
 *
 * The index oracle deliberately does **not** come from a hand-written model of
 * Lucid's input ordering: every expected index is read back off the transaction
 * body Lucid actually assembled (and, for the scheduler output, off the ledger
 * after the transaction is submitted to the emulator). A drift between the
 * builder's `RedeemerContext` arithmetic and the serialized transaction is what
 * the on-chain validator would see, so that is what these tests compare.
 */
import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { h28 } from "@al-ft/midgard-test-support/hex";
import {
  applyDoubleCborEncoding,
  type BuildTxWithRedeemer,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  mintingPolicyToId,
  type Network,
  PROTOCOL_PARAMETERS_DEFAULT,
  type RedeemerContext,
  type Script,
  toUnit,
  type TxSignBuilder,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
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

const moduleDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(moduleDir, "../../..");
const alwaysSucceedsBlueprintPath = resolve(
  repoRoot,
  "demo/midgard-node/blueprints/always-succeeds/plutus.json",
);

const NETWORK: Network = "Custom";
const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxCollateralInputs: 3,
} as const;

/**
 * The scheduler validator is scaffolded with the shared always-succeeds script
 * so that a genuine Plutus V3 spend — script witness, redeemer, collateral,
 * local UPLC evaluation — runs through Lucid. The refresh builder's contract is
 * the transaction layout, not the validator's own logic.
 */
const loadAlwaysSucceedsScript = (): Script => {
  const blueprint = JSON.parse(
    readFileSync(alwaysSucceedsBlueprintPath, "utf8"),
  ) as { readonly validators: readonly { readonly compiledCode: string }[] };
  const compiledCode = blueprint.validators[0]?.compiledCode;
  if (compiledCode === undefined) {
    throw new Error("always-succeeds blueprint carries no validators");
  }
  return { type: "PlutusV3", script: applyDoubleCborEncoding(compiledCode) };
};

const alwaysSucceedsSchedulerValidator = (): AuthenticatedValidator => {
  const script = loadAlwaysSucceedsScript();
  return {
    policyId: mintingPolicyToId(script),
    spendingScriptAddress: validatorToAddress(NETWORK, script),
    spendingScriptHash: validatorToScriptHash(script),
    spendingScriptCBOR: script.script,
    mintingScriptCBOR: script.script,
    spendingScript: script,
    mintingScript: script,
  } as AuthenticatedValidator;
};

type OutRef = { readonly txHash: string; readonly outputIndex: number };

const outRefKey = (outRef: OutRef): string =>
  `${outRef.txHash}#${outRef.outputIndex.toString()}`;

type CmlInputs = {
  readonly len: () => number;
  readonly get: (index: number) => {
    readonly transaction_id: () => { readonly to_hex: () => string };
    readonly index: () => bigint | number;
  };
};

const cmlOutRefKeys = (inputs: CmlInputs | undefined): readonly string[] => {
  if (inputs === undefined) {
    return [];
  }
  const keys: string[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    keys.push(`${input.transaction_id().to_hex()}#${input.index().toString()}`);
  }
  return keys;
};

const bodyOf = (tx: TxSignBuilder) =>
  tx.toTransaction().body() as unknown as {
    readonly inputs: () => CmlInputs;
    readonly reference_inputs: () => CmlInputs | undefined;
  };

/** Indices of the transaction body Lucid actually serialized. */
const assembledIndices = (tx: TxSignBuilder) => {
  const body = bodyOf(tx);
  return {
    inputs: cmlOutRefKeys(body.inputs()),
    referenceInputs: cmlOutRefKeys(body.reference_inputs()),
  };
};

const requireIndex = (keys: readonly string[], outRef: OutRef): bigint => {
  const index = keys.indexOf(outRefKey(outRef));
  if (index < 0) {
    throw new Error(
      `${outRefKey(outRef)} is absent from the assembled transaction: ${keys.join(", ")}`,
    );
  }
  return BigInt(index);
};

/**
 * Independent reference model for the ledger's canonical input ordering:
 * ascending by transaction id, then by output index. Asserting the assembled
 * body against this as well as asserting the builder against the assembled body
 * keeps the pair of claims from collapsing into one.
 */
const canonicalOutRefOrder = (keys: readonly string[]): readonly string[] =>
  [...keys].sort((left, right) => {
    const [leftHash = "", leftIndex = "0"] = left.split("#");
    const [rightHash = "", rightIndex = "0"] = right.split("#");
    if (leftHash !== rightHash) {
      return leftHash < rightHash ? -1 : 1;
    }
    return Number(leftIndex) - Number(rightIndex);
  });

type SchedulerScene = {
  readonly lucid: LucidEvolution;
  readonly emulator: Emulator;
  readonly scheduler: AuthenticatedValidator;
  readonly schedulerUnit: string;
  readonly schedulerInput: UTxO;
  readonly activeTail: UTxO;
  readonly activeRoot: UTxO;
  readonly registeredWitness: UTxO;
  readonly schedulerScriptRef: UTxO;
  readonly operatorKeyHash: string;
  readonly baseConfig: Omit<BuildSchedulerRefreshTxConfig, "selection">;
};

/**
 * Publishes, on the emulator, the scheduler UTxO (carrying the scheduler NFT),
 * the three witness UTxOs the three selections read, and a scheduler reference
 * script. All of them live at the scheduler script address so that Lucid's coin
 * selection cannot pull a witness UTxO into the input set.
 */
const setupSchedulerScene = async (): Promise<SchedulerScene> => {
  const account = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const paymentCredential = getAddressDetails(
    account.address,
  ).paymentCredential;
  if (paymentCredential === undefined || paymentCredential.type !== "Key") {
    throw new Error("expected the emulator wallet to expose a payment key");
  }
  const scheduler = alwaysSucceedsSchedulerValidator();
  const schedulerUnit = toUnit(scheduler.policyId, SCHEDULER_ASSET_NAME);
  const address = scheduler.spendingScriptAddress;
  const tagCbor = (label: string): string =>
    Data.to(Buffer.from(label, "utf8").toString("hex"));
  const tag = (label: string) => ({
    kind: "inline" as const,
    value: tagCbor(label),
  });
  const setup = await (
    await lucid
      .newTx()
      .mintAssets({ [schedulerUnit]: 1n }, Data.void())
      .attach.MintingPolicy(scheduler.mintingScript)
      .pay.ToContract(address, tag("scheduler"), {
        lovelace: 20_000_000n,
        [schedulerUnit]: 1n,
      })
      .pay.ToContract(address, tag("active-tail"), { lovelace: 5_000_000n })
      .pay.ToContract(address, tag("active-root"), { lovelace: 6_000_000n })
      .pay.ToContract(address, tag("registered-witness"), {
        lovelace: 7_000_000n,
      })
      .pay.ToContract(
        address,
        tag("scheduler-script-ref"),
        { lovelace: 40_000_000n },
        scheduler.spendingScript,
      )
      .complete()
  ).sign
    .withWallet()
    .complete();
  await lucid.awaitTx(await setup.submit());
  emulator.awaitBlock(1);

  const published = await lucid.utxosAt(address);
  const byTag = (label: string): UTxO => {
    const wanted = tagCbor(label);
    const found = published.filter((utxo) => utxo.datum === wanted);
    if (found.length !== 1) {
      throw new Error(
        `expected exactly one published ${label} UTxO, got ${found.length.toString()}`,
      );
    }
    return found[0]!;
  };
  const schedulerInput = byTag("scheduler");
  expect(schedulerInput.assets[schedulerUnit]).toBe(1n);
  const schedulerScriptRef = byTag("scheduler-script-ref");
  expect(schedulerScriptRef.scriptRef?.script).toBe(
    scheduler.spendingScript.script,
  );

  const validFrom = BigInt(emulator.now());
  return {
    lucid,
    emulator,
    scheduler,
    schedulerUnit,
    schedulerInput,
    activeTail: byTag("active-tail"),
    activeRoot: byTag("active-root"),
    registeredWitness: byTag("registered-witness"),
    schedulerScriptRef,
    operatorKeyHash: paymentCredential.hash,
    baseConfig: {
      lucid,
      scheduler,
      operatorKeyHash: paymentCredential.hash,
      schedulerInput,
      refreshedDatum: {
        ActiveOperator: {
          operator: paymentCredential.hash,
          start_time: 42n,
        },
      },
      validFrom,
      validTo: validFrom + 600_000n,
      schedulerSpendingScriptRef: schedulerScriptRef,
    },
  };
};

describe("scheduler refresh datum encoding", () => {
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
});

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
const makeCallbackProbeLucid = (
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

const probeContext = (
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

describe("scheduler refresh redeemer-callback consistency", () => {
  const scheduler = alwaysSucceedsSchedulerValidator();
  const schedulerUnit = toUnit(scheduler.policyId, SCHEDULER_ASSET_NAME);
  const utxo = (byte: string, outputIndex: number): UTxO =>
    ({
      txHash: byte.repeat(32),
      outputIndex,
      address: scheduler.spendingScriptAddress,
      assets: { lovelace: 5_000_000n },
      datum: null,
    }) as UTxO;
  const schedulerInput = utxo("10", 0);
  const activeTail = utxo("30", 0);
  const refreshedDatum = {
    ActiveOperator: { operator: h28(0x99), start_time: 42n },
  };
  const refreshedDatumCbor = encodeSchedulerDatumForChain(refreshedDatum);
  const config = (lucid: LucidEvolution) =>
    ({
      lucid,
      scheduler,
      operatorKeyHash: h28(0x99),
      schedulerInput,
      refreshedDatum,
      validFrom: 1_000n,
      validTo: 2_000n,
      selection: { kind: "Advance", activeNode: { utxo: activeTail } },
    }) satisfies BuildSchedulerRefreshTxConfig;

  it("publishes the redeemer when every callback resolution agrees", async () => {
    const context = probeContext(
      scheduler,
      schedulerInput,
      refreshedDatumCbor,
      schedulerUnit,
      1n,
      [activeTail],
    );
    const result = await Effect.runPromise(
      buildUnsignedSchedulerRefreshTxProgram(
        config(makeCallbackProbeLucid([context, context, context])),
      ),
    );
    expect(result.layout).toEqual({
      kind: "Advance",
      schedulerInputIndex: 1n,
      schedulerOutputIndex: 0n,
      activeNodeRefInputIndex: 0n,
    });
  });

  it("refuses to publish a redeemer when two callback resolutions disagree", async () => {
    const first = probeContext(
      scheduler,
      schedulerInput,
      refreshedDatumCbor,
      schedulerUnit,
      1n,
      [activeTail],
    );
    const second = probeContext(
      scheduler,
      schedulerInput,
      refreshedDatumCbor,
      schedulerUnit,
      2n,
      [activeTail],
    );
    await expect(
      Effect.runPromise(
        buildUnsignedSchedulerRefreshTxProgram(
          config(makeCallbackProbeLucid([first, second])),
        ),
      ),
    ).rejects.toThrow(
      /resolved inconsistent scheduler refresh redeemers|Failed to build scheduler refresh tx/,
    );
  });
});
