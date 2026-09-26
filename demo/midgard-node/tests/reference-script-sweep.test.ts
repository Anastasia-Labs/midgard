import "./utils.js";

import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  assetsToValue,
  CML,
  type Script,
  toUnit,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildReferenceScriptSweepPlan,
  decideRetiredAuthPolicyDisposition,
  type LiveReferenceScriptDeployment,
  referenceScriptFee,
  referenceScriptLedgerBytes,
  type ReferenceScriptSweepLimits,
  type ReferenceScriptSweepPlan,
  ReferenceScriptSweepRefusal,
  type RetiredAuthPolicyDisposition,
  selectRetiredReferenceScriptUtxos,
  summarizeReferenceScriptSweepPlan,
  valueCborBytes,
} from "../src/transactions/reference-script-sweep.js";

const WALLET =
  "addr_test1qq7kh4kps5dknl2ntzp57r5yywjc6uld79nd9vqwa9hrsvwxtp05cv8ax7wyws8r3h8ut00q0axzvm3dlz0nd7exu8csgx4p7w";
const RETIRED = "1a".repeat(28);
const LIVE = "ef".repeat(28);
const SIGNER = "3d".repeat(28);

/** Current preprod protocol parameters. */
const PREPROD_LIMITS: ReferenceScriptSweepLimits = {
  maxTxSize: 16_384,
  maxValueSize: 5_000,
  maxReferenceScriptBytesPerTx: 204_800,
  minFeeA: 44n,
  minFeeB: 155_381n,
  coinsPerUtxoByte: 4_310n,
  referenceScriptFee: {
    base: { numerator: 15n, denominator: 1n },
    range: 25_600,
    multiplier: { numerator: 6n, denominator: 5n },
  },
};

const QUARANTINE: RetiredAuthPolicyDisposition = {
  kind: "quarantine",
  reason: "expired",
};

const NO_LIVE_SCRIPTS: LiveReferenceScriptDeployment = {
  authPolicyId: LIVE,
  targets: [],
};

const cborBytesHeader = (length: number): string =>
  length < 24
    ? (0x40 + length).toString(16).padStart(2, "0")
    : length < 0x100
      ? `58${length.toString(16).padStart(2, "0")}`
      : length < 0x10000
        ? `59${length.toString(16).padStart(4, "0")}`
        : `5a${length.toString(16).padStart(8, "0")}`;

/** A Plutus script whose single-CBOR program is `payloadBytes` bytes long. */
const plutusScript = (seed: number, payloadBytes: number): Script => {
  const payload = seed
    .toString(16)
    .padStart(8, "0")
    .repeat(payloadBytes / 4);
  const single = `${cborBytesHeader(payloadBytes)}${payload}`;
  return {
    type: "PlutusV3",
    script: `${cborBytesHeader(single.length / 2)}${single}`,
  };
};

const tokenName = (index: number): string =>
  Buffer.from(`Role${index.toString().padStart(3, "0")}`).toString("hex");

const txHash = (index: number): string => index.toString(16).padStart(64, "0");

const refUtxo = ({
  index,
  policyId = RETIRED,
  script = plutusScript(index, 4_000),
  lovelace = 40_000_000n,
  extraAssets = {},
}: {
  readonly index: number;
  readonly policyId?: string;
  readonly script?: Script;
  readonly lovelace?: bigint;
  readonly extraAssets?: Assets;
}): UTxO => ({
  txHash: txHash(index),
  outputIndex: 0,
  address: WALLET,
  assets: {
    lovelace,
    [toUnit(policyId, tokenName(index))]: 1n,
    ...extraAssets,
  },
  scriptRef: script,
});

const LIVE_ROLE_NAMES = Object.keys(SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES);

const liveTarget = (
  roleIndex: number,
  script: Script,
): SDK.ReferenceScriptTarget => ({
  name: LIVE_ROLE_NAMES[roleIndex]!,
  script,
});

/** A reference script the live resolution accepts for `target`. */
const liveRef = (
  index: number,
  target: SDK.ReferenceScriptTarget,
  address = WALLET,
): UTxO => ({
  txHash: txHash(index),
  outputIndex: 0,
  address,
  assets: {
    lovelace: 40_000_000n,
    [SDK.referenceScriptAuthUnit(LIVE, target.name)]: 1n,
  },
  scriptRef: target.script,
});

const liveDeployment = (
  ...targets: SDK.ReferenceScriptTarget[]
): LiveReferenceScriptDeployment => ({ authPolicyId: LIVE, targets });

const plainUtxo = (index: number): UTxO => ({
  txHash: txHash(index),
  outputIndex: 0,
  address: WALLET,
  assets: { lovelace: 100_000_000n },
});

const plan = ({
  utxos,
  live = NO_LIVE_SCRIPTS,
  limits = PREPROD_LIMITS,
  disposition = QUARANTINE,
  maxReferenceScriptBytesPerBatch,
  maxInputsPerBatch,
  retiredAuthPolicyId = RETIRED,
}: {
  readonly utxos: readonly UTxO[];
  readonly live?: LiveReferenceScriptDeployment;
  readonly limits?: ReferenceScriptSweepLimits;
  readonly disposition?: RetiredAuthPolicyDisposition;
  readonly maxReferenceScriptBytesPerBatch?: number;
  readonly maxInputsPerBatch?: number;
  readonly retiredAuthPolicyId?: string;
}): ReferenceScriptSweepPlan =>
  buildReferenceScriptSweepPlan({
    utxos,
    referenceScriptsAddress: WALLET,
    returnAddress: WALLET,
    quarantineAddress: WALLET,
    retiredAuthPolicyId,
    live,
    limits,
    disposition,
    maxReferenceScriptBytesPerBatch,
    maxInputsPerBatch,
  });

const refusalCheck = (run: () => unknown): string => {
  try {
    run();
  } catch (error) {
    if (error instanceof ReferenceScriptSweepRefusal) {
      return error.check;
    }
    throw error;
  }
  throw new Error("expected the sweep to be refused");
};

const batchOutRefs = (sweep: ReferenceScriptSweepPlan): string[][] =>
  sweep.batches.map((batch) =>
    batch.inputs.map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`),
  );

/** Applies one batch the way the chain would: inputs gone, quarantine and change added. */
const applyBatch = (
  utxos: readonly UTxO[],
  sweep: ReferenceScriptSweepPlan,
  batchIndex: number,
): UTxO[] => {
  const batch = sweep.batches[batchIndex]!;
  const spent = new Set(batch.inputs.map((utxo) => utxo.txHash));
  const txId = txHash(900_000 + batchIndex);
  return [
    ...utxos.filter((utxo) => !spent.has(utxo.txHash)),
    ...batch.quarantineOutputs.map((assets, outputIndex) => ({
      txHash: txId,
      outputIndex,
      address: WALLET,
      assets: { ...assets },
    })),
    {
      txHash: txId,
      outputIndex: batch.quarantineOutputs.length,
      address: WALLET,
      assets: { lovelace: batch.netReclaimedLovelace },
    },
  ];
};

describe("retired reference-script sweep scope", () => {
  it("refuses the live deployment's auth policy, in any letter case", () => {
    const utxos = [refUtxo({ index: 1, policyId: LIVE })];
    expect(refusalCheck(() => plan({ utxos, retiredAuthPolicyId: LIVE }))).toBe(
      "live-auth-policy",
    );
    expect(
      refusalCheck(() =>
        plan({ utxos, retiredAuthPolicyId: LIVE.toUpperCase() }),
      ),
    ).toBe("live-auth-policy");
  });

  it("refuses a malformed retired policy id", () => {
    expect(
      refusalCheck(() =>
        plan({ utxos: [], retiredAuthPolicyId: RETIRED.slice(2) }),
      ),
    ).toBe("invalid-retired-policy");
  });

  it("selects only the retired policy's reference-script UTxOs from a mixed wallet", () => {
    const retired = [refUtxo({ index: 1 }), refUtxo({ index: 2 })];
    const live = [
      refUtxo({ index: 3, policyId: LIVE }),
      refUtxo({ index: 4, policyId: LIVE }),
    ];
    const retiredTokenWithoutScript: UTxO = {
      ...refUtxo({ index: 5 }),
      scriptRef: undefined,
    };
    const utxos = [
      plainUtxo(6),
      ...live,
      retiredTokenWithoutScript,
      ...retired,
    ];

    const selected = selectRetiredReferenceScriptUtxos({
      utxos,
      referenceScriptsAddress: WALLET,
      retiredAuthPolicyId: RETIRED,
      live: NO_LIVE_SCRIPTS,
    });
    const sweep = plan({ utxos });

    expect(selected.map((utxo) => utxo.txHash)).toEqual(
      retired.map((utxo) => utxo.txHash),
    );
    expect(sweep.retainedUtxoCount).toBe(4);
    expect(sweep.batches.flatMap((batch) => batch.inputs)).toEqual(retired);
  });

  it("sweeps a retired copy of a live script while the live-policy copy keeps resolving", () => {
    const shared = plutusScript(7, 4_000);
    const target = liveTarget(0, shared);
    const retiredCopy = refUtxo({ index: 1, script: shared });
    const liveCopy = liveRef(2, target);
    const utxos = [retiredCopy, liveCopy, refUtxo({ index: 3 })];

    const sweep = plan({ utxos, live: liveDeployment(target) });

    expect(sweep.batches.flatMap((batch) => batch.inputs)).toEqual([
      retiredCopy,
      utxos[2],
    ]);
    expect(
      sweep.batches.flatMap((batch) => batch.inputs).includes(liveCopy),
    ).toBe(false);
  });

  it("refuses when a live target's only copy is under the retired policy", () => {
    const shared = plutusScript(8, 4_000);
    const stranded = liveTarget(1, shared);
    const resolved = liveTarget(2, plutusScript(9, 4_000));
    const utxos = [refUtxo({ index: 1, script: shared }), liveRef(2, resolved)];

    expect(
      refusalCheck(() =>
        plan({ utxos, live: liveDeployment(stranded, resolved) }),
      ),
    ).toBe("live-target-stranded");
  });

  it("refuses when the only live-policy copy sits at another address", () => {
    const shared = plutusScript(10, 4_000);
    const target = liveTarget(3, shared);
    const utxos = [
      refUtxo({ index: 1, script: shared }),
      liveRef(2, target, "addr_test1elsewhere"),
    ];

    expect(
      refusalCheck(() => plan({ utxos, live: liveDeployment(target) })),
    ).toBe("live-target-stranded");
  });

  it("refuses when the live role copy carries a different script", () => {
    const shared = plutusScript(12, 4_000);
    const target = liveTarget(5, shared);
    const utxos = [
      refUtxo({ index: 1, script: shared }),
      liveRef(2, liveTarget(5, plutusScript(13, 4_000))),
    ];

    expect(
      refusalCheck(() => plan({ utxos, live: liveDeployment(target) })),
    ).toBe("live-target-stranded");
  });

  it("refuses a selected UTxO that the live resolution accepts", () => {
    const script = plutusScript(11, 4_000);
    const target = liveTarget(4, script);
    const accepted: UTxO = {
      ...liveRef(1, target),
      assets: {
        ...liveRef(1, target).assets,
        [toUnit(RETIRED, tokenName(1))]: 1n,
      },
    };
    const utxos = [accepted, liveRef(2, target)];

    expect(
      refusalCheck(() => plan({ utxos, live: liveDeployment(target) })),
    ).toBe("live-resolved-outref");
  });

  it("refuses a retired reference-script UTxO that also holds a live auth token", () => {
    const utxos = [
      refUtxo({
        index: 1,
        extraAssets: { [toUnit(LIVE, tokenName(1))]: 1n },
      }),
    ];

    expect(refusalCheck(() => plan({ utxos }))).toBe("live-auth-token");
  });

  it("refuses a selected UTxO carrying an asset outside the retired policy", () => {
    const utxos = [
      refUtxo({
        index: 1,
        extraAssets: { [toUnit("ab".repeat(28), "00")]: 5n },
      }),
    ];

    expect(refusalCheck(() => plan({ utxos }))).toBe("foreign-asset");
  });

  it("never re-selects quarantine outputs, so a finished sweep plans nothing", () => {
    const utxos = [refUtxo({ index: 1 }), refUtxo({ index: 2 }), plainUtxo(3)];
    const first = plan({ utxos });
    const afterSweep = applyBatch(utxos, first, 0);

    expect(first.batches).toHaveLength(1);
    expect(
      afterSweep.some(
        (utxo) =>
          utxo.scriptRef === undefined &&
          Object.keys(utxo.assets).some((unit) => unit.startsWith(RETIRED)),
      ),
    ).toBe(true);
    expect(plan({ utxos: afterSweep }).batches).toEqual([]);
  });
});

describe("retired reference-script sweep batching", () => {
  it("derives budgets from the protocol limits with a 10% margin", () => {
    const sweep = plan({ utxos: [refUtxo({ index: 1 })] });

    expect(sweep.budgets).toEqual({
      referenceScriptBytesPerBatch: 184_320,
      txBytesPerBatch: 14_745,
      valueBytesPerOutput: 4_500,
      inputsPerBatch: 64,
    });
    expect(() =>
      plan({
        utxos: [refUtxo({ index: 1 })],
        maxReferenceScriptBytesPerBatch: 184_321,
      }),
    ).toThrow(/no larger than 184320/);
  });

  it("closes a batch exactly at the reference-script byte budget", () => {
    // 9_996-byte programs plus a 3-byte CBOR header: 9_999 ledger bytes.
    const utxos = Array.from({ length: 7 }, (_, index) =>
      refUtxo({ index: index + 1, script: plutusScript(index + 1, 9_996) }),
    );
    expect(referenceScriptLedgerBytes(utxos[0]!.scriptRef!)).toBe(9_999);

    const atBoundary = plan({
      utxos,
      maxReferenceScriptBytesPerBatch: 3 * 9_999,
    });
    const belowBoundary = plan({
      utxos,
      maxReferenceScriptBytesPerBatch: 3 * 9_999 - 1,
    });

    expect(atBoundary.batches.map((batch) => batch.inputs.length)).toEqual([
      3, 3, 1,
    ]);
    expect(atBoundary.batches[0]!.referenceScriptBytes).toBe(3 * 9_999);
    expect(belowBoundary.batches.map((batch) => batch.inputs.length)).toEqual([
      2, 2, 2, 1,
    ]);
  });

  it("keeps every batch under the protocol reference-script budget", () => {
    const utxos = Array.from({ length: 60 }, (_, index) =>
      refUtxo({ index: index + 1, script: plutusScript(index + 1, 14_000) }),
    );
    const sweep = plan({ utxos });

    // 14_003 ledger bytes each: 13 fit in 184_320, a 14th would not.
    expect(sweep.batches.map((batch) => batch.inputs.length)).toEqual([
      13, 13, 13, 13, 8,
    ]);
    for (const batch of sweep.batches) {
      expect(batch.referenceScriptBytes).toBeLessThanOrEqual(184_320);
    }
  });

  it("caps the input count per batch", () => {
    const utxos = Array.from({ length: 70 }, (_, index) =>
      refUtxo({ index: index + 1, script: plutusScript(index + 1, 16) }),
    );

    expect(plan({ utxos }).batches.map((batch) => batch.inputs.length)).toEqual(
      [64, 6],
    );
  });

  it("keeps the estimated transaction size under its budget", () => {
    const utxos = Array.from({ length: 400 }, (_, index) =>
      refUtxo({ index: index + 1, script: plutusScript(index + 1, 16) }),
    );
    const sweep = plan({ utxos, maxInputsPerBatch: 1_000 });

    expect(sweep.batches.length).toBeGreaterThan(1);
    for (const batch of sweep.batches) {
      expect(batch.estimatedTxBytes).toBeLessThanOrEqual(14_745);
    }
    // The first batch stopped because one more input would not fit.
    const first = sweep.batches[0]!;
    const next = sweep.batches[1]!.inputs[0]!;
    const extended = plan({
      utxos: [...first.inputs, next],
      maxInputsPerBatch: 1_000,
    });
    expect(extended.batches).toHaveLength(2);
  });

  it("packs a batch's tokens into as few quarantine outputs as the value size allows", () => {
    const utxos = Array.from({ length: 60 }, (_, index) =>
      refUtxo({ index: index + 1, script: plutusScript(index + 1, 16) }),
    );
    const limits = { ...PREPROD_LIMITS, maxValueSize: 500 };
    const [batch] = plan({ utxos, limits }).batches;
    const budget = Math.floor((500 * 9) / 10);
    const tokenCount = (assets: Readonly<Assets>) =>
      Object.keys(assets).filter((unit) => unit !== "lovelace").length;

    expect(batch!.quarantineOutputs.length).toBeGreaterThan(1);
    for (const output of batch!.quarantineOutputs) {
      expect(valueCborBytes(output)).toBeLessThanOrEqual(budget);
    }
    // Each output but the last is full: one more token would overflow it.
    for (const output of batch!.quarantineOutputs.slice(0, -1)) {
      const units = Object.keys(output).filter((unit) => unit !== "lovelace");
      const anyOther = Object.keys(batch!.quarantineOutputs.at(-1)!).find(
        (unit) => unit !== "lovelace",
      )!;
      expect(
        valueCborBytes({
          ...output,
          lovelace: 4_000_000_000n,
          [anyOther]: 1n,
        }),
      ).toBeGreaterThan(budget);
      expect(units.length).toBeGreaterThan(0);
    }
    expect(
      batch!.quarantineOutputs.reduce(
        (total, output) => total + tokenCount(output),
        0,
      ),
    ).toBe(60);
  });

  it("gives quarantine outputs only their minimum ADA and returns the rest", () => {
    const utxos = [refUtxo({ index: 1 }), refUtxo({ index: 2 })];
    const [batch] = plan({ utxos }).batches;

    expect(batch!.quarantineOutputs).toHaveLength(1);
    const output = batch!.quarantineOutputs[0]!;
    expect(output.lovelace).toBeLessThan(2_000_000n);
    expect(batch!.netReclaimedLovelace).toBe(
      80_000_000n - batch!.estimatedFee - batch!.quarantineLovelace,
    );
  });

  it("refuses a batch whose inputs cannot pay its fee and quarantine output", () => {
    const utxos = [refUtxo({ index: 1, lovelace: 1_000_000n })];

    expect(refusalCheck(() => plan({ utxos }))).toBe("unfunded-batch");
  });

  it("re-plans idempotently from chain after each confirmed batch", () => {
    const utxos = [
      ...Array.from({ length: 5 }, (_, index) =>
        refUtxo({ index: index + 1, script: plutusScript(index + 1, 9_996) }),
      ),
      refUtxo({ index: 50, policyId: LIVE }),
      plainUtxo(60),
    ];
    const options = { maxReferenceScriptBytesPerBatch: 2 * 9_999 };
    const initial = plan({ utxos, ...options });

    expect(plan({ utxos, ...options })).toEqual(initial);
    expect(batchOutRefs(initial)).toHaveLength(3);
    const afterFirst = applyBatch(utxos, initial, 0);
    expect(batchOutRefs(plan({ utxos: afterFirst, ...options }))).toEqual(
      batchOutRefs(initial).slice(1),
    );
    const afterSecond = applyBatch(
      afterFirst,
      plan({ utxos: afterFirst, ...options }),
      0,
    );
    expect(batchOutRefs(plan({ utxos: afterSecond, ...options }))).toEqual(
      batchOutRefs(initial).slice(2),
    );
  });

  it("summarizes per-batch and total lovelace, fee and quarantine", () => {
    const utxos = Array.from({ length: 5 }, (_, index) =>
      refUtxo({ index: index + 1, script: plutusScript(index + 1, 9_996) }),
    );
    const summary = summarizeReferenceScriptSweepPlan(
      plan({ utxos, maxReferenceScriptBytesPerBatch: 2 * 9_999 }),
    );

    expect(summary.totals).toMatchObject({
      batchCount: 3,
      inputCount: 5,
      inputLovelace: 200_000_000n,
      referenceScriptBytes: 5 * 9_999,
    });
    expect(summary.totals.netReclaimedLovelace).toBe(
      summary.totals.inputLovelace -
        summary.totals.estimatedFee -
        summary.totals.quarantineLovelace,
    );
    expect(summary.tokenDisposition).toBe("quarantine");
  });
});

describe("retired reference-script sweep fees and sizes", () => {
  it("prices reference-script bytes with Conway's tiers", () => {
    expect(referenceScriptFee(PREPROD_LIMITS, 0)).toBe(0n);
    expect(referenceScriptFee(PREPROD_LIMITS, 25_600)).toBe(384_000n);
    expect(referenceScriptFee(PREPROD_LIMITS, 25_601)).toBe(384_018n);
    expect(referenceScriptFee(PREPROD_LIMITS, 51_200)).toBe(844_800n);
    // 1_000 bytes at 21.6 lovelace in the third tier.
    expect(referenceScriptFee(PREPROD_LIMITS, 52_200)).toBe(866_400n);
  });

  it("measures ledger script bytes as the single-CBOR program", () => {
    expect(referenceScriptLedgerBytes(plutusScript(1, 5_200))).toBe(5_203);
    expect(
      referenceScriptLedgerBytes({
        type: "Native",
        script: "8200581c" + SIGNER,
      }),
    ).toBe(32);
  });

  it("computes value sizes exactly as the ledger serializes them", () => {
    const shapes: Assets[] = [
      { lovelace: 1_500_000n },
      { lovelace: 4_000_000_000n, [toUnit(RETIRED, tokenName(1))]: 1n },
      Object.fromEntries([
        ["lovelace", 23n],
        ...Array.from({ length: 30 }, (_, index) => [
          toUnit(RETIRED, "ff".repeat(index + 1)),
          BigInt(index * 1_000),
        ]),
        [toUnit(LIVE, ""), 70_000n],
      ]),
    ];
    for (const assets of shapes) {
      const positive = Object.fromEntries(
        Object.entries(assets).filter(
          ([unit, amount]) => unit === "lovelace" || amount > 0n,
        ),
      );
      const value = assetsToValue(positive);
      try {
        expect(valueCborBytes(positive)).toBe(value.to_cbor_bytes().length);
      } finally {
        value.free();
      }
    }
  });
});

describe("retired auth policy disposition", () => {
  const authPolicy = (signer: string, invalidHereafter: number): Script => {
    const conditions = CML.NativeScriptList.new();
    conditions.add(
      CML.NativeScript.new_script_pubkey(CML.Ed25519KeyHash.from_hex(signer)),
    );
    conditions.add(
      CML.NativeScript.new_script_invalid_hereafter(BigInt(invalidHereafter)),
    );
    const script = CML.NativeScript.new_script_all(conditions);
    return { type: "Native", script: script.to_cbor_hex() };
  };
  const policyIdOf = (script: Script): string => validatorToScriptHash(script);

  it("burns while the wallet can still satisfy the policy", () => {
    const script = authPolicy(SIGNER, 10_000);
    const disposition = decideRetiredAuthPolicyDisposition({
      retiredAuthPolicyId: policyIdOf(script),
      policyScript: script,
      signerKeyHash: SIGNER,
      currentSlot: 9_000,
    });

    expect(disposition).toMatchObject({ kind: "burn", validToSlot: 9_600 });
  });

  it("quarantines once the policy's validity window has closed", () => {
    const script = authPolicy(SIGNER, 10_000);

    expect(
      decideRetiredAuthPolicyDisposition({
        retiredAuthPolicyId: policyIdOf(script),
        policyScript: script,
        signerKeyHash: SIGNER,
        currentSlot: 9_401,
      }).kind,
    ).toBe("quarantine");
  });

  it("quarantines when the reference wallet is not the policy signer", () => {
    const script = authPolicy("aa".repeat(28), 10_000);

    expect(
      decideRetiredAuthPolicyDisposition({
        retiredAuthPolicyId: policyIdOf(script),
        policyScript: script,
        signerKeyHash: SIGNER,
        currentSlot: 1,
      }).kind,
    ).toBe("quarantine");
  });

  it("quarantines when no policy script is supplied", () => {
    expect(
      decideRetiredAuthPolicyDisposition({
        retiredAuthPolicyId: RETIRED,
        signerKeyHash: SIGNER,
        currentSlot: 1,
      }),
    ).toMatchObject({ kind: "quarantine" });
  });

  it("refuses a policy script that does not hash to the retired policy", () => {
    const script = authPolicy(SIGNER, 10_000);

    expect(
      refusalCheck(() =>
        decideRetiredAuthPolicyDisposition({
          retiredAuthPolicyId: RETIRED,
          policyScript: script,
          signerKeyHash: SIGNER,
          currentSlot: 1,
        }),
      ),
    ).toBe("policy-script-mismatch");
  });

  it("burns every retired token in place of quarantine outputs", () => {
    const script = authPolicy(SIGNER, 10_000);
    const policyId = policyIdOf(script);
    const utxos = [
      refUtxo({ index: 1, policyId }),
      refUtxo({ index: 2, policyId }),
    ];
    const disposition = decideRetiredAuthPolicyDisposition({
      retiredAuthPolicyId: policyId,
      policyScript: script,
      signerKeyHash: SIGNER,
      currentSlot: 1,
    });
    const [batch] = plan({
      utxos,
      retiredAuthPolicyId: policyId,
      disposition,
    }).batches;

    expect(batch!.quarantineOutputs).toEqual([]);
    expect(batch!.quarantineLovelace).toBe(0n);
    expect(batch!.burnedAssets).toEqual({
      [toUnit(policyId, tokenName(1))]: 1n,
      [toUnit(policyId, tokenName(2))]: 1n,
    });
  });
});
