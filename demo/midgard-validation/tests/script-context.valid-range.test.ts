import {
  type L1SlotConfig,
  NAMED_NETWORK_L1_SLOT_CONFIGS,
} from "@al-ft/midgard-core/l1-slot-config";
import {
  type Data,
  DataConstr,
  DataI,
  dataToCbor,
} from "@harmoniclabs/plutus-data";
import { slotToUnixTime } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { MidgardLedgerRedeemer } from "../src/ledger-tx/types.js";
import type { MidgardScriptPurpose } from "../src/midgard-redeemers.js";
import {
  buildMidgardScriptContext,
  buildPlutusV3ScriptContext,
  type ScriptContextView,
} from "../src/script-context.js";
import { outRefFromByte } from "./validation-fixtures.js";

// A committed body carries its validity interval in L1 slots; a script sees
// it as Cardano's `transValidityInterval` builds it: finite bounds in POSIX
// milliseconds on the deployment's slot mapping, the finite lower bound
// inclusive and the finite upper bound strict, and an absent bound as
// `NegInf`/`PosInf` with an inclusive closure. Both context builders share
// the one `validRange`.

const PREPROD = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;
const DEVNET: L1SlotConfig = {
  zeroTime: 1_789_056_000_000,
  zeroSlot: 0,
  slotLength: 1000,
};

const SPEND_HASH = "22".repeat(28);
const spend: MidgardScriptPurpose = {
  kind: "spend",
  scriptHash: SPEND_HASH,
  outRefHex: outRefFromByte(0x33).toString("hex"),
};
const spendRedeemer: MidgardLedgerRedeemer = {
  tag: 0,
  index: 0n,
  dataCbor: Buffer.from([0x00]),
  exUnits: { memory: 1n, steps: 1n },
};

const view = (
  interval: { start?: bigint; end?: bigint },
  l1SlotConfig: L1SlotConfig = PREPROD,
): ScriptContextView => ({
  l1SlotConfig,
  txId: Buffer.alloc(32, 0x44),
  inputs: [],
  referenceInputs: [],
  outputs: [],
  fee: 0n,
  ...(interval.start === undefined
    ? {}
    : { validityIntervalStart: interval.start }),
  ...(interval.end === undefined ? {} : { validityIntervalEnd: interval.end }),
  observers: [],
  signatories: [],
  mint: new Map(),
  redeemers: [{ purpose: spend, redeemer: spendRedeemer }],
});

type Bound =
  | { readonly kind: "NegInf"; readonly inclusive: boolean }
  | { readonly kind: "PosInf"; readonly inclusive: boolean }
  | {
      readonly kind: "Finite";
      readonly time: bigint;
      readonly inclusive: boolean;
    };

const asConstr = (value: Data | undefined): DataConstr => {
  if (!(value instanceof DataConstr)) throw new Error("expected a constr");
  return value;
};

const decodeBool = (value: Data | undefined): boolean => {
  const flag = asConstr(value);
  expect(flag.fields).toHaveLength(0);
  if (flag.constr === 0n) return false;
  if (flag.constr === 1n) return true;
  throw new Error(`not a Bool: constr ${flag.constr.toString()}`);
};

const decodeBound = (value: Data | undefined): Bound => {
  const bound = asConstr(value);
  expect(bound.constr).toBe(0n);
  expect(bound.fields).toHaveLength(2);
  const extended = asConstr(bound.fields[0]);
  const inclusive = decodeBool(bound.fields[1]);
  switch (extended.constr) {
    case 0n:
      expect(extended.fields).toHaveLength(0);
      return { kind: "NegInf", inclusive };
    case 1n: {
      expect(extended.fields).toHaveLength(1);
      const time = extended.fields[0];
      if (!(time instanceof DataI)) throw new Error("expected a POSIX time");
      return { kind: "Finite", time: time.int, inclusive };
    }
    case 2n:
      expect(extended.fields).toHaveLength(0);
      return { kind: "PosInf", inclusive };
    default:
      throw new Error(`not an Extended: ${extended.constr.toString()}`);
  }
};

// PlutusV3 txInfo field 7 and MidgardV1 txInfo field 4 are `validRange`.
const builders = [
  [
    "PlutusV3",
    (scriptView: ScriptContextView) =>
      asConstr(
        asConstr(buildPlutusV3ScriptContext(scriptView, spend, spendRedeemer))
          .fields[0],
      ).fields[7],
  ],
  [
    "MidgardV1",
    (scriptView: ScriptContextView) =>
      asConstr(
        asConstr(buildMidgardScriptContext(scriptView, spend, spendRedeemer))
          .fields[0],
      ).fields[4],
  ],
] as const;

const validRange = (
  build: (typeof builders)[number][1],
  scriptView: ScriptContextView,
): readonly [Bound, Bound] => {
  const interval = asConstr(build(scriptView));
  expect(interval.constr).toBe(0n);
  expect(interval.fields).toHaveLength(2);
  return [decodeBound(interval.fields[0]), decodeBound(interval.fields[1])];
};

describe.each(builders)("%s validRange", (_name, build) => {
  it("carries finite bounds in POSIX milliseconds, lower inclusive and upper strict", () => {
    expect(validRange(build, view({ start: 87_400n, end: 90_000n }))).toEqual([
      { kind: "Finite", time: 1_655_770_600_000n, inclusive: true },
      { kind: "Finite", time: 1_655_773_200_000n, inclusive: false },
    ]);
  });

  it("agrees with Lucid's slot-to-time conversion for the named network", () => {
    for (const [start, end] of [
      [86_400n, 86_401n],
      [12_345_678n, 12_349_278n],
      [75_000_000n, 75_000_120n],
    ] as const) {
      const [lower, upper] = validRange(build, view({ start, end }));
      expect(lower).toEqual({
        kind: "Finite",
        time: BigInt(slotToUnixTime("Preprod", Number(start))),
        inclusive: true,
      });
      expect(upper).toEqual({
        kind: "Finite",
        time: BigInt(slotToUnixTime("Preprod", Number(end))),
        inclusive: false,
      });
    }
  });

  it("encodes an absent upper bound as PosInf and an absent lower bound as NegInf, both inclusive", () => {
    expect(validRange(build, view({ start: 87_400n }))).toEqual([
      { kind: "Finite", time: 1_655_770_600_000n, inclusive: true },
      { kind: "PosInf", inclusive: true },
    ]);
    expect(validRange(build, view({ end: 90_000n }))).toEqual([
      { kind: "NegInf", inclusive: true },
      { kind: "Finite", time: 1_655_773_200_000n, inclusive: false },
    ]);
    expect(validRange(build, view({}))).toEqual([
      { kind: "NegInf", inclusive: true },
      { kind: "PosInf", inclusive: true },
    ]);
  });

  it("follows the deployment's slot mapping, never the raw slot", () => {
    const interval = { start: 120n, end: 180n };
    expect(validRange(build, view(interval, DEVNET))).toEqual([
      { kind: "Finite", time: 1_789_056_120_000n, inclusive: true },
      { kind: "Finite", time: 1_789_056_180_000n, inclusive: false },
    ]);
    const [devnetLower] = validRange(build, view(interval, DEVNET));
    const [shortSlotLower] = validRange(
      build,
      view(interval, { ...DEVNET, slotLength: 100 }),
    );
    expect(shortSlotLower).toEqual({
      kind: "Finite",
      time: 1_789_056_012_000n,
      inclusive: true,
    });
    expect(shortSlotLower).not.toEqual(devnetLower);
    for (const bound of validRange(build, view(interval, DEVNET)))
      expect(bound).not.toMatchObject({ time: interval.start });
  });
});

// Shared with `validity_interval_summary_v1` in
// onchain/aiken/lib/midgard/script-context-v1.test.ak: the same config and
// bounds must give these exact `serialise_data` bytes on both sides.
const GOLDEN_SLOT_CONFIG: L1SlotConfig = {
  zeroTime: 1_700_000_000_000,
  zeroSlot: 4_492_800,
  slotLength: 200,
};
const VALID_RANGE_GOLDENS = [
  {
    start: 4_500_000n,
    end: 4_500_600n,
    cbor: "d8799fd8799fd87a9f1b0000018bcffb6100ffd87a80ffd8799fd87a9f1b0000018bcffd35c0ffd87980ffff",
  },
  {
    end: 4_500_600n,
    cbor: "d8799fd8799fd87980d87a80ffd8799fd87a9f1b0000018bcffd35c0ffd87980ffff",
  },
  {
    start: 4_500_000n,
    cbor: "d8799fd8799fd87a9f1b0000018bcffb6100ffd87a80ffd8799fd87b80d87a80ffff",
  },
  {
    cbor: "d8799fd8799fd87980d87a80ffd8799fd87b80d87a80ffff",
  },
] as const;

describe.each(builders)(
  "%s validRange cross-language golden",
  (_name, build) => {
    it.each(VALID_RANGE_GOLDENS)(
      "encodes start=$start end=$end as the Aiken golden bytes",
      (golden) => {
        const interval = {
          ...("start" in golden ? { start: golden.start } : {}),
          ...("end" in golden ? { end: golden.end } : {}),
        };
        expect(
          Buffer.from(
            dataToCbor(build(view(interval, GOLDEN_SLOT_CONFIG))!),
          ).toString("hex"),
        ).toBe(golden.cbor);
      },
    );
  },
);
