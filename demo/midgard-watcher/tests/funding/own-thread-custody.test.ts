import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "@al-ft/midgard-sdk";
import {
  Constr,
  credentialToAddress,
  Data,
  scriptHashToCredential,
  type UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { resolveOwnThreadCustody } from "../../src/funding/prover-funding-authority.resolve-own-thread-custody.js";

const POLICY = "a1".repeat(28);
const OTHER_POLICY = "a2".repeat(28);
const HEADER = "b1".repeat(28);
const OTHER_HEADER = "b2".repeat(28);
const PROVER = "c1".repeat(28);
const OTHER_PROVER = "c2".repeat(28);
const CATEGORY = "witnessScriptDecoding" as const;
const CATEGORY_ID = FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[CATEGORY];
const STEP_ADDRESS = credentialToAddress(
  "Custom",
  scriptHashToCredential("d1".repeat(28)),
);
const OUT_REF = `${"e1".repeat(32)}#1`;

const output = ({
  unit = `${POLICY}${CATEGORY_ID}${HEADER}`,
  quantity = 1n,
  prover = PROVER,
  extra = {},
  datum = Data.to(new Constr(0, [prover, new Constr(1, [])])),
}: {
  readonly unit?: string | null;
  readonly quantity?: bigint;
  readonly prover?: string;
  readonly extra?: Readonly<Record<string, bigint>>;
  readonly datum?: string | null;
} = {}): UTxO => ({
  txHash: "e1".repeat(32),
  outputIndex: 1,
  address: STEP_ADDRESS,
  assets: {
    lovelace: 5_000_000n,
    ...(unit === null ? {} : { [unit]: quantity }),
    ...extra,
  },
  datum,
  datumHash: null,
  scriptRef: null,
});

/** Resolves `OUT_REF` against an L1 source that shows exactly `observed`. */
const resolve = async (
  observed: readonly UTxO[],
  overrides: Partial<Parameters<typeof resolveOwnThreadCustody>[0]> = {},
) =>
  await resolveOwnThreadCustody({
    outRef: OUT_REF,
    category: CATEGORY,
    headerHash: HEADER,
    fundingPaymentKeyHash: PROVER,
    computationThreadPolicyId: POLICY,
    resolveInputs: async (outRefs) => {
      expect(outRefs).toEqual([OUT_REF]);
      return observed;
    },
    ...overrides,
  });

describe("released custody of the reservation's own computation thread, proved from the chain", () => {
  it("accepts the objective's thread naming the reservation's funding key, with its exact output", async () => {
    const thread = output();
    expect(await resolve([thread])).toEqual({
      sourceActionKind: "computation_thread.observed",
      sourceOutputIndex: 1,
      outRef: OUT_REF,
      resolvedOutputCborHex: utxoToCore(thread)
        .output()
        .to_canonical_cbor_hex(),
    });
  });

  it("refuses a thread token of another objective", async () => {
    expect(
      await resolve([
        output({ unit: `${POLICY}${CATEGORY_ID}${OTHER_HEADER}` }),
      ]),
    ).toBeNull();
    expect(
      await resolve([
        output({
          unit: `${POLICY}${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.zeroInput}${HEADER}`,
        }),
      ]),
    ).toBeNull();
  });

  it("refuses a thread whose datum names another prover", async () => {
    expect(await resolve([output({ prover: OTHER_PROVER })])).toBeNull();
  });

  it.each([
    [
      "another constructor",
      Data.to(new Constr(1, [PROVER, new Constr(1, [])])),
    ],
    ["a missing field", Data.to(new Constr(0, [PROVER]))],
    ["an extra field", Data.to(new Constr(0, [PROVER, new Constr(1, []), 0n]))],
    ["a datum that is not a constructor", Data.to([PROVER, new Constr(1, [])])],
    ["no datum", null],
  ])("refuses a thread whose step datum has %s", async (_shape, datum) => {
    expect(await resolve([output({ datum })])).toBeNull();
  });

  it("refuses a contract input that carries no thread token of this deployment", async () => {
    expect(await resolve([output({ unit: null })])).toBeNull();
    expect(
      await resolve([
        output({ unit: `${OTHER_POLICY}${CATEGORY_ID}${HEADER}` }),
      ]),
    ).toBeNull();
    expect(await resolve([output({ quantity: 2n })])).toBeNull();
    expect(
      await resolve([output({ extra: { [`${OTHER_POLICY}00`]: 1n } })]),
    ).toBeNull();
  });

  it("refuses an output the authenticated L1 source does not show, or a deployment with no thread policy", async () => {
    expect(await resolve([])).toBeNull();
    expect(await resolve([{ ...output(), outputIndex: 0 }])).toBeNull();
    expect(
      await resolve([output()], { computationThreadPolicyId: undefined }),
    ).toBeNull();
  });
});
