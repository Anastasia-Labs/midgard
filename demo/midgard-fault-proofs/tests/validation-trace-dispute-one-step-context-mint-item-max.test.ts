/** The validation-trace dispute runner at a CEK context step the mint item
 * stage verifies, over a failing transaction whose mint has the deepest
 * frontier and the most peaks the mint and output fields hold: each policy
 * names its assets from the shortest names up, and the transaction's outputs
 * hold them a run of whole policies each, within the ledger's value bound.
 * At the asset whose proof is the longest: each move of its resolution
 * within the transaction size limit less its reserve, driven to the award
 * (support/validation-trace-dispute-one-step-context-drive.ts). */
import { MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { Constr, Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { nativeMintPreimageCbor } from "./helpers/plutus-execution-seeds.js";
import { contextStageDrive } from "./support/validation-trace-dispute-one-step-context-drive.js";

/** 40 policies, the last minting 216 assets and each other one 257 (the
 * empty name and every one-byte one): 10,239 assets, whose frontier is 13
 * deep with 12 peaks. A frontier 14 deep, or 13 deep with more peaks, needs
 * 11,263 assets or more, past the mint field's bound. */
const MINT = {
  policies: 40,
  assets: 257,
  lastAssets: 216,
  shortNames: true,
} as const;
const ASSETS = 39 * 257 + 216;
const DEPTH = 13;
/** The step opening the last leaf of the first peak, whose proof carries
 * the peak's depth in siblings: the mint fold opens leaf 8,191 at cursor
 * 2,047, past the first item step at 33,206. */
const FROM = 33_206 + 2_047;
/** The operator's last agreed state: the first CEK compute step's. */
const OPERATOR_FROM = 43_496;

/** The ledger's transaction size limit, and the reserve a move keeps under
 * it. */
const MAX_TX_SIZE = 16_384;
const SIZE_RESERVE = 512;

const LABELS = [
  "EnterResolution",
  "PrepareResolution",
  "PrepareSelected context",
  "ContextBound",
  "ContextStaged",
  "ContextVerified mintItem",
  "ContextSettle",
] as const;

const drive = contextStageDrive("mintItem", FROM, {
  failingNativeMint: MINT,
  operatorFrom: OPERATOR_FROM,
});

describe("validation-trace dispute runner: driving a thread at a CEK context step of the mint item stage, at the deepest asset of the deepest mint", () => {
  beforeAll(drive.setUp, 3_600_000);

  it("mints a frontier 13 deep with 12 peaks within the field bound, which the next such mint passes", () => {
    expect(ASSETS.toString(2)).toBe("10011111111111");
    expect(nativeMintPreimageCbor(MINT).length).toBeLessThanOrEqual(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
    // 44 policies of 257, the last 212: 11,263 assets.
    expect(
      nativeMintPreimageCbor({ ...MINT, policies: 44, lastAssets: 212 }).length,
    ).toBeGreaterThan(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES);
  });

  it("routes the step to the mint item stage, its asset proven by the first peak's depth in siblings", async () => {
    await drive.routes();
    const plan = await drive.plan();
    const item = Data.from(plan.auxiliaryCbor) as Constr<Data>;
    expect(item.index).toBe(16);
    const [, , quantity, siblings] = item.fields;
    expect(quantity).toBe(1n);
    expect(siblings).toHaveLength(DEPTH);
  }, 1_800_000);

  it("moves the thread through the context step's resolution to the award, each move within the transaction size limit less its reserve", async () => {
    await drive.awards();
    for (const label of LABELS)
      expect(drive.bytesOf(label)).toBeLessThanOrEqual(
        MAX_TX_SIZE - SIZE_RESERVE,
      );
  }, 3_600_000);

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
