import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardForcedTxCanonical,
  encodeMidgardMintPolicyItem,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  materializeMidgardForcedTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import {
  MIDGARD_FORCED_TX_ORDER_MINT_MEMORY_BASIS,
  MIDGARD_FORCED_TX_ORDER_MINT_MEMORY_RATES as RATES,
} from "@al-ft/midgard-core/consensus-validation";
import { describe, expect, it } from "vitest";

import * as SDK from "../src/index.js";

/**
 * The P1 forced-transaction capacity limits: the SDK refuses, before signing,
 * an admissible forced transaction whose order the tx-order mint's execution
 * budget is not expected to fit, by the measured per-shape rates. Each case
 * pins the largest count of one shape it admits beside a minimal order.
 */

const output = (inlineDatum: boolean) =>
  encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x11)]),
    value: { lovelace: 2_000_000n, assets: new Map() },
    ...(inlineDatum
      ? { datum: { kind: "inline" as const, cbor: Buffer.from("00", "hex") } }
      : {}),
  });

/** A policy id unique to `index`, ascending with it. */
const policyId = (index: number) =>
  Buffer.concat([
    Buffer.alloc(26, 0x44),
    Buffer.from([index >> 8, index & 0xff]),
  ]);

/** Two-byte asset names, ascending with their index. */
const assetName = (index: number) => Buffer.from([index >> 8, index & 0xff]);

const forcedTx = ({
  outputs = [output(false)],
  mint = [] as Buffer[],
}: {
  readonly outputs?: readonly Buffer[];
  readonly mint?: readonly Buffer[];
}) =>
  encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      body: {
        spendInputsPreimageCbor: encodeCbor([
          encodeMidgardSpendInputItem({
            txId: Buffer.alloc(32, 0x33),
            outputIndex: 0,
          }),
        ]),
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: encodeCbor([...outputs]),
        fee: 0n,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
        requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        mintPreimageCbor: encodeCbor([...mint]),
        scriptIntegrityHash: EMPTY_NULL_ROOT,
        auxiliaryDataHash: EMPTY_NULL_ROOT,
        networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    }),
  );

const derive = (submittedTxCbor: Buffer) => () =>
  SDK.deriveTxOrderMaterial({ submittedTxCbor, owner: Buffer.alloc(28) });

const CAPACITY = /^Forced transaction exceeds the tx-order mint's capacity: /u;

const singleAssetPolicies = (count: number, pairHead = Buffer.from([0x82])) =>
  Array.from({ length: count }, (_, index) => {
    const item = encodeMidgardMintPolicyItem({
      policyId: policyId(index),
      assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
    });
    return Buffer.concat([pairHead, item.subarray(1)]);
  });

const onePolicyOfAssets = (
  count: number,
  quantity = 1n,
  nameBytes = 2,
  pairHead = "82",
) => {
  const item = encodeMidgardMintPolicyItem({
    policyId: policyId(0),
    assets: Array.from({ length: count }, (_, index) => ({
      assetName: Buffer.concat([
        Buffer.alloc(nameBytes - 2, 0x44),
        assetName(index),
      ]),
      quantity,
    })),
  });
  return [Buffer.concat([Buffer.from(pairHead, "hex"), item.subarray(1)])];
};

const outputsWithData = (count: number) =>
  Array.from({ length: count }, () => output(true));

const outputsWithoutData = (count: number) =>
  Array.from({ length: count }, () => output(false));

const minimalMemory = RATES.base + RATES.fixedItem;
const memoryWithOneOutput =
  minimalMemory + RATES.variableItem + RATES.twoEntryOutput;
const countAtBasis = (fixed: number, perItem: number) =>
  Math.floor((MIDGARD_FORCED_TX_ORDER_MINT_MEMORY_BASIS - fixed) / perItem);

const assetLimitAtBasis = (
  quantityExtra: number,
  nameBytes = 2,
  pairHeadExtra = 0,
) =>
  1 +
  countAtBasis(
    memoryWithOneOutput +
      RATES.variableItem +
      RATES.mintPolicy +
      RATES.mintFirstAssetName +
      quantityExtra +
      pairHeadExtra,
    RATES.mintAsset +
      quantityExtra +
      (nameBytes >= 24 ? RATES.mintLongAssetName : 0),
  );

describe("forced-transaction capacity limits", () => {
  it.each([
    [
      "single-asset mint policies",
      121,
      countAtBasis(memoryWithOneOutput, RATES.variableItem + RATES.mintPolicy),
      (n: number) => forcedTx({ mint: singleAssetPolicies(n) }),
    ],
    [
      "assets of one mint policy",
      397,
      1 +
        countAtBasis(
          memoryWithOneOutput +
            RATES.variableItem +
            RATES.mintPolicy +
            RATES.mintFirstAssetName,
          RATES.mintAsset,
        ),
      (n: number) => forcedTx({ mint: onePolicyOfAssets(n) }),
    ],
    [
      "outputs carrying a datum",
      197,
      countAtBasis(minimalMemory, RATES.variableItem + RATES.threeEntryOutput),
      (n: number) => forcedTx({ outputs: outputsWithData(n) }),
    ],
    [
      "outputs without a datum",
      287,
      countAtBasis(minimalMemory, RATES.variableItem + RATES.twoEntryOutput),
      (n: number) => forcedTx({ outputs: outputsWithoutData(n) }),
    ],
  ] as const)(
    "admits %s up to %i and refuses one more",
    (_shape, expectedLimit, limit, build) => {
      expect(limit).toBe(expectedLimit);
      expect(derive(build(limit))).not.toThrow();
      expect(derive(build(limit + 1))).toThrow(CAPACITY);
    },
  );

  it.each([
    [0xffn, 356, RATES.mintPositiveQuantity[1]],
    [0xffffn, 332, RATES.mintPositiveQuantity[2]],
    [0xffffffffn, 299, RATES.mintPositiveQuantity[3]],
    [0xffffffffffffffffn, 250, RATES.mintPositiveQuantity[4]],
    [-1n, 374, RATES.mintNegativeQuantity[0]],
    [-0x100n, 343, RATES.mintNegativeQuantity[1]],
    [-0x10000n, 319, RATES.mintNegativeQuantity[2]],
    [-0x100000000n, 288, RATES.mintNegativeQuantity[3]],
    [-0x10000000000000000n, 242, RATES.mintNegativeQuantity[4]],
  ] as const)(
    "admits quantity %s up to %i assets and refuses one more",
    (quantity, expectedLimit, extra) => {
      const limit = assetLimitAtBasis(extra);
      expect(limit).toBe(expectedLimit);
      expect(
        derive(forcedTx({ mint: onePolicyOfAssets(limit, quantity) })),
      ).not.toThrow();
      expect(
        derive(forcedTx({ mint: onePolicyOfAssets(limit + 1, quantity) })),
      ).toThrow(CAPACITY);
    },
  );

  it.each([
    [1n, "82", 381, RATES.mintPositiveQuantity[0], 0],
    [0xffffffffffffffffn, "82", 243, RATES.mintPositiveQuantity[4], 0],
    [
      -0x10000000000000000n,
      "9a00000002",
      236,
      RATES.mintNegativeQuantity[4],
      RATES.mintPairHead[3],
    ],
  ] as const)(
    "admits maximal names with quantity %s and head %s up to %i assets",
    (quantity, head, expectedLimit, extra, headExtra) => {
      const limit = assetLimitAtBasis(extra, 32, headExtra);
      expect(limit).toBe(expectedLimit);
      expect(
        derive(
          forcedTx({ mint: onePolicyOfAssets(limit, quantity, 32, head) }),
        ),
      ).not.toThrow();
      expect(
        derive(
          forcedTx({ mint: onePolicyOfAssets(limit + 1, quantity, 32, head) }),
        ),
      ).toThrow(CAPACITY);
    },
  );

  it.each([
    ["9802", 119, RATES.mintPairHead[1]],
    ["990002", 116, RATES.mintPairHead[2]],
    ["9a00000002", 112, RATES.mintPairHead[3]],
  ] as const)(
    "admits pair head %s up to %i policies and refuses one more",
    (head, expectedLimit, extra) => {
      const limit = countAtBasis(
        memoryWithOneOutput,
        RATES.variableItem + RATES.mintPolicy + extra,
      );
      expect(limit).toBe(expectedLimit);
      const pairHead = Buffer.from(head, "hex");
      expect(
        derive(forcedTx({ mint: singleAssetPolicies(limit, pairHead) })),
      ).not.toThrow();
      expect(
        derive(forcedTx({ mint: singleAssetPolicies(limit + 1, pairHead) })),
      ).toThrow(CAPACITY);
    },
  );

  it.each([0xffffffffffffffffn, -0x10000000000000000n])(
    "refuses the admitted 397-asset mint with quantity %s",
    (quantity) => {
      expect(derive(forcedTx({ mint: onePolicyOfAssets(397) }))).not.toThrow();
      expect(
        derive(forcedTx({ mint: onePolicyOfAssets(397, quantity) })),
      ).toThrow(CAPACITY);
    },
  );

  it.each(["9802", "990002", "9a00000002"])(
    "refuses 121 policies with a nonminimal pair head %s",
    (head) => {
      const pairHead = Buffer.from(head, "hex");
      expect(
        derive(forcedTx({ mint: singleAssetPolicies(1, pairHead) })),
      ).not.toThrow();
      expect(
        derive(forcedTx({ mint: singleAssetPolicies(121, pairHead) })),
      ).toThrow(CAPACITY);
    },
  );

  it("prices maximal asset-name encodings above the narrow-name frontier", () => {
    expect(derive(forcedTx({ mint: onePolicyOfAssets(397) }))).not.toThrow();
    expect(derive(forcedTx({ mint: onePolicyOfAssets(397, 1n, 32) }))).toThrow(
      CAPACITY,
    );
  });

  it("counts asset walking only when an output carries a datum", () => {
    const withAssets = (datum: boolean) =>
      encodeMidgardTxOutput({
        address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x11)]),
        value: {
          lovelace: 2_000_000n,
          assets: new Map([
            [
              policyId(0).toString("hex"),
              new Map(
                Array.from({ length: 100 }, (_, index) => [
                  Buffer.concat([
                    Buffer.alloc(30, 0x44),
                    assetName(index),
                  ]).toString("hex"),
                  0xffffffffffffffffn,
                ]),
              ),
            ],
          ]),
        },
        ...(datum
          ? {
              datum: {
                kind: "inline" as const,
                cbor: Buffer.from("00", "hex"),
              },
            }
          : {}),
      });
    const mint = singleAssetPolicies(121);
    expect(
      derive(forcedTx({ outputs: [withAssets(false)], mint })),
    ).not.toThrow();
    expect(derive(forcedTx({ outputs: [withAssets(true)], mint }))).toThrow(
      CAPACITY,
    );
  });

  it("names the estimate and the shapes in its refusal", () => {
    expect(derive(forcedTx({ mint: singleAssetPolicies(122) }))).toThrow(
      /estimated \d+ memory units, above the 13200000 basis \(1 outputs, 122 mint policies, 122 mint assets, 0 output policies, 0 output assets\)/u,
    );
  });
});
