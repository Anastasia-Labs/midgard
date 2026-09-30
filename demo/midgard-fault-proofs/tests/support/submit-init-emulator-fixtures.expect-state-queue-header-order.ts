import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  headerHashFromStateQueueUTxO,
  type MidgardValidators,
  sortStateQueueUTxOs,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  utxosToStateQueueUTxOs,
} from "@al-ft/midgard-sdk";
import { CML, Lucid, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { nativeTxFromCoreCompact } from "./legacy-submit-emulator.js";
import { h32 } from "./submit-init-emulator-shared.js";

export const positiveNonAdaAssets = (utxo: UTxO) =>
  Object.entries(utxo.assets).filter(
    ([unit, amount]) => unit !== "lovelace" && amount > 0n,
  );

export const expectStateQueueHeaderOrder = async ({
  lucid,
  contracts,
  expectedHeaderHashes,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
  readonly expectedHeaderHashes: readonly string[];
}) => {
  const utxos = await lucid.utxosAt(contracts.stateQueue.spendingScriptAddress);
  const parsedStateQueueUtxos = await Effect.runPromise(
    utxosToStateQueueUTxOs(utxos, contracts.stateQueue.policyId),
  );
  expect(parsedStateQueueUtxos).toHaveLength(expectedHeaderHashes.length + 1);
  expect(
    parsedStateQueueUtxos.map(({ assetName }) => assetName).sort(),
  ).toEqual(
    [
      STATE_QUEUE_ROOT_ASSET_NAME,
      ...expectedHeaderHashes.map(
        (headerHash) => STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
      ),
    ].sort(),
  );

  const sortedStateQueueUtxos = await Effect.runPromise(
    Effect.succeed(parsedStateQueueUtxos).pipe(
      Effect.andThen(sortStateQueueUTxOs),
    ),
  );
  expect(sortedStateQueueUtxos).toHaveLength(parsedStateQueueUtxos.length);
  const [root, ...blocks] = sortedStateQueueUtxos;
  if (root === undefined) {
    throw new Error("Expected state-queue topology to include the root node");
  }
  expect(root.assetName).toBe(STATE_QUEUE_ROOT_ASSET_NAME);
  expect(root.datum.key).toBe("Empty");
  expect(root.datum.next).toEqual(
    expectedHeaderHashes[0] === undefined
      ? "Empty"
      : { Key: { key: expectedHeaderHashes[0] } },
  );

  const observedHeaderHashes = await Promise.all(
    blocks.map((block) =>
      Effect.runPromise(headerHashFromStateQueueUTxO(block)),
    ),
  );
  expect(observedHeaderHashes).toEqual(expectedHeaderHashes);
  expect(new Set(observedHeaderHashes).size).toBe(observedHeaderHashes.length);

  for (let index = 0; index < blocks.length; index += 1) {
    const block = blocks[index]!;
    const expectedHeaderHash = expectedHeaderHashes[index]!;
    const nextExpectedHeaderHash = expectedHeaderHashes[index + 1];
    expect(block.datum.key).toEqual({ Key: { key: expectedHeaderHash } });
    expect(block.datum.next).toEqual(
      nextExpectedHeaderHash === undefined
        ? "Empty"
        : { Key: { key: nextExpectedHeaderHash } },
    );
  }
};

export type TestOutputReference = {
  readonly transactionId: string;
  readonly outputIndex: bigint;
};

export type TransactionInclusionEntry = {
  readonly inclusion: unknown;
  readonly nativeTx: ReturnType<typeof nativeTxFromCoreCompact>;
  readonly nativeTxId: string;
  readonly spendInputCbors: readonly string[];
};

export const tx1InputsPreimage: readonly TestOutputReference[] = [
  { transactionId: h32("a1"), outputIndex: 0n },
  { transactionId: h32("a2"), outputIndex: 1n },
];

export const tx2InputsPreimage: readonly TestOutputReference[] = [
  { transactionId: h32("b1"), outputIndex: 0n },
  tx1InputsPreimage[1]!,
];

/**
 * Distinct filler spend inputs, used to drive the spend-input preimage
 * cardinality axis (finding Q1X-F6, issue #549). A filler is never the input a
 * family selects — it is what the step's authenticated preimage must
 * nevertheless re-hash, item by item, before it can select anything.
 */
export const spendInputFiller = (
  domain: number,
  index: number,
): TestOutputReference => {
  const transactionId = Buffer.alloc(32, 0x00);
  transactionId.writeUInt32BE(domain >>> 0, 0);
  transactionId.writeUInt32BE(index >>> 0, 4);
  return { transactionId: transactionId.toString("hex"), outputIndex: 0n };
};

/**
 * `selected` at the LAST position of a `cardinality`-long preimage, which is
 * the worst case for both costs this axis has: the whole collection is
 * re-hashed either way, and the selection walk is longest at the last index.
 */
export const spendInputsOfCardinality = ({
  selected,
  cardinality,
  domain,
}: {
  readonly selected: TestOutputReference;
  readonly cardinality: number;
  readonly domain: number;
}): readonly TestOutputReference[] => {
  if (!Number.isInteger(cardinality) || cardinality < 1) {
    throw new Error(
      `Spend-input cardinality must be a positive integer, got ${String(cardinality)}.`,
    );
  }
  return [
    ...Array.from({ length: cardinality - 1 }, (_unused, index) =>
      spendInputFiller(domain, index),
    ),
    selected,
  ];
};

export const outputReferenceCbor = (outRef: TestOutputReference): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(outRef.transactionId, "hex"),
    outputIndex: Number(outRef.outputIndex),
  });

export const largeFittingOutputCbor = (
  inlineDatumPayloadBytes: number = 13_600,
): Buffer =>
  encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x55)]),
    value: { lovelace: 100_000_000n, assets: new Map() },
    datum: {
      kind: "inline",
      cbor: Buffer.from(
        aikenSerialisedPlutusDataCborPreservingMapOrder(
          CML.PlutusData.new_bytes(
            Buffer.alloc(inlineDatumPayloadBytes, 0xa5),
          ).to_cbor_hex(),
        ),
        "hex",
      ),
    },
  });

export const midgardTxInput = (outRef: TestOutputReference) => ({
  tx_id: outRef.transactionId,
  output_index: outRef.outputIndex,
});

export const compactTxEntry = (
  nativeTx: MidgardNativeTxFull,
): Omit<TransactionInclusionEntry, "inclusion"> => ({
  nativeTx: nativeTxFromCoreCompact(nativeTx.compact),
  nativeTxId: computeMidgardNativeTxId(nativeTx).toString("hex"),
  spendInputCbors: decodeSpendInputCbors(nativeTx),
});

export const decodeSpendInputCbors = (
  nativeTx: MidgardNativeTxFull,
): readonly string[] =>
  decodeMidgardNativeByteListPreimage(
    nativeTx.body.spendInputsPreimageCbor,
    "test.spend_inputs",
  ).map((bytes) => Buffer.from(bytes).toString("hex"));

// ---------------------------------------------------------------------------
// Adversarial MPF membership-proof depth (GOAL_SPEC.md 9.1 output 5, axis 1)
// ---------------------------------------------------------------------------
//
// Every family's step-01 redeemer carries `tx_membership_proof`, and that proof
// is the only part of a fault proof whose size an adversary controls by
// choosing what the challenged block contains. The proof serializes one CBOR
// step per level at which the trie branches along the challenged
// transaction's hashed path, so the lever is not "more transactions" — a
// larger random block only grows the path logarithmically — but
// "transactions whose hashed paths branch at consecutive nibbles of the
// challenged transaction's own path".
//
// Two siblings per level force the on-path node to hold at least three
// children, which is what makes each step serialize as the largest shape the
// MPF proof encoding has (`branch`, carrying four 32-byte neighbour hashes)
// rather than the smaller `fork` or `leaf`. That is the worst case, not a
// typical one.
//
// Grinding is deterministic: candidate keys are a counter written into a fixed
// buffer, so the same fixture is reproduced byte for byte on any machine, in
// the same spirit as the deterministic emulator wallets.
//
// Forcing a branch at level `i` costs ~16^i digest evaluations, so this axis is
// bounded by adversary WORK, not by protocol structure. See
// `membershipProofBranchLevelsReachableWithWork`.
export const ADVERSARIAL_MEMBERSHIP_PROOF_BRANCH_LEVELS = 5;

// Marginal CBOR cost of one additional `branch` proof step. Measured, and
// re-measured by `tests/max-proof-fit-membership-depth.test.ts` rather than
// assumed: the MPF proof encoding is a definite list of fixed-shape steps, so
// the cost per level is exactly constant.
export const MPF_BRANCH_PROOF_STEP_CBOR_BYTES = 139;

// Marginal cost of one additional forced branch level in the COMPLETE SIGNED
// step-01 transaction, which is what the L1 envelope actually measures. It is
// not the MPF CBOR figure above: the proof reaches the chain as Plutus data in
// the step redeemer, and that representation is roughly twice the size of the
// library's own compact CBOR. Measured end to end by
// `submit-init-emulator-max-proof-fit.test.ts` at two real depths, not derived.
export const PROOF_TRANSACTION_BRANCH_LEVEL_BYTES = 276;

const MPF_PATH_NIBBLES = 64;

const mpfPathDigest = (key: Buffer): Buffer =>
  Buffer.from(computeHash32(Uint8Array.from(key)));

const sharedNibbleCount = (left: Buffer, right: Buffer): number => {
  const a = left.toString("hex");
  const b = right.toString("hex");
  let shared = 0;
  while (shared < a.length && a[shared] === b[shared]) {
    shared += 1;
  }
  return shared;
};

/**
 * Deterministically grind trie keys whose hashed path diverges from
 * `targetKey`'s hashed path at exactly level 0, 1, ... `branchLevels - 1`, two
 * per level. `domain` separates one family's grind from another's so two
 * fixtures in the same block cannot reuse each other's keys.
 */
export const adversarialMembershipSiblingKeys = ({
  targetKey,
  branchLevels,
  domain,
}: {
  readonly targetKey: Buffer;
  readonly branchLevels: number;
  readonly domain: number;
}): readonly Buffer[] => {
  if (branchLevels < 0 || branchLevels > MPF_PATH_NIBBLES) {
    throw new Error(
      `Adversarial branch level count ${branchLevels.toString()} is outside the 0..${MPF_PATH_NIBBLES.toString()} nibbles of an MPF path.`,
    );
  }
  const targetPath = mpfPathDigest(targetKey);
  const candidate = Buffer.alloc(32, 0x00);
  candidate.writeUInt32BE(domain >>> 0, 8);
  const keys: Buffer[] = [];
  let counter = 0;
  for (let level = 0; level < branchLevels; level += 1) {
    // Two siblings that diverge at this level are not enough on their own: if
    // they diverge into the SAME nibble slot the on-path node still holds only
    // two children and serializes as the cheaper `fork`. Requiring distinct
    // divergence nibbles makes every on-path node a three-child node, which is
    // what forces the largest `branch` step shape at every level.
    const takenNibbles = new Set<string>();
    while (takenNibbles.size < 2) {
      candidate.writeUInt32BE(counter >>> 0, 0);
      counter += 1;
      const path = mpfPathDigest(candidate);
      if (sharedNibbleCount(path, targetPath) !== level) {
        continue;
      }
      const nibble = path.toString("hex")[level]!;
      if (takenNibbles.has(nibble)) {
        continue;
      }
      takenNibbles.add(nibble);
      keys.push(Buffer.from(candidate));
    }
  }
  return keys;
};

/** Every grinded sibling carries the same one-byte filler value. */
export const ADVERSARIAL_MEMBERSHIP_SIBLING_VALUE = Buffer.from("ad", "hex");
