import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import test from "node:test";

import {
  createCanonicalCorpusPrefixSelector,
  validateCanonicalCorpusVerificationEvidence,
} from "./mpf-architecture-g-corpus.mjs";

const h32 = (value) => value.toString(16).padStart(2, "0").repeat(32);
const row = ({ chain, index, parent, input, slice = "large" }) => {
  const cbor = Buffer.from([chain, index]);
  const txHash = h32(chain * 16 + index);
  return {
    txHash,
    canonicalCborHex: cbor.toString("hex"),
    canonicalCborSha256: createHash("sha256").update(cbor).digest("hex"),
    canonicalCborByteLength: cbor.length,
    senderWalletId: `wallet-${chain.toString()}`,
    selectedInputOutref: input,
    outputOutrefs: [`${txHash}#0`, `${txHash}#1`],
    planShape: "chain",
    parentTxHash: parent,
    corpusSliceId: slice,
  };
};

const chain = (id, length) => {
  const rows = [];
  for (let index = 1; index <= length; index += 1) {
    const parent = index === 1 ? null : rows.at(-1).txHash;
    rows.push(
      row({
        chain: id,
        index,
        parent,
        input: parent === null ? `funding-${id.toString()}#0` : `${parent}#1`,
      }),
    );
  }
  return rows;
};

const select = (rows, transactionCount) => {
  const selector = createCanonicalCorpusPrefixSelector({
    corpusSliceId: "large",
    transactionCount,
  });
  rows.forEach((value, index) =>
    selector.consider({
      line: JSON.stringify(value),
      row: value,
      corpusRowNumber: index + 11,
    }),
  );
  return selector.finish();
};

test("selects full chains plus a dependency-closed prefix", () => {
  const result = select([...chain(1, 2), ...chain(2, 3)], 4);
  assert.equal(result.selectedRowCount, 4);
  assert.equal(result.completeChainCount, 1);
  assert.equal(result.finalChainPrefixLength, 2);
  assert.deepEqual(result.sourceCorpusRowRange, { start: 11, end: 14 });
  assert.deepEqual(result.sourceSliceOrdinalRange, { start: 1, end: 4 });
  assert.deepEqual(result.fundingRootOutrefs, ["funding-1#0", "funding-2#0"]);
  assert.deepEqual(result.fundingRoots, [
    { walletId: "wallet-1", outref: "funding-1#0" },
    { walletId: "wallet-2", outref: "funding-2#0" },
  ]);
  assert.match(result.fundingRootsSha256, /^[0-9a-f]{64}$/);
});

test("recognizes an exact complete-chain boundary", () => {
  const result = select([...chain(1, 2), ...chain(2, 2)], 2);
  assert.equal(result.completeChainCount, 1);
  assert.equal(result.finalChainPrefixLength, 0);
});

test("rejects a selected child whose parent output is not consumed", () => {
  const rows = chain(1, 2);
  rows[1] = { ...rows[1], selectedInputOutref: `${rows[0].txHash}#9` };
  assert.throws(
    () => select(rows, 2),
    /does not spend a declared parent output/,
  );
});

test("rejects a chain that reappears after another chain", () => {
  const first = chain(1, 1)[0];
  const second = chain(2, 1)[0];
  const reappeared = row({
    chain: 1,
    index: 2,
    parent: first.txHash,
    input: `${first.txHash}#1`,
  });
  assert.throws(
    () => select([first, second, reappeared], 3),
    /reappeared after another chain/,
  );
});

test("rejects a child that cross-links to another wallet chain", () => {
  const first = chain(1, 1)[0];
  const secondRoot = chain(2, 1)[0];
  const crossLinked = row({
    chain: 2,
    index: 2,
    parent: first.txHash,
    input: `${first.txHash}#1`,
  });
  assert.throws(
    () => select([first, secondRoot, crossLinked], 3),
    /cross-links parent .* from another wallet chain/,
  );
});

test("rejects a wallet chain split across corpus slices", () => {
  const rows = chain(1, 2);
  rows[1] = { ...rows[1], corpusSliceId: "other" };
  assert.throws(
    () => select(rows, 1),
    /crosses slice boundaries \(large, other\)/,
  );
});

test("rejects a fork that skips the immediate chain predecessor", () => {
  const rows = chain(1, 3);
  rows[2] = {
    ...rows[2],
    parentTxHash: rows[0].txHash,
    selectedInputOutref: `${rows[0].txHash}#1`,
  };
  assert.throws(() => select(rows, 3), /is not the immediate predecessor/);
});

test("validates dependency continuity after the selected prefix", () => {
  const rows = chain(1, 3);
  rows[2] = {
    ...rows[2],
    selectedInputOutref: `${rows[0].txHash}#1`,
  };
  assert.throws(
    () => select(rows, 1),
    /does not spend a declared parent output/u,
  );
});

test("verification evidence binds corpus, index, counts, and rebuild sample", () => {
  const evidence = {
    schemaVersion: "midgard-stress-corpus-generation-v1",
    verified: {
      corpusSha256: "11".repeat(32),
      indexSha256: "22".repeat(32),
      rowCount: 50,
      chainCount: 5,
      rebuildSample: {
        algorithm: "sha256-corpus-chain-id-order-v1",
        sampleRate: 0.2,
        checkedChainCount: 1,
        checkedRowCount: 5,
        sampledChainIds: ["wallet-a"],
      },
    },
  };
  assert.equal(
    validateCanonicalCorpusVerificationEvidence({
      artifact: evidence,
      corpusSha256: "11".repeat(32),
      indexSha256: "22".repeat(32),
      rowCount: 50,
      chainCount: 5,
    }),
    evidence.verified,
  );
  assert.throws(() =>
    validateCanonicalCorpusVerificationEvidence({
      artifact: {
        schemaVersion: "midgard-stress-corpus-verification-v1",
        ...evidence.verified,
      },
      corpusSha256: "11".repeat(32),
      indexSha256: "22".repeat(32),
      rowCount: 50,
      chainCount: 5,
    }),
  );
  for (const override of [
    { corpusSha256: "33".repeat(32) },
    { indexSha256: "33".repeat(32) },
    { rowCount: 49 },
    { chainCount: 4 },
  ]) {
    assert.throws(() =>
      validateCanonicalCorpusVerificationEvidence({
        artifact: evidence,
        corpusSha256: "11".repeat(32),
        indexSha256: "22".repeat(32),
        rowCount: 50,
        chainCount: 5,
        ...override,
      }),
    );
  }
  assert.throws(() =>
    validateCanonicalCorpusVerificationEvidence({
      artifact: {
        ...evidence,
        verified: { ...evidence.verified, rebuildSample: undefined },
      },
      corpusSha256: "11".repeat(32),
      indexSha256: "22".repeat(32),
      rowCount: 50,
      chainCount: 5,
    }),
  );
  for (const rebuildSample of [
    {},
    { ...evidence.verified.rebuildSample, algorithm: "unrecognized" },
    { ...evidence.verified.rebuildSample, checkedChainCount: 2 },
    {
      ...evidence.verified.rebuildSample,
      checkedChainCount: 2,
      sampledChainIds: ["wallet-a", "wallet-a"],
    },
  ]) {
    assert.throws(() =>
      validateCanonicalCorpusVerificationEvidence({
        artifact: {
          ...evidence,
          verified: { ...evidence.verified, rebuildSample },
        },
        corpusSha256: "11".repeat(32),
        indexSha256: "22".repeat(32),
        rowCount: 50,
        chainCount: 5,
      }),
    );
  }
});
