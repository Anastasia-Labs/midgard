import { createHash } from "node:crypto";

import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { HistoryChainTip } from "../src/l1-event-history-chain.js";
import {
  type BoundHistoryChainBlock,
  eventHistoryCanonicalJson,
} from "../src/l1-event-history-source.js";
import type { HistoryChainTransaction } from "../src/l1-event-history-transaction.js";
import {
  evaluateSignedIntentCoverage,
  type SignedIntentCoverageBlock,
} from "../src/services/signed-intent-canonical-coverage.js";

const hash = (n: number) => n.toString(16).padStart(64, "0");
const sha256 = (value: string | Buffer) =>
  createHash("sha256").update(value).digest("hex");

// Genuine CML body/signature construction; the block rosters are explicit
// source models, not emulator acceptance, chain authority or ledger validity.
const signed = (
  ttl: bigint | null = 20n,
  validityStart: bigint | null = 10n,
  fee = 200_000n,
) => {
  const key = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 7));
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(key.to_public().hash()),
  ).to_address();
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(hash(1)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(address, CML.Value.from_coin(5_000_000n)),
  );
  const body = CML.TransactionBody.new(inputs, outputs, fee);
  if (ttl !== null) body.set_ttl(ttl);
  if (validityStart !== null) body.set_validity_interval_start(validityStart);
  const bodyHash = CML.hash_transaction(body);
  const witnesses = CML.TransactionWitnessSet.new();
  const keys = CML.VkeywitnessList.new();
  const witness = CML.make_vkey_witness(bodyHash, key);
  expect(
    key
      .to_public()
      .verify(bodyHash.to_raw_bytes(), witness.ed25519_signature()),
  ).toBe(true);
  keys.add(witness);
  witnesses.set_vkeywitnesses(keys);
  return {
    signedTxCbor: CML.Transaction.new(body, witnesses, true).to_cbor_hex(),
    expectedTxHash: bodyHash.to_hex(),
  };
};
const transaction = (
  txHash: string,
  spends: HistoryChainTransaction["spends"] = "inputs",
): HistoryChainTransaction => ({
  txHash,
  spends,
  inputs: [],
  references: [],
  collaterals: [],
  outputs: [],
  mint: {},
  withdrawals: [],
  redeemers: [],
});
const chain = (
  slots: readonly number[] = [10, 20, 30, 40],
): BoundHistoryChainBlock[] =>
  slots.map((slot, i) => ({
    point: { id: hash(100 + i), slot, height: 50 + i },
    parent: hash(99 + i),
    transactions: [transaction(hash(200 + i))],
  }));
const input = (
  blocks = chain(),
  tx = signed(),
): Parameters<typeof evaluateSignedIntentCoverage>[0] => ({
  ...tx,
  bindingDigest: hash(300),
  manifestId: hash(301),
  start: blocks[0]!.point,
  head: blocks.at(-1)!.point,
  blocks,
  requiredFinalityDepth: 2,
});

describe("signed intent canonical coverage supplied-evidence classifier", () => {
  it("accepts the strict minimal retained roster including genuinely empty blocks", () => {
    const blocks: readonly SignedIntentCoverageBlock[] = chain().map(
      ({ point, parent }) => ({ point, parent, transactions: [] }),
    );
    expect(evaluateSignedIntentCoverage({ ...input(), blocks }).kind).toBe(
      "covered_absent",
    );
  });

  it("requires two descendants after the first TTL crossing and binds the complete roster", () => {
    const value = input();
    const result = evaluateSignedIntentCoverage(value);
    expect(result).toMatchObject({
      kind: "covered_absent",
      bodyHash: value.expectedTxHash,
      ttl: 20n,
      bindingDigest: value.bindingDigest,
      manifestId: value.manifestId,
      start: value.start,
      head: value.head,
      expiryCrossing: value.blocks[1]!.point,
      descendantDepth: 2,
    });
    expect(result.evidenceDigest).toBe(
      sha256(
        eventHistoryCanonicalJson({
          domain: "midgard-signed-intent-canonical-coverage-v1",
          bodyHash: value.expectedTxHash,
          signedCborSha256: sha256(Buffer.from(value.signedTxCbor, "hex")),
          bindingDigest: value.bindingDigest,
          manifestId: value.manifestId,
          ttl: 20n,
          validityStart: 10n,
          authenticatedPreSigningSlot: null,
          coverageStartBoundary: { source: "signed_validity_start", slot: 10n },
          start: value.start,
          head: value.head,
          requiredFinalityDepth: 2,
          blocks: value.blocks.map((block) => ({
            point: block.point,
            parent: block.parent,
            transactions: block.transactions.map(({ txHash, spends }) => ({
              txHash,
              spends,
            })),
          })),
        }),
      ),
    );
  });

  it.each([
    [[10, 19], "before_expiry", 0],
    [[10, 20], "insufficient_depth", 0],
    [[10, 21, 100], "insufficient_depth", 1],
  ] as const)("retains pending for slots %j", (slots, reason, depth) => {
    expect(evaluateSignedIntentCoverage(input(chain(slots)))).toMatchObject({
      kind: "pending",
      reason,
      descendantDepth: depth,
      expiryCrossing:
        reason === "before_expiry" ? null : chain(slots)[1]!.point,
    });
  });

  it("does not count a long slot gap as more descendant blocks", () => {
    expect(
      evaluateSignedIntentCoverage(input(chain([10, 20, 1_000_000]))).kind,
    ).toBe("pending");
  });

  it("handles TTL zero and TTL beyond safe JavaScript slots without rounding", () => {
    expect(
      evaluateSignedIntentCoverage(input(chain([0, 1, 2]), signed(0n, 0n))),
    ).toMatchObject({ kind: "covered_absent", descendantDepth: 2 });
    const ttl = BigInt(Number.MAX_SAFE_INTEGER) + 1n;
    expect(
      evaluateSignedIntentCoverage(
        input(
          chain([Number.MAX_SAFE_INTEGER - 1, Number.MAX_SAFE_INTEGER]),
          signed(ttl, BigInt(Number.MAX_SAFE_INTEGER - 1)),
        ),
      ),
    ).toMatchObject({ kind: "pending", reason: "before_expiry", ttl });
  });

  it("rejects a contiguous mature suffix that omits the signed lower-bound prefix", () => {
    const blocks = chain().slice(1);
    expect(() => evaluateSignedIntentCoverage(input(blocks))).toThrow(
      "earliest-inclusion boundary",
    );
    // Even a later independently supplied signing point cannot relax the
    // explicit signed validity-start rule.
    expect(() =>
      evaluateSignedIntentCoverage({
        ...input(blocks),
        authenticatedPreSigningSlot: 40,
      }),
    ).toThrow("earliest-inclusion boundary");
  });

  it("fails closed without a signed lower bound or authenticated historical signing point", () => {
    expect(() =>
      evaluateSignedIntentCoverage(input(chain(), signed(20n, null))),
    ).toThrow(
      "signed validity start or authenticated pre-signing slot is required",
    );
  });

  it("accepts an explicit pre-signing boundary and refuses an omitted prefix without a signed lower bound", () => {
    const value = input(chain(), signed(20n, null));
    expect(
      evaluateSignedIntentCoverage({
        ...value,
        authenticatedPreSigningSlot: 10,
      }).kind,
    ).toBe("covered_absent");
    const blocks = chain().slice(1);
    expect(() =>
      evaluateSignedIntentCoverage({
        ...input(blocks, signed(20n, null)),
        authenticatedPreSigningSlot: 10,
      }),
    ).toThrow("earliest-inclusion boundary");
    expect(() =>
      evaluateSignedIntentCoverage({
        ...value,
        authenticatedPreSigningSlot: 0,
      }),
    ).toThrow("earliest-inclusion boundary");
  });

  it.each([-1, 0.5, Infinity, NaN, Number.MAX_SAFE_INTEGER + 1])(
    "rejects invalid authenticated pre-signing slot %s even with a signed boundary",
    (authenticatedPreSigningSlot) => {
      expect(() =>
        evaluateSignedIntentCoverage({
          ...input(),
          authenticatedPreSigningSlot,
        }),
      ).toThrow("pre-signing slot must be a safe natural number");
    },
  );

  it("binds the selected boundary provenance and explicitly supplied historical evidence", () => {
    const value = input();
    const bare = evaluateSignedIntentCoverage(value);
    const supplied = evaluateSignedIntentCoverage({
      ...value,
      authenticatedPreSigningSlot: 12,
    });
    expect(supplied.kind).toBe(bare.kind);
    expect(supplied.evidenceDigest).not.toBe(bare.evidenceDigest);
    const noSignedStart = input(chain(), signed(20n, null));
    const first = evaluateSignedIntentCoverage({
      ...noSignedStart,
      authenticatedPreSigningSlot: 10,
    });
    const later = evaluateSignedIntentCoverage({
      ...noSignedStart,
      authenticatedPreSigningSlot: 12,
    });
    expect(first.kind).toBe("covered_absent");
    expect(later.kind).toBe(first.kind);
    expect(first.evidenceDigest).not.toBe(later.evidenceDigest);
  });

  it.each([
    [[10, 19], "pending"],
    [[10, 20, 30], "pending"],
    [[10, 20, 30, 40], "covered_absent"],
  ] as const)(
    "does not substitute a competing spend for complete expiry coverage (%j)",
    (slots, kind) => {
      const intent = signed();
      const competing = signed(20n, 10n, 201_000n);
      const originalInput = CML.Transaction.from_cbor_hex(intent.signedTxCbor)
        .body()
        .inputs()
        .get(0);
      const competingInput = CML.Transaction.from_cbor_hex(
        competing.signedTxCbor,
      )
        .body()
        .inputs()
        .get(0);
      expect(competingInput.to_cbor_hex()).toBe(originalInput.to_cbor_hex());
      expect(competing.expectedTxHash).not.toBe(intent.expectedTxHash);
      const blocks = chain(slots);
      blocks[0] = {
        ...blocks[0]!,
        transactions: [transaction(competing.expectedTxHash)],
      };
      expect(evaluateSignedIntentCoverage(input(blocks, intent)).kind).toBe(
        kind,
      );
    },
  );

  it.each(["inputs", "collaterals"] as const)(
    "finds historical inclusion despite later queue removal (%s)",
    (spends) => {
      const tx = signed();
      const blocks = chain();
      blocks[0] = {
        ...blocks[0]!,
        transactions: [
          transaction(hash(900)),
          transaction(tx.expectedTxHash, spends),
        ],
      };
      // Later modeled blocks do not repeat the transaction. No current queue
      // output is consulted, so merge/removal cannot turn inclusion into absence.
      expect(evaluateSignedIntentCoverage(input(blocks, tx))).toMatchObject({
        kind: "included",
        point: blocks[0]!.point,
        transactionIndex: 1,
      });
    },
  );

  it("validates the suffix even after finding the signed transaction", () => {
    const tx = signed();
    const blocks = chain();
    blocks[0] = {
      ...blocks[0]!,
      transactions: [transaction(tx.expectedTxHash)],
    };
    blocks[3] = { ...blocks[3]!, parent: hash(999) };
    expect(() => evaluateSignedIntentCoverage(input(blocks, tx))).toThrow(
      "contiguous parent-linked range",
    );
  });

  it("rejects absent TTL, changed signed identity and malformed full CBOR", () => {
    expect(() =>
      evaluateSignedIntentCoverage(input(chain(), signed(null))),
    ).toThrow("finite TTL");
    expect(() =>
      evaluateSignedIntentCoverage({ ...input(), expectedTxHash: hash(999) }),
    ).toThrow("body hash differs");
    expect(() =>
      evaluateSignedIntentCoverage({ ...input(), signedTxCbor: "ff" }),
    ).toThrow();
    const trailing = input();
    expect(() =>
      evaluateSignedIntentCoverage({
        ...trailing,
        signedTxCbor: trailing.signedTxCbor + "00",
      }),
    ).toThrow();
  });

  it.each([0, -1, 1.5, Infinity, Number.MAX_SAFE_INTEGER + 1])(
    "rejects invalid finality depth %s",
    (requiredFinalityDepth) => {
      expect(() =>
        evaluateSignedIntentCoverage({ ...input(), requiredFinalityDepth }),
      ).toThrow("positive safe integer");
    },
  );

  it.each(["expectedTxHash", "bindingDigest", "manifestId"] as const)(
    "requires exact lowercase digest %s",
    (name) => {
      expect(() =>
        evaluateSignedIntentCoverage({ ...input(), [name]: "AA".repeat(32) }),
      ).toThrow("lowercase 32-byte hash");
    },
  );

  it("rejects missing endpoints, missing interior blocks and forks", () => {
    const value = input();
    for (const blocks of [
      [],
      value.blocks.slice(1),
      value.blocks.slice(0, -1),
      [value.blocks[0]!, ...value.blocks.slice(2)],
    ]) {
      expect(() =>
        evaluateSignedIntentCoverage({ ...value, blocks }),
      ).toThrow();
    }
    expect(() =>
      evaluateSignedIntentCoverage({
        ...value,
        head: { ...value.head, id: hash(990) },
      }),
    ).toThrow("exact start and head");
    expect(() =>
      evaluateSignedIntentCoverage({
        ...value,
        start: { ...value.start, height: 0 },
      }),
    ).toThrow("exact start and head");
  });

  it.each(["slot", "height"] as const)(
    "rejects unsafe/negative/noninteger block %s",
    (field) => {
      for (const n of [-1, 0.5, Number.MAX_SAFE_INTEGER + 1, Infinity]) {
        const blocks = chain();
        blocks[1] = {
          ...blocks[1]!,
          point: { ...blocks[1]!.point, [field]: n },
        };
        expect(() => evaluateSignedIntentCoverage(input(blocks))).toThrow(
          "safe natural numbers",
        );
      }
    },
  );

  it("rejects repeated/decreasing slots and skipped/repeated heights", () => {
    for (const change of [
      { slot: 10 },
      { slot: 9 },
      { height: 50 },
      { height: 52 },
    ]) {
      const blocks = chain();
      blocks[1] = { ...blocks[1]!, point: { ...blocks[1]!.point, ...change } };
      expect(() => evaluateSignedIntentCoverage(input(blocks))).toThrow(
        "contiguous parent-linked range",
      );
    }
  });

  it("rejects duplicate block identities, cyclic first parent and malformed roster hashes", () => {
    const blocks = chain();
    blocks[1] = {
      ...blocks[1]!,
      point: { ...blocks[1]!.point, id: blocks[0]!.point.id },
    };
    expect(() => evaluateSignedIntentCoverage(input(blocks))).toThrow(
      "duplicate block id",
    );
    const cycle = chain();
    cycle[0] = { ...cycle[0]!, parent: cycle.at(-1)!.point.id };
    expect(() => evaluateSignedIntentCoverage(input(cycle))).toThrow(
      "first parent",
    );
    const malformed = chain();
    malformed[0] = { ...malformed[0]!, transactions: [transaction("ab")] };
    expect(() => evaluateSignedIntentCoverage(input(malformed))).toThrow(
      "roster transaction id",
    );
  });

  it.each([false, true])(
    "rejects duplicate transactions within/across blocks (%s)",
    (acrossBlocks) => {
      const blocks = chain();
      const duplicate = transaction(
        blocks[0]!.transactions[0]!.txHash,
        "collaterals",
      );
      const index = acrossBlocks ? 1 : 0;
      blocks[index] = {
        ...blocks[index]!,
        transactions: [...blocks[index]!.transactions, duplicate],
      };
      expect(() => evaluateSignedIntentCoverage(input(blocks))).toThrow(
        "duplicate transaction id",
      );
    },
  );

  it("binds roster order/disposition and returns detached immutable points", () => {
    const value = input();
    const first = evaluateSignedIntentCoverage(value);
    const blocks = chain();
    blocks[0] = {
      ...blocks[0]!,
      transactions: [
        transaction(blocks[0]!.transactions[0]!.txHash, "collaterals"),
      ],
    };
    expect(
      evaluateSignedIntentCoverage({ ...value, blocks }).evidenceDigest,
    ).not.toBe(first.evidenceDigest);
    const orderedBlocks = chain();
    const transactions = [transaction(hash(900)), transaction(hash(901))];
    orderedBlocks[0] = { ...orderedBlocks[0]!, transactions };
    const orderedResult = evaluateSignedIntentCoverage({
      ...value,
      blocks: orderedBlocks,
    });
    orderedBlocks[0] = {
      ...orderedBlocks[0]!,
      transactions: [...transactions].reverse(),
    };
    expect(
      evaluateSignedIntentCoverage({ ...value, blocks: orderedBlocks })
        .evidenceDigest,
    ).not.toBe(orderedResult.evidenceDigest);
    expect(
      evaluateSignedIntentCoverage({ ...value, bindingDigest: hash(999) })
        .evidenceDigest,
    ).not.toBe(first.evidenceDigest);
    expect(
      evaluateSignedIntentCoverage({ ...value, manifestId: hash(999) })
        .evidenceDigest,
    ).not.toBe(first.evidenceDigest);
    expect(Object.isFrozen(first)).toBe(true);
    expect(Object.isFrozen(first.start)).toBe(true);
    expect(first.start).not.toBe(value.start);
    const start: HistoryChainTip = { ...value.start };
    expect(
      evaluateSignedIntentCoverage({ ...value, start }).evidenceDigest,
    ).toBe(first.evidenceDigest);
  });
});
