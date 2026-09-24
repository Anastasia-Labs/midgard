import { createHash } from "node:crypto";

import { CML } from "@lucid-evolution/lucid";

import type { HistoryChainTip } from "../l1-event-history-chain.js";
import { eventHistoryCanonicalJson } from "../l1-event-history-source.js";
import type { HistoryChainTransaction } from "../l1-event-history-transaction.js";

export type SignedIntentCoverageBlock = Readonly<{
  point: HistoryChainTip;
  parent: string;
  transactions: readonly Pick<HistoryChainTransaction, "txHash" | "spends">[];
}>;

type CoverageInput = Readonly<{
  signedTxCbor: string;
  expectedTxHash: string;
  bindingDigest: string;
  manifestId: string;
  start: HistoryChainTip;
  head: HistoryChainTip;
  blocks: readonly SignedIntentCoverageBlock[];
  requiredFinalityDepth: number;
  /** Source slot observed BEFORE signing. A lower bound on signing time and
   * therefore the latest permissible coverage start when no signed validity
   * start exists. The caller must bind its historical authority; a current
   * checkpoint, wall clock or queue absence cannot establish it retroactively. */
  authenticatedPreSigningSlot?: number;
}>;

type CoverageEvidence = Readonly<{
  bodyHash: string;
  bindingDigest: string;
  manifestId: string;
  ttl: bigint;
  start: HistoryChainTip;
  head: HistoryChainTip;
  requiredFinalityDepth: number;
  evidenceDigest: string;
}>;

export type SignedIntentCoverage = CoverageEvidence &
  (
    | Readonly<{
        kind: "included";
        point: HistoryChainTip;
        transactionIndex: number;
      }>
    | Readonly<{
        kind: "pending";
        reason: "before_expiry" | "insufficient_depth";
        expiryCrossing: HistoryChainTip | null;
        descendantDepth: number;
      }>
    | Readonly<{
        kind: "covered_absent";
        expiryCrossing: HistoryChainTip;
        descendantDepth: number;
      }>
  );

const fail = (message: string): never => {
  throw new Error(`Invalid signed intent coverage: ${message}`);
};
const hash = (value: string, name: string): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/u.test(value))
    return fail(`${name} must be a lowercase 32-byte hash`);
  return value;
};
const point = (value: HistoryChainTip): HistoryChainTip => {
  if (
    !Number.isSafeInteger(value.slot) ||
    value.slot < 0 ||
    !Number.isSafeInteger(value.height) ||
    value.height < 0
  )
    return fail("block slot and height must be safe natural numbers");
  return Object.freeze({
    id: hash(value.id, "block id"),
    slot: value.slot,
    height: value.height,
  });
};
const samePoint = (left: HistoryChainTip, right: HistoryChainTip) =>
  left.id === right.id &&
  left.slot === right.slot &&
  left.height === right.height;
const sha256 = (value: string | Buffer) =>
  createHash("sha256").update(value).digest("hex");

/** Classifies supplied evidence only. This is neither L1 authority nor release
 * authorization. The caller must freshly admit the complete canonical range,
 * bind any authenticatedPreSigningSlot to source evidence observed before
 * signing, and enforce SQL/generation/deployment binding and retention. The
 * range must start no later than the signed validity start, or that historical
 * pre-signing source slot when the signed lower bound is absent. A matching
 * roster entry counts even when it spends collateral; no transaction is filtered
 * out. This does not revalidate signatures, ledger acceptance or source RPCs. */
export const evaluateSignedIntentCoverage = (
  input: CoverageInput,
): SignedIntentCoverage => {
  const expectedTxHash = hash(input.expectedTxHash, "expected transaction id");
  const bindingDigest = hash(input.bindingDigest, "binding digest");
  const manifestId = hash(input.manifestId, "manifest id");
  const start = point(input.start);
  const head = point(input.head);
  if (
    !Number.isSafeInteger(input.requiredFinalityDepth) ||
    input.requiredFinalityDepth <= 0
  )
    return fail("finality depth must be a positive safe integer");
  if (!/^(?:[0-9a-f]{2})+$/u.test(input.signedTxCbor))
    return fail("signed transaction must be nonempty lowercase CBOR hex");
  const tx = CML.Transaction.from_cbor_hex(input.signedTxCbor);
  if (tx.to_cbor_hex() !== input.signedTxCbor) {
    tx.free();
    return fail("signed CBOR must contain exactly one complete transaction");
  }
  const body = tx.body();
  const transactionHash = CML.hash_transaction(body);
  const bodyHash = transactionHash.to_hex();
  const ttl = body.ttl();
  const validityStart = body.validity_interval_start();
  transactionHash.free();
  body.free();
  tx.free();
  if (bodyHash !== expectedTxHash)
    return fail("signed body hash differs from expected transaction id");
  if (ttl === undefined) return fail("signed body requires a finite TTL");
  const authenticatedPreSigningSlot = input.authenticatedPreSigningSlot;
  if (
    authenticatedPreSigningSlot !== undefined &&
    (!Number.isSafeInteger(authenticatedPreSigningSlot) ||
      authenticatedPreSigningSlot < 0)
  )
    return fail("authenticated pre-signing slot must be a safe natural number");
  if (validityStart === undefined && authenticatedPreSigningSlot === undefined)
    return fail(
      "signed validity start or authenticated pre-signing slot is required",
    );
  const coverageStartBoundary =
    validityStart !== undefined
      ? { source: "signed_validity_start", slot: validityStart }
      : {
          source: "authenticated_pre_signing_slot",
          slot: BigInt(authenticatedPreSigningSlot!),
        };
  if (BigInt(start.slot) > coverageStartBoundary.slot)
    return fail("coverage start omits the earliest-inclusion boundary");
  if (!Array.isArray(input.blocks) || input.blocks.length === 0)
    return fail("a nonempty complete block range is required");

  const blockIds = new Set<string>();
  const txIds = new Set<string>();
  let previous: HistoryChainTip | undefined;
  let inclusion:
    | Readonly<{ point: HistoryChainTip; transactionIndex: number }>
    | undefined;
  let expiryCrossing: HistoryChainTip | undefined;
  const roster = input.blocks.map((block: SignedIntentCoverageBlock) => {
    const at = point(block.point);
    const parent = hash(block.parent, "parent id");
    if (blockIds.has(at.id)) return fail("duplicate block id");
    blockIds.add(at.id);
    if (
      previous !== undefined &&
      (parent !== previous.id ||
        at.height !== previous.height + 1 ||
        at.slot <= previous.slot)
    )
      return fail("blocks must form a contiguous parent-linked range");
    previous = at;
    if (expiryCrossing === undefined && BigInt(at.slot) >= ttl)
      expiryCrossing = at;
    const transactions = block.transactions.map((transaction, index) => {
      const txHash = hash(transaction.txHash, "roster transaction id");
      if (txIds.has(txHash)) return fail("duplicate transaction id");
      txIds.add(txHash);
      if (
        transaction.spends !== "inputs" &&
        transaction.spends !== "collaterals"
      )
        return fail(
          "transaction disposition must identify inputs or collaterals",
        );
      if (txHash === bodyHash)
        inclusion = { point: at, transactionIndex: index };
      return { txHash, spends: transaction.spends };
    });
    return { point: at, parent, transactions };
  });
  if (
    !samePoint(roster[0]!.point, start) ||
    !samePoint(roster.at(-1)!.point, head)
  )
    return fail("range endpoints must equal the exact start and head");
  if (blockIds.has(roster[0]!.parent))
    return fail("first parent must precede the supplied range");

  const common: CoverageEvidence = {
    bodyHash,
    bindingDigest,
    manifestId,
    ttl,
    start,
    head,
    requiredFinalityDepth: input.requiredFinalityDepth,
    evidenceDigest: sha256(
      eventHistoryCanonicalJson({
        domain: "midgard-signed-intent-canonical-coverage-v1",
        bodyHash,
        signedCborSha256: sha256(Buffer.from(input.signedTxCbor, "hex")),
        bindingDigest,
        manifestId,
        ttl,
        validityStart: validityStart ?? null,
        authenticatedPreSigningSlot: authenticatedPreSigningSlot ?? null,
        coverageStartBoundary,
        start,
        head,
        requiredFinalityDepth: input.requiredFinalityDepth,
        blocks: roster,
      }),
    ),
  };
  // Validate the entire range before returning inclusion; a malformed suffix
  // cannot be hidden by an early matching transaction.
  if (inclusion !== undefined)
    return Object.freeze({ ...common, kind: "included", ...inclusion });
  const descendantDepth =
    expiryCrossing === undefined ? 0 : head.height - expiryCrossing.height;
  if (
    expiryCrossing === undefined ||
    descendantDepth < input.requiredFinalityDepth
  )
    return Object.freeze({
      ...common,
      kind: "pending",
      reason:
        expiryCrossing === undefined ? "before_expiry" : "insufficient_depth",
      expiryCrossing: expiryCrossing ?? null,
      descendantDepth,
    });
  return Object.freeze({
    ...common,
    kind: "covered_absent",
    expiryCrossing,
    descendantDepth,
  });
};
