import { createDaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import JSONBig from "json-bigint";

import { openAcceptanceNativeSession } from "../../src/devnet-stack/acceptance-native-session.js";

/** What the local node says about one exact signed transaction. */
export type SignedTransactionRecovery = Readonly<{
  transactionHash: string;
  signedTransactionCborHex: string;
  status: "included" | "expired" | "invalidated" | "rebroadcast" | "pending";
  reason: string;
}>;

/** The local node could not answer; nothing about the transaction is settled. */
export class SignedTransactionRecoveryUnavailableError extends Error {
  override name = "SignedTransactionRecoveryUnavailableError";
}

const json = JSONBig({ useNativeBigInt: true, strict: true });
const outRef = (txHash: string, index: bigint | number) => `${txHash}#${index}`;

/** The exact identity, inputs, outputs and expiry of recorded signed bytes. */
export const signedTransactionFacts = (
  transactionHash: string,
  signedTransactionCborHex: string,
) => {
  const transaction = CML.Transaction.from_cbor_hex(signedTransactionCborHex);
  try {
    const body = transaction.body();
    if (CML.hash_transaction(body).to_hex() !== transactionHash)
      throw new Error("Recorded transaction bytes changed their identity");
    const inputs: string[] = [];
    for (let index = 0; index < body.inputs().len(); index++) {
      const input = body.inputs().get(index);
      inputs.push(outRef(input.transaction_id().to_hex(), input.index()));
    }
    const outputs = Array.from({ length: body.outputs().len() }, (_, index) =>
      outRef(transactionHash, index),
    );
    return { inputs, outputs, ttl: body.ttl() };
  } finally {
    transaction.free();
  }
};

/**
 * Classifies recorded signed bytes from one local-node ledger read.
 *
 * - `included`: one of its outputs is in the ledger, or `includedThrough`
 *   saw it.
 * - `expired`: every input is unspent at a ledger state whose tip had already
 *   reached the transaction's TTL, so no block can include it any more.
 * - `rebroadcast`: every input is unspent and the TTL (if any) is still ahead.
 * - `invalidated`: an input is spent and `includedThrough` proves the
 *   transaction was not included through the ledger read.
 * - `pending`: anything that cannot yet be settled, for example a spent input
 *   without an inclusion oracle, or an oracle still behind the ledger read.
 *
 * Tips are read before and after the UTxO query, so the expiry check uses a
 * tip at or before the ledger state and the inclusion check one at or after
 * it.
 */
export const classifySignedTransaction = (input: {
  facts: ReturnType<typeof signedTransactionFacts>;
  present: ReadonlySet<string>;
  tipSlotBefore: number;
  tipSlotAfter: number;
  includedThrough?: (slot: number) => boolean | undefined;
}): Pick<SignedTransactionRecovery, "status" | "reason"> => {
  const { facts, present } = input;
  if (facts.outputs.some((ref) => present.has(ref)))
    return { status: "included", reason: "an output is in the node ledger" };
  const seen = input.includedThrough?.(input.tipSlotAfter);
  if (seen === true)
    return { status: "included", reason: "the chain recorder saw it" };
  if (facts.inputs.every((ref) => present.has(ref))) {
    if (facts.ttl !== undefined && BigInt(input.tipSlotBefore) >= facts.ttl)
      return {
        status: "expired",
        reason: `tip slot ${input.tipSlotBefore} reached TTL ${facts.ttl} with every input unspent`,
      };
    return {
      status: "rebroadcast",
      reason: "absent with every input unspent and the TTL ahead",
    };
  }
  if (seen === false)
    return {
      status: "invalidated",
      reason: `an input is spent and the recorder never saw it through slot ${input.tipSlotAfter}`,
    };
  return {
    status: "pending",
    reason:
      seen === undefined && input.includedThrough !== undefined
        ? "the chain recorder is behind the ledger read"
        : "an input is spent and inclusion cannot be settled",
  };
};

const tipSlot = (frame: string) => {
  const tip = (json.parse(frame) as { result?: { tip?: unknown } }).result?.tip;
  if (tip === "origin") return 0;
  const slot = (tip as { slot?: unknown } | undefined)?.slot;
  if (typeof slot !== "number" || !Number.isSafeInteger(slot) || slot < 0)
    throw new Error("Ogmios tip has no safe slot");
  return slot;
};

/** One bounded read of the local node's ledger for recorded signed bytes. */
export const readSignedTransactionRecovery = async (input: {
  ogmiosUrl: string;
  timeoutMs: number;
  transactionHash: string;
  signedTransactionCborHex: string;
  includedThrough?: (txHash: string, slot: number) => boolean | undefined;
}): Promise<SignedTransactionRecovery> => {
  const facts = signedTransactionFacts(
    input.transactionHash,
    input.signedTransactionCborHex,
  );
  const outputReferences = [...facts.inputs, ...facts.outputs].map((ref) => {
    const [id, index] = ref.split("#");
    return { transaction: { id: id! }, index: Number(index) };
  });
  const scope = createDaAvailabilityReadScope({
    deadlineEpochMs: Date.now() + input.timeoutMs,
    attemptTimeoutMs: input.timeoutMs,
  });
  let ledger: {
    present: Set<string>;
    tipSlotBefore: number;
    tipSlotAfter: number;
  };
  try {
    const session = await openAcceptanceNativeSession({
      endpoint: input.ogmiosUrl,
      scope,
      parseJson: (text) => json.parse(text),
    });
    try {
      const tip = async () =>
        tipSlot(
          await session.request("findIntersection", { points: ["origin"] }),
        );
      const tipSlotBefore = await tip();
      const rows = (
        json.parse(
          await session.request("queryLedgerState/utxo", { outputReferences }),
        ) as { result: { transaction: { id: string }; index: number }[] }
      ).result;
      ledger = {
        present: new Set(
          rows.map((row) => outRef(row.transaction.id, row.index)),
        ),
        tipSlotBefore,
        tipSlotAfter: await tip(),
      };
    } finally {
      await session.close();
    }
  } catch (cause) {
    throw new SignedTransactionRecoveryUnavailableError(
      "The local node could not answer the transaction recovery read",
      { cause },
    );
  } finally {
    scope.close();
  }
  const { includedThrough } = input;
  return {
    transactionHash: input.transactionHash,
    signedTransactionCborHex: input.signedTransactionCborHex,
    ...classifySignedTransaction({
      facts,
      ...ledger,
      ...(includedThrough === undefined
        ? {}
        : {
            includedThrough: (slot: number) =>
              includedThrough(input.transactionHash, slot),
          }),
    }),
  };
};
