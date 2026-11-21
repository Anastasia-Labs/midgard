/**
 * Whether a transaction of ours that may still land stands where a
 * requirement read no output (`absentAt` of an `action_required`
 * observation). A provider that shows the mempool stops listing an output
 * once our transaction spending it is submitted, but lists the outputs that
 * transaction makes only once it is in a block: a read in between finds the
 * address empty, and a requirement inferred from that absence (no run, so
 * bind one; no certificate, so open one; no thread, so init one) would make
 * a second copy beside the one our transaction carries on. Such a
 * requirement waits until that transaction lands or is dropped.
 *
 * A transaction stands at an address when it creates an output there, or
 * spends an output there that a transaction this process holds created.
 * Only the signed bytes the in-flight set holds are read: no provider read.
 */
import { CML } from "@lucid-evolution/lucid";

import type { InFlightTransaction } from "./in-flight.js";

type Decoded = Readonly<{
  /** Each output's out-ref and address, in output order. */
  creates: readonly Readonly<{ outRef: string; address: string }>[];
}>;

const decoded = new WeakMap<InFlightTransaction, Decoded>();

/** `transaction`'s outputs, decoded once. */
const decode = (transaction: InFlightTransaction): Decoded => {
  const known = decoded.get(transaction);
  if (known !== undefined) return known;
  const tx = CML.Transaction.from_cbor_hex(
    transaction.signedTransactionCborHex,
  );
  try {
    const outputs = tx.body().outputs();
    const found = Object.freeze({
      creates: Object.freeze(
        Array.from({ length: outputs.len() }, (_, index) =>
          Object.freeze({
            outRef: `${transaction.txHash}#${index.toString()}`,
            address: outputs.get(index).address().to_bech32(),
          }),
        ),
      ),
    });
    decoded.set(transaction, found);
    return found;
  } finally {
    tx.free();
  }
};

/**
 * The first of `mayLand` that stands at an address of `absentAt`, with that
 * address; undefined when none does. `known` is every transaction this
 * process holds, whose outputs say where the outputs `mayLand` spends sit.
 */
export const inFlightAtAbsence = ({
  absentAt,
  mayLand,
  known,
}: {
  readonly absentAt: readonly string[];
  readonly mayLand: readonly InFlightTransaction[];
  readonly known: readonly InFlightTransaction[];
}):
  | Readonly<{ transaction: InFlightTransaction; address: string }>
  | undefined => {
  if (absentAt.length === 0 || mayLand.length === 0) return undefined;
  const watched = new Set(absentAt);
  const addressOf = new Map<string, string>();
  for (const transaction of [...known, ...mayLand])
    for (const { outRef, address } of decode(transaction).creates)
      addressOf.set(outRef, address);
  for (const transaction of mayLand) {
    const created = decode(transaction).creates.find(({ address }) =>
      watched.has(address),
    );
    if (created !== undefined) return { transaction, address: created.address };
    for (const outRef of transaction.inputs) {
      const address = addressOf.get(outRef);
      if (address !== undefined && watched.has(address))
        return { transaction, address };
    }
  }
  return undefined;
};
