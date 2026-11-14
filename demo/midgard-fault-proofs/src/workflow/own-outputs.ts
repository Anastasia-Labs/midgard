/**
 * Our own outputs, found by identity. An output is ours exactly when it sits
 * at an index of a transaction whose signed body we hold: the out-ref is
 * taken from that body before the transaction is submitted, journaled, and
 * read back by that out-ref alone. Nothing here searches an address for
 * matching content, so another party's output at the same address, whatever
 * it holds, is never taken for ours and never makes a step plan again.
 *
 * A transaction id is the hash of its signed body, so a rollback that
 * re-includes the transaction keeps the out-ref. One that leaves it out for
 * good (expired, or its inputs spent elsewhere) leaves the out-ref gone, and
 * the slot is published again, naming the out-ref it replaces.
 *
 * Workflows keep their claims in the workflow journal (each port's durable
 * recovery). Callers with no workflow journal (the CLI and direct step
 * paths) keep them in an in-memory `OwnOutputJournal`.
 */
import {
  coreToTxOutput,
  type OutRef,
  type TxOutput,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";

const OUT_REF = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u;

/** `txHash#index`. */
export const ownOutRefLabel = (utxo: OutRef): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The out-ref `label` names, or throws. */
export const parseOwnOutRef = (label: string): OutRef => {
  const match = OUT_REF.exec(label);
  if (match === null) throw new Error(`${label} is not an exact out-ref`);
  return { txHash: match[1]!, outputIndex: Number(match[2]) };
};

/**
 * One planned output: where it goes and what it must hold. `matches` is
 * checked against our own signed body when the output is claimed, and
 * against the chain's copy when it is read back.
 */
export type OwnOutputSlot = Readonly<{
  address: string;
  matches: (output: TxOutput) => boolean;
}>;

/** A carriage slot: an ADA-only output holding `datumCbor` inline at `address`. */
export const inlineDatumCarriageSlot = (
  address: string,
  datumCbor: string,
): OwnOutputSlot =>
  Object.freeze({
    address,
    matches: (output: TxOutput) =>
      output.address === address &&
      output.datum === datumCbor &&
      output.datumHash == null &&
      output.scriptRef == null &&
      Object.entries(output.assets).every(
        ([unit, quantity]) => unit === "lovelace" || quantity === 0n,
      ),
  });

/** A claimed output: its out-ref in our signed body, and that body's copy of it. */
export type OwnOutputClaim = Readonly<{
  outRef: string;
  output: TxOutput;
}>;

/**
 * The outputs of our signed transaction that carry `slots`, in slot order.
 * Each output index is claimed at most once, so two slots that expect the
 * same content take two outputs. Throws when a slot finds no output.
 */
export const claimOwnOutputs = ({
  signed,
  slots,
}: {
  readonly signed: TxSigned;
  readonly slots: readonly OwnOutputSlot[];
}): readonly OwnOutputClaim[] => {
  const txHash = signed.toHash().toLowerCase();
  const outputs = signed.toTransaction().body().outputs();
  const decoded: TxOutput[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    decoded.push(coreToTxOutput(outputs.get(index)));
  }
  const claimed = new Set<number>();
  return Object.freeze(
    slots.map((slot, ordinal) => {
      const index = decoded.findIndex(
        (output, at) =>
          !claimed.has(at) &&
          output.address === slot.address &&
          slot.matches(output),
      );
      if (index < 0) {
        throw new Error(
          `own output ${ordinal.toString()} is not in the signed transaction ${txHash}`,
        );
      }
      claimed.add(index);
      return Object.freeze({
        outRef: `${txHash}#${index.toString()}`,
        output: decoded[index]!,
      });
    }),
  );
};

/** Reads outputs by exact out-ref: the provider's `utxosByOutRef`, or an authenticated read. */
export type OwnOutputReader = (
  outRefs: readonly OutRef[],
) => Promise<readonly UTxO[]>;

/**
 * Our live outputs among `outRefs`, keyed by out-ref: read by out-ref alone,
 * and only those asked for.
 */
export const readOwnOutputs = async (
  read: OwnOutputReader,
  outRefs: readonly string[],
): Promise<ReadonlyMap<string, UTxO>> => {
  const asked = [...new Set(outRefs)];
  if (asked.length === 0) return new Map();
  const wanted = new Set(asked);
  const found = new Map<string, UTxO>();
  for (const utxo of await read(asked.map(parseOwnOutRef))) {
    const label = ownOutRefLabel(utxo);
    if (wanted.has(label)) found.set(label, utxo);
  }
  return found;
};

/** One of our claims on a slot, and when its transaction was confirmed. */
export type OwnOutputClaimed = Readonly<{
  outRef: string;
  /** When the confirmation was journaled (ISO time); absent off a workflow. */
  confirmedAt?: string;
}>;

/** A slot and every claim on it, oldest first: its output and replacements. */
export type OwnOutputSlotClaims = Readonly<{
  slot: OwnOutputSlot;
  claims: readonly OwnOutputClaimed[];
}>;

export type OwnOutputResolution =
  /** Every slot has a live output of ours, in slot order. */
  | Readonly<{ kind: "ready"; utxos: readonly UTxO[] }>
  /** Slot `ordinal` has none: publish it, replacing `replaces` when set. */
  | Readonly<{ kind: "publish"; ordinal: number; replaces: string | null }>
  /** Slot `ordinal`'s newest output is confirmed but not yet visible. */
  | Readonly<{ kind: "pending"; ordinal: number; outRef: string }>;

/**
 * Where a step's slots stand. A slot is ready when any claim on it is live
 * with the content the slot expects, the newest first. Otherwise its newest
 * claim, when `recent`, is taken for a read that lags the chain (pending);
 * else the slot is published again, replacing that claim. The first slot
 * that is not ready decides.
 */
export const resolveOwnOutputs = async ({
  slots,
  read,
  recent = () => false,
}: {
  readonly slots: readonly OwnOutputSlotClaims[];
  readonly read: OwnOutputReader;
  readonly recent?: (claim: OwnOutputClaimed) => boolean;
}): Promise<OwnOutputResolution> => {
  const live = await readOwnOutputs(
    read,
    slots.flatMap(({ claims }) => claims.map(({ outRef }) => outRef)),
  );
  const ready: UTxO[] = [];
  for (const [ordinal, { slot, claims }] of slots.entries()) {
    const utxo = [...claims]
      .reverse()
      .map(({ outRef }) => live.get(outRef))
      .find((candidate) => candidate !== undefined && slot.matches(candidate));
    if (utxo !== undefined) {
      ready.push(utxo);
      continue;
    }
    const newest = claims.at(-1);
    if (newest !== undefined && recent(newest)) {
      return Object.freeze({ kind: "pending", ordinal, outRef: newest.outRef });
    }
    return Object.freeze({
      kind: "publish",
      ordinal,
      replaces: newest?.outRef ?? null,
    });
  }
  return Object.freeze({ kind: "ready", utxos: Object.freeze(ready) });
};

/**
 * Claims kept in memory, for callers with no workflow journal: each key's
 * claims, oldest first. A process that loses it publishes again.
 */
export type OwnOutputJournal = Readonly<{
  claims: (key: string) => readonly OwnOutputClaimed[];
  claim: (key: string, outRef: string) => void;
}>;

export const createOwnOutputJournal = (): OwnOutputJournal => {
  const kept = new Map<string, OwnOutputClaimed[]>();
  return Object.freeze({
    claims: (key: string) => Object.freeze([...(kept.get(key) ?? [])]),
    claim: (key: string, outRef: string) => {
      parseOwnOutRef(outRef);
      const list = kept.get(key) ?? [];
      list.push(Object.freeze({ outRef }));
      kept.set(key, list);
    },
  });
};
