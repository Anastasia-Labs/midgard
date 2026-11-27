import {
  buildMidgardBoundedItemChunkProof,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  type MidgardBoundedItem,
} from "./bounded-item.js";
import { isWellFormedMidgardLedgerOutputProofControl } from "./ledger-output-proof.is-well-formed-midgard-ledger-output-proof-control.js";
import {
  type MidgardLedgerOutputProofControl,
  MidgardLedgerOutputProofStages,
  type MidgardLedgerOutputProofWitness,
} from "./ledger-output-proof.midgard-ledger-output-proof-witness.js";

export const chunkWitness = ({
  item,
  chunkIndex,
  nextChunkIndex,
}: {
  readonly item: MidgardBoundedItem;
  readonly chunkIndex: number;
  readonly nextChunkIndex: number | null;
}): MidgardLedgerOutputProofWitness => ({
  kind: "chunks",
  chunkProof: buildMidgardBoundedItemChunkProof(item, chunkIndex),
  nextChunkProof:
    nextChunkIndex === null
      ? null
      : buildMidgardBoundedItemChunkProof(item, nextChunkIndex),
});

export const spanChunkWitness = ({
  item,
  absoluteStart,
  length,
}: {
  readonly item: MidgardBoundedItem;
  readonly absoluteStart: number;
  readonly length: number;
}): MidgardLedgerOutputProofWitness => {
  const firstChunkIndex = Math.floor(
    absoluteStart / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  const lastChunkIndex = Math.floor(
    (absoluteStart + length - 1) / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  return chunkWitness({
    item,
    chunkIndex: firstChunkIndex,
    nextChunkIndex: lastChunkIndex === firstChunkIndex ? null : lastChunkIndex,
  });
};

export const isExactMidgardLedgerOutputProofTerminal = (
  control: MidgardLedgerOutputProofControl,
): boolean =>
  isWellFormedMidgardLedgerOutputProofControl(control) &&
  control.stage === MidgardLedgerOutputProofStages.Terminal;
