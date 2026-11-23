/**
 * Edits of the `predecessorWitness` a workflow artifact carries, and the
 * check that an admission refused one, for the artifact tests of every
 * family that stores a ledger witness.
 */
import { expect } from "vitest";

import { isReplayLocalFault } from "../../src/block-replay.faults.js";
import {
  LedgerNodeMissing,
  LedgerOutputMissing,
  LedgerWitnessRefused,
} from "../../src/ledger-store/index.js";
import type { LedgerWitnessJson } from "../../src/ledger-store/witness-json.js";
import type { WorkflowJsonObject } from "../../src/workflow/identity.js";

/** The artifact's witness; fails the test when the artifact has none. */
export const predecessorWitnessOf = (
  artifact: WorkflowJsonObject,
): LedgerWitnessJson => {
  const witness = artifact.predecessorWitness;
  if (witness === null || witness === undefined)
    throw new Error("the artifact carries no predecessor witness");
  return witness as unknown as LedgerWitnessJson;
};

export const withPredecessorWitness = (
  artifact: WorkflowJsonObject,
  witness: LedgerWitnessJson,
): WorkflowJsonObject =>
  ({ ...artifact, predecessorWitness: witness }) as WorkflowJsonObject;

const flip = (hex: string): string =>
  hex.slice(0, -1) + (hex.endsWith("0") ? "1" : "0");

/** One witness per node, that node's content changed so its hash differs. */
export const tamperedWitnesses = (
  witness: LedgerWitnessJson,
): LedgerWitnessJson[] =>
  witness.nodes.map((node, index) => {
    const changed =
      node.__kind === "Leaf"
        ? { ...node, value: flip(node.value) }
        : {
            ...node,
            children: node.children.map((child, slot) =>
              slot === node.children.findIndex((c) => c !== null)
                ? flip(child!)
                : child,
            ),
          };
    return {
      ...witness,
      nodes: witness.nodes.map((other, at) => (at === index ? changed : other)),
    };
  });

/** One witness per node, that node left out. */
export const droppedWitnesses = (
  witness: LedgerWitnessJson,
): LedgerWitnessJson[] =>
  witness.nodes.map((_, index) => ({
    ...witness,
    nodes: witness.nodes.filter((__, at) => at !== index),
  }));

/**
 * Asserts `use` refuses the witness: `ledgerFromWitness` refuses it, or a
 * read meets a node or output the witness does not hold.
 */
export const expectWitnessRefused = async (
  use: () => Promise<unknown>,
  what: string,
) => {
  const error = await use().then(
    () => undefined,
    (cause: unknown) => cause,
  );
  const missing =
    isReplayLocalFault(error) &&
    (error.cause instanceof LedgerNodeMissing ||
      error.cause instanceof LedgerOutputMissing);
  expect(
    error instanceof LedgerWitnessRefused ||
      error instanceof LedgerNodeMissing ||
      missing,
    `${what}: ${String(error)}`,
  ).toBe(true);
};
