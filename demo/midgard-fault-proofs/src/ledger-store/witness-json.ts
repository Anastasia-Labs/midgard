/**
 * The JSON form a workflow artifact stores a ledger witness in, and its
 * parser.
 *
 * Every value is a string, `null`, an array or a plain object, which is what
 * `normalizeWorkflowJson` accepts. Nodes and outputs keep the order the
 * witness has (`recordWitness` sorts them by hash and by out-ref), so the
 * same recorded reads always write the same JSON.
 *
 * The parser checks only the JSON's shape. Whether the nodes are well formed
 * and open a given root is `ledgerFromWitness`'s check.
 */
import type { LedgerWitness, WitnessNode } from "./witness.js";

export type LedgerWitnessJsonNode =
  | {
      readonly __kind: "Leaf";
      readonly prefix: string;
      readonly key: string;
      readonly value: string;
    }
  | {
      readonly __kind: "Branch";
      readonly prefix: string;
      readonly children: readonly (string | null)[];
    };

export type LedgerWitnessJson = {
  readonly nodes: readonly LedgerWitnessJsonNode[];
  readonly outputs: readonly {
    readonly outRef: string;
    readonly output: string;
  }[];
};

const HEX = /^(?:[0-9a-f]{2})+$/u;

/** The witness as an artifact stores it. */
export const ledgerWitnessJson = (
  witness: LedgerWitness,
): LedgerWitnessJson => ({
  nodes: witness.nodes.map((node) =>
    node.__kind === "Leaf"
      ? {
          __kind: "Leaf",
          prefix: node.prefix,
          key: node.key,
          value: node.value,
        }
      : { __kind: "Branch", prefix: node.prefix, children: [...node.children] },
  ),
  outputs: witness.outputs.map(({ outRef, output }) => ({
    outRef: outRef.toString("hex"),
    output: output.toString("hex"),
  })),
});

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const hasKeys = (value: Record<string, unknown>, keys: string): boolean =>
  Object.keys(value).sort().join(",") === keys;

/** Parses `LedgerWitnessJson`; refuses a value of another shape. */
export const parseLedgerWitnessJson = (
  value: unknown,
  label: string,
): LedgerWitness => {
  if (!isRecord(value) || !hasKeys(value, "nodes,outputs"))
    throw new Error(`${label}: a witness has exactly nodes and outputs`);
  const { nodes, outputs } = value;
  if (!Array.isArray(nodes) || !Array.isArray(outputs))
    throw new Error(`${label}: nodes and outputs must be arrays`);
  return {
    nodes: nodes.map((node: unknown, index): WitnessNode => {
      const at = `${label}: node ${index.toString()}`;
      if (!isRecord(node)) throw new Error(`${at} must be an object`);
      if (node.__kind === "Leaf" && hasKeys(node, "__kind,key,prefix,value"))
        return node as WitnessNode;
      if (node.__kind === "Branch" && hasKeys(node, "__kind,children,prefix"))
        return node as WitnessNode;
      throw new Error(`${at} is neither a leaf nor a branch`);
    }),
    outputs: outputs.map((entry: unknown, index) => {
      const at = `${label}: output ${index.toString()}`;
      if (!isRecord(entry) || !hasKeys(entry, "outRef,output"))
        throw new Error(`${at} has exactly outRef and output`);
      const { outRef, output } = entry;
      if (
        typeof outRef !== "string" ||
        typeof output !== "string" ||
        !HEX.test(outRef) ||
        !HEX.test(output)
      )
        throw new Error(`${at}: outRef and output must be hex bytes`);
      return {
        outRef: Buffer.from(outRef, "hex"),
        output: Buffer.from(output, "hex"),
      };
    }),
  };
};
