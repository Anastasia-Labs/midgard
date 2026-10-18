import {
  hashMidgardCekBlobChunk,
  hashMidgardCekContinuationFrame,
  hashMidgardCekSequenceNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";
import { DataConstr, DataI, DataList } from "@harmoniclabs/plutus-data";

import { type MidgardCekDirectValueWitness } from "./cek-builtin.js";
import {
  decodeMidgardCekConstantTypeCbor,
  decodeMidgardCekConstantWitness,
  encodeMidgardCekConstantTypeCbor,
  midgardCekConstantMemorySize,
} from "./cek-constant.js";
import { semanticPairComponents } from "./cek-machine.data-node-topology-matches.js";
import {
  errorSuccessor,
  exactState,
  sameState,
} from "./cek-machine.midgard-cek-builtin-argument-count.js";
import {
  type Bytes,
  type MidgardCekCoreStepWitness,
  MidgardCekErrorCodes,
  type MidgardCekSemanticBuiltinWitness,
  sameBytes,
} from "./cek-machine.midgard-cek-core-step-witness.js";
import { type DataSummary } from "./cek-machine.verify-case-select.js";
import {
  listDataSummary,
  semanticSummary,
} from "./cek-machine.verify-semantic-builtin-control.js";
import { semanticListHeadAndTail } from "./cek-machine.verify-semantic-list.js";

/**
 * Where `case` sends a constant scrutinee under the protocol version 11
 * rules: a branch and the values it is applied to, a missing branch, or a
 * type that `case` does not accept.
 */
export type CaseConstantSelection =
  | {
      readonly kind: "branch";
      readonly tag: bigint;
      readonly valuesCount: bigint;
      readonly valuesRoot: Bytes;
    }
  | { readonly kind: "missing" }
  | { readonly kind: "unsupported" };

const MISSING: CaseConstantSelection = { kind: "missing" };
const UNSUPPORTED: CaseConstantSelection = { kind: "unsupported" };

const constantRoot = (
  typeCbor: Bytes,
  summary: DataSummary,
  memory: bigint,
): Bytes =>
  hashMidgardCekValueNode({
    kind: "constant",
    typeRoot: hashMidgardCekBlobChunk(typeCbor),
    payloadRoot: summary.root,
    payloadLength: summary.cborLength,
    semanticRoot: summary.root,
    memory,
  });

const branch = (tag: bigint): CaseConstantSelection => ({
  kind: "branch",
  tag,
  valuesCount: 0n,
  valuesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
});

// The case-apply step applies the deepest value first, so `first` is pushed
// below `second`, exactly as a constructor's fields are.
const twoValues = (first: Bytes, second: Bytes): CaseConstantSelection => ({
  kind: "branch",
  tag: 0n,
  valuesCount: 2n,
  valuesRoot: hashMidgardCekSequenceNode({
    head: second,
    tail: hashMidgardCekSequenceNode({
      head: first,
      tail: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
      length: 1n,
    }),
    length: 2n,
  }),
});

const directSelection = (
  witness: Extract<MidgardCekDirectValueWitness, { kind: "constant" }>,
  count: bigint,
): { readonly root: Bytes; readonly selection: CaseConstantSelection } => {
  const { typeCbor } = witness.witness;
  const { type, payload } = decodeMidgardCekConstantWitness(witness.witness);
  const memory = midgardCekConstantMemorySize(type, payload);
  const root = constantRoot(typeCbor, semanticSummary(payload), memory);
  const select = (selection: CaseConstantSelection) => ({ root, selection });
  switch (type.kind) {
    case "unit":
      return select(count === 1n ? branch(0n) : MISSING);
    case "boolean":
      if ((payload as DataConstr).constr === 1n) {
        return select(count === 2n ? branch(1n) : MISSING);
      }
      return select(count === 1n || count === 2n ? branch(0n) : MISSING);
    case "integer": {
      const value = (payload as DataI).int;
      return select(value >= 0n && value < count ? branch(value) : MISSING);
    }
    case "list": {
      const [head, ...tail] = (payload as DataList).list;
      if (head === undefined) {
        return select(count === 2n ? branch(1n) : MISSING);
      }
      if (count !== 1n && count !== 2n) return select(MISSING);
      const headMemory = midgardCekConstantMemorySize(type.element, head);
      return select(
        twoValues(
          constantRoot(
            encodeMidgardCekConstantTypeCbor(type.element),
            semanticSummary(head),
            headMemory,
          ),
          constantRoot(
            typeCbor,
            semanticSummary(new DataList(tail)),
            memory - headMemory,
          ),
        ),
      );
    }
    case "pair": {
      if (count !== 1n) return select(MISSING);
      const [first, second] = (payload as DataConstr).fields;
      const firstMemory = midgardCekConstantMemorySize(type.first, first!);
      return select(
        twoValues(
          constantRoot(
            encodeMidgardCekConstantTypeCbor(type.first),
            semanticSummary(first!),
            firstMemory,
          ),
          constantRoot(
            encodeMidgardCekConstantTypeCbor(type.second),
            semanticSummary(second!),
            memory - firstMemory,
          ),
        ),
      );
    }
    case "string":
    case "bytes":
    case "data":
    case "blsG1":
    case "blsG2":
    case "blsMillerLoopResult":
    case "array":
    case "value":
      return select(UNSUPPORTED);
  }
};

const semanticSelection = (
  witness: Extract<MidgardCekDirectValueWitness, { kind: "semanticConstant" }>,
  material: MidgardCekSemanticBuiltinWitness,
  count: bigint,
): {
  readonly root: Bytes;
  readonly selection: CaseConstantSelection;
} | null => {
  const { typeCbor, payload, memory } = witness.witness;
  const type = decodeMidgardCekConstantTypeCbor(typeCbor);
  const root = constantRoot(typeCbor, payload, memory);
  switch (type.kind) {
    case "unit":
    case "boolean":
      return null;
    case "integer":
      // A committed integer of two or more words has magnitude at least
      // 2^64, past every branch index a term can hold.
      return memory >= 2n ? { root, selection: MISSING } : null;
    case "list": {
      if (count !== 1n && count !== 2n) return { root, selection: MISSING };
      const opened = semanticListHeadAndTail(witness, material);
      if (opened === null) return null;
      return {
        root,
        selection: twoValues(
          constantRoot(
            encodeMidgardCekConstantTypeCbor(opened.element),
            opened.head.summary,
            opened.head.memory,
          ),
          constantRoot(
            typeCbor,
            listDataSummary(opened.tail.sequence),
            opened.tail.memory,
          ),
        ),
      };
    }
    case "pair": {
      if (count !== 1n) return { root, selection: MISSING };
      const opened = semanticPairComponents(witness, material);
      if (opened === null) return null;
      return {
        root,
        selection: twoValues(
          constantRoot(
            encodeMidgardCekConstantTypeCbor(opened.first.type),
            opened.first.summary,
            opened.first.memory,
          ),
          constantRoot(
            encodeMidgardCekConstantTypeCbor(opened.second.type),
            opened.second.summary,
            opened.second.memory,
          ),
        ),
      };
    }
    case "string":
    case "bytes":
    case "data":
    case "blsG1":
    case "blsG2":
    case "blsMillerLoopResult":
    case "array":
    case "value":
      return { root, selection: UNSUPPORTED };
  }
};

/**
 * The committed root of a constant scrutinee and where `case` sends it. An
 * inline constant is read whole; a semantic one through its list or pair
 * material. Returns null when the witness cannot open the scrutinee.
 */
export const caseConstantSelection = (
  scrutinee: MidgardCekDirectValueWitness,
  material: MidgardCekSemanticBuiltinWitness,
  branchesCount: bigint,
): {
  readonly root: Bytes;
  readonly selection: CaseConstantSelection;
} | null => {
  if (scrutinee.kind === "constant") {
    return directSelection(scrutinee, branchesCount);
  }
  if (scrutinee.kind === "semanticConstant") {
    return semanticSelection(scrutinee, material, branchesCount);
  }
  return null;
};

/**
 * `case` on a constant: the value returns into a case frame and selects its
 * branch, or halts with a missing branch or an unsupported scrutinee.
 */
export const verifyReturnCaseConstant = (
  pre: MidgardCekMachineState,
  post: MidgardCekMachineState,
  witness: Extract<MidgardCekCoreStepWitness, { kind: "returnCaseConstant" }>,
): boolean => {
  const opened = caseConstantSelection(
    witness.scrutinee,
    witness.material,
    witness.branchesCount,
  );
  if (opened === null) return false;
  const { selection } = opened;
  return (
    sameBytes(pre.focusRoot, opened.root) &&
    sameBytes(
      pre.continuationRoot,
      hashMidgardCekContinuationFrame({
        kind: "case",
        branchesCount: witness.branchesCount,
        branchesRoot: witness.branchesRoot,
        environment: witness.capturedEnvironment,
        tail: witness.tail,
      }),
    ) &&
    sameState(
      post,
      selection.kind === "branch"
        ? exactState(pre, {
            mode: "caseSelect",
            focusRoot: witness.branchesRoot,
            environmentRoot: selection.valuesRoot,
            continuationRoot: hashMidgardCekContinuationFrame({
              kind: "caseSelect",
              environment: witness.capturedEnvironment,
              tail: witness.tail,
              valuesCount: selection.valuesCount,
            }),
            auxiliary: selection.tag,
          })
        : errorSuccessor(
            pre,
            selection.kind === "missing"
              ? MidgardCekErrorCodes.CaseBranchMissing
              : MidgardCekErrorCodes.InvalidCaseScrutinee,
          ),
    )
  );
};
