import { isDeepStrictEqual } from "node:util";

import {
  advanceMidgardCekDataTraverse,
  advanceMidgardRedeemerItemProof,
  buildMidgardLedgerOutputProofTrace,
  buildMidgardRedeemerItemProofTrace,
  encodeMidgardCekDataFrame,
  encodeMidgardCekDataTraverseControl,
  finalizeMidgardCekDataTraverse,
  isExactMidgardLedgerOutputProofTerminal,
  nextMidgardCekDataTraverseSpan,
} from "@al-ft/midgard-core";
import { expect } from "vitest";

import {
  type DataBreadthKind,
  type DataTraverseStep,
  jsonFrame,
  jsonSummary,
} from "./data-breadth-boundary.assert-exact-fold-semantics.js";

export const exactBroadFrontierVector = (
  kind: DataBreadthKind,
  steps: readonly DataTraverseStep[],
) => {
  let step: DataTraverseStep | undefined;
  let membershipDepth = -1;
  for (const candidate of steps) {
    const action = candidate.action;
    const candidateDepth =
      kind === "map"
        ? action?.kind === "foldMap" &&
          action.keySiblings.length > 0 &&
          action.valueSiblings.length > 0
          ? action.keySiblings.length + action.valueSiblings.length
          : -1
        : action?.kind === "foldList"
          ? action.siblings.length
          : -1;
    if (candidateDepth > membershipDepth) {
      step = candidate;
      membershipDepth = candidateDepth;
    }
  }
  if (step?.action?.kind !== "foldList" && step?.action?.kind !== "foldMap") {
    throw new Error("Broad Data trace lost its frontier fold");
  }
  expect(membershipDepth).toBeGreaterThan(0);
  const action = step.action;
  const mutatedAction =
    action.kind === "foldList"
      ? {
          ...action,
          childIndex: action.childIndex - 1,
        }
      : {
          ...action,
          pairIndex: action.pairIndex - 1,
        };
  expect(
    advanceMidgardCekDataTraverse({
      control: step.control,
      sourceBytes: null,
      action: mutatedAction,
    }),
  ).toBeNull();
  const mutateFirstSibling = (
    siblings: readonly Uint8Array[],
  ): readonly Buffer[] => {
    expect(siblings.length).toBeGreaterThan(0);
    const mutatedFirst = Buffer.from(siblings[0]!);
    mutatedFirst[0] = mutatedFirst[0]! ^ 0x01;
    return [
      mutatedFirst,
      ...siblings.slice(1).map((sibling) => Buffer.from(sibling)),
    ];
  };
  if (action.kind === "foldList") {
    expect(
      advanceMidgardCekDataTraverse({
        control: step.control,
        sourceBytes: null,
        action: {
          ...action,
          siblings: mutateFirstSibling(action.siblings),
        },
      }),
    ).toBeNull();
  } else {
    expect(
      advanceMidgardCekDataTraverse({
        control: step.control,
        sourceBytes: null,
        action: {
          ...action,
          keySiblings: mutateFirstSibling(action.keySiblings),
        },
      }),
    ).toBeNull();
    expect(
      advanceMidgardCekDataTraverse({
        control: step.control,
        sourceBytes: null,
        action: {
          ...action,
          valueSiblings: mutateFirstSibling(action.valueSiblings),
        },
      }),
    ).toBeNull();
  }
  return {
    preControlCborHex: encodeMidgardCekDataTraverseControl(
      step.control,
    ).toString("hex"),
    sourceBytesHex: null,
    membershipDepth,
    action:
      action.kind === "foldList"
        ? {
            kind: action.kind,
            frame: jsonFrame(action.frame),
            childIndex: action.childIndex,
            child: jsonSummary(action.child),
            siblingHexes: action.siblings.map((sibling) =>
              Buffer.from(sibling).toString("hex"),
            ),
          }
        : {
            kind: action.kind,
            frame: jsonFrame(action.frame),
            pairIndex: action.pairIndex,
            key: jsonSummary(action.key),
            value: jsonSummary(action.value),
            keySiblingHexes: action.keySiblings.map((sibling) =>
              Buffer.from(sibling).toString("hex"),
            ),
            valueSiblingHexes: action.valueSiblings.map((sibling) =>
              Buffer.from(sibling).toString("hex"),
            ),
          },
    postControlCborHex: encodeMidgardCekDataTraverseControl(step.next).toString(
      "hex",
    ),
  };
};

export const exactTerminalVector = (steps: readonly DataTraverseStep[]) => {
  const terminalStep = steps.at(-1);
  if (terminalStep?.action?.kind !== "finalizeFrame") {
    throw new Error("Broad Data trace lost its final frame");
  }
  const summary = finalizeMidgardCekDataTraverse(terminalStep.next);
  if (summary === null) {
    throw new Error("Broad Data trace did not terminate");
  }
  const mutatedFrame = {
    ...terminalStep.action.frame,
    sequence: {
      ...terminalStep.action.frame.sequence,
      root: Buffer.concat([
        Buffer.from([terminalStep.action.frame.sequence.root[0]! ^ 0x01]),
        Buffer.from(terminalStep.action.frame.sequence.root.subarray(1)),
      ]),
    },
  };
  expect(
    advanceMidgardCekDataTraverse({
      control: terminalStep.control,
      sourceBytes: null,
      action: {
        ...terminalStep.action,
        frame: mutatedFrame,
      },
    }),
  ).toBeNull();
  return {
    preControlCborHex: encodeMidgardCekDataTraverseControl(
      terminalStep.control,
    ).toString("hex"),
    frameCborHex: encodeMidgardCekDataFrame(terminalStep.action.frame).toString(
      "hex",
    ),
    postControlCborHex: encodeMidgardCekDataTraverseControl(
      terminalStep.next,
    ).toString("hex"),
    summary: {
      rootHex: Buffer.from(summary.root).toString("hex"),
      cborLength: summary.cborLength.toString(),
      memory: summary.memory.toString(),
    },
  };
};

export const maximumSourceSpan = (steps: readonly DataTraverseStep[]): number =>
  steps.reduce(
    (maximum, { control }) =>
      Math.max(maximum, nextMidgardCekDataTraverseSpan(control)?.length ?? 0),
    0,
  );

export const extractAuthenticatedLedgerOutputDataSteps = (
  trace: ReturnType<typeof buildMidgardLedgerOutputProofTrace>,
): readonly DataTraverseStep[] => {
  const dataSteps: DataTraverseStep[] = [];
  let expectedControl = trace.initial;
  for (let index = 0; index < trace.steps.length; index += 1) {
    const { control, witness, next } = trace.steps[index]!;
    if (control !== expectedControl) {
      throw new Error(
        `ledger-output production trace lost successor identity at step ${index.toString()}`,
      );
    }
    expectedControl = next;
    if (
      witness?.kind === "datum" &&
      control.datum !== null &&
      next.datum !== null
    ) {
      dataSteps.push({
        control: control.datum,
        action: witness.action,
        next: next.datum,
      });
    }
  }
  if (
    expectedControl !== trace.terminal ||
    !isExactMidgardLedgerOutputProofTerminal(trace.terminal)
  ) {
    throw new Error("ledger-output production trace did not terminate");
  }
  return dataSteps;
};

export const replayRedeemerItemProof = (
  trace: ReturnType<typeof buildMidgardRedeemerItemProofTrace>,
): readonly DataTraverseStep[] => {
  const dataSteps: DataTraverseStep[] = [];
  for (let index = 0; index < trace.steps.length; index += 1) {
    const { control, witness, next } = trace.steps[index]!;
    const replay = advanceMidgardRedeemerItemProof({
      control,
      witness,
    });
    if (replay === null || !isDeepStrictEqual(replay, next)) {
      throw new Error(
        `redeemer-item production replay diverged at step ${index.toString()}`,
      );
    }
    if (
      witness.action.kind === "traverseData" &&
      control.traversal !== null &&
      next.traversal !== null
    ) {
      dataSteps.push({
        control: control.traversal,
        action: witness.action.action,
        next: next.traversal,
      });
    }
  }
  return dataSteps;
};

export const maximumDatumChunkBytes = (
  trace: ReturnType<typeof buildMidgardLedgerOutputProofTrace>,
): number =>
  trace.steps.reduce((maximum, { witness }) => {
    if (witness?.kind !== "datum") return maximum;
    return Math.max(maximum, witness.window?.length ?? 0);
  }, 0);

export const maximumRedeemerChunkBytes = (
  trace: ReturnType<typeof buildMidgardRedeemerItemProofTrace>,
): number =>
  trace.steps.reduce(
    (maximum, { witness }) =>
      Math.max(
        maximum,
        witness.chunkProof?.chunk.length ?? 0,
        witness.nextChunkProof?.chunk.length ?? 0,
      ),
    0,
  );
