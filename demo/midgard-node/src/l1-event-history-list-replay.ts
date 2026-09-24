import { createHash } from "node:crypto";

import type { OutRefLike } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";

import { verifyEventHistoryActivation } from "./l1-event-history-activation.js";
import { stageEventHistoryBlock } from "./l1-event-history-block-stage.js";
import type { BoundHistoryCapture } from "./l1-event-history-projection.js";
import {
  type HistoryIncarnation,
  stageHistoryProvenance,
} from "./l1-event-history-provenance.js";
import { verifyEventHistoryReferenceBody } from "./l1-event-history-reference.js";
import {
  type BoundHistoryChainBlock,
  eventHistoryCanonicalJson,
  type EventHistorySourceBinding,
  verifyEventHistoryHubOutputs,
} from "./l1-event-history-source.js";
import type { LedgerSnapshotOutput } from "./l1-ledger-snapshot.js";

type ReplayInput = Readonly<{
  block: BoundHistoryChainBlock;
  binding: EventHistorySourceBinding;
  histories: SDK.EventHistoryContractPair;
  slotToUnixTime: (slot: number) => number;
  getCreatingBody: (txHash: string) => string | undefined;
  maximumBodyBytes: number;
}>;

type Activation = Readonly<{
  point: BoundHistoryChainBlock["point"];
  parent: string;
  transactionIndex: number;
  transactionHash: string;
  receipt: string;
}>;

/** Complete authenticated node sets and historical origins, NOT a complete
 * address/retention ledger. Older unrelated dust and retained preimages are not
 * enumerated by this replay. Join a genuine acquired capture before seeding SQL.
 */
export type EventHistoryListReplay = Readonly<{
  bindingDigest: string;
  manifestId: string;
  point: BoundHistoryChainBlock["point"];
  activation: Activation;
  outputs: readonly LedgerSnapshotOutput[];
  incarnations: readonly HistoryIncarnation[];
  replayDigest: string;
  blocksReplayed: number;
}>;

const digest = (value: string) =>
  createHash("sha256").update(value).digest("hex");
const refLabel = (ref: OutRefLike) => `${ref.txHash}#${ref.outputIndex}`;
const fail = (message: string): never => {
  throw new Error(`Invalid history list replay: ${message}`);
};
const authenticatedOutput =
  (binding: EventHistorySourceBinding) => (output: LedgerSnapshotOutput) => {
    const policy =
      output.address === binding.hubAddress
        ? binding.hubUnit.slice(0, 56)
        : Object.values(binding.deployments).find(
            (deployment) => deployment.address === output.address,
          )?.policyId;
    return (
      policy !== undefined &&
      Object.keys(output.assets).some((unit) => unit.startsWith(policy))
    );
  };

const checkLists = (
  outputs: readonly LedgerSnapshotOutput[],
  binding: EventHistorySourceBinding,
) => {
  verifyEventHistoryHubOutputs(outputs, binding);
  for (const deployment of Object.values(binding.deployments)) {
    const members = outputs.filter(
      (output) => output.address === deployment.address,
    );
    if (
      members.some(
        (output) => output.hasReferenceScript || output.datumHash !== undefined,
      )
    )
      fail(
        "authenticated list output has an invalid datum or reference script",
      );
    const nodes = SDK.authenticateHistoryNodes(
      members.map((output) => ({ ...output, assets: { ...output.assets } })),
      deployment,
    ).sort((a, b) =>
      a.key === null ? -1 : b.key === null ? 1 : a.key.localeCompare(b.key),
    );
    if (nodes[0]?.key !== null) fail("authenticated list root is missing");
    for (const [index, member] of nodes.entries())
      if (member.node.next !== (nodes[index + 1]?.key ?? null))
        fail("authenticated list contains missing or disconnected nodes");
  }
};

const stage = (
  input: ReplayInput,
  outputs: readonly LedgerSnapshotOutput[],
) => {
  if (
    !Number.isSafeInteger(input.maximumBodyBytes) ||
    input.maximumBodyBytes <= 0
  )
    fail("creating body byte bound is invalid");
  const transactions = new Map(
    input.block.transactions.map((transaction) => [
      transaction.txHash,
      transaction,
    ]),
  );
  const bodies = new Map<string, string>();
  const result = stageEventHistoryBlock({
    ...input,
    outputs,
    isTrackedOutput: authenticatedOutput(input.binding),
    resolveReference: (transactionHash, ref) => {
      const body = bodies.get(ref.txHash) ?? input.getCreatingBody(ref.txHash);
      if (body === undefined)
        return fail("referenced creating body is unavailable");
      const output = verifyEventHistoryReferenceBody({
        transaction: transactions.get(transactionHash)!,
        ref,
        creatingBodyCbor: body,
        maximumBodyBytes: input.maximumBodyBytes,
      });
      bodies.set(ref.txHash, body);
      return output;
    },
  });
  const receipt = eventHistoryCanonicalJson({
    domain: "midgard-node-authenticated-history-block-v1",
    bindingDigest: input.binding.digest,
    block: input.block,
    creatingBodies: [...bodies.entries()]
      .sort(([a], [b]) => a.localeCompare(b))
      .map(([txHash, bodyCbor]) => ({ txHash, bodyCbor })),
  });
  return { ...result, receipt };
};

/** The owner admits this entire valid activation block on its bound source.
 * No archive, manifest locator, or empty current list substitutes for that.
 * Shared one-shot initialization proves there were no earlier authenticated
 * nodes; it makes no claim about pre-existing unauthenticated address outputs.
 */
export const beginEventHistoryListReplay = (input: ReplayInput) => {
  const staged = stage(input, []);
  const activation = verifyEventHistoryActivation({
    ...input,
    transitions: staged.transitions,
  });
  checkLists(staged.outputs, input.binding);
  const incarnations = stageHistoryProvenance({
    bindingDigest: input.binding.digest,
    block: input.block,
    transitions: staged.transitions,
    incarnations: [],
  }).map(({ after }) => after);
  const state: EventHistoryListReplay = Object.freeze({
    bindingDigest: input.binding.digest,
    manifestId: input.binding.manifestId,
    point: Object.freeze({ ...input.block.point }),
    activation: Object.freeze({
      point: Object.freeze({ ...input.block.point }),
      parent: input.block.parent,
      transactionIndex: activation.activationIndex,
      transactionHash: activation.activationTransactionHash,
      receipt: staged.receipt,
    }),
    outputs: staged.outputs,
    incarnations: Object.freeze(incarnations),
    replayDigest: digest(staged.receipt),
    blocksReplayed: 1,
  });
  return Object.freeze({
    state,
    transitions: staged.transitions,
    receipt: staged.receipt,
  });
};

/** Continuous valid source blocks preserve all origins, including retired ones.
 * Rollback is handled by the owner retaining/reconstructing a prior immutable
 * state; an orphaned replay must never be joined to another branch's capture.
 */
export const advanceEventHistoryListReplay = ({
  previous,
  ...input
}: ReplayInput & {
  readonly previous: EventHistoryListReplay;
}) => {
  if (
    previous.bindingDigest !== input.binding.digest ||
    previous.manifestId !== input.binding.manifestId ||
    input.block.parent !== previous.point.id ||
    input.block.point.id === previous.point.id ||
    input.block.point.slot <= previous.point.slot ||
    input.block.point.height !== previous.point.height + 1
  )
    fail("block does not extend its bound replay state");
  const staged = stage(input, previous.outputs);
  if (
    staged.transitions.some(
      ({ transition }) => transition.operation === "Initialize",
    )
  )
    fail("authenticated histories cannot initialize again");
  checkLists(staged.outputs, input.binding);
  const incarnations = new Map(
    previous.incarnations.map((entry) => [entry.id, entry]),
  );
  for (const { after } of stageHistoryProvenance({
    bindingDigest: input.binding.digest,
    block: input.block,
    transitions: staged.transitions,
    incarnations: previous.incarnations,
  }))
    incarnations.set(after.id, after);
  const state: EventHistoryListReplay = Object.freeze({
    ...previous,
    point: Object.freeze({ ...input.block.point }),
    outputs: staged.outputs,
    incarnations: Object.freeze([...incarnations.values()]),
    replayDigest: digest(
      eventHistoryCanonicalJson({
        previous: previous.replayDigest,
        receipt: staged.receipt,
      }),
    ),
    blocksReplayed: previous.blocksReplayed + 1,
  });
  return Object.freeze({
    state,
    transitions: staged.transitions,
    receipt: staged.receipt,
  });
};

/** Join source replay and a genuine complete capture at exactly one admitted
 * point. The caller must still fence rollback, persist seed + L2 repair, reload
 * caches, and publish Ready under its current owner. Retired origins come from
 * replay, never absence in the capture. P, not activation, is the undo anchor.
 */
export const joinEventHistoryListReplay = ({
  state,
  capture,
  binding,
}: {
  readonly state: EventHistoryListReplay;
  readonly capture: BoundHistoryCapture;
  readonly binding: EventHistorySourceBinding;
}) => {
  if (
    state.bindingDigest !== binding.digest ||
    state.manifestId !== binding.manifestId ||
    capture.bindingDigest !== binding.digest ||
    state.point.id !== capture.history.ledger.point.id ||
    state.point.slot !== capture.history.ledger.point.slot
  )
    fail("capture and replay do not share their bound point");
  const ordered = (outputs: readonly LedgerSnapshotOutput[]) =>
    [...outputs].sort((a, b) => refLabel(a).localeCompare(refLabel(b)));
  const captured = capture.history.ledger.outputs.filter(
    authenticatedOutput(binding),
  );
  if (
    eventHistoryCanonicalJson(ordered(captured)) !==
    eventHistoryCanonicalJson(ordered(state.outputs))
  )
    fail("complete captured authenticated nodes differ from replay");
  const originReceipt = eventHistoryCanonicalJson({
    domain: "midgard-node-authenticated-history-origin-v1",
    bindingDigest: binding.digest,
    manifestId: binding.manifestId,
    genesisAlgorithm: binding.genesisAlgorithm,
    genesisSha256: binding.genesisSha256,
    activation: state.activation,
    replay: {
      head: state.point,
      blocks: state.blocksReplayed,
      digest: state.replayDigest,
    },
    anchorSnapshotDigest: capture.snapshotDigest,
  });
  return Object.freeze({
    capture,
    height: state.point.height,
    incarnations: state.incarnations,
    originReceipt,
    originReceiptDigest: digest(originReceipt),
  });
};
