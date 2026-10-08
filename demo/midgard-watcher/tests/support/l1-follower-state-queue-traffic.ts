/**
 * State-queue traffic for the L1 follower's fork simulator (ticket W1): the
 * protocol-init tx (hub oracle, Idle CorrectionLock, queue root),
 * CommitBlockHeader appends shaped as the authenticated observation admits
 * them, DA attestations (a DAAT minted for the tail header, later burned by
 * an apply that re-outputs the node), merges of the oldest header, and
 * third-party payments to the queue and lock addresses that no side may
 * pick up.
 *
 * Every tx is a pure function of the queue it extends: the same queue on
 * another branch yields the same tx, so a rollback followed by the same
 * decision re-lands the identical tx in a different block. No Plutus
 * evaluation, signature or ledger validity is claimed.
 */
import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import type { OutRef } from "@al-ft/midgard-l1-follower";
import type {
  SimChain,
  SimOutput,
  SimTx,
  SimUtxo,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { WatcherProjectionDeployment } from "../../src/l1-follower/projection.js";
import { createSyntheticStateQueueHeader } from "./state-queue-observation-fixture.commit-transaction.js";

const h28 = (byte: string): string => byte.repeat(28);

export const SIM_WATCHER_DEPLOYMENT: WatcherProjectionDeployment = {
  network: "Preprod",
  stateQueueSpend: h28("61"),
  stateQueueMint: h28("62"),
  correctionLockSpend: h28("63"),
  hubOracleMint: h28("64"),
  fraudProofSpend: h28("65"),
  fraudProofMint: h28("66"),
  availabilityChallengeSpend: h28("68"),
  availabilityChallengeMint: h28("67"),
  daBondPoolSpend: h28("69"),
  daAttestationMint: h28("6a"),
};

/** Where the simulated DA attestations sit (tracked by the DAAT policy only). */
export const SIM_DA_ATTESTATION_ADDRESS = Buffer.concat([
  Buffer.of(0x70),
  Buffer.from(h28("6b"), "hex"),
]);

/** The one-shot outref the protocol-init tx spends (outside every branch). */
export const SIM_HUB_ORACLE_ONE_SHOT: OutRef = {
  txHash: Buffer.concat([Buffer.of(0xef), Buffer.alloc(31, 0x01)]),
  index: 0,
};

export const outsideInput = (n: number): OutRef => {
  const txHash = Buffer.alloc(32, 0x02);
  txHash[0] = 0xef;
  txHash.writeUInt32BE(n, 28);
  return { txHash, index: 0 };
};

export const scriptAddress = (hash: string): Buffer =>
  Buffer.concat([Buffer.of(0x70), Buffer.from(hash, "hex")]);

const asset = (
  entries: readonly (readonly [string, string])[],
): ReadonlyMap<string, ReadonlyMap<string, bigint>> => {
  const map = new Map<string, Map<string, bigint>>();
  for (const [policy, name] of [...entries].sort(([a], [b]) =>
    a < b ? -1 : a > b ? 1 : 0,
  )) {
    const names = map.get(policy) ?? new Map<string, bigint>();
    names.set(name, 1n);
    map.set(policy, names);
  }
  return map;
};

const datum = (cborHex: string): Buffer => Buffer.from(cborHex, "hex");

const ZERO_ROOT = "00".repeat(32);

const redeemer = (value: SDK.StateQueueRedeemer) => [
  {
    purpose: "mint" as const,
    index: 0,
    data: datum(Data.to(value, SDK.StateQueueRedeemer)),
  },
];

/** The protocol-init tx: hub oracle (0), Idle CorrectionLock (1), queue root (2). */
export const initTx = (
  deployment: WatcherProjectionDeployment = SIM_WATCHER_DEPLOYMENT,
  /** Where the root output goes (the state-queue address unless set). */
  rootAddress: Buffer = scriptAddress(deployment.stateQueueSpend),
): SimTx => {
  const { stateQueueMint: sq, hubOracleMint: hub } = deployment;
  return {
    inputs: [SIM_HUB_ORACLE_ONE_SHOT],
    outputs: [
      {
        address: scriptAddress(hub),
        lovelace: 2_000_000n,
        assets: asset([[hub, SDK.HUB_ORACLE_ASSET_NAME]]),
        datum: datum(Data.to(0n)),
      },
      {
        address: scriptAddress(deployment.correctionLockSpend),
        lovelace: 2_000_000n,
        assets: asset([[hub, SDK.CORRECTION_LOCK_ASSET_NAME]]),
        datum: datum(Data.to("Idle", SDK.CorrectionLockDatum)),
      },
      {
        address: rootAddress,
        lovelace: 2_000_000n,
        assets: asset([[sq, SDK.STATE_QUEUE_ROOT_ASSET_NAME]]),
        datum: datum(
          Data.to(
            SDK.nodeViewToLinkedListDatum({
              key: "Empty",
              next: "Empty",
              data: Data.to([]),
            }),
            SDK.LinkedListDatum,
          ),
        ),
      },
    ],
    mint: asset([
      [sq, SDK.STATE_QUEUE_ROOT_ASSET_NAME],
      [hub, SDK.HUB_ORACLE_ASSET_NAME],
      [hub, SDK.CORRECTION_LOCK_ASSET_NAME],
    ]),
    redeemers: redeemer({ InitV1: { output_index: 2n } }),
    nonce: 900_000,
  };
};

export type QueueState = Readonly<{
  /** Root plus nodes, live on the model chain. */
  length: number;
  /** The root, then each node in link order. */
  ordered: readonly SimUtxo[];
  tail: SimUtxo;
  tailHeaderHash: string | null;
  lock: SimUtxo;
  /** Live DAAT outputs by header hash. */
  attestations: ReadonlyMap<string, SimUtxo>;
}>;

const unitOf = (utxo: SimUtxo, policy: string): string | null => {
  const names = utxo.output.assets?.get(policy);
  return names === undefined ? null : ([...names.keys()][0] ?? null);
};

const linkOf = (utxo: SimUtxo): string | null =>
  Data.from((utxo.output.datum as Buffer).toString("hex"), SDK.LinkedListDatum)
    .link;

/** The header hash a node utxo carries, or null for the root. */
export const headerOf = (
  utxo: SimUtxo,
  deployment: WatcherProjectionDeployment = SIM_WATCHER_DEPLOYMENT,
): string | null => {
  const unit = unitOf(utxo, deployment.stateQueueMint) as string;
  return unit === SDK.STATE_QUEUE_ROOT_ASSET_NAME
    ? null
    : unit.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length);
};

/** The live queue on the model chain, or null before the init tx. */
export const queueState = (
  chain: SimChain,
  deployment: WatcherProjectionDeployment = SIM_WATCHER_DEPLOYMENT,
  /** Where the init tx put the root, when not at the state-queue address. */
  rootAddress?: Buffer,
): QueueState | null => {
  const sqAddress = scriptAddress(deployment.stateQueueSpend);
  const live = chain.live();
  const queue = live.filter(
    (utxo) =>
      (utxo.output.address.equals(sqAddress) ||
        rootAddress?.equals(utxo.output.address) === true) &&
      unitOf(utxo, deployment.stateQueueMint) !== null,
  );
  const lock = live.find(
    (utxo) =>
      unitOf(utxo, deployment.hubOracleMint) === SDK.CORRECTION_LOCK_ASSET_NAME,
  );
  if (queue.length === 0 || lock === undefined) return null;
  const root = queue.find((utxo) => headerOf(utxo, deployment) === null);
  if (root === undefined) throw new Error("the model queue has no root");
  const byHeader = new Map(
    queue.flatMap((utxo) => {
      const header = headerOf(utxo, deployment);
      return header === null ? [] : [[header, utxo] as const];
    }),
  );
  const ordered: SimUtxo[] = [root];
  for (let next = linkOf(root); next !== null; ) {
    const node = byHeader.get(next);
    if (node === undefined) throw new Error("the model queue is broken");
    ordered.push(node);
    next = linkOf(node);
  }
  if (ordered.length !== queue.length)
    throw new Error("the model queue has unlinked nodes");
  const tail = ordered.at(-1) as SimUtxo;
  const attestations = new Map<string, SimUtxo>();
  for (const utxo of live) {
    const name = unitOf(utxo, deployment.daAttestationMint);
    if (name !== null)
      attestations.set(
        name.slice(SDK.DA_ATTESTATION_ASSET_NAME_PREFIX.length),
        utxo,
      );
  }
  return {
    length: queue.length,
    ordered,
    tail,
    tailHeaderHash: headerOf(tail, deployment),
    lock,
    attestations,
  };
};

/** The CommitBlockHeader append on top of `state`: continued tail (0), new node (1). */
export const commitTx = (
  state: QueueState,
  deployment: WatcherProjectionDeployment = SIM_WATCHER_DEPLOYMENT,
): SimTx => {
  const header: SDK.Header = {
    ...createSyntheticStateQueueHeader(),
    blockSlot: BigInt(state.length),
    prevHeaderHash: state.tailHeaderHash ?? h28("00"),
  };
  const headerHash = computeHash28(
    Buffer.from(Data.to(header, SDK.Header), "hex"),
  ).toString("hex");
  const tailDatum = Data.from(
    (state.tail.output.datum as Buffer).toString("hex"),
    SDK.LinkedListDatum,
  );
  const node: SDK.StateQueueNode = {
    proven_fraud: null,
    header,
    da_attestation: "Unattested",
  };
  const nodeUnit = `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`;
  const continued: SimOutput = {
    ...state.tail.output,
    datum: datum(
      Data.to({ ...tailDatum, link: headerHash }, SDK.LinkedListDatum),
    ),
  };
  return {
    inputs: [state.tail.outRef],
    referenceInputs: [state.lock.outRef],
    outputs: [
      continued,
      {
        address: scriptAddress(deployment.stateQueueSpend),
        lovelace: 2_000_000n,
        assets: asset([[deployment.stateQueueMint, nodeUnit]]),
        datum: datum(
          Data.to(
            SDK.nodeViewToLinkedListDatum({
              key: { Key: { key: headerHash } },
              next: "Empty",
              data: Data.castTo(node, SDK.StateQueueNode),
            }),
            SDK.LinkedListDatum,
          ),
        ),
      },
    ],
    mint: asset([[deployment.stateQueueMint, nodeUnit]]),
    redeemers: redeemer({
      CommitBlockHeader: {
        yield_to_ref_input_index: 0n,
        new_block_output_index: 1n,
        continued_latest_block_output_index: 0n,
        operator: header.operatorVkey,
        scheduler_ref_input_index: 0n,
        active_operators_input_index: 0n,
        active_operators_redeemer_index: 0n,
        m_confirmed_state_ref_input_index: null,
        m_head_state_queue_node_ref_input_index: null,
      },
    }),
    nonce: 900_001 + state.length,
  };
};

const daatName = (headerHash: string): string =>
  `${SDK.DA_ATTESTATION_ASSET_NAME_PREFIX}${headerHash}`;

const nonceOf = (outRef: OutRef): number =>
  960_000 + outRef.txHash.readUInt16BE(0) * 8 + outRef.index;

/** Mints the DAAT of the tail header (a stand-in attestation, no commitment). */
export const attestTx = (
  state: QueueState,
  deployment: WatcherProjectionDeployment = SIM_WATCHER_DEPLOYMENT,
): SimTx => {
  const header = state.tailHeaderHash as string;
  return {
    inputs: [outsideInput(nonceOf(state.tail.outRef) % 64)],
    referenceInputs: [state.tail.outRef],
    outputs: [
      {
        address: SIM_DA_ATTESTATION_ADDRESS,
        lovelace: 2_000_000n,
        assets: asset([[deployment.daAttestationMint, daatName(header)]]),
        datum: datum(Data.to(header)),
      },
    ],
    mint: asset([[deployment.daAttestationMint, daatName(header)]]),
    nonce: nonceOf(state.tail.outRef),
  };
};

/** The header a stray attestation names: never queued by any branch. */
export const strayHeader = (n: number): string =>
  computeHash28(Buffer.from(`stray-${n.toString()}`)).toString("hex");

/** Mints a DAAT naming a header that was never queued. */
export const strayAttestTx = (
  n: number,
  deployment: WatcherProjectionDeployment = SIM_WATCHER_DEPLOYMENT,
): SimTx => ({
  inputs: [outsideInput(n % 64)],
  outputs: [
    {
      address: SIM_DA_ATTESTATION_ADDRESS,
      lovelace: 2_000_000n,
      assets: asset([[deployment.daAttestationMint, daatName(strayHeader(n))]]),
      datum: datum(Data.to(strayHeader(n))),
    },
  ],
  mint: asset([[deployment.daAttestationMint, daatName(strayHeader(n))]]),
  nonce: 970_000 + n,
});

/** The node output with its DA status set to Attested (a stand-in commitment). */
const attestedNode = (node: SimUtxo, headerHash: string): SimOutput => {
  const assetName = `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`;
  const view = SDK.linkedListDatumToNodeView(
    Data.from(
      (node.output.datum as Buffer).toString("hex"),
      SDK.LinkedListDatum,
    ),
    assetName,
  );
  const data = Data.castFrom(
    view.data,
    SDK.StateQueueNode,
  ) as SDK.StateQueueNode;
  const attested: SDK.StateQueueNode = {
    ...data,
    da_attestation: {
      Attested: {
        commitment_hash: computeHash28(Buffer.from(headerHash, "hex"))
          .toString("hex")
          .padEnd(64, "0"),
      },
    },
  };
  return {
    ...node.output,
    datum: datum(
      Data.to(
        SDK.nodeViewToLinkedListDatum({
          ...view,
          data: Data.castTo(attested, SDK.StateQueueNode),
        }),
        SDK.LinkedListDatum,
      ),
    ),
  };
};

/**
 * Burns a header's DAAT and re-outputs its node (a stand-in Apply):
 * unchanged, or with its DA status set to Attested.
 */
export const applyTx = (
  node: SimUtxo,
  attestation: SimUtxo,
  headerHash: string,
  deployment: WatcherProjectionDeployment = SIM_WATCHER_DEPLOYMENT,
  attestNode = false,
): SimTx => ({
  inputs: [node.outRef, attestation.outRef],
  outputs: [attestNode ? attestedNode(node, headerHash) : node.output],
  mint: new Map([
    [deployment.daAttestationMint, new Map([[daatName(headerHash), -1n]])],
  ]),
  nonce: nonceOf(attestation.outRef) + 1,
});

/** Merges the oldest header: the root takes its link, its node unit burns. */
export const mergeTx = (
  state: QueueState,
  deployment: WatcherProjectionDeployment = SIM_WATCHER_DEPLOYMENT,
): SimTx => {
  const [root, first] = state.ordered as [SimUtxo, SimUtxo];
  const header = headerOf(first, deployment) as string;
  const rootDatum = Data.from(
    (root.output.datum as Buffer).toString("hex"),
    SDK.LinkedListDatum,
  );
  return {
    inputs: [root.outRef, first.outRef],
    referenceInputs: [state.lock.outRef],
    outputs: [
      {
        ...root.output,
        datum: datum(
          Data.to({ ...rootDatum, link: linkOf(first) }, SDK.LinkedListDatum),
        ),
      },
    ],
    mint: new Map([
      [
        deployment.stateQueueMint,
        new Map([[`${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${header}`, -1n]]),
      ],
    ]),
    redeemers: redeemer({
      MergeToConfirmedStateV1: {
        yield_to_ref_input_index: 0n,
        header_node_key: header,
        confirmed_state_input_outref: {
          transactionId: root.outRef.txHash.toString("hex"),
          outputIndex: BigInt(root.outRef.index),
        },
        confirmed_state_output_index: 0n,
        m_settlement_redeemer_index: null,
        merged_block_withdrawals_root: ZERO_ROOT,
        merged_block_forced_transactions_root: ZERO_ROOT,
        merged_block_transactions_root: ZERO_ROOT,
        merged_block_deposits_root: ZERO_ROOT,
        merged_block_transition_trace_root: ZERO_ROOT,
        merged_block_event_to_step_root: ZERO_ROOT,
        merged_block_validation_traces_root: ZERO_ROOT,
        merged_block_withdrawal_count: 0n,
        merged_block_forced_transaction_count: 0n,
        merged_block_l2_transaction_count: 0n,
        merged_block_deposit_count: 0n,
        merged_block_total_event_count: 0n,
        merged_block_transition_step_count: 0n,
        merged_block_validation_trace_count: 0n,
      },
    }),
    nonce: nonceOf(first.outRef) + 2,
  };
};
