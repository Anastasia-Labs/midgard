/**
 * The fabricatedDeposit, fabricatedWithdrawal and transitionTrace L1 reads
 * over the watcher's L1 follower, as the fault-proof application wires them
 * (`fault-proof-application.create-application.ts`):
 *
 * - the fabricated families' event-history reads (`fetchCurrentHistory`,
 *   which detection, re-admission and step 02 all make) run on a Lucid
 *   instance over the real `L1FollowerProvider`;
 * - the transitionTrace event capture runs on the real follower-backed
 *   fault-proof L1 source (`createWatcherFaultProofL1Source`).
 *
 * The store is the in-memory follower of `follower-user-events-fixture`
 * with the watcher and event projections; only the node is a double, and
 * a tracked-scope read never reaches it. Both reads follow the store
 * through a rewind.
 */
import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type ReleaseL1FinalityPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "@al-ft/midgard-fault-proofs";
import {
  type FabricatedHistoryEnvironment,
  fetchCurrentHistory,
} from "@al-ft/midgard-fault-proofs/test-support/fabricated-history-witness";
import {
  captureTransitionTraceL1Events,
  readFreshTransitionTraceL1Events,
} from "@al-ft/midgard-fault-proofs/test-support/transition-trace-l1-events";
import { L1FollowerProvider } from "@al-ft/midgard-l1-follower/provider";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  getAddressDetails,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import { RELEASE_FINALITY_DEPTH } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import {
  createWatcherFaultProofL1Source,
  WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX,
} from "../../src/l1-follower/fault-proof-l1-source.js";
import { rawUtxo } from "../../src/l1-follower/reads.js";
import { createTxInputsResolver } from "../../src/l1-follower/tx-inputs.js";
import { WATCHER_EMULATOR_HISTORY_RECIPE } from "../support/deployment-authority-fixture.js";
import {
  ELSEWHERE_ADDRESS_HEX,
  type FollowerUserEvents,
  followerUserEventsDeployment,
  forcedOrderTransaction,
  listOrderTransaction,
  openFollowerUserEvents,
  syntheticChain,
  syntheticTransaction,
  type UserEventId,
  userEventId,
} from "../support/follower-user-events-fixture.js";
import {
  buildWatcherOriginFixtureHistoryDeployments,
  INITIALIZATION,
} from "../support/user-event-origin-fixture.make-config.js";

const deployment = followerUserEventsDeployment();
const K = RELEASE_FINALITY_DEPTH + 2;
const NETWORK = "Preprod" as const;
/** The header the capture is for; no proof pins it. */
const HEADER = "07".repeat(28);
const HUB_POLICY = deployment.authority.protocolScriptHashes.hubOracleMint;
const HUB_UNIT = toUnit(HUB_POLICY, SDK.HUB_ORACLE_ASSET_NAME);
const HISTORY = buildWatcherOriginFixtureHistoryDeployments(
  INITIALIZATION.canonicalOneShotOutRef,
  HUB_POLICY,
);
const BOUNDS = WATCHER_EMULATOR_HISTORY_RECIPE.bounds;
const environment = (
  kind: "deposit" | "withdrawal",
): FabricatedHistoryEnvironment => ({
  inlineLimitBytes: BigInt(BOUNDS.inlineLimitBytes),
  maxPayloadBytes: BigInt(BOUNDS.maxPayloadBytes),
  maxPayloadNodes: BigInt(BOUNDS.maxPayloadNodes),
  retentionAddress: HISTORY[kind].retention.spendingScriptAddress,
});

const ID = Object.freeze({
  deposit: userEventId("d1"),
  withdrawal: userEventId("e1"),
  forced: userEventId("f1"),
  neverListed: userEventId("d9"),
});

const RELEASE: VerifiedFraudProofReleaseFinalityPolicy = (() => {
  const policy = {
    confirmationDepth: 2,
    automaticRecoveryMaxDepth:
      K as ReleaseL1FinalityPolicy["automaticRecoveryMaxDepth"],
    deepRollbackPolicy: "automated_rewind_replay_incident-v1",
  } as const satisfies ReleaseL1FinalityPolicy;
  return {
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: "d1".repeat(32),
    blueprintHash: deployment.deploymentIdentity.blueprintHash,
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  };
})();

/**
 * The transitionTrace deployment fields the capture reads, from the
 * synthetic deployment's own history contracts (the watcher binds them
 * from its manifest; the binding is not what this file tests).
 */
const transitionBinding = (headerHash: string) =>
  ({
    network: NETWORK,
    releaseFinality: RELEASE,
    definition: { headerHash },
    resolvedContracts: {
      hubOraclePolicyId: HUB_POLICY,
      contracts: {
        transitionTrace: {
          history: {
            retentionAddresses: {
              deposit: HISTORY.deposit.retention.spendingScriptAddress,
              withdrawal: HISTORY.withdrawal.retention.spendingScriptAddress,
            },
            inlineLimitBytes: BigInt(BOUNDS.inlineLimitBytes),
          },
        },
      },
    },
  }) as unknown as Parameters<
    typeof captureTransitionTraceL1Events
  >[0]["binding"];

/** The node: every query is recorded; only `protocol_params` is ever answered. */
const nodeDouble = () => {
  const queries: string[] = [];
  const transport = {
    query: async (query: { query: string }) => {
      queries.push(query.query);
      throw new Error(`the node was asked ${query.query}`);
    },
    submit: async () => {
      throw new Error("the node was asked to submit");
    },
    hasTx: async () => false,
  } as unknown as L1NodeTransport;
  return { transport, queries };
};

/**
 * The node's ledger at a predecessor, for the follower's input resolution:
 * an output the activation's creating transactions made, else a plain one
 * (a synthetic nonce or wallet input no store holds).
 */
const creatingOutputs = new Map<string, string>(
  INITIALIZATION.creatingTransactions.flatMap(
    ({ transactionCbor }: { transactionCbor: string }) => {
      const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
      const hash = CML.hash_transaction(transaction.body()).to_hex();
      const outputs = transaction.body().outputs();
      return Array.from(
        { length: outputs.len() },
        (_, index) =>
          [
            `${hash}#${index.toString()}`,
            outputs.get(index).to_canonical_cbor_hex(),
          ] as const,
      );
    },
  ),
);
const plainOutput = CML.TransactionOutput.new(
  CML.Address.from_hex(ELSEWHERE_ADDRESS_HEX),
  CML.Value.from_coin(2_000_000n),
).to_canonical_cbor_hex();

const resolveInputs = async (follower: FollowerUserEvents) => {
  const resolver = createTxInputsResolver({
    store: follower.store,
    ledger: async (_point, outRefs) => ({
      kind: "ok",
      outputs: new Map(
        outRefs.map((outRef) => {
          const label = `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;
          return [
            label,
            rawUtxo(
              label,
              CML.TransactionOutput.from_cbor_hex(
                creatingOutputs.get(label) ?? plainOutput,
              ),
            ),
          ] as const;
        }),
      ),
    }),
  });
  try {
    expect(await resolver.step()).toEqual([]);
  } finally {
    await resolver.close();
  }
};

const opened: FollowerUserEvents[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((follower) => follower.close()));
});

const listAddress = (kind: "deposit" | "withdrawal"): string =>
  CML.Address.from_hex(
    deployment.scripts.eventProjection.lists.find((list) => list.kind === kind)!
      .listAddress,
  ).to_bech32(undefined);

/** The list's root, re-output with `next` set to `event`'s key. */
const linkRoot = (root: UTxO, event: UserEventId): string => {
  const node = Data.from(root.datum!, SDK.EventHistoryNode);
  return syntheticTransaction({
    inputs: [`${root.txHash}#${root.outputIndex.toString()}`],
    outputs: [
      {
        addressHex: getAddressDetails(root.address).address.hex,
        lovelace: root.assets.lovelace!,
        units: Object.entries(root.assets).flatMap(([unit, quantity]) =>
          unit === "lovelace"
            ? []
            : [[unit.slice(0, 56), unit.slice(56), quantity] as const],
        ),
        datumCbor: Data.to({ ...node, next: event.key }, SDK.EventHistoryNode),
      },
    ],
  });
};

/**
 * The activation block, then one block listing a deposit and a withdrawal
 * (each root linked to its one Order) and a forced order, then enough
 * empty blocks for inclusion depth.
 */
const listedChain = async () => {
  const follower = await openFollowerUserEvents({ deployment, k: K });
  opened.push(follower);
  const node = nodeDouble();
  const provider = new L1FollowerProvider({
    store: follower.store,
    transport: node.transport,
  });
  const lucid = await Lucid(provider, NETWORK, {
    presetProtocolParameters: PROTOCOL_PARAMETERS_DEFAULT,
  });
  const chain = syntheticChain();
  const activation = chain.next([INITIALIZATION.transactionCbor]);
  await follower.apply(chain.blocks);
  const root = async (kind: "deposit" | "withdrawal") => {
    const roots = (await lucid.utxosAt(listAddress(kind))).filter(
      (utxo) =>
        Data.from(utxo.datum!, SDK.EventHistoryNode).position === "Root",
    );
    expect(roots).toHaveLength(1);
    return roots[0]!;
  };
  const listing = chain.next([
    linkRoot(await root("deposit"), ID.deposit),
    listOrderTransaction(deployment, "deposit", ID.deposit),
    linkRoot(await root("withdrawal"), ID.withdrawal),
    listOrderTransaction(deployment, "withdrawal", ID.withdrawal),
    forcedOrderTransaction(deployment, ID.forced),
  ]);
  chain.empties(2);
  await follower.apply(chain.blocks.slice(1));
  await resolveInputs(follower);
  const source = createWatcherFaultProofL1Source({
    store: follower.store,
    rawReads: follower.rawReads,
    node: node.transport,
    sourceId: `${WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX}follower-family-reads`,
    proofRetention: follower.proofRetention,
  });
  const authority = source.snapshotAuthority({
    releaseFinality: RELEASE,
    observationDepth: "inclusion",
  });
  return { follower, node, lucid, chain, activation, listing, authority };
};

type Listed = Awaited<ReturnType<typeof listedChain>>;

const history = (
  lucid: Listed["lucid"],
  kind: "Deposit" | "Withdrawal",
  event: UserEventId,
) =>
  fetchCurrentHistory({
    lucid,
    network: NETWORK,
    hubOraclePolicyId: HUB_POLICY,
    history: environment(kind === "Deposit" ? "deposit" : "withdrawal"),
    kind,
    id: event.id,
  });

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const transitionEvents = async (listed: Listed) =>
  readFreshTransitionTraceL1Events(
    await captureTransitionTraceL1Events({
      binding: transitionBinding(HEADER),
      authority: listed.authority,
    }),
  );

describe("fabricated* history reads through the follower provider", () => {
  it("open a listed deposit and withdrawal from follower facts, never asking the node", async () => {
    const listed = await listedChain();
    for (const [kind, event, list] of [
      ["Deposit", ID.deposit, "deposit"],
      ["Withdrawal", ID.withdrawal, "withdrawal"],
    ] as const) {
      const current = await history(listed.lucid, kind, event);
      expect(outRefOf(current.hubOracleUtxo)).toBe(deployment.hubOutRef);
      expect(current.witness.kind).toBe("Present");
      expect(current.witness.anchor.key).toBe(event.key);
      expect(current.witness.anchor.utxo.address).toBe(listAddress(list));
      expect(current.captured).toBeDefined();
    }
    expect(listed.node.queries).toEqual([]);
  });

  it("answer an event never listed with its absence witness, and follow a rewind that removes a listed one", async () => {
    const listed = await listedChain();
    const absent = await history(listed.lucid, "Deposit", ID.neverListed);
    expect(absent.witness.kind).toBe("Absent");
    expect(absent.captured).toBeUndefined();
    await listed.follower.rewindTo(listed.activation.point);
    const removed = await history(listed.lucid, "Deposit", ID.deposit);
    expect(removed.witness.kind).toBe("Absent");
    expect(removed.witness.anchor.key).toBeNull();
    expect(listed.node.queries).toEqual([]);
  });

  it("refuse before the activation block brings the hub oracle", async () => {
    const follower = await openFollowerUserEvents({ deployment, k: K });
    opened.push(follower);
    const node = nodeDouble();
    const lucid = await Lucid(
      new L1FollowerProvider({
        store: follower.store,
        transport: node.transport,
      }),
      NETWORK,
      { presetProtocolParameters: PROTOCOL_PARAMETERS_DEFAULT },
    );
    await expect(history(lucid, "Deposit", ID.deposit)).rejects.toThrow(
      /Expected exactly one event history hub oracle UTxO .* found 0/u,
    );
    expect(node.queries).toEqual([]);
  });
});

describe("transitionTrace event capture through the follower source", () => {
  it("captures the hub and every listed event with its history from follower facts", async () => {
    const listed = await listedChain();
    const captured = await transitionEvents(listed);
    expect(outRefOf(captured.hub)).toBe(deployment.hubOutRef);
    expect(captured.hub.assets[HUB_UNIT]).toBe(1n);
    expect(
      captured.events.map((event) => [event.kind, event.assetName]),
    ).toEqual(
      expect.arrayContaining([
        ["deposit", ID.deposit.key],
        ["withdrawal", ID.withdrawal.key],
        ["forcedTransaction", expect.any(String)],
      ]),
    );
    expect(captured.events).toHaveLength(3);
    for (const event of captured.events)
      if (event.kind !== "forcedTransaction")
        expect(event.history.openingCbor).toMatch(/^[0-9a-f]+$/u);
    expect(captured.snapshot.provenance.sourceId).toBe(
      `${WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX}follower-family-reads`,
    );
    expect(listed.node.queries).toEqual([]);
  });

  it("captures no event once a rewind removes the listing block, and refuses with no hub", async () => {
    const listed = await listedChain();
    await listed.follower.rewindTo(listed.activation.point);
    listed.chain.blocks.splice(1);
    listed.chain.empties(2);
    await listed.follower.apply(listed.chain.blocks.slice(1));
    expect((await transitionEvents(listed)).events).toEqual([]);

    const empty = await openFollowerUserEvents({ deployment, k: K });
    opened.push(empty);
    const chain = syntheticChain();
    chain.empties(2);
    await empty.apply(chain.blocks);
    await expect(
      captureTransitionTraceL1Events({
        binding: transitionBinding(HEADER),
        authority: createWatcherFaultProofL1Source({
          store: empty.store,
          rawReads: empty.rawReads,
          node: nodeDouble().transport,
          sourceId: `${WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX}follower-family-reads`,
        }).snapshotAuthority({
          releaseFinality: RELEASE,
          observationDepth: "inclusion",
        }),
      }),
    ).rejects.toThrow(/needs the unique authenticated hub oracle/u);
  });
});
