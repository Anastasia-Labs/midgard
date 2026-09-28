import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  castConfirmedStateToData,
  castStateQueueNodeToData,
  type DaAvailabilityStateQueueStatus,
  encodeLinkedListNodeView,
  type LinkedListNodeView,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_NODE_MIN_LOVELACE,
  STATE_QUEUE_ROOT_ASSET_NAME,
  type StateQueueNode,
} from "../src/index.js";

// The on-chain twin is `state_queue_node_min_lovelace_v1` in
// onchain/aiken/lib/midgard/state-queue.ak, pinned to the same value by
// `state_queue_node_min_lovelace_is_five_ada` in
// onchain/aiken/validators/state-queue-commit.test.ak.
const ON_CHAIN_STATE_QUEUE_NODE_MIN_LOVELACE_V1 = 5_000_000n;

// The largest integer each bounded header field admits. `block_slot`,
// `min_fee_a` and `min_fee_b` are bounded below 2^64 by the commit arm;
// `start_time` and `end_time` are ledger validity bounds, so 2^64 - 1 is a
// pessimistic stand-in for any POSIX millisecond value.
const MAX_BOUNDED_INT = 2n ** 64n - 1n;
// The count ceilings from onchain/aiken/lib/midgard/ledger-state.ak.
const MAX_PER_KIND_COUNT = 10_000n;
const MAX_VALIDATION_TRACE_COUNT = 20_000n;

const largestHeader = (): StateQueueNode["header"] => {
  const total = 4n * MAX_PER_KIND_COUNT;
  return {
    prevUtxosRoot: h32(0x01),
    utxosRoot: h32(0x02),
    withdrawalsRoot: h32(0x03),
    forcedTransactionsRoot: h32(0x04),
    transactionsRoot: h32(0x05),
    depositsRoot: h32(0x06),
    transitionTraceRoot: h32(0x07),
    eventToStepRoot: h32(0x08),
    validationTracesRoot: h32(0x09),
    withdrawalCount: MAX_PER_KIND_COUNT,
    forcedTransactionCount: MAX_PER_KIND_COUNT,
    l2TransactionCount: MAX_PER_KIND_COUNT,
    depositCount: MAX_PER_KIND_COUNT,
    totalEventCount: total,
    transitionStepCount: total,
    validationTraceCount: MAX_VALIDATION_TRACE_COUNT,
    startTime: MAX_BOUNDED_INT,
    endTime: MAX_BOUNDED_INT,
    blockSlot: MAX_BOUNDED_INT,
    expectedNetworkId: 1n,
    minFeeA: MAX_BOUNDED_INT,
    minFeeB: MAX_BOUNDED_INT,
    prevHeaderHash: h28(0x0a),
    operatorVkey: h28(0x0b),
    protocolVersion: 1n,
  };
};

const STATUSES: Record<string, DaAvailabilityStateQueueStatus> = {
  Unattested: "Unattested",
  Attested: { Attested: { commitment_hash: h32(0x0c) } },
  Challenged: {
    Challenged: {
      commitment_hash: h32(0x0c),
      challenge_asset_name: h32(0x0d),
    },
  },
  Published: { Published: { terminal_commitment: h32(0x0e) } },
};

const coinsPerUtxoByte = BigInt(
  MIDGARD_CONSENSUS_PROFILE.limits.coinsPerUtxoByte,
);

/**
 * The ledger minimum of a state-queue element holding exactly the floor: a
 * mainnet base address with a script payment credential and a key stake
 * credential (the widest shape the anchor-shared payment credential admits),
 * the floor plus the element's NFT, the given inline datum and no reference
 * script.
 */
const elementMinUtxo = (
  datumCbor: string,
  assetNameHex: string,
): { readonly minLovelace: bigint; readonly outputBytes: number } => {
  const address = CML.BaseAddress.new(
    1,
    CML.Credential.new_script(CML.ScriptHash.from_hex(h28(0x12))),
    CML.Credential.new_pub_key(CML.Ed25519KeyHash.from_hex(h28(0x13))),
  ).to_address();
  const multiAsset = CML.MultiAsset.new();
  multiAsset.set(
    CML.ScriptHash.from_hex(h28(0x14)),
    CML.AssetName.from_raw_bytes(Buffer.from(assetNameHex, "hex")),
    1n,
  );
  const output = CML.TransactionOutput.new(
    address,
    CML.Value.new(STATE_QUEUE_NODE_MIN_LOVELACE, multiAsset),
    CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datumCbor)),
  );
  return {
    minLovelace: CML.min_ada_required(output, coinsPerUtxoByte),
    outputBytes: output.to_cbor_bytes().length,
  };
};

/** A linked node at every field's width bound, holding exactly the floor. */
const nodeMinUtxo = (
  status: DaAvailabilityStateQueueStatus,
  provenFraud: string | null,
): { readonly minLovelace: bigint; readonly outputBytes: number } => {
  const nodeKey = h28(0x10);
  const node: StateQueueNode = {
    header: largestHeader(),
    da_attestation: status,
    proven_fraud: provenFraud,
  };
  const view: LinkedListNodeView = {
    key: { Key: { key: nodeKey } },
    next: { Key: { key: h28(0x11) } },
    data: castStateQueueNodeToData(node) as LinkedListNodeView["data"],
  };
  return elementMinUtxo(
    encodeLinkedListNodeView(view),
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + nodeKey,
  );
};

/** The confirmed-state root at its widest, linked or not. */
const rootMinUtxo = (
  linked: boolean,
): { readonly minLovelace: bigint; readonly outputBytes: number } => {
  const view: LinkedListNodeView = {
    key: "Empty",
    next: linked ? { Key: { key: h28(0x11) } } : "Empty",
    data: castConfirmedStateToData({
      headerHash: h28(0x0a),
      prevHeaderHash: h28(0x0b),
      utxoRoot: h32(0x02),
      startTime: MAX_BOUNDED_INT,
      endTime: MAX_BOUNDED_INT,
      protocolVersion: 1n,
    }) as LinkedListNodeView["data"],
  };
  return elementMinUtxo(
    encodeLinkedListNodeView(view),
    STATE_QUEUE_ROOT_ASSET_NAME,
  );
};

describe("state-queue node lovelace floor", () => {
  it("pins the SDK floor to the on-chain constant (two-sided pin)", () => {
    expect(STATE_QUEUE_NODE_MIN_LOVELACE).toBe(5_000_000n);
    expect(STATE_QUEUE_NODE_MIN_LOVELACE).toBe(
      ON_CHAIN_STATE_QUEUE_NODE_MIN_LOVELACE_V1,
    );
  });

  it("uses a 32-byte node NFT name", () => {
    expect((STATE_QUEUE_NODE_ASSET_NAME_PREFIX + h28(0x10)).length / 2).toBe(
      32,
    );
  });

  it("covers the ledger minimum of the largest admissible node", () => {
    const sizes = Object.fromEntries(
      Object.entries(STATUSES).map(([kind, status]) => [
        kind,
        nodeMinUtxo(status, h32(0x0f)),
      ]),
    );
    const largest = sizes.Challenged;
    for (const entry of Object.values(sizes)) {
      expect(entry.minLovelace).toBeLessThanOrEqual(largest.minLovelace);
    }
    expect(largest.minLovelace).toBeLessThanOrEqual(
      STATE_QUEUE_NODE_MIN_LOVELACE,
    );
    // Recorded for the ticket: floor / ledger minimum of the largest node.
    const headroom =
      Number(STATE_QUEUE_NODE_MIN_LOVELACE) / Number(largest.minLovelace);
    console.info(
      `state-queue node floor headroom: coinsPerUtxoByte=${coinsPerUtxoByte.toString()} ` +
        `largestOutputBytes=${largest.outputBytes.toString()} ` +
        `largestMinUtxo=${largest.minLovelace.toString()} ` +
        `floor=${STATE_QUEUE_NODE_MIN_LOVELACE.toString()} ` +
        `ratio=${headroom.toFixed(3)}`,
    );
    expect(headroom).toBeGreaterThan(1);
  });

  it("orders the ledger minimum Challenged > Attested > Unattested", () => {
    const min = (kind: string) =>
      nodeMinUtxo(STATUSES[kind]!, null).minLovelace;
    expect(min("Challenged")).toBeGreaterThan(min("Attested"));
    expect(min("Attested")).toBeGreaterThan(min("Unattested"));
    expect(min("Published")).toBeLessThan(min("Challenged"));
  });

  it("costs more for a proven-fraud mark than without one", () => {
    expect(
      nodeMinUtxo(STATUSES.Challenged!, h32(0x0f)).minLovelace,
    ).toBeGreaterThan(nodeMinUtxo(STATUSES.Challenged!, null).minLovelace);
  });

  it("funds a linked root at the floor", () => {
    // Linking grows the root's datum, so the init builder funds the root at
    // the floor rather than at Lucid's minimum for the unlinked genesis root,
    // and the first commit need not top it up. (The commit arm lets a root
    // anchor gain lovelace, so a root drained to its unlinked minimum by a
    // queue-emptying merge or head removal can still be relinked.)
    expect(rootMinUtxo(true).minLovelace).toBeGreaterThan(
      rootMinUtxo(false).minLovelace,
    );
    expect(rootMinUtxo(true).minLovelace).toBeLessThanOrEqual(
      STATE_QUEUE_NODE_MIN_LOVELACE,
    );
  });
});
