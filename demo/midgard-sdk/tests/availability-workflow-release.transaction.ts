import type { AvailabilityOperationRecord } from "@al-ft/midgard-core/availability-operation-journal";
import { CML } from "@lucid-evolution/lucid";

import {
  availabilityResponseGeometry,
  buildDaAvailabilityCommitment,
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  daAvailabilityChallengeAssetName,
  daAvailabilityResponseDeadline,
  encodeDaAvailabilityChallengeRecord,
} from "../src/availability-challenge.js";
import {
  type DaAvailabilityForeignSpendReaders,
  resolveDaAvailabilityWorkflowRelease,
} from "../src/availability-challenge-operation.js";
import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "../src/linked-list.js";

/**
 * P20: a watcher's challenge workflow row is released only on a finalized,
 * verified transaction that burns the header's queue node or closes its
 * challenge, found by walking the node chain from the watcher's own Open.
 * The transactions here are synthetic; each carries only what the walk reads
 * (inputs, outputs by asset, the record datum and the mint).
 */
const QUEUE = "a1".repeat(28);

export const AVAILABILITY = "b2".repeat(28);

export const HEADER = "22".repeat(28);

export const DESCENDANT = "23".repeat(28);

export const SECOND_DESCENDANT = "24".repeat(28);

export const APPENDED = "25".repeat(28);

export const MIN_DEPTH = 30;

export const BOUNDARY = 10_000;

export const nodeUnit = (header: string) =>
  QUEUE + STATE_QUEUE_NODE_ASSET_NAME_PREFIX + header;

export const CHALLENGE_ASSET = daAvailabilityChallengeAssetName({
  transactionId: "99".repeat(32),
  outputIndex: 0n,
});

export const RECORD_DATUM = encodeDaAvailabilityChallengeRecord({
  commitment: buildDaAvailabilityCommitment({
    deploymentIdentity: "11".repeat(28),
    headerHash: HEADER,
    payload: Uint8Array.from({ length: 1024 }, (_, i) => i % 256),
    responseGeometry: availabilityResponseGeometry(
      DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
    ),
  }),
  challenge_asset_name: CHALLENGE_ASSET,
  challenger: "33".repeat(28),
  opened_at: 1_000n,
  response_deadline: daAvailabilityResponseDeadline({
    payloadByteLength: 1024,
    openedAt: 1_000n,
  }),
});

const ADDRESS = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(CML.Ed25519KeyHash.from_hex("44".repeat(28))),
).to_address();

type Output = Readonly<{
  assets?: Readonly<Record<string, bigint>>;
  datum?: string;
}>;

type Synthetic = Readonly<{ id: string; cbor: string }>;

const split = (unit: string) =>
  [
    CML.ScriptHash.from_hex(unit.slice(0, 56)),
    CML.AssetName.from_hex(unit.slice(56)),
  ] as const;

let nonce = 0;

export const transaction = (input: {
  inputs: readonly string[];
  outputs: readonly Output[];
  mint?: Readonly<Record<string, bigint>>;
  valid?: boolean;
}): Synthetic => {
  const inputs = CML.TransactionInputList.new();
  for (const ref of input.inputs) {
    const [txHash, index] = ref.split("#");
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(txHash!),
        BigInt(index!),
      ),
    );
  }
  const outputs = CML.TransactionOutputList.new();
  for (const output of input.outputs) {
    const multiAsset = CML.MultiAsset.new();
    for (const [unit, quantity] of Object.entries(output.assets ?? {}))
      multiAsset.set(...split(unit), quantity);
    outputs.add(
      CML.TransactionOutput.new(
        ADDRESS,
        CML.Value.new(2_000_000n, multiAsset),
        output.datum === undefined
          ? undefined
          : CML.DatumOption.new_datum(
              CML.PlutusData.from_cbor_hex(output.datum),
            ),
      ),
    );
  }
  // A distinct fee keeps otherwise identical bodies distinct.
  const body = CML.TransactionBody.new(inputs, outputs, BigInt(++nonce));
  if (input.mint !== undefined) {
    const mint = CML.Mint.new();
    for (const [unit, quantity] of Object.entries(input.mint))
      mint.set(...split(unit), quantity);
    body.set_mint(mint);
  }
  return {
    id: CML.hash_transaction(body).to_hex(),
    cbor: CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      input.valid ?? true,
      undefined,
    ).to_cbor_hex(),
  };
};

export const ref = (tx: Synthetic, index: number) =>
  `${tx.id}#${index.toString()}`;

// The Open: the record (with the challenge asset it mints) and the header's
// node continuing as Challenged. Real Opens put the record first; the walk
// finds both by asset.
export const openTx = (
  outputs: readonly Output[] = [
    { assets: { [AVAILABILITY + CHALLENGE_ASSET]: 1n }, datum: RECORD_DATUM },
    { assets: { [nodeUnit(HEADER)]: 1n } },
  ],
  mint: Readonly<Record<string, bigint>> = {
    [AVAILABILITY + CHALLENGE_ASSET]: 1n,
  },
) =>
  transaction({
    inputs: [`${"f0".repeat(32)}#0`, `${"f1".repeat(32)}#0`],
    outputs,
    mint,
  });

export const OPEN = openTx();

export const Q0 = ref(OPEN, 1);

export const R0 = ref(OPEN, 0);

export const openRecord = (
  open: Synthetic = OPEN,
  state: AvailabilityOperationRecord["state"] = "confirmed",
  overrides: Partial<AvailabilityOperationRecord["intent"]> = {},
): AvailabilityOperationRecord => ({
  intent: {
    id: "open-intent",
    deploymentIdentity: "d1".repeat(32),
    actor: "a0".repeat(28),
    headerHash: HEADER,
    action: "open",
    signedCbor: open.cbor,
    txHash: open.id,
    spentOutRefs: [`${"f0".repeat(32)}#0`, `${"f1".repeat(32)}#0`],
    collateralOutRefs: [],
    expectedOutRefs: [],
    validUntilSlot: 0,
    completesWorkflow: false,
    ...overrides,
  },
  state,
  inclusionPoint: state === "confirmed" ? "1:ab" : null,
  detail: null,
});

// The canonical chain as the readers report it: which transaction spends each
// outRef, at which block. Kupo may also name a spender whose bytes disagree.
export type Spend = Readonly<{ tx: Synthetic; blockNo: number }>;

export const chain = (
  spends: Readonly<Record<string, Spend>>,
  boundary = BOUNDARY,
): DaAvailabilityForeignSpendReaders => {
  const byId = new Map(
    Object.values(spends).map((spend) => [spend.tx.id, spend]),
  );
  return {
    readBoundary: async () => ({ pointId: "boundary", blockNo: boundary }),
    fetchSpend: async ({ txHash, outputIndex }) => {
      const spend = spends[`${txHash}#${outputIndex.toString()}`];
      return spend === undefined
        ? undefined
        : {
            transactionId: spend.tx.id,
            point: { slot: spend.blockNo * 20, blockHash: spend.tx.id },
          };
    },
    fetchAncestor: async (slot) => ({ slot: slot - 1, blockHash: "00" }),
    readTransaction: async ({ point, txHash }) => {
      const spend = byId.get(txHash);
      return spend === undefined
        ? undefined
        : {
            txHash,
            point: { ...point, blockNo: spend.blockNo },
            cbor: spend.tx.cbor,
          };
    },
  };
};

export const release = (
  readers: DaAvailabilityForeignSpendReaders,
  record: AvailabilityOperationRecord = openRecord(),
) => resolveDaAvailabilityWorkflowRelease(readers, record, HEADER, MIN_DEPTH);

export const DEEP = BOUNDARY - MIN_DEPTH;

// A Close: spends the node and the record, burns the record's assets, mints
// nothing under the queue policy; the node continues as Published.
export const closeOf = (node: string, valid = true) =>
  transaction({
    inputs: [R0, node],
    outputs: [{ assets: { [nodeUnit(HEADER)]: 1n } }],
    mint: { [AVAILABILITY + CHALLENGE_ASSET]: -1n },
    valid,
  });

// A Timeout with no descendant: spends the record and burns the header's node.
export const timeoutOf = (node: string) =>
  transaction({
    inputs: [R0, node],
    outputs: [{}],
    mint: {
      [AVAILABILITY + CHALLENGE_ASSET]: -1n,
      [nodeUnit(HEADER)]: -1n,
    },
  });

// A Timeout with a descendant: spends the record, burns the descendant's node,
// and the header's node continues.
export const timeoutWithDescendantOf = (node: string) =>
  transaction({
    inputs: [R0, node, `${"d0".repeat(32)}#0`],
    outputs: [{ assets: { [nodeUnit(HEADER)]: 1n } }, {}],
    mint: {
      [AVAILABILITY + CHALLENGE_ASSET]: -1n,
      [nodeUnit(DESCENDANT)]: -1n,
    },
  });
