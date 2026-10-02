import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Point,
  type FraudProofRawL1Transaction,
  type LocalKupmiosVerifiedSpend,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { expect, vi } from "vitest";

import {
  unsafeResolveMergedWatcherStateQueueHeadersForTest as resolveMergedHeaders,
  type WatcherMergedHeaderReaders,
  type WatcherReleasedHeaderProof,
} from "../../src/indexers/authenticated-state-queue-observation.js";
import {
  headerFixture,
  observation,
} from "../fault-proofs/fault-decision-bridge.observation.js";
import { h32 } from "../support/deployment-authority-fixture.js";
import {
  protocolAuthority as authority,
  RELEASE_DEPTH,
  value,
} from "./authenticated-state-queue-observation.fixture.js";

export { authority, RELEASE_DEPTH };

export const policy = authority.protocolScriptHashes.stateQueueMint;

export const ROOT_ASSET = SDK.STATE_QUEUE_ROOT_ASSET_NAME;

export const nodeAsset = (headerHash: string): string =>
  `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`;

export const l1Point = (blockNo: number): FraudProofRawL1Point => {
  const fields = {
    blockHash: h32(blockNo.toString(16).padStart(2, "0")),
    blockNo: blockNo.toString(),
    slot: (blockNo * 10).toString(),
  };
  return Object.freeze({
    ...fields,
    pointId: computeFraudProofRawL1PointId(fields),
  });
};

/** Observation at block 100 with two queued headers over root `16..#0`. */
export const behind = () =>
  observation([headerFixture("01"), headerFixture("02")]);

const outRefParts = (outRef: string) => {
  const [txHash, index] = outRef.split("#") as [string, string];
  return { transactionId: txHash, outputIndex: BigInt(index) };
};

/**
 * A release-final transaction spending `inputs`, paying one state-queue
 * output per entry of `outputs` (an asset name, or null for plain ADA), and
 * minting `mint` under the state-queue policy with `redeemer`.
 */
export const queueTx = (input: {
  inputs: readonly string[];
  outputs: readonly (string | null)[];
  mint: readonly (readonly [string, bigint])[];
  redeemer: string;
  blockNo: number;
  depth?: number;
  /** Varies the fee, and so the hash, of otherwise equal transactions. */
  fee?: bigint;
}): FraudProofRawL1Transaction => {
  const inputs = CML.TransactionInputList.new();
  for (const outRef of input.inputs) {
    const { transactionId, outputIndex } = outRefParts(outRef);
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(transactionId),
        outputIndex,
      ),
    );
  }
  const address = CML.Address.from_bech32(
    credentialToAddress(
      authority.network,
      scriptHashToCredential(authority.protocolScriptHashes.stateQueueSpend),
    ),
  );
  const outputs = CML.TransactionOutputList.new();
  for (const assetName of input.outputs)
    outputs.add(
      CML.TransactionOutput.new(
        address,
        assetName === null
          ? CML.Value.from_coin(2_000_000n)
          : value(policy, assetName),
        undefined,
        undefined,
      ),
    );
  const body = CML.TransactionBody.new(inputs, outputs, input.fee ?? 170_000n);
  const mint = CML.Mint.new();
  for (const [assetName, quantity] of input.mint)
    mint.set(
      CML.ScriptHash.from_hex(policy),
      CML.AssetName.from_hex(assetName),
      quantity,
    );
  body.set_mint(mint);
  const redeemers = CML.LegacyRedeemerList.new();
  redeemers.add(
    CML.LegacyRedeemer.new(
      CML.RedeemerTag.Mint,
      0n,
      CML.PlutusData.from_cbor_hex(input.redeemer),
      CML.ExUnits.new(0n, 0n),
    ),
  );
  const witnessSet = CML.TransactionWitnessSet.new();
  witnessSet.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
  return Object.freeze({
    txHash: CML.hash_transaction(body).to_hex(),
    bodyCbor: body.to_canonical_cbor_hex(),
    witnessSetCbor: witnessSet.to_canonical_cbor_hex(),
    redeemersCbor: witnessSet.redeemers()!.to_canonical_cbor_hex(),
    isValid: true,
    inclusionPoint: l1Point(input.blockNo),
    confirmationDepth: input.depth ?? RELEASE_DEPTH,
    resolvedInputs: Object.freeze([]),
    resolvedReferenceInputs: Object.freeze([]),
  });
};

export type MergeOptions = Readonly<{
  depth?: number;
  headerKey?: string;
  consumed?: string;
  burn?: boolean;
  /** False pays no confirmed-state unit at the redeemer's output index. */
  rootOutput?: boolean;
  /** False leaves the spent root out of the body's inputs. */
  spendsRoot?: boolean;
  fee?: bigint;
}>;

/** The first `build(fee)` over successive fees that `accept` takes. */
export const grind = <T>(
  build: (fee: bigint) => T,
  accept: (value: T) => boolean,
): T => {
  for (let fee = 170_000n; fee < 170_256n; fee += 1n) {
    const value = build(fee);
    if (accept(value)) return value;
  }
  throw new Error("no fee gives the wanted hash order");
};

/**
 * A MergeToConfirmedStateV1 of `headerHash` as the SDK builder shapes it:
 * it spends the root at `rootOutRef` and the head's node at `nodeOutRef`,
 * burns that node and pays the confirmed state then the settlement.
 */
export const mergeTx = (
  rootOutRef: string,
  nodeOutRef: string,
  headerHash: string,
  blockNo: number,
  options: MergeOptions = {},
): FraudProofRawL1Transaction => {
  const root = h32("33");
  return queueTx({
    inputs: [
      options.spendsRoot === false ? `${h32("35")}#0` : rootOutRef,
      nodeOutRef,
    ],
    outputs:
      options.rootOutput === false ? [null, ROOT_ASSET] : [ROOT_ASSET, null],
    mint: [[nodeAsset(headerHash), options.burn === false ? 1n : -1n]],
    redeemer: Data.to(
      {
        MergeToConfirmedStateV1: {
          yield_to_ref_input_index: 0n,
          header_node_key: options.headerKey ?? headerHash,
          confirmed_state_input_outref: outRefParts(
            options.consumed ?? rootOutRef,
          ),
          confirmed_state_output_index: 0n,
          m_settlement_redeemer_index: null,
          merged_block_withdrawals_root: root,
          merged_block_forced_transactions_root: root,
          merged_block_transactions_root: root,
          merged_block_deposits_root: root,
          merged_block_transition_trace_root: root,
          merged_block_event_to_step_root: root,
          merged_block_validation_traces_root: root,
          merged_block_withdrawal_count: 0n,
          merged_block_forced_transaction_count: 0n,
          merged_block_l2_transaction_count: 0n,
          merged_block_deposit_count: 0n,
          merged_block_total_event_count: 0n,
          merged_block_transition_step_count: 0n,
          merged_block_validation_trace_count: 0n,
        },
      },
      SDK.StateQueueRedeemer,
    ),
    blockNo,
    ...(options.depth === undefined ? {} : { depth: options.depth }),
    ...(options.fee === undefined ? {} : { fee: options.fee }),
  });
};

/** The redeemers that remove a queued header, as the builders encode them. */
export const removalRedeemer = {
  timedOutHead: (rootOutRef: string) =>
    Data.to(
      {
        RemoveUnavailableBlockAfterTimeout: {
          yield_to_ref_input_index: 0n,
          unavailable_header_hash: "aa".repeat(28),
          challenge_asset_name: "bb".repeat(32),
          removal_approach: {
            RemoveTimedOutHead: {
              confirmed_state_input_outref: outRefParts(rootOutRef),
              confirmed_state_output_index: 0n,
            },
          },
        },
      },
      SDK.StateQueueRedeemer,
    ),
  lastFraudulent: (anchorOutRef: string, headerHash: string) =>
    Data.to(
      {
        RemoveFraudulentBlockHeader: {
          yield_to_ref_input_index: 0n,
          fraudulent_operator: "09".repeat(28),
          fraudulent_blocks_header_hash: headerHash,
          slashing_approach: {
            SlashActiveOperator: {
              active_operators_redeemer_index: 0n,
              m_fraud_prover_reward_output_index: null,
            },
          },
          fraud_proof_ref_input_index: 0n,
          block_removal_approach: {
            RemoveLastFraudulentBlock: {
              anchor_element_input_outref: outRefParts(anchorOutRef),
              anchor_element_output_index: 0n,
            },
          },
        },
      },
      SDK.StateQueueRedeemer,
    ),
  lastUnattested: (predecessorOutRef: string, headerHash: string) =>
    Data.to(
      {
        RemoveUnattestedBlockAfterTimeout: {
          yield_to_ref_input_index: 0n,
          timed_out_header_hash: headerHash,
          removal_approach: {
            RemoveLastUnattestedBlock: {
              predecessor_input_outref: outRefParts(predecessorOutRef),
              predecessor_output_index: 0n,
            },
          },
        },
      },
      SDK.StateQueueRedeemer,
    ),
} as const;

/**
 * `raw` with every input another transaction of `transactions` produced
 * resolved to that output, as the admitted reader resolves all inputs.
 */
const withResolvedInputs = (
  raw: FraudProofRawL1Transaction,
  transactions: readonly FraudProofRawL1Transaction[],
): FraudProofRawL1Transaction => {
  const produced = new Map<string, string>();
  for (const { txHash, bodyCbor } of transactions) {
    const outputs = CML.TransactionBody.from_cbor_hex(bodyCbor).outputs();
    for (let index = 0; index < outputs.len(); index += 1)
      produced.set(
        `${txHash}#${index.toString()}`,
        outputs.get(index).to_cbor_hex(),
      );
  }
  const inputs = CML.TransactionBody.from_cbor_hex(raw.bodyCbor).inputs();
  const resolvedInputs: FraudProofRawL1Transaction["resolvedInputs"][number][] =
    [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    const outRef = `${input.transaction_id().to_hex()}#${input.index().toString()}`;
    const outputCbor = produced.get(outRef);
    if (outputCbor !== undefined)
      resolvedInputs.push({
        outRef,
        outputCbor,
        datumCbor: null,
        referenceScriptCbor: null,
      });
  }
  return Object.freeze({
    ...raw,
    resolvedInputs: Object.freeze(resolvedInputs),
  });
};

/**
 * Local Kupo/Ogmios readers over fixed spends: each entry names the outref a
 * transaction spends, so a transaction spending two queue elements appears
 * twice.
 */
export const chain = (
  spends: readonly (readonly [string, FraudProofRawL1Transaction])[],
  boundaryBlockNo = 200,
) => {
  const boundary = l1Point(boundaryBlockNo);
  const transactions = spends.map(([, raw]) => raw);
  const bySpentOutRef = new Map(
    spends.map(
      ([outRef, raw]) =>
        [outRef, withResolvedInputs(raw, transactions)] as const,
    ),
  );
  const readers = {
    readBoundary: vi.fn(async () =>
      Object.freeze({
        kupoCheckpoint: boundary,
        ogmiosTip: l1Point(boundaryBlockNo + RELEASE_DEPTH - 1),
        confirmationDepth: RELEASE_DEPTH,
      }),
    ),
    readOutRefs: vi.fn(
      async (outRefs: readonly string[], point: FraudProofRawL1Point) => {
        expect(point).toEqual(boundary);
        const found: LocalKupmiosVerifiedSpend[] = [];
        for (const outRef of outRefs) {
          const raw = bySpentOutRef.get(outRef);
          if (raw !== undefined)
            found.push({
              outRef,
              spendingTxHash: raw.txHash,
              spendPoint: raw.inclusionPoint,
            });
        }
        return Object.freeze({ outputs: [], spends: found });
      },
    ),
    readTransaction: vi.fn(async (txHash: string) => {
      const raw = [...bySpentOutRef.values()].find(
        (tx) => tx.txHash === txHash,
      );
      if (raw === undefined) throw new Error("unknown transaction");
      return raw;
    }),
  } satisfies WatcherMergedHeaderReaders;
  return readers;
};

export const resolveReleased = async (
  readers: WatcherMergedHeaderReaders,
  current = behind(),
): Promise<ReadonlyMap<string, WatcherReleasedHeaderProof>> =>
  await resolveMergedHeaders({ observation: current, authority, readers });

/** Merged headers only, as `{ headerHash, mergeTransactionHash }`. */
export const resolve = async (
  readers: WatcherMergedHeaderReaders,
  current = behind(),
) =>
  [...(await resolveReleased(readers, current)).values()].flatMap((proof) =>
    "mergeTransactionHash" in proof
      ? [
          {
            headerHash: proof.headerHash,
            mergeTransactionHash: proof.mergeTransactionHash,
          },
        ]
      : [],
  );
