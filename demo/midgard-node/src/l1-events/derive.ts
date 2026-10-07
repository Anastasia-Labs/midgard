/**
 * The node event projection's S3 derivation (plan §5.4, §5.5 P2, §7.2).
 *
 * Per qualifying valid tx, in block order, for each event list:
 * - a burn of one list token whose key is a live event retires it (the
 *   reason comes from the retirement observer's redeemer only, §5.4);
 * - an Order output at the list address whose key is not a live event is
 *   an admission: a key already in `l1_event_keys` is refused (never
 *   reused), any other is admitted and its key enters `l1_event_keys`.
 *
 * Every lookup is by key or by outref, so a block costs O(r) in its
 * qualifying txs, never in the number of stored events (B2). A pure
 * function of the block, the stored facts and the config: no clock, no
 * network. The rewind of everything written here is the registry's.
 */
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  type DerivationContext,
  type DerivationHook,
  encodeOutRef,
  type OutputSummary,
  type OutRef,
  type RedeemerPurpose,
  type RedeemerSummary,
  type TxSummary,
  type WithdrawalSummary,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, datumToHash, type UTxO } from "@lucid-evolution/lucid";

import type { EventListConfig, EventProjectionConfig } from "./config.js";
import { EVENTS_TABLE, REFUSALS_TABLE, RETIREMENTS_TABLE } from "./schema.js";

export type RetirementReason = "absorbed" | "payout_initialized" | "refunded";

const hex = (bytes: Buffer): string => bytes.toString("hex");

const lucidUtxo = (outRef: OutRef, output: OutputSummary): UTxO => {
  const assets: Record<string, bigint> = { lovelace: output.lovelace };
  for (const [policy, names] of output.assets)
    for (const [name, quantity] of names) assets[policy + name] = quantity;
  return {
    txHash: hex(outRef.txHash),
    outputIndex: outRef.index,
    address: hex(output.address),
    assets,
    ...(output.datum === null ? {} : { datum: hex(output.datum) }),
    ...(output.datumHash === null ? {} : { datumHash: hex(output.datumHash) }),
  };
};

/** Ledger order of reward accounts: network, script before key, then hash. */
const compareRewardAccounts = (a: Buffer, b: Buffer): number => {
  const network = ((a[0] ?? 0) & 0x0f) - ((b[0] ?? 0) & 0x0f);
  if (network !== 0) return network;
  const script = (((b[0] ?? 0) >> 4) & 1) - (((a[0] ?? 0) >> 4) & 1);
  if (script !== 0) return script;
  return Buffer.compare(a.subarray(1), b.subarray(1));
};

const PURPOSE_ORDER: readonly RedeemerPurpose[] = [
  "spend",
  "mint",
  "cert",
  "reward",
  "voting",
  "proposing",
];

/** Redeemers in ledger pointer order (purpose, then index). */
export const ledgerOrderedRedeemers = (
  redeemers: readonly RedeemerSummary[],
): RedeemerSummary[] =>
  [...redeemers].sort(
    (a, b) =>
      PURPOSE_ORDER.indexOf(a.purpose) - PURPOSE_ORDER.indexOf(b.purpose) ||
      a.index - b.index,
  );

/** The redeemer of the zero withdrawal from `scriptHash` on `networkId`, if exactly one. */
export const zeroWithdrawalRedeemer = (
  tx: Pick<TxSummary, "withdrawals" | "redeemers">,
  scriptHash: string,
  networkId: number,
): Readonly<{ redeemer: RedeemerSummary; ordinal: number }> | null => {
  const account = Buffer.concat([
    Buffer.of(0xf0 | networkId),
    Buffer.from(scriptHash, "hex"),
  ]);
  const sorted: WithdrawalSummary[] = [...tx.withdrawals].sort((a, b) =>
    compareRewardAccounts(a.rewardAccount, b.rewardAccount),
  );
  const index = sorted.findIndex((w) => w.rewardAccount.equals(account));
  if (index < 0 || sorted[index]!.amount !== 0n) return null;
  const ordered = ledgerOrderedRedeemers(tx.redeemers);
  const ordinal = ordered.findIndex(
    (r) => r.purpose === "reward" && r.index === index,
  );
  return ordinal < 0 ? null : { redeemer: ordered[ordinal]!, ordinal };
};

const REASONS = {
  AbsorbDeposit: "absorbed",
  InitializeWithdrawalPayout: "payout_initialized",
} as const;

/** The retirement observer's reason and witness, or nulls when it is absent or unreadable. */
export const retirementEvidence = (
  tx: TxSummary,
  list: EventListConfig,
  networkId: number,
): Readonly<{
  reason: RetirementReason | null;
  observerRedeemerIndex: number | null;
  witnessCbor: Buffer | null;
}> => {
  const observed = zeroWithdrawalRedeemer(
    tx,
    list.retirementScriptHash,
    networkId,
  );
  if (observed === null)
    return { reason: null, observerRedeemerIndex: null, witnessCbor: null };
  try {
    const { witness } = Data.from(
      hex(observed.redeemer.data),
      SDK.EventHistoryRetirementArgs,
    );
    return {
      reason:
        typeof witness.purpose === "string"
          ? REASONS[witness.purpose]
          : "refunded",
      observerRedeemerIndex: observed.ordinal,
      witnessCbor: Buffer.from(
        Data.to(witness, SDK.EventHistoryRetirementWitness),
        "hex",
      ),
    };
  } catch {
    return {
      reason: null,
      observerRedeemerIndex: observed.ordinal,
      witnessCbor: null,
    };
  }
};

/** The admitted event's immutable content, byte-equal to the old journal's. */
export type AdmittedEvent = Readonly<{
  key: string;
  idCbor: string;
  inclusionTime: bigint;
  factsCbor: string;
  payloadCbor: string;
  originalAssetsCbor: string;
}>;

/**
 * Opens an Order output exactly as the SDK's witness capture does, with an
 * external payload read from `retained` (the referenced retention output).
 * Throws when the output does not authenticate as an Order of this list.
 */
export const openOrder = (
  utxo: UTxO,
  list: EventListConfig,
  retained: readonly UTxO[],
): AdmittedEvent | "not_an_order" => {
  if (utxo.datum == null) throw new Error("list output has no inline datum");
  const node = Data.from(utxo.datum, SDK.EventHistoryNode);
  if (
    node.position === "Root" ||
    node.payload === "RootContent" ||
    !("Order" in node.payload)
  )
    return "not_an_order";
  const facts = node.payload.Order.facts;
  let rawPayload: string;
  let retainedDataUtxo: UTxO | undefined;
  if ("Inline" in facts.location) {
    rawPayload = plutusConstrFieldCbor(utxo.datum, [3, 0, 2, 0]);
  } else {
    const storage = facts.location.External.storage_datum_hash;
    retainedDataUtxo = retained.find(
      (candidate) =>
        candidate.datum != null &&
        datumToHash(
          aikenSerialisedPlutusDataCborPreservingMapOrder(candidate.datum),
        ) === storage,
    );
    if (retainedDataUtxo?.datum == null)
      throw new Error("external order references no retention datum");
    rawPayload = plutusConstrFieldCbor(retainedDataUtxo.datum, [1]);
  }
  const payloadCbor =
    aikenSerialisedPlutusDataCborPreservingMapOrder(rawPayload);
  const captured = SDK.captureEventHistoryWitness(
    {
      kind: "Present",
      anchor: { utxo, node, key: node.position.Key[0] },
      payload: Data.from(payloadCbor, SDK.EventHistoryPayload),
      payloadCbor,
      ...(retainedDataUtxo === undefined ? {} : { retainedDataUtxo }),
    },
    list.policyId,
    list.kind === "deposit" ? "Deposit" : "Withdrawal",
  );
  return {
    key: node.position.Key[0],
    idCbor: Data.to(captured.commitment.event_id, SDK.OutputReference),
    inclusionTime: captured.commitment.inclusion_time,
    factsCbor: captured.factsCbor,
    payloadCbor,
    originalAssetsCbor: Data.to(captured.originalAssets, SDK.Value),
  };
};

type Context = DerivationContext;

const isLive = async (
  { tx }: Context,
  kind: string,
  key: Buffer,
): Promise<boolean> =>
  (
    await tx.query(
      `SELECT 1 AS live FROM ${EVENTS_TABLE} WHERE kind = ? AND event_key = ? AND retired_slot IS NULL`,
      [kind, key],
    )
  ).length > 0;

const keyKnown = async (
  { tx }: Context,
  kind: string,
  key: Buffer,
): Promise<boolean> =>
  (
    await tx.query(
      "SELECT 1 AS known FROM l1_event_keys WHERE kind = ? AND key = ?",
      [kind, key],
    )
  ).length > 0;

const refuse = async (
  context: Context,
  at: Readonly<{ tx: TxSummary; outRef: OutRef }>,
  kind: string,
  key: Buffer,
  reason: "retired_key" | "malformed",
  detail: string,
): Promise<void> => {
  await context.tx.query(
    `INSERT INTO ${REFUSALS_TABLE} (slot, tx_hash, output_index, kind, event_key, reason, detail) VALUES (?, ?, ?, ?, ?, ?, ?)`,
    [
      context.block.point.slot,
      at.outRef.txHash,
      at.outRef.index,
      kind,
      key,
      reason,
      detail.slice(0, 500),
    ],
  );
};

const retire = async (
  context: Context,
  tx: TxSummary,
  list: EventListConfig,
  key: Buffer,
  networkId: number,
): Promise<void> => {
  const { block } = context;
  const spent = (
    await context.tx.query(
      `SELECT o.tx_hash, o.output_index FROM l1_output_assets a
        JOIN l1_outputs o ON o.tx_hash = a.tx_hash AND o.output_index = a.output_index
        WHERE a.policy_id = ? AND a.asset_name = ? AND o.spent_tx = ?`,
      [Buffer.from(list.policyId, "hex"), key, tx.hash],
    )
  )[0];
  if (spent === undefined)
    throw new Error(
      `retirement of ${list.kind} ${hex(key)} in ${hex(tx.hash)} spends no stored Order`,
    );
  const evidence = retirementEvidence(tx, list, networkId);
  await context.tx.query(
    `UPDATE ${EVENTS_TABLE} SET retired_slot = ? WHERE kind = ? AND event_key = ? AND retired_slot IS NULL`,
    [block.point.slot, list.kind, key],
  );
  await context.tx.query(
    `INSERT INTO ${RETIREMENTS_TABLE} (kind, event_key, retired_slot, retirement_tx_hash, retirement_tx_index, retired_block_hash, retired_height, order_tx_hash, order_output_index, reason, observer_redeemer_index, witness_cbor) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
    [
      list.kind,
      key,
      block.point.slot,
      tx.hash,
      tx.index,
      block.point.hash,
      block.height,
      Buffer.from(spent.tx_hash as Uint8Array),
      Number(spent.output_index),
      evidence.reason,
      evidence.observerRedeemerIndex,
      evidence.witnessCbor,
    ],
  );
};

const retainedOutputs = async (
  context: Context,
  tx: TxSummary,
  list: EventListConfig,
): Promise<UTxO[]> => {
  const out: UTxO[] = [];
  for (const outRef of tx.referenceInputs) {
    const row = (
      await context.tx.query(
        "SELECT address, datum FROM l1_outputs WHERE tx_hash = ? AND output_index = ?",
        [outRef.txHash, outRef.index],
      )
    )[0];
    if (row === undefined || row.datum === null || row.datum === undefined)
      continue;
    if (hex(Buffer.from(row.address as Uint8Array)) !== list.retentionAddress)
      continue;
    out.push({
      txHash: hex(outRef.txHash),
      outputIndex: outRef.index,
      address: list.retentionAddress,
      assets: {},
      datum: hex(Buffer.from(row.datum as Uint8Array)),
    });
  }
  return out;
};

const admit = async (
  context: Context,
  tx: TxSummary,
  list: EventListConfig,
  created: Readonly<{ outRef: OutRef; output: OutputSummary }>,
  key: Buffer,
): Promise<void> => {
  if (await isLive(context, list.kind, key)) return; // a continuation
  const at = { tx, outRef: created.outRef };
  let event: AdmittedEvent | "not_an_order";
  try {
    if (created.output.scriptRef !== null)
      throw new Error("list output carries a reference script");
    event = openOrder(
      lucidUtxo(created.outRef, created.output),
      list,
      created.output.datum === null
        ? []
        : await retainedOutputs(context, tx, list),
    );
  } catch (error) {
    await refuse(context, at, list.kind, key, "malformed", String(error));
    return;
  }
  if (event === "not_an_order") return; // a filler or the root
  if (await keyKnown(context, list.kind, key)) {
    await refuse(
      context,
      at,
      list.kind,
      key,
      "retired_key",
      "key already used",
    );
    return;
  }
  const { block } = context;
  await context.tx.query(
    "INSERT INTO l1_event_keys (kind, key, origin_outref, first_canonical_slot) VALUES (?, ?, ?, ?)",
    [list.kind, key, encodeOutRef(created.outRef), block.point.slot],
  );
  await context.tx.query(
    `INSERT INTO ${EVENTS_TABLE} (kind, event_key, event_id, inclusion_time, facts_cbor, payload_cbor, original_assets_cbor, admission_tx_hash, admission_output_index, admission_tx_index, admitted_block_hash, admitted_height, admitted_slot, retired_slot) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL)`,
    [
      list.kind,
      key,
      Buffer.from(event.idCbor, "hex"),
      event.inclusionTime.toString(),
      Buffer.from(event.factsCbor, "hex"),
      Buffer.from(event.payloadCbor, "hex"),
      Buffer.from(event.originalAssetsCbor, "hex"),
      created.outRef.txHash,
      created.outRef.index,
      tx.index,
      block.point.hash,
      block.height,
      block.point.slot,
    ],
  );
};

/** The S3 derivation of the node's event lists, events and key set. */
export const eventDerivation = (
  config: EventProjectionConfig,
): DerivationHook => ({
  name: "node_l1_events",
  // It also inserts into the follower's class A `l1_event_keys`, whose
  // rewind is the follower's own (§5.4); only D-t tables are listed here.
  writes: [EVENTS_TABLE, RETIREMENTS_TABLE, REFUSALS_TABLE],
  apply: async (context) => {
    for (const { tx, created } of context.qualified) {
      if (!tx.isValid) continue;
      for (const list of config.lists) {
        for (const [name, quantity] of tx.mint.get(list.policyId) ?? []) {
          if (quantity !== -1n || name.length !== 64) continue;
          const key = Buffer.from(name, "hex");
          if (await isLive(context, list.kind, key))
            await retire(context, tx, list, key, config.networkId);
        }
        for (const entry of created) {
          if (hex(entry.output.address) !== list.listAddress) continue;
          const names = entry.output.assets.get(list.policyId);
          if (names === undefined) continue; // a donation without the list token
          for (const [name] of names)
            if (name.length === 64)
              await admit(context, tx, list, entry, Buffer.from(name, "hex"));
        }
      }
    }
  },
});
