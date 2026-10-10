/**
 * The node §8.4 predicates of the families that settle events or move the
 * node's own funds and certificates: payouts, reference-script publication,
 * sweep and funding, and stake registrations. Each reads the follower's
 * facts in the predicate's store transaction (`Read`).
 */
import {
  credentialsRegisteredBy,
  decodeTransaction,
  landedStakeRegistrationsIn,
  liveUtxosIn,
  type OutputSummary,
  readIntentIn,
} from "@al-ft/midgard-l1-follower";
import { eventKeyOfId } from "@al-ft/midgard-l1-follower/events";

import {
  allInputsLive,
  count,
  keyRest,
  type Read,
} from "./l1-follower.intent-predicates.read.js";

/**
 * Settlement and reserve payouts; the content reference is the settled
 * event's id CBOR. Absorb and initialize retire the list event, so the
 * event must still be listed (its admission identity in the key set) and
 * not retired. Fund and conclude spend the payout the initialize created
 * (the event is retired by then): every input must still be a live fact,
 * so no other transaction settled the payout. Key: `...:<step>`.
 */
export const payout = async (read: Read): Promise<boolean> => {
  const step = read.state.intent.workflowKey.split(":").at(-1);
  const eventId = read.state.intent.contentRef;
  if (eventId === null) throw new Error("a payout intent names no event");
  if (step === "absorb" || step === "absorb_deposit" || step === "initialize")
    return (
      (await count(
        read.tx,
        `SELECT count(*) AS n FROM node_l1_events e JOIN l1_event_keys k
          ON k.kind = e.kind AND k.key = e.event_key
          WHERE e.kind = ? AND e.event_key = ? AND e.event_id = ?
            AND e.retired_slot IS NULL`,
        [
          step === "initialize" ? "withdrawal" : "deposit",
          eventKeyOfId(eventId),
          eventId,
        ],
      )) > 0
    );
  if (step !== "fund" && step !== "add_funds" && step !== "conclude")
    throw new Error(`unknown payout step ${step ?? ""}`);
  return allInputsLive(read);
};

/** The intent's own journaled transaction bytes. */
const ownTxCbor = async (read: Read): Promise<Buffer> => {
  const intent = await readIntentIn(
    read.tx,
    read.dialect,
    read.state.intent.txHash,
  );
  if (intent === null) throw new Error("the intent left the journal");
  return intent.txCbor;
};

/** Some live output other than the intent's own carries the reference script `hash`. */
const scriptLiveElsewhere = async (
  read: Read,
  hash: Buffer,
): Promise<boolean> =>
  (await count(
    read.tx,
    "SELECT count(*) AS n FROM l1_outputs WHERE script_ref_hash = ? AND spent_slot IS NULL AND tx_hash <> ?",
    [hash, read.state.intent.txHash],
  )) > 0;

const someScriptUnpublished = async (
  read: Read,
  hashes: readonly Buffer[],
): Promise<boolean> => {
  for (const hash of hashes)
    if (!(await scriptLiveElsewhere(read, hash))) return true;
  return false;
};

/** Publication: some script it publishes is not yet live at a reference output of another tx. */
export const referencePublication = async (read: Read): Promise<boolean> => {
  const scripts = decodeTransaction(await ownTxCbor(read)).outputs.flatMap(
    (output) => (output.scriptRef === null ? [] : [output.scriptRef.hash]),
  );
  if (scripts.length === 0)
    throw new Error("a reference publication publishes no script");
  return someScriptUnpublished(read, scripts);
};

/** Sweep: every reference-script output it spends is still live. */
export const referenceSweep = async (read: Read): Promise<boolean> => {
  const live = await liveUtxosIn(read.tx, read.dialect, {
    by: "outref",
    outRefs: read.state.intent.inputs,
  });
  if (live.kind !== "ok") throw new Error(`sweep inputs: ${live.kind}`);
  const swept = live.utxos.filter((utxo) => utxo.output.scriptRef !== null);
  return (
    swept.length > 0 && live.utxos.length === read.state.intent.inputs.length
  );
};

/** A script hash's length: the content reference of a publication funding step concatenates them. */
const SCRIPT_HASH_BYTES = 28;

const isPlain = (output: OutputSummary): boolean =>
  output.assets.size === 0 &&
  output.datum === null &&
  output.datumHash === null &&
  output.scriptRef === null;

/**
 * Reference-script funding (E2): the target is in the intent, set at record
 * time, and the intent is wanted while the facts show it unreached.
 *
 * - Working capital, key `reference_funding:<scope>:<address hex>:<lovelace>`:
 *   the plain (ADA-only, no datum, no script) live outputs at the
 *   reference-script address sum to less than the target balance.
 * - A publication's funding step, key `reference_publication:<step>`, content
 *   reference the 28-byte hashes of the scripts it funds, concatenated: one
 *   of them is not yet live at a reference output.
 */
export const referenceFunding = async (read: Read): Promise<boolean> => {
  const { workflowKey, contentRef } = read.state.intent;
  if (workflowKey.startsWith("reference_publication:")) {
    if (
      contentRef === null ||
      contentRef.length === 0 ||
      contentRef.length % SCRIPT_HASH_BYTES !== 0
    )
      throw new Error(
        `publication funding ${workflowKey} names no script hashes`,
      );
    const hashes = Array.from(
      { length: contentRef.length / SCRIPT_HASH_BYTES },
      (_, i) =>
        contentRef.subarray(i * SCRIPT_HASH_BYTES, (i + 1) * SCRIPT_HASH_BYTES),
    );
    return someScriptUnpublished(read, hashes);
  }
  const parts = keyRest(read.state, "reference_funding:").split(":");
  const [address, target] = parts.slice(-2);
  if (
    parts.length < 3 ||
    !/^[0-9a-f]+$/u.test(address ?? "") ||
    !/^\d+$/u.test(target ?? "")
  )
    throw new Error(
      `working-capital funding ${workflowKey} names no address and target`,
    );
  const live = await liveUtxosIn(read.tx, read.dialect, {
    by: "address",
    address: Buffer.from(address!, "hex"),
  });
  if (live.kind !== "ok")
    throw new Error(`reference-script wallet: ${live.kind}`);
  const balance = live.utxos
    .filter((utxo) => isPlain(utxo.output))
    .reduce((sum, utxo) => sum + utxo.output.lovelace, 0n);
  return balance < BigInt(target!);
};

/**
 * Stake registration (E1: a script reward account, the PHAS membership
 * reward account): wanted while some credential its own certificates
 * register is not shown registered by the latest certificate on it among
 * the canonical landed transactions the store holds. With no such
 * certificate (the credential's state predates the facts, or an untracked
 * transaction changed it) the intent is wanted and S6 resends it; a ledger
 * refusal is then the family's `INTENT_RESUBMIT_REJECTED` hold.
 */
export const stakeRegistration = async (read: Read): Promise<boolean> => {
  const credentials = credentialsRegisteredBy(await ownTxCbor(read));
  if (credentials.length === 0)
    throw new Error(
      `${read.state.intent.family} intent ${read.state.intent.workflowKey} registers no stake credential`,
    );
  const landed = await landedStakeRegistrationsIn(
    read.tx,
    read.dialect,
    credentials,
  );
  return credentials.some((credential) => landed.get(credential) !== true);
};
