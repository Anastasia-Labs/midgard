/** One committed block whose single event spends a deposit-derived output.
 * The operator commits the event and applies its ledger effect (none for a
 * transaction stamped `TxIsInvalid`); every root is computed from the actual
 * outputs. The spend runs a PlutusV3 script (failing or passing), beside an
 * output a native script holds and a mint under its policy when asked, or
 * is a key spend with no redeemers. */
import {
  computeHash28,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../../src/evidence/canonical-block-evidence.js";
import type { LedgerEntry } from "../../src/ledger-at.js";
import { ledgerEntriesRoot } from "../../src/ledger-at.js";
import { buildCountedRoot } from "../../src/transition-trace/phas.js";
import { admitCompleteCanonicalReplayPredecessor } from "../../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  fixtureTransactionFromCanonicalCbor,
  reencodeFixturePayload,
} from "./canonical-block-evidence-fixture.js";
import {
  type PlutusExecutionScenario,
  scenarioOutputs,
  scenarioSeedLovelace,
  scenarioTransaction,
  type ScenarioTransactionOptions,
  scriptCredential,
} from "./plutus-execution-scenario-transaction.js";
import { type ScriptSpend } from "./plutus-execution-scripts.js";
import {
  depositSeed,
  keyCredential,
  nativeSpendSeed,
  SEED_LOVELACE,
  seedDepositEvent,
} from "./plutus-execution-seeds.js";

export {
  anyoneObserverScript,
  extraSignerKeyHash,
  type PlutusExecutionScenario,
  type ScenarioNative,
  scenarioSeedLovelace,
  type ScenarioSigners,
  scenarioTransaction,
  type ScenarioTransactionOptions,
  scriptCredential,
} from "./plutus-execution-scenario-transaction.js";
export {
  ADMITTED_TX_EX_UNITS,
  FAILING_SCRIPT,
  LONGEST_PASSING_SCRIPT,
  PASSING_SCRIPT,
  type ScriptSpend,
} from "./plutus-execution-scripts.js";
export {
  depositSeed,
  nativeSpendSeedDeposit,
  referenceSeedDeposits,
  type ScenarioMint,
  SEED_LOVELACE,
} from "./plutus-execution-seeds.js";

const DEFAULT_SCRIPT_SEED_BYTE = 0x61;
const KEY_SEED_BYTE = 0x62;

/**
 * The deposit of `lovelace` that creates the output `scenario`'s
 * transaction spends, as a committed deposit block carries it. A script
 * spend's seed is named by `seedByte`, so one deposit block can seed several
 * scenarios; it is held by `spend`'s script when given, by default the
 * scenario's.
 */
export const plutusExecutionSeedDeposit = (
  scenario: PlutusExecutionScenario,
  seedByte: number = DEFAULT_SCRIPT_SEED_BYTE,
  spend?: ScriptSpend,
  lovelace: bigint = SEED_LOVELACE,
) => ({
  event:
    scenario === "no_redeemers"
      ? seedDepositEvent(KEY_SEED_BYTE, keyCredential())
      : seedDepositEvent(seedByte, scriptCredential(scenario, spend)),
  originalAssets: { lovelace },
  honest: true,
});

const sorted = (rows: SDK.DaPayloadEntry[]): SDK.DaPayloadEntry[] =>
  rows.sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0));

const countedRoot = async (
  domain: SDK.RootDomain,
  rows: SDK.DaPayloadEntry[],
) =>
  (
    await buildCountedRoot(
      domain,
      rows.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    )
  ).root;

/** Header fields an emulator commit fixes: the operator, the covered
 * interval, its commit slot and the chain link. */
export type PlutusExecutionHeaderFields = Partial<
  Pick<
    SDK.Header,
    "operatorVkey" | "startTime" | "endTime" | "blockSlot" | "prevHeaderHash"
  >
>;

/** How `plutusExecutionBlockFixture` builds the block; the script spend's
 * own options are `scenarioTransaction`'s. */
export type PlutusExecutionBlockOptions = Omit<
  ScenarioTransactionOptions,
  "nativeSpend" | "passingSpends"
> & {
  readonly header?: PlutusExecutionHeaderFields;
  /** The deposit byte of the spent seed (`plutusExecutionSeedDeposit`). */
  readonly seedByte?: number;
  /** The predecessor block's ledger, which must hold the spent seed; by
   * default the scenario's script seed and a key-owned seed. */
  readonly predecessorEntries?: readonly LedgerEntry[];
  /** Whether the script spend also spends the output the native script
   * holds (`nativeSpendSeedDeposit`), which the predecessor ledger must
   * hold. */
  readonly nativeSpend?: boolean;
  /** How many passing-script outputs, seeded below `seedByte`, it spends first. */
  readonly passingSpends?: number;
};

export const plutusExecutionBlockFixture = async (
  scenario: PlutusExecutionScenario,
  {
    header: headerFields = {},
    seedByte = DEFAULT_SCRIPT_SEED_BYTE,
    predecessorEntries: requestedPredecessor,
    nativeSpend = false,
    passingSpends: passingSpendCount = 0,
    ...spendOptions
  }: PlutusExecutionBlockOptions = {},
) => {
  const passingSpends = Array.from({ length: passingSpendCount }, (_, index) =>
    depositSeed(
      seedByte - passingSpendCount + index,
      scriptCredential("passing"),
    ),
  );
  const nativeMint = spendOptions.nativeMint ?? false;
  if (nativeSpend && spendOptions.native?.script !== undefined)
    throw new Error("the native spend's output holds the default script");
  const scriptSeed = depositSeed(
    seedByte,
    scriptCredential(scenario, spendOptions.spend),
    scenarioSeedLovelace(nativeMint, spendOptions.native),
  );
  const keySeed = depositSeed(KEY_SEED_BYTE, keyCredential());
  const predecessorEntries = requestedPredecessor ?? [
    ...passingSpends,
    scriptSeed,
    keySeed,
  ];
  const seed = scenario === "no_redeemers" ? keySeed : scriptSeed;
  const spent = [
    ...passingSpends,
    seed,
    ...(nativeSpend ? [nativeSpendSeed()] : []),
  ];
  if (
    !spent.every(({ outRef }) =>
      predecessorEntries.some((entry) => entry.outRef.equals(outRef)),
    )
  )
    throw new Error("the predecessor ledger does not hold the spent seed");
  const native = scenarioTransaction(scenario, seed, {
    ...spendOptions,
    nativeSpend: nativeSpend ? spent.at(-1) : undefined,
    passingSpends,
  });
  const tx = fixtureTransactionFromCanonicalCbor({
    txId: native.txId.toString("hex"),
    canonicalCbor: native.txCbor,
  });
  const forced = scenario === "forced_failing";
  const forcedKey: SDK.OutputReference = {
    transactionId: "a1".repeat(32),
    outputIndex: 0n,
  };
  const eventKey: SDK.EventKey = forced
    ? { ForcedTransactionEventKey: { tx_order_id: forcedKey } }
    : { L2TransactionEventKey: { tx_id: tx.txId } };
  const phase: SDK.TransitionPhase = forced
    ? "ForcedTransaction"
    : "L2Transaction";

  const prevUtxosRoot = await ledgerEntriesRoot(predecessorEntries);
  const createdEntries: readonly LedgerEntry[] = scenarioOutputs(
    BigInt(spent.length),
    nativeMint,
    spendOptions.native,
  ).map((output, outputIndex) => ({
    outRef: encodeMidgardSpendInputItem({
      txId: Buffer.from(tx.txId, "hex"),
      outputIndex,
    }),
    output,
  }));
  const postEntries =
    scenario === "failing_stamped_invalid"
      ? [...predecessorEntries]
      : [
          ...predecessorEntries.filter(
            (entry) => !spent.some(({ outRef }) => entry.outRef.equals(outRef)),
          ),
          ...createdEntries,
        ];
  const utxosRoot = await ledgerEntriesRoot(postEntries);

  const forcedRows: SDK.DaPayloadEntry[] = [];
  const forcedPreimages: SDK.DaPayloadEntry[] = [];
  if (forced) {
    const full = materializeMidgardForcedTxFromCanonical(
      decodeMidgardNativeTxFullFromCanonicalCbor(tx.canonicalCbor),
    );
    const source = deriveMidgardForcedTxProofSource(full);
    const keyCbor = Data.to(forcedKey, SDK.OutputReference);
    forcedRows.push([
      keyCbor,
      Data.to(
        {
          tx_id: tx.txId,
          submitted_source: {
            compact_cbor: source.compactCbor.toString("hex"),
            witness_set_compact_cbor:
              source.witnessSetCompactCbor.toString("hex"),
            field_preimage_lengths_cbor:
              source.fieldPreimageLengthsCbor.toString("hex"),
          },
          verdict: "ForcedTxValid",
        } satisfies SDK.ForcedInclusionTxV1,
        SDK.ForcedInclusionTxV1,
      ),
    ]);
    forcedPreimages.push([
      keyCbor,
      encodeMidgardForcedTxCanonical(full).toString("hex"),
    ]);
  }
  const trace: SDK.DaPayloadEntry[] = [
    [
      Data.to(0n),
      Data.to(
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: eventKey,
          phase,
          pre_utxos_root: prevUtxosRoot,
          post_utxos_root: utxosRoot,
        },
        SDK.TransitionStep,
      ),
    ],
  ];
  const events: SDK.DaPayloadEntry[] = [
    [
      Data.to(eventKey, SDK.EventKey),
      Data.to({ step_index: 0n, phase }, SDK.EventToStepValue),
    ],
  ];

  const base = await buildCanonicalBlockFixture({
    transactions: forced ? [] : [tx],
    prevUtxosRoot,
  });
  const header: SDK.Header = {
    ...base.header,
    ...headerFields,
    forcedTransactionCount: BigInt(forcedRows.length),
    totalEventCount: 1n,
    transitionStepCount: 1n,
    utxosRoot,
    forcedTransactionsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      forcedRows,
    ),
    transitionTraceRoot: await countedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      trace,
    ),
    eventToStepRoot: await countedRoot(SDK.ROOT_DOMAINS.eventToStep, events),
  };
  const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
    "hex",
  );
  const body = base.payload.block_body;
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...body,
      header,
      header_hash: headerHash,
      forced_transactions: sorted(forcedRows),
      forced_transaction_preimages: sorted(forcedPreimages),
      transition_trace: trace,
      event_to_step: events,
      counts: {
        ...body.counts,
        forcedTransactionCount: header.forcedTransactionCount,
        totalEventCount: header.totalEventCount,
        transitionStepCount: header.transitionStepCount,
      },
    },
  };
  const payloadEnvelopeCbor = await reencodeFixturePayload(payload);
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({ header, headerHash }),
    payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "test/plutus-execution",
      grade: "security",
    },
  });
  const context = {
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      currentEvidence: evidence,
      ledgerAt: async (hash) =>
        hash === header.prevHeaderHash ? predecessorEntries : undefined,
    }),
  };
  return {
    evidence,
    /** The committed block's retained-DA payload envelope. */
    payloadEnvelopeCbor,
    context,
    eventKey,
    tx,
    predecessorEntries,
    seed,
    /** The transaction's first output. */
    created: createdEntries[0]!,
    postEntries,
  };
};
