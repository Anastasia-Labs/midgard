import {
  computeHash28,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKDa from "@al-ft/midgard-sdk/da";
import { Data } from "@lucid-evolution/lucid";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../../src/evidence/canonical-block-evidence.js";
import { ledgerEntriesRoot } from "../../src/ledger-at.js";
import {
  buildCountedRoot,
  type CountedRoot,
} from "../../src/transition-trace/phas.js";
import { admitCompleteCanonicalReplayPredecessor } from "../../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  reencodeFixturePayload,
} from "../helpers/canonical-block-evidence-fixture.js";
import { withPlaceholderWithdrawalBlock } from "../helpers/canonical-block-evidence-fixture.with-placeholder-withdrawal.js";
import { fixtureBlockSlot } from "./emulator/header-fixtures.js";
import { ledgerAtAfter } from "./predecessor-ledger.js";
import {
  buildWithdrawalMistagEvidenceMaterial,
  type WithdrawalMistagDirectionFixture,
} from "./withdrawal-mistag-emulator.build-withdrawal-mistag-evidence-material.js";

const provenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/withdrawal-mistag-retained",
  grade: "security",
} as const;
type LedgerOutput = Readonly<{ key: Buffer; value: Buffer }>;

/**
 * A retained block of withdrawal events, with its committed withdrawal,
 * event-to-step and transition-trace roots, over a predecessor block holding
 * `predecessorUtxos`; the block leaves `blockUtxos`.
 */
export const retainedBlock = async ({
  predecessorUtxos,
  blockUtxos,
  source,
  event,
  trace,
  operatorVkey = "aa".repeat(28),
  now = 1_000_000,
}: {
  predecessorUtxos: readonly LedgerOutput[];
  blockUtxos: readonly LedgerOutput[];
  source: CountedRoot;
  event: CountedRoot;
  trace: CountedRoot;
  operatorVkey?: string;
  now?: number;
}) => {
  const predecessorBase = await withPlaceholderWithdrawalBlock(
    await buildCanonicalBlockFixture({
      transactions: [],
      utxos: predecessorUtxos,
      startTime: BigInt(now),
      endTime: BigInt(now + 1000),
    }),
  );
  const predecessorHeader = {
    ...predecessorBase.header,
    operatorVkey,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    blockSlot: fixtureBlockSlot(BigInt(now + 1000), 9n),
  };
  const predecessorHash = computeHash28(
    SDK.encodeHeaderCbor(predecessorHeader),
  ).toString("hex");
  const predecessorPayload = {
    ...predecessorBase.payload,
    block_body: {
      ...predecessorBase.payload.block_body,
      header: predecessorHeader,
      header_hash: predecessorHash,
    },
  };
  const predecessorEnvelope = await reencodeFixturePayload(predecessorPayload);
  const predecessorObservation = authenticatedHeaderObservation(
    predecessorBase,
    { header: predecessorHeader, headerHash: predecessorHash },
  );
  const predecessor = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: predecessorObservation,
    payloadEnvelopeCbor: predecessorEnvelope,
    daProvenance: provenance,
  });
  const base = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: blockUtxos,
    startTime: BigInt(now + 1000),
    endTime: BigInt(now + 61000),
    prevHeaderHash: predecessorHash,
    prevUtxosRoot: predecessor.header.utxosRoot,
  });
  const counts = {
    ...base.payload.block_body.counts,
    withdrawalCount: source.count,
    totalEventCount: source.count,
    transitionStepCount: trace.count,
  };
  const header = {
    ...base.header,
    ...counts,
    operatorVkey,
    blockSlot: fixtureBlockSlot(base.header.endTime, 10n),
    withdrawalsRoot: source.root,
    eventToStepRoot: event.root,
    transitionTraceRoot: trace.root,
  };
  const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
    "hex",
  );
  const entries = (root: CountedRoot): SDKDa.DaPayloadEntry[] =>
    root.entries.map((e) => [e.key.toString("hex"), e.value.toString("hex")]);
  const payload: SDKDa.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      header,
      header_hash: headerHash,
      counts,
      withdrawals: entries(source),
      event_to_step: entries(event),
      transition_trace: entries(trace),
    },
  };
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(base, { header, headerHash }),
    payloadEnvelopeCbor: await reencodeFixturePayload(payload),
    daProvenance: provenance,
  });
  const context = {
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      ledgerAt: ledgerAtAfter(predecessor),
      currentEvidence: evidence,
    }),
  };
  return { evidence, predecessor, context };
};

type WithdrawalMistagMaterial = Awaited<
  ReturnType<typeof buildWithdrawalMistagEvidenceMaterial>
>;

/** The ledger output a material's withdrawal spends. */
export const withdrawalMistagOutput = (
  material: WithdrawalMistagMaterial,
): LedgerOutput => {
  const outref = material.args.committedWithdrawal.value.body.l2_outref;
  return {
    key: encodeMidgardSpendInputItem({
      txId: Buffer.from(outref.transactionId, "hex"),
      outputIndex: Number(outref.outputIndex),
    }),
    value: Buffer.from(
      material.args.ledgerEvidence.PresentLedgerOutput.output_cbor,
      "hex",
    ),
  };
};

/** The material's one-step trace, its step starting from `utxos`. */
const traceFrom = async (
  material: WithdrawalMistagMaterial,
  utxos: readonly LedgerOutput[],
): Promise<CountedRoot> =>
  await buildCountedRoot(SDK.ROOT_DOMAINS.transitionTrace, [
    {
      key: Buffer.from(Data.to(0n), "hex"),
      value: Buffer.from(
        Data.to(
          {
            ...material.args.transitionStep.value,
            pre_utxos_root: await ledgerEntriesRoot(
              utxos.map(({ key, value }) => ({ outRef: key, output: value })),
            ),
          },
          SDK.TransitionStep,
        ),
        "hex",
      ),
    },
  ]);

export const withdrawalMistagRetainedFixture = async ({
  direction,
  honest = false,
  outputBytes = 0,
  assetCount = 0,
  payoutDatumBytes = 0,
  operatorVkey,
  now,
  material: given,
  predecessorExtra = [],
}: {
  direction: WithdrawalMistagDirectionFixture;
  honest?: boolean;
  outputBytes?: number;
  assetCount?: number;
  payoutDatumBytes?: number;
  operatorVkey?: string;
  now?: number;
  /** The withdrawal's material, when the caller built it. */
  material?: WithdrawalMistagMaterial;
  /** Outputs the predecessor holds beside the withdrawn one. */
  predecessorExtra?: readonly LedgerOutput[];
}) => {
  const material =
    given ??
    (await buildWithdrawalMistagEvidenceMaterial(
      direction,
      honest,
      outputBytes,
      assetCount,
      payoutDatumBytes,
    ));
  const output = withdrawalMistagOutput(material);
  const keep = !SDK.withdrawalClaimsValid(
    material.args.committedWithdrawal.value,
  );
  const predecessorUtxos = [output, ...predecessorExtra];
  return {
    ...(await retainedBlock({
      predecessorUtxos,
      blockUtxos: keep ? [output] : [],
      source: material.source,
      event: material.event,
      trace:
        predecessorExtra.length === 0
          ? material.trace
          : await traceFrom(material, predecessorUtxos),
      operatorVkey,
      now,
    })),
    material,
  };
};
