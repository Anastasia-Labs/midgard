import {
  computeHash28,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../../src/evidence/canonical-block-evidence.js";
import { admitCompleteCanonicalReplayPredecessor } from "../../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  reencodeFixturePayload,
} from "../helpers/canonical-block-evidence-fixture.js";
import {
  buildWithdrawalMistagEvidenceMaterial,
  type WithdrawalMistagDirectionFixture,
} from "./withdrawal-mistag-emulator.js";

const provenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/withdrawal-mistag-retained",
  grade: "security",
} as const;
export const withdrawalMistagRetainedFixture = async ({
  direction,
  honest = false,
  outputBytes = 0,
  assetCount = 0,
  payoutDatumBytes = 0,
  operatorVkey = "aa".repeat(28),
  now = 1_000_000,
}: {
  direction: WithdrawalMistagDirectionFixture;
  honest?: boolean;
  outputBytes?: number;
  assetCount?: number;
  payoutDatumBytes?: number;
  operatorVkey?: string;
  now?: number;
}) => {
  const material = await buildWithdrawalMistagEvidenceMaterial(
    direction,
    honest,
    outputBytes,
    assetCount,
    payoutDatumBytes,
  );
  const outref = material.args.committedWithdrawal.value.body.l2_outref;
  const output = {
    key: encodeMidgardSpendInputItem({
      txId: Buffer.from(outref.transactionId, "hex"),
      outputIndex: Number(outref.outputIndex),
    }),
    value: Buffer.from(
      material.args.ledgerEvidence.PresentLedgerOutput.output_cbor,
      "hex",
    ),
  };
  const predecessorBase = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: [output],
    startTime: BigInt(now),
    endTime: BigInt(now + 1000),
  });
  const predecessorHeader = {
    ...predecessorBase.header,
    operatorVkey,
    prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    blockSlot: 9n,
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
  const keep = !SDK.withdrawalClaimsValid(
    material.args.committedWithdrawal.value,
  );
  const base = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: keep ? [output] : [],
    startTime: BigInt(now + 1000),
    endTime: BigInt(now + 61000),
    prevHeaderHash: predecessorHash,
    prevUtxosRoot: predecessor.header.utxosRoot,
  });
  const counts = {
    ...base.payload.block_body.counts,
    withdrawalCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
  };
  const header = {
    ...base.header,
    ...counts,
    operatorVkey,
    blockSlot: 10n,
    withdrawalsRoot: material.source.root,
    eventToStepRoot: material.event.root,
    transitionTraceRoot: material.trace.root,
  };
  const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
    "hex",
  );
  const entries = (root: typeof material.source): SDK.DaPayloadEntry[] =>
    root.entries.map((e) => [e.key.toString("hex"), e.value.toString("hex")]);
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      header,
      header_hash: headerHash,
      counts,
      withdrawals: entries(material.source),
      event_to_step: entries(material.event),
      transition_trace: entries(material.trace),
    },
  };
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(base, { header, headerHash }),
    payloadEnvelopeCbor: await reencodeFixturePayload(payload),
    daProvenance: provenance,
  });
  const context = {
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      value: {
        observation: predecessorObservation,
        payloadEnvelopeCborHex: predecessorEnvelope.toString("hex"),
        daProvenance: provenance,
      },
      currentEvidence: evidence,
      minimumConfirmationDepth: 1,
    }),
  };
  return { evidence, predecessor, context, material };
};
