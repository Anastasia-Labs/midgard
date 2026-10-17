import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { LedgerRootMismatchError, singleLedgerAt } from "../src/ledger-at.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  COMPLETE_CANONICAL_REPLAY_PREDECESSOR,
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

const evidenceFor = async () => {
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        spendInputs: [outRefCbor(12, 0n)],
        referenceInputs: [outRefCbor(13, 1n)],
        fee: 1n,
      }),
    ],
  });
  return await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/predecessor-replay-test",
      grade: "security",
    },
  });
};

describe("production predecessor-relative complete replay V1", () => {
  it("accepts an authenticated empty predecessor after genesis", async () => {
    const predecessor = await buildCanonicalBlockFixture({ transactions: [] });
    const current = await buildCanonicalBlockFixture({
      transactions: [],
      prevHeaderHash: predecessor.headerHash,
      prevUtxosRoot: predecessor.header.utxosRoot,
    });
    const daProvenance = {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/predecessor-replay-test",
      grade: "security",
    } as const;
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(current),
      payloadEnvelopeCbor: current.payloadEnvelopeCbor,
      daProvenance,
    });
    const context = {
      predecessor: await admitCompleteCanonicalReplayPredecessor({
        ledgerAt: singleLedgerAt(predecessor.headerHash, []),
        currentEvidence: evidence,
      }),
    };
    for (const replayer of [
      NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
      NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
    ]) {
      await expect(replayer.replay(evidence, context)).resolves.toMatchObject({
        detections: [],
      });
    }
  });

  it("admits a predecessor ledger only when it opens prev_utxos_root", async () => {
    const outRef = outRefCbor(14, 0n);
    const output = (lovelace: bigint) =>
      encodeMidgardTxOutput({
        address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x44)]),
        value: { lovelace, assets: new Map() },
      });
    const predecessor = await buildCanonicalBlockFixture({
      transactions: [],
      utxos: [{ key: outRef, value: output(2_000_000n) }],
    });
    const current = await buildCanonicalBlockFixture({
      transactions: [],
      prevHeaderHash: predecessor.headerHash,
      prevUtxosRoot: predecessor.header.utxosRoot,
    });
    const currentEvidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(current),
      payloadEnvelopeCbor: current.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/predecessor-replay-test",
        grade: "security",
      },
    });
    const admit = (entries: { outRef: Buffer; output: Buffer }[]) =>
      admitCompleteCanonicalReplayPredecessor({
        ledgerAt: singleLedgerAt(predecessor.headerHash, entries),
        currentEvidence,
      });
    await expect(
      admit([{ outRef, output: output(2_000_000n) }]),
    ).resolves.toMatchObject({
      schemaVersion: COMPLETE_CANONICAL_REPLAY_PREDECESSOR,
      headerHash: predecessor.headerHash,
      utxosRoot: predecessor.header.utxosRoot,
    });
    // One wrong entry is refused at the root check.
    await expect(
      admit([{ outRef, output: output(2_000_001n) }]),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
    // A source without the ledger after prev_header_hash is refused.
    await expect(
      admitCompleteCanonicalReplayPredecessor({
        ledgerAt: singleLedgerAt(current.headerHash, []),
        currentEvidence,
      }),
    ).rejects.toThrow("is unavailable");
  });

  it("detects absent spend and reference inputs against the committed empty genesis ledger", async () => {
    const evidence = await evidenceFor();
    await expect(
      NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY.replay(evidence),
    ).resolves.toMatchObject({
      context: null,
      detections: [{ violationId: "non-existent-input", position: 0n }],
    });
    await expect(
      NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY.replay(evidence),
    ).resolves.toMatchObject({
      context: null,
      detections: [{ violationId: "no-reference-input", position: 0n }],
    });
  });

  it("emits an explicit unprovable gap when a non-empty predecessor is unavailable", async () => {
    const base = await evidenceFor();
    const evidence = {
      ...base,
      header: { ...base.header, prevUtxosRoot: "ee".repeat(32) },
    };
    const decision =
      await NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    expect(decision.detections).toEqual([
      expect.objectContaining({
        violationId: "authenticated-predecessor-context-unavailable",
        position: 0n,
      }),
    ]);
  });

  it("rejects a structural predecessor clone that did not cross raw L1/DA admission", async () => {
    const base = await evidenceFor();
    const evidence = {
      ...base,
      header: { ...base.header, prevUtxosRoot: "ee".repeat(32) },
    };
    await expect(
      NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY.replay(evidence, {
        predecessor: {
          schemaVersion: COMPLETE_CANONICAL_REPLAY_PREDECESSOR,
          challengedHeaderHash: evidence.headerHash,
          headerHash: evidence.header.prevHeaderHash,
          utxosRoot: evidence.header.prevUtxosRoot,
        },
      }),
    ).rejects.toThrow("was not admitted for this challenged header");
  });
});
