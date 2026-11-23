import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/core";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { LedgerRootMismatchError } from "../src/ledger-at.js";
import { admitCompleteCanonicalReplayPredecessor } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import {
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.detect-double-withdraws.js";
import { COMPLETE_CANONICAL_REPLAY_PREDECESSOR } from "../src/workflow/complete-replay.replay-context-identity.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { singleLedgerAt } from "./support/predecessor-ledger.js";

const evidenceFor = async () => {
  const transaction = buildFixtureTransaction({
    spendInputs: [outRefCbor(12, 0n)],
    referenceInputs: [outRefCbor(13, 1n)],
    fee: 1n,
  });
  // Inputs are classified at the transaction's committed event pre-state, so
  // the header commits the transaction's transition step from the empty
  // genesis ledger.
  const fixture = await buildCanonicalBlockFixture({
    transactions: [transaction],
    transitionSteps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: { L2TransactionEventKey: { tx_id: transaction.txId } },
        phase: "L2Transaction",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
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
        ledgerAt: singleLedgerAt(
          predecessor.headerHash,
          [],
          predecessor.header.utxosRoot,
        ),
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
    const admit = (
      entries: { outRef: Buffer; output: Buffer }[],
      root: string,
    ) =>
      admitCompleteCanonicalReplayPredecessor({
        ledgerAt: singleLedgerAt(predecessor.headerHash, entries, root),
        currentEvidence,
      });
    await expect(
      admit(
        [{ outRef, output: output(2_000_000n) }],
        predecessor.header.utxosRoot,
      ),
    ).resolves.toMatchObject({
      schemaVersion: COMPLETE_CANONICAL_REPLAY_PREDECESSOR,
      headerHash: predecessor.headerHash,
      utxosRoot: predecessor.header.utxosRoot,
    });
    // A ledger opening another root, one wrong entry, is refused at the root
    // check.
    const other = await buildCanonicalBlockFixture({
      transactions: [],
      utxos: [{ key: outRef, value: output(2_000_001n) }],
    });
    await expect(
      admit([{ outRef, output: output(2_000_001n) }], other.header.utxosRoot),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
    // A source without the ledger after prev_header_hash is refused.
    await expect(
      admitCompleteCanonicalReplayPredecessor({
        ledgerAt: singleLedgerAt(
          current.headerHash,
          [],
          current.header.utxosRoot,
        ),
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

  it("detects only the spend input the predecessor ledger does not hold", async () => {
    const held = outRefCbor(14, 0n);
    const absent = outRefCbor(15, 1n);
    const output = encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x44)]),
      value: { lovelace: 2_000_000n, assets: new Map() },
    });
    const predecessor = await buildCanonicalBlockFixture({
      transactions: [],
      utxos: [{ key: held, value: output }],
    });
    const transaction = buildFixtureTransaction({
      spendInputs: [held, absent],
      fee: 1n,
    });
    const current = await buildCanonicalBlockFixture({
      transactions: [transaction],
      prevHeaderHash: predecessor.headerHash,
      prevUtxosRoot: predecessor.header.utxosRoot,
      transitionSteps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: { L2TransactionEventKey: { tx_id: transaction.txId } },
          phase: "L2Transaction",
          pre_utxos_root: predecessor.header.utxosRoot,
          post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      ],
    });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(current),
      payloadEnvelopeCbor: current.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/predecessor-replay-test",
        grade: "security",
      },
    });
    const decision = await NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY.replay(
      evidence,
      {
        predecessor: await admitCompleteCanonicalReplayPredecessor({
          ledgerAt: singleLedgerAt(
            predecessor.headerHash,
            [{ outRef: held, output }],
            predecessor.header.utxosRoot,
          ),
          currentEvidence: evidence,
        }),
      },
    );
    // The identity names the violation, the transaction's position and the
    // absent input's index.
    expect(
      decision.detections.map(({ detectionId }) =>
        detectionId.split(":").slice(0, 3),
      ),
    ).toEqual([["non-existent-input", "0", "1"]]);
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
