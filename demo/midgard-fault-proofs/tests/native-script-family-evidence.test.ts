import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk/core";
import "@al-ft/midgard-sdk/da";
import "@al-ft/midgard-sdk/fraud-proof";
import "@al-ft/midgard-validation/ledger-output-descriptor";
import "vitest";
import "../src/evidence/canonical-block-evidence.js";
import "../src/native-script-invalid/artifact.js";
import "../src/native-script-invalid/prepare.js";
import "../src/resolved-output-non-canonical/resolved-output-non-canonical.js";
import "../src/transition-trace/phas.js";
import "./helpers/canonical-block-evidence-fixture.js";
import "./native-script-family-evidence.retained-source.js";

import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation/ledger-output-descriptor";
import { describe, expect, it } from "vitest";

import { LedgerRootMismatchError } from "../src/ledger-at.js";
import {
  admitNativeScriptInvalidArtifact,
  nativeScriptInvalidDetectionId,
  prepareNativeScriptInvalidArtifact,
} from "../src/native-script-invalid/artifact.js";
import { prepareNativeScriptInvalidFromCanonicalEvidence } from "../src/native-script-invalid/prepare.js";
import { readResolvedOutputPriorLedger } from "../src/resolved-output-non-canonical/resolved-output-non-canonical.js";
import { keyValuePhasRootWithCount } from "../src/transition-trace/phas.js";
import { BLOCK_SUBJECT } from "../src/workflow/detection-subject.js";
import { buildCanonicalBlockFixture } from "./helpers/canonical-block-evidence-fixture.js";
import {
  canonicalEvidence,
  evidenceFromFixture,
  fixtureTransaction,
  nativeScript,
  nativeTx,
} from "./native-script-family-evidence.retained-source.js";
import { memoryLedger } from "./support/fixture-ledger.js";
import { ledgerAfter } from "./support/predecessor-ledger.js";

/** A canonical ledger entry for the predecessor output 0x55..55#0. */
const ledgerEntry = (lovelace: bigint) => ({
  key: Buffer.from(
    encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0x55),
      outputIndex: 0,
    }),
  ),
  value: Buffer.from(
    encodeMidgardTxOutput({
      address: Buffer.concat([
        Buffer.from([0x70]),
        Buffer.from(hashMidgardVersionedScript(nativeScript), "hex"),
      ]),
      value: { lovelace, assets: new Map() },
    }),
  ),
});

describe("native-script family retained-DA evidence", () => {
  it("prepares an authenticated evaluation-false native witness", async () => {
    const evidence = await canonicalEvidence(
      nativeTx({ scripts: [nativeScript] }),
    );
    const prepared = await prepareNativeScriptInvalidFromCanonicalEvidence({
      evidence,
    });
    expect(prepared.scriptIndex).toBe(0n);
    expect(prepared.scriptHash).toBe(hashMidgardVersionedScript(nativeScript));
    expect(prepared.addrWitnessItemCbors).toEqual([]);

    const detectionId = nativeScriptInvalidDetectionId({
      txId: prepared.badTxId,
      scriptIndex: prepared.scriptIndex,
    });
    const detection = {
      ...BLOCK_SUBJECT,
      detectionId,
      headerHash: evidence.headerHash,
      violationId: SDKFraudProof.NATIVE_SCRIPT_INVALID_VIOLATION_ID,
      position: 0n,
    };
    const artifact = await prepareNativeScriptInvalidArtifact({
      evidence,
      classification: {
        schemaVersion: "midgard-fraud-proof-classification-v1",
        decision: "fault_detected",
        headerHash: evidence.headerHash,
        category: "nativeScriptInvalid",
        selected: detection,
        detections: [detection],
        unprovableGaps: [],
      },
    });
    expect(admitNativeScriptInvalidArtifact(artifact).prepared.scriptHash).toBe(
      prepared.scriptHash,
    );
    expect(() =>
      admitNativeScriptInvalidArtifact({
        ...artifact,
        scriptHash: "ee".repeat(28),
      }),
    ).toThrow(/script bytes and committed script hash disagree/u);
  });

  it("rejects a transaction without an invalid native witness", async () => {
    await expect(
      prepareNativeScriptInvalidFromCanonicalEvidence({
        evidence: await canonicalEvidence(nativeTx({})),
      }),
    ).rejects.toThrow(/no accepted false native witness/u);
  });

  it.each([false, true])(
    "derives the genesis prior ledger without a predecessor (transaction present: %s)",
    async (hasTransaction) => {
      const fixture = await buildCanonicalBlockFixture({
        transactions: hasTransaction ? [fixtureTransaction(nativeTx({}))] : [],
        prevHeaderHash: SDK.GENESIS_HEADER_HASH,
      });
      const block = await evidenceFromFixture(fixture);
      await expect(
        readResolvedOutputPriorLedger({
          block,
          predecessor: undefined,
        }),
      ).resolves.toEqual({
        priorRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        outputs: new Map(),
      });
      // A ledger for any root other than the committed empty one is substituted.
      await expect(
        readResolvedOutputPriorLedger({
          block,
          predecessor: await ledgerAfter(
            await evidenceFromFixture(
              await buildCanonicalBlockFixture({
                transactions: [],
                utxos: [ledgerEntry(2_000_000n)],
                prevHeaderHash: SDK.GENESIS_HEADER_HASH,
              }),
            ),
          ),
        }),
      ).rejects.toThrow(/substituted/u);
    },
  );

  it("refuses a genesis header committing a non-empty previous ledger without its predecessor", async () => {
    const fixture = await buildCanonicalBlockFixture({
      transactions: [],
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
      prevUtxosRoot: "00".repeat(32),
    });
    await expect(
      readResolvedOutputPriorLedger({
        block: await evidenceFromFixture(fixture),
        predecessor: undefined,
      }),
    ).rejects.toThrow(/absent/u);
  });

  it("refuses an absent or substituted non-genesis predecessor even when the current block is empty", async () => {
    const predecessor = await buildCanonicalBlockFixture({
      transactions: [],
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    });
    const named = await buildCanonicalBlockFixture({
      transactions: [],
      prevHeaderHash: predecessor.headerHash,
      prevUtxosRoot: "00".repeat(32),
    });
    const block = await evidenceFromFixture(named);
    await expect(
      readResolvedOutputPriorLedger({ block, predecessor: undefined }),
    ).rejects.toThrow(/absent/u);
    // The named predecessor's ledger root differs from prev_utxos_root.
    await expect(
      readResolvedOutputPriorLedger({
        block,
        predecessor: await ledgerAfter(await evidenceFromFixture(predecessor)),
      }),
    ).rejects.toThrow(/substituted/u);
  });

  it("derives the predecessor ledger output a spend resolves against", async () => {
    const predecessorTxId = Buffer.alloc(32, 0x55);
    const outRefKey = encodeMidgardSpendInputItem({
      txId: predecessorTxId,
      outputIndex: 0,
    });
    const outputCbor = encodeMidgardTxOutput({
      address: Buffer.concat([
        Buffer.from([0x70]),
        Buffer.from(hashMidgardVersionedScript(nativeScript), "hex"),
      ]),
      value: { lovelace: 2_000_000n, assets: new Map() },
    });
    const descriptorCbor = buildCanonicalMidgardLedgerEntryOutputMaterial({
      outRef: outRefKey,
      outputCbor,
    }).descriptorCbor;
    const previousRoot = await keyValuePhasRootWithCount([
      { key: outRefKey, value: descriptorCbor },
    ]);
    const previousFixture = await buildCanonicalBlockFixture({
      transactions: [fixtureTransaction(nativeTx({ scripts: [nativeScript] }))],
      utxos: [{ key: outRefKey, value: outputCbor }],
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
    });
    expect(previousFixture.header.utxosRoot).toBe(previousRoot.root);
    const challengedFixture = await buildCanonicalBlockFixture({
      transactions: [
        fixtureTransaction(nativeTx({ spendInputs: [outRefKey] })),
      ],
      prevHeaderHash: previousFixture.headerHash,
      prevUtxosRoot: previousFixture.header.utxosRoot,
    });
    const challenged = await evidenceFromFixture(challengedFixture);
    const priorLedger = await readResolvedOutputPriorLedger({
      block: challenged,
      predecessor: await ledgerAfter(
        await evidenceFromFixture(previousFixture),
      ),
    });
    expect(priorLedger.priorRoot).toBe(previousRoot.root);
    expect([...priorLedger.outputs.keys()]).toEqual([
      `${predecessorTxId.toString("hex")}#0`,
    ]);
    expect(
      priorLedger.outputs.get(`${predecessorTxId.toString("hex")}#0`),
    ).toMatchObject({
      transactionId: predecessorTxId.toString("hex"),
      outputIndex: 0,
      descriptorCborHex: Buffer.from(descriptorCbor).toString("hex"),
      outputCborHex: outputCbor.toString("hex"),
    });
    // The ledger is admitted by its root alone: the same ledger read after a
    // different block opens the same prior root.
    const lookalike = await buildCanonicalBlockFixture({
      transactions: [],
      utxos: [{ key: outRefKey, value: outputCbor }],
      prevHeaderHash: SDK.GENESIS_HEADER_HASH,
      startTime: 30n,
      endTime: 40n,
    });
    await expect(
      readResolvedOutputPriorLedger({
        block: challenged,
        predecessor: await ledgerAfter(await evidenceFromFixture(lookalike)),
      }),
    ).resolves.toMatchObject({ priorRoot: previousRoot.root });
    // One wrong entry and the ledger no longer opens the prior root.
    const wrong = ledgerEntry(2_000_001n);
    await expect(
      memoryLedger(
        [{ outRef: wrong.key, output: wrong.value }],
        challenged.header.prevUtxosRoot,
      ),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
  });
});
