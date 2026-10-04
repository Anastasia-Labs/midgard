import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, Data } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { describe, expect, it, vi } from "vitest";

import { ForeignBlockVerificationError } from "../src/mpf/verified-block-import.js";
import { resolveCommitBaseLedgerEntries } from "../src/workers/commit-block-header.resolve-commit-base-ledger-entries.js";
import {
  type VerifiedForeignCommitBase,
  VerifiedForeignBase,
} from "../src/workers/commit-block-header.verify-foreign-base.js";
import { serializeStateQueueUTxO } from "../src/workers/utils/commit-block-header.js";
import { fixture, verdict } from "./foreign-block-import.fixture.js";
import { normalFixture } from "./foreign-block-import.normal-fixture.js";
import { provideDatabaseLayers } from "./utils.js";

// Only local-journal presence is controlled. The real resolver and ordinary
// foreign event replay run; this layer tests mandatory capability use/order,
// not source-owner admission or network acquisition.
vi.mock("../src/database/index.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/database/index.js")>();
  return {
    ...original,
    PendingBlockFinalizationsDB: {
      ...original.PendingBlockFinalizationsDB,
      retrieveByHeaderHash: () => Effect.succeed(Option.none()),
    },
  };
});

const open = async (changedRoot = false) => {
  const normal = changedRoot ? await normalFixture() : undefined;
  const payload = normal?.payload ?? (await fixture());
  const imported =
    normal === undefined ? await verdict(payload) : await normal.imported();
  if (imported._tag !== "Right") throw imported.left;
  const header = payload.block_body.header;
  const headerHash = payload.block_body.header_hash;
  const node: SDK.StateQueueUTxO = {
    utxo: {
      txHash: "8a".repeat(32),
      outputIndex: 0,
      address: credentialToAddress("Preprod", {
        type: "Script",
        hash: "9a".repeat(28),
      }),
      assets: { lovelace: 2_000_000n },
    },
    datum: {
      key: { Key: { key: headerHash } },
      next: "Empty",
      data: Data.castTo(
        { header, da_attestation: SDK.NO_DA_ATTESTATION, proven_fraud: null },
        SDK.StateQueueNode,
      ),
    },
    assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  };
  node.utxo.datum = SDK.encodeLinkedListNodeView(node.datum);
  const serialized = await Effect.runPromise(serializeStateQueueUTxO(node));
  const base: VerifiedForeignCommitBase = {
    authority: "ready",
    headerHash,
    root: imported.right.root,
    entries: imported.right.entries,
    history: {
      token: {
        deploymentIdentity: "ab".repeat(32),
        ownerToken: "test-source-owner",
        generation: "1",
      },
      coverage: {
        bindingDigest: "bc".repeat(32),
        checkpointRevision: "1",
        point: { id: "cd".repeat(32), slot: 1 },
        snapshotDigest: "de".repeat(32),
        includedThroughMs: 2,
      },
    },
    observation: [],
    importedBlocks: [],
    verification: {
      status: "verified",
      foreignHeaderHash: headerHash,
      verifiedHeaderHashes: [headerHash],
    },
  };
  const resolve = () =>
    resolveCommitBaseLedgerEntries({
      availableConfirmedBlock: serialized,
      nativeMpfRoot: header.utxosRoot,
      requireEntries: false,
    });
  return { base, resolve };
};

describe("actual foreign commit-base resolver", () => {
  it("refuses an unchanged native root without complete foreign preflight", async () => {
    const f = await open();
    const result = await Effect.runPromise(
      provideDatabaseLayers(Effect.either(f.resolve())),
    );
    expect(result._tag).toBe("Left");
    if (result._tag === "Left")
      expect(result.left).toMatchObject({
        reason: "missing",
        detail: expect.stringContaining("complete canonical verification"),
      });
  });
  it.each([false, true])(
    "uses honest fully replayed entries (changed root=%s) and rechecks immediately before reuse",
    async (changedRoot) => {
      const f = await open(changedRoot);
      const recheck = vi.fn();
      const result = await Effect.runPromise(
        provideDatabaseLayers(
          f.resolve().pipe(
            Effect.provideService(VerifiedForeignBase, {
              base: f.base,
              assertCurrent: Effect.sync(recheck),
            }),
          ),
        ),
      );
      expect(result.root).toBe(f.base.root);
      expect(result.entries).toEqual(f.base.entries);
      expect(result.source).toBe(`verified-foreign:${f.base.headerHash}`);
      expect(recheck).toHaveBeenCalledTimes(1);
    },
  );
  it("refuses a stale generation and a restart that retained the same native root", async () => {
    const f = await open();
    const revoked = new ForeignBlockVerificationError({
      foreignHeaderHash: f.base.headerHash,
      reason: "missing",
      detail: "source generation superseded by rollback",
    });
    const stale = await Effect.runPromise(
      provideDatabaseLayers(
        Effect.either(
          f.resolve().pipe(
            Effect.provideService(VerifiedForeignBase, {
              base: f.base,
              assertCurrent: Effect.fail(revoked),
            }),
          ),
        ),
      ),
    );
    expect(stale._tag).toBe("Left");
    if (stale._tag === "Left") expect(stale.left).toBe(revoked);
    const restarted = await Effect.runPromise(
      provideDatabaseLayers(Effect.either(f.resolve())),
    );
    expect(restarted._tag).toBe("Left");
  });
});
