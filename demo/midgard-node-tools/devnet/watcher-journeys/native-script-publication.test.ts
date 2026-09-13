import { mkdir, mkdtemp, readdir, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import { createJourneyNativeScriptArchive } from "./history-archives.js";
import {
  JOURNEY_NATIVE_REFERENCE_HASH,
  publishJourneyNativeScriptReference,
} from "./native-script-publication.js";

const fixture = async () => {
  const account = generateEmulatorAccount({ lovelace: 100_000_000n });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const directory = await mkdtemp(join(tmpdir(), "native-reference-test-"));
  const input = {
    lucid,
    network: "Custom" as const,
    deploymentFingerprint: "ab".repeat(32),
    journalPath: join(directory, "publication.json"),
  };
  return {
    emulator,
    input,
    close: () => rm(directory, { recursive: true, force: true }),
  };
};

it("durably records signed bytes before publication and preserves its separate reference output", async () => {
  const { emulator, input, close } = await fixture();
  try {
    const submit = emulator.submitTx.bind(emulator);
    const submission = vi
      .spyOn(emulator, "submitTx")
      .mockImplementation(async (bytes) => {
        const journal = JSON.parse(await readFile(input.journalPath, "utf8"));
        expect(journal.outcome).toBe("prepared");
        expect(journal.signedCbor).toBe(bytes);
        expect(
          CML.hash_transaction(
            CML.Transaction.from_cbor_hex(bytes).body(),
          ).to_hex(),
        ).toBe(journal.txHash);
        return submit(bytes);
      });
    const published = await publishJourneyNativeScriptReference(input);
    expect(published.scriptHash).toBe(JOURNEY_NATIVE_REFERENCE_HASH);
    expect(published.reference.scriptRef).toEqual({
      type: "Native",
      script: "820180",
    });
    expect(published.destination).not.toBe(
      await input.lucid.wallet().address(),
    );
    expect(
      (await input.lucid.wallet().getUtxos()).some(
        (utxo) =>
          utxo.txHash === published.txHash &&
          utxo.outputIndex === published.reference.outputIndex,
      ),
    ).toBe(false);
    const restarted = await publishJourneyNativeScriptReference(input);
    expect(restarted.reference).toEqual(published.reference);
    expect(submission).toHaveBeenCalledTimes(1);
    expect(JSON.parse(await readFile(input.journalPath, "utf8")).outcome).toBe(
      "confirmed",
    );
  } finally {
    await close();
  }
});

it("reconciles submission accepted before acknowledgement without a duplicate publication", async () => {
  const { emulator, input, close } = await fixture();
  try {
    const submit = emulator.submitTx.bind(emulator);
    const submission = vi
      .spyOn(emulator, "submitTx")
      .mockImplementationOnce(async (bytes) => {
        const hash = await submit(bytes);
        await emulator.awaitTx(hash);
        throw new Error("acknowledgement interrupted");
      });
    await expect(publishJourneyNativeScriptReference(input)).rejects.toThrow(
      "requires reconciliation",
    );
    const unknown = JSON.parse(await readFile(input.journalPath, "utf8"));
    expect(unknown.outcome).toBe("unknown");
    const restarted = await publishJourneyNativeScriptReference(input);
    expect(restarted.signedCbor).toBe(unknown.signedCbor);
    expect(submission).toHaveBeenCalledTimes(1);
  } finally {
    await close();
  }
});

it("resubmits identical recorded bytes when interruption preceded node acceptance", async () => {
  const { emulator, input, close } = await fixture();
  try {
    const submit = emulator.submitTx.bind(emulator);
    const submission = vi
      .spyOn(emulator, "submitTx")
      .mockRejectedValueOnce(new Error("transport unavailable"));
    await expect(publishJourneyNativeScriptReference(input)).rejects.toThrow(
      "requires reconciliation",
    );
    const unknown = JSON.parse(await readFile(input.journalPath, "utf8"));
    submission.mockImplementation(submit);
    const restarted = await publishJourneyNativeScriptReference(input);
    expect(submission).toHaveBeenCalledTimes(2);
    expect(submission.mock.calls[0]![0]).toBe(submission.mock.calls[1]![0]);
    expect(restarted.signedCbor).toBe(unknown.signedCbor);
  } finally {
    await close();
  }
});

it("does not submit when the signed journal cannot be persisted", async () => {
  const { emulator, input, close } = await fixture();
  try {
    const submission = vi.spyOn(emulator, "submitTx");
    await expect(
      publishJourneyNativeScriptReference({
        ...input,
        journalPath: join(input.journalPath, "missing", "publication.json"),
      }),
    ).rejects.toMatchObject({ code: "ENOENT" });
    expect(submission).not.toHaveBeenCalled();
  } finally {
    await close();
  }
});

it("archives exact native reference outputs and removes rolled-back canonical inclusion", async () => {
  const { input, close } = await fixture();
  const directories = await Promise.all(
    ["a", "b"].map(async (role) => {
      const directory = await mkdtemp(
        join(tmpdir(), `native-archive-${role}-`),
      );
      await mkdir(join(directory, "canonical"));
      await mkdir(join(directory, "native-scripts"));
      return directory;
    }),
  );
  try {
    const publication = await publishJourneyNativeScriptReference(input);
    const archive = createJourneyNativeScriptArchive(directories);
    const block = {
      schemaVersion: "midgard-watcher-native-block-admission-v1" as const,
      blockType: "7",
      protocolMajor: "11",
      blockHash: "11".repeat(32),
      prevHash: "00".repeat(32),
      slot: "300",
      blockNo: "15",
      rawBlockCbor: "80",
      rawHeaderCbor: "80",
      transactionIds: [publication.txHash],
      transactionCbors: [publication.signedCbor],
    };
    await archive.retainNativeBlock(block);
    for (const directory of directories) {
      const files = await readdir(
        join(directory, "native-scripts", publication.scriptHash),
      );
      expect(files).toHaveLength(1);
      const record = JSON.parse(
        await readFile(
          join(directory, "native-scripts", publication.scriptHash, files[0]!),
          "utf8",
        ),
      );
      const body = CML.Transaction.from_cbor_hex(publication.signedCbor).body();
      expect(record.publicationTransactionBodyCbor).toBe(
        body.to_canonical_cbor_hex(),
      );
      expect(record.publicationOutputCbor).toBe(
        body
          .outputs()
          .get(publication.reference.outputIndex)
          .to_canonical_cbor_hex(),
      );
      expect(record.inclusionBlockTransactionIds).toEqual([publication.txHash]);
    }
    await archive.retainNativeBlock({
      ...block,
      blockHash: "22".repeat(32),
      prevHash: block.blockHash,
      blockNo: "16",
      slot: "900",
      transactionIds: [],
      transactionCbors: [],
    });
    await archive.rollbackNativeBlocks({
      kind: "point",
      blockHash: block.blockHash,
      slot: block.slot,
    });
    for (const directory of directories)
      expect(await readdir(join(directory, "canonical"))).toEqual(["15.json"]);
    await archive.rollbackNativeBlocks({ kind: "origin" });
    for (const directory of directories) {
      expect(await readdir(join(directory, "canonical"))).toEqual([]);
      expect(
        await readdir(
          join(directory, "native-scripts", publication.scriptHash),
        ),
      ).toHaveLength(1);
    }
    await archive.retainNativeBlock(block);
    await expect(
      archive.retainNativeBlock({ ...block, rawBlockCbor: "81" }),
    ).rejects.toThrow("immutable authenticated bytes");
    for (const directory of directories)
      await expect(
        readFile(join(directory, "canonical-ready")),
      ).rejects.toMatchObject({ code: "ENOENT" });
  } finally {
    await close();
    for (const directory of directories)
      await rm(directory, { recursive: true, force: true });
  }
});
