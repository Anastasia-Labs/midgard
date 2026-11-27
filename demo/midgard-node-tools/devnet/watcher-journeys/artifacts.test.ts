import { mkdtemp, readdir, readFile, rm, stat } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { expect, it } from "vitest";

import {
  readJourneyArtifact,
  writeJourneyArtifact,
  writeJourneyFile,
} from "./artifacts.js";

it("preserves raw signed CBOR without JSON serialization", async () => {
  const directory = await mkdtemp(join(tmpdir(), "journey-checkpoint-"));
  try {
    const path = join(directory, "signed.cbor");
    const bytes = Uint8Array.from([0x84, 0xa0, 0xa0, 0xf5, 0xf6]);
    await writeJourneyFile(path, bytes);
    expect(await readFile(path)).toEqual(Buffer.from(bytes));
    await writeJourneyFile(path, "84a0a0f5f6\n");
    expect(await readFile(path, "utf8")).toBe("84a0a0f5f6\n");
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});

it("preserves exact signed bytes and typed retained material across checkpoint replacement", async () => {
  const directory = await mkdtemp(join(tmpdir(), "journey-checkpoint-"));
  try {
    const path = join(directory, "signed.json");
    const prepared = {
      signedCbor:
        "84a10081825820000000000000000000000000000000000000000000000000000000000000000000",
      payload: Buffer.from([0, 128, 255]),
      slot: 9_007_199_254_740_993n,
      confirmed: false,
    };
    await writeJourneyArtifact(path, prepared);
    expect(await readJourneyArtifact(path)).toEqual(prepared);
    await writeJourneyArtifact(path, { ...prepared, confirmed: true });
    expect(await readJourneyArtifact(path)).toEqual({
      ...prepared,
      confirmed: true,
    });
    expect((await stat(path)).mode & 0o777).toBe(0o600);
    expect(await readdir(directory)).toEqual(["signed.json"]);
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});

it("keeps the last checkpoint when the next record cannot be serialized", async () => {
  const directory = await mkdtemp(join(tmpdir(), "journey-checkpoint-"));
  try {
    const path = join(directory, "signed.json");
    const saved = { txHash: "a".repeat(64), signedCbor: "84a0a0f5f6" };
    await writeJourneyArtifact(path, saved);
    const circular: { self?: unknown } = {};
    circular.self = circular;
    await expect(writeJourneyArtifact(path, circular)).rejects.toThrow();
    expect(await readJourneyArtifact(path)).toEqual(saved);
    expect(await readdir(directory)).toEqual(["signed.json"]);
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});
