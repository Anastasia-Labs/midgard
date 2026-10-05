import { randomUUID } from "node:crypto";
import { writeFileSync } from "node:fs";
import fs from "node:fs/promises";
import { syncBuiltinESMExports } from "node:module";
import { dirname, join } from "node:path";

import { expect, it, vi } from "vitest";

import { sha256 } from "../../src/runtime/trusted-head-authority.exact-record.js";
import {
  auditLegacyWatcherTrustedHeadAuthority,
  importLegacyAuthorityStore,
  openWatcherTrustedHeadAuthorityStore,
  repairLegacyWatcherTrustedHeadAuthorityFinalRecord,
} from "../../src/runtime/trusted-head-authority.js";
import { legacyScene } from "./trusted-head-authority.legacy-fixture.js";
import { directory, head } from "./trusted-head-authority.policy.js";

const scene = async (torn = Buffer.from('{"head":')) => {
  const legacy = await legacyScene(3),
    recoveryDirectory = await directory();
  const recordName = "00000000000000000002.json",
    finalPath = join(legacy.path, recordName);
  await fs.writeFile(finalPath, torn);
  const input = {
    legacyDirectory: legacy.path,
    recoveryDirectory,
    policy: legacy.policy,
    recordAuthenticationKey: legacy.recordAuthenticationKey,
    attemptId: `repair-${randomUUID()}`,
    expectedPriorHead: head(legacy.policy, 1, "77"),
    expectedTornRecordName: recordName,
    expectedTornSha256: sha256(torn),
    reason: "Explicit synthetic torn final publication recovery",
  };
  return { legacy, input, finalPath, torn };
};

it("repairs a torn first publication only with an explicitly expected empty prior prefix", async () => {
  const legacy = await legacyScene(0),
    recoveryDirectory = await directory(),
    torn = Buffer.from("{");
  const expectedTornRecordName = "00000000000000000000.json";
  await fs.writeFile(join(legacy.path, expectedTornRecordName), torn);
  const input = {
    legacyDirectory: legacy.path,
    recoveryDirectory,
    policy: legacy.policy,
    recordAuthenticationKey: legacy.recordAuthenticationKey,
    attemptId: `repair-${randomUUID()}`,
    expectedPriorHead: null,
    expectedTornRecordName,
    expectedTornSha256: sha256(torn),
    reason: "Explicit interrupted first publication",
  };
  await repairLegacyWatcherTrustedHeadAuthorityFinalRecord(input);
  expect(
    (
      await auditLegacyWatcherTrustedHeadAuthority({
        directory: legacy.path,
        policy: legacy.policy,
        recordAuthenticationKey: legacy.recordAuthenticationKey,
        liveRecordLimit: 1,
      })
    ).head,
  ).toBeNull();
  expect(
    await fs.readFile(join(recoveryDirectory, "removed-record.bin")),
  ).toEqual(torn);
});

it.each([Buffer.from('{"head":'), Buffer.alloc(0), Buffer.from([255, 0])])(
  "preserves raw torn bytes and authenticated provenance, then permits strict import of the exact prior head",
  async (torn) => {
    const value = await scene(torn);
    const storage = {
      directory: await directory(),
      policy: value.input.policy,
      recordAuthenticationKey: value.input.recordAuthenticationKey,
      liveRecordLimit: 1,
      generation: `generation-${randomUUID()}`,
      legacyDirectory: value.legacy.path,
    };
    await expect(importLegacyAuthorityStore(storage)).rejects.toThrow();
    await repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input);
    await expect(fs.readFile(value.finalPath)).rejects.toThrow();
    expect(
      await fs.readFile(
        join(value.input.recoveryDirectory, "removed-record.bin"),
      ),
    ).toEqual(torn);
    const intent = JSON.parse(
      await fs.readFile(
        join(value.input.recoveryDirectory, "intent.json"),
        "utf8",
      ),
    );
    expect(intent.payload).toMatchObject({
      priorHead: value.input.expectedPriorHead,
      tornSha256: sha256(torn),
      tornBytesLength: torn.length,
      reason: value.input.reason,
    });
    expect(intent.policyDigest).toBe(value.input.policy.policyDigest);
    await repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input);
    await importLegacyAuthorityStore(storage);
    const store = await openWatcherTrustedHeadAuthorityStore(storage);
    try {
      expect(await store.readCurrent()).toEqual(value.input.expectedPriorHead);
    } finally {
      store.close();
    }
  },
);
it.each([
  "parseable",
  "interior",
  "head",
  "key",
  "digest",
  "future",
  "symlink",
])("refuses unsafe repair %s without removing final bytes", async (mode) => {
  const value = await scene();
  let input = value.input;
  if (mode === "parseable") {
    await fs.writeFile(value.finalPath, "{}");
    input = { ...input, expectedTornSha256: sha256("{}") };
  }
  if (mode === "interior")
    await fs.writeFile(
      join(value.legacy.path, "00000000000000000000.json"),
      "corrupt prior record",
    );
  if (mode === "head")
    input = { ...input, expectedPriorHead: head(input.policy, 1, "88") };
  if (mode === "key")
    input = { ...input, recordAuthenticationKey: new Uint8Array(32).fill(9) };
  if (mode === "digest")
    input = { ...input, expectedTornSha256: "11".repeat(32) };
  if (mode === "future")
    await fs.writeFile(
      join(value.legacy.path, "00000000000000000003.json"),
      "future entry",
    );
  if (mode === "symlink") {
    await fs.rename(value.finalPath, join(input.recoveryDirectory, "external"));
    await fs.symlink(
      join(input.recoveryDirectory, "external"),
      value.finalPath,
    );
  }
  const original = await fs.readFile(value.finalPath);
  await expect(
    repairLegacyWatcherTrustedHeadAuthorityFinalRecord(input),
  ).rejects.toThrow();
  expect(await fs.readFile(value.finalPath)).toEqual(original);
  await expect(
    fs.readFile(join(input.recoveryDirectory, "completed.json")),
  ).rejects.toThrow();
});
it.each(["before", "after"] as const)(
  "resumes the same durable repair intent after %s final removal acknowledgement loss",
  async (phase) => {
    const value = await scene(),
      original = fs.unlink;
    const fault = vi.spyOn(fs, "unlink").mockImplementation(async (path) => {
      if (path.toString() === value.finalPath) {
        if (phase === "after") await original(path);
        throw new Error("synthetic removal acknowledgement loss");
      }
      await original(path);
    });
    syncBuiltinESMExports();
    try {
      await expect(
        repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input),
      ).rejects.toThrow(/acknowledgement loss/);
    } finally {
      fault.mockRestore();
      syncBuiltinESMExports();
    }
    const retained = await fs.readFile(
      join(value.input.recoveryDirectory, "intent.json"),
    );
    await repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input);
    expect(
      await fs.readFile(join(value.input.recoveryDirectory, "intent.json")),
    ).toEqual(retained);
    expect(
      (
        await auditLegacyWatcherTrustedHeadAuthority({
          directory: value.legacy.path,
          policy: value.input.policy,
          recordAuthenticationKey: value.input.recordAuthenticationKey,
          liveRecordLimit: 1,
        })
      ).head,
    ).toEqual(value.input.expectedPriorHead);
  },
);

it("reasserts retained intent directory durability before final removal on retry", async () => {
  const value = await scene(),
    originalLink = fs.link;
  const interrupted = vi
    .spyOn(fs, "link")
    .mockImplementation(async (from, to) => {
      await originalLink(from, to);
      if (to.toString() === join(value.input.recoveryDirectory, "intent.json"))
        throw new Error("synthetic intent publication loss");
    });
  syncBuiltinESMExports();
  try {
    await expect(
      repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input),
    ).rejects.toThrow(/intent publication loss/);
  } finally {
    interrupted.mockRestore();
    syncBuiltinESMExports();
  }
  const originalOpen = fs.open;
  let reasserted = false;
  const fault = vi
    .spyOn(fs, "open")
    .mockImplementation(async (path, flags, mode) => {
      const handle = await originalOpen(path, flags, mode);
      if (path.toString() === value.input.recoveryDirectory) {
        reasserted = true;
        vi.spyOn(handle, "sync").mockRejectedValueOnce(
          new Error("synthetic evidence directory sync loss"),
        );
      }
      return handle;
    });
  syncBuiltinESMExports();
  try {
    await expect(
      repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input),
    ).rejects.toThrow(/directory sync loss/);
  } finally {
    fault.mockRestore();
    syncBuiltinESMExports();
  }
  expect(reasserted).toBe(true);
  expect(await fs.readFile(value.finalPath)).toEqual(value.torn);
  await repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input);
});
it.each(["before", "after"] as const)(
  "resumes after %s completion receipt acknowledgement loss",
  async (phase) => {
    const value = await scene(),
      original = fs.link,
      completed = join(value.input.recoveryDirectory, "completed.json");
    const fault = vi.spyOn(fs, "link").mockImplementation(async (from, to) => {
      if (to.toString() === completed) {
        if (phase === "after") await original(from, to);
        throw new Error("synthetic completion acknowledgement loss");
      }
      await original(from, to);
    });
    syncBuiltinESMExports();
    try {
      await expect(
        repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input),
      ).rejects.toThrow(/acknowledgement loss/);
    } finally {
      fault.mockRestore();
      syncBuiltinESMExports();
    }
    await repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input);
    await expect(fs.readFile(value.finalPath)).rejects.toThrow();
    expect(
      await fs.readFile(
        join(value.input.recoveryDirectory, "removed-record.bin"),
      ),
    ).toEqual(value.torn);
  },
);
it("holds a prefix change after durable intent and before removal", async () => {
  const value = await scene(),
    original = fs.link;
  const fault = vi.spyOn(fs, "link").mockImplementation(async (from, to) => {
    await original(from, to);
    if (to.toString() === join(value.input.recoveryDirectory, "intent.json"))
      writeFileSync(
        join(value.legacy.path, "00000000000000000000.json"),
        "changed prefix",
      );
  });
  syncBuiltinESMExports();
  try {
    await expect(
      repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input),
    ).rejects.toThrow();
  } finally {
    fault.mockRestore();
    syncBuiltinESMExports();
  }
  expect(await fs.readFile(value.finalPath)).toEqual(value.torn);
  await expect(
    fs.readFile(join(value.input.recoveryDirectory, "completed.json")),
  ).rejects.toThrow();
});
it.each(["reason", "intent", "completion", "removed", "reappeared"])(
  "rejects changed completed repair %s",
  async (mode) => {
    const value = await scene();
    await repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input);
    let input = value.input;
    if (mode === "reason") input = { ...input, reason: "different reason" };
    if (mode === "removed")
      await fs.writeFile(
        join(input.recoveryDirectory, "removed-record.bin"),
        "changed retained bytes",
      );
    if (mode === "intent" || mode === "completion")
      await fs.writeFile(
        join(
          input.recoveryDirectory,
          mode === "intent" ? "intent.json" : "completed.json",
        ),
        "{}",
      );
    if (mode === "reappeared") await fs.writeFile(value.finalPath, value.torn);
    await expect(
      repairLegacyWatcherTrustedHeadAuthorityFinalRecord(input),
    ).rejects.toThrow();
    if (mode === "reappeared")
      expect(await fs.readFile(value.finalPath)).toEqual(value.torn);
  },
);
it("binds retained repair evidence to its exact configured recovery directory", async () => {
  const value = await scene();
  await repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input);
  const recoveryDirectory = await directory();
  for (const name of ["removed-record.bin", "intent.json", "completed.json"]) {
    await fs.copyFile(
      join(value.input.recoveryDirectory, name),
      join(recoveryDirectory, name),
    );
  }
  await expect(
    repairLegacyWatcherTrustedHeadAuthorityFinalRecord({
      ...value.input,
      recoveryDirectory,
    }),
  ).rejects.toThrow(/receipt identity/);
});

it("does not skip a new evidence-directory parent sync refusal on matching mkdir retry", async () => {
  const value = await scene();
  value.input.recoveryDirectory = join(
    value.input.recoveryDirectory,
    "new-evidence",
  );
  const originalOpen = fs.open;
  const fault = vi
    .spyOn(fs, "open")
    .mockImplementation(async (path, flags, mode) => {
      const handle = await originalOpen(path, flags, mode);
      if (path.toString() === dirname(value.input.recoveryDirectory))
        vi.spyOn(handle, "sync").mockRejectedValue(
          new Error("synthetic parent sync refusal"),
        );
      return handle;
    });
  syncBuiltinESMExports();
  try {
    for (let attempt = 0; attempt < 2; attempt++)
      await expect(
        repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input),
      ).rejects.toThrow(/parent sync refusal/);
    expect(await fs.readFile(value.finalPath)).toEqual(value.torn);
  } finally {
    fault.mockRestore();
    syncBuiltinESMExports();
  }
  await repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input);
});

it.each(["legacy", "completion"] as const)(
  "reasserts %s namespace durability before acknowledging an already-linked completion receipt",
  async (boundary) => {
    const value = await scene(),
      originalLink = fs.link;
    const interrupted = vi
      .spyOn(fs, "link")
      .mockImplementation(async (from, to) => {
        await originalLink(from, to);
        if (
          to.toString() ===
          join(value.input.recoveryDirectory, "completed.json")
        )
          throw new Error("synthetic completion link acknowledgement loss");
      });
    syncBuiltinESMExports();
    try {
      await expect(
        repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input),
      ).rejects.toThrow(/acknowledgement loss/);
    } finally {
      interrupted.mockRestore();
      syncBuiltinESMExports();
    }
    const originalOpen = fs.open;
    let evidenceSyncs = 0;
    const fault = vi
      .spyOn(fs, "open")
      .mockImplementation(async (path, flags, mode) => {
        const handle = await originalOpen(path, flags, mode);
        if (path.toString() === value.input.recoveryDirectory) evidenceSyncs++;
        if (
          (boundary === "legacy" && path.toString() === value.legacy.path) ||
          (boundary === "completion" &&
            path.toString() === value.input.recoveryDirectory &&
            evidenceSyncs === 3)
        )
          vi.spyOn(handle, "sync").mockRejectedValue(
            new Error("synthetic completed namespace sync refusal"),
          );
        return handle;
      });
    syncBuiltinESMExports();
    try {
      await expect(
        repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input),
      ).rejects.toThrow(/namespace sync refusal/);
      await expect(fs.readFile(value.finalPath)).rejects.toThrow();
      expect(
        await fs.readFile(
          join(value.input.recoveryDirectory, "removed-record.bin"),
        ),
      ).toEqual(value.torn);
    } finally {
      fault.mockRestore();
      syncBuiltinESMExports();
    }
    await repairLegacyWatcherTrustedHeadAuthorityFinalRecord(value.input);
  },
);
