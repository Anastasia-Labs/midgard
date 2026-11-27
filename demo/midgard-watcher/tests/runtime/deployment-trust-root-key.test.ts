import { createHash, createPublicKey, generateKeyPairSync } from "node:crypto";
import { mkdtemp, readFile, stat, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { loadOrCreateWatcherTestTrustRootKey } from "../support/published-deployment-authority.js";

describe("watcher test trust-root key", () => {
  it("creates the key once and reloads the identical trust root", async () => {
    const path = join(
      await mkdtemp(join(tmpdir(), "watcher-trust-root-")),
      "deployment-trust-root.pem",
    );
    const created = await loadOrCreateWatcherTestTrustRootKey(path);
    expect(created.privateKey.asymmetricKeyType).toBe("ed25519");
    expect(created.trustRootId).toBe(
      createHash("sha256")
        .update(Buffer.from(created.publicKeySpkiDerHex, "hex"))
        .digest("hex"),
    );
    expect((await stat(path)).mode & 0o777).toBe(0o600);
    const pem = await readFile(path, "utf8");
    const reloaded = await loadOrCreateWatcherTestTrustRootKey(path);
    expect(reloaded.trustRootId).toBe(created.trustRootId);
    expect(reloaded.publicKeySpkiDerHex).toBe(created.publicKeySpkiDerHex);
    expect(
      createPublicKey(reloaded.privateKey)
        .export({ format: "der", type: "spki" })
        .toString("hex"),
    ).toBe(created.publicKeySpkiDerHex);
    expect(await readFile(path, "utf8")).toBe(pem);
  });

  it("refuses a saved key of another algorithm", async () => {
    const path = join(
      await mkdtemp(join(tmpdir(), "watcher-trust-root-")),
      "deployment-trust-root.pem",
    );
    await writeFile(
      path,
      generateKeyPairSync("ec", { namedCurve: "P-256" }).privateKey.export({
        format: "pem",
        type: "pkcs8",
      }),
    );
    await expect(loadOrCreateWatcherTestTrustRootKey(path)).rejects.toThrow(
      "not an ed25519 key",
    );
  });
});
