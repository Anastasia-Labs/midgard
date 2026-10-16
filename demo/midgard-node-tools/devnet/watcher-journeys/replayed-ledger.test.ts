import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { promisify } from "node:util";

import { createPublishedWatcherDeploymentAuthority } from "midgard-watcher/tests/support/published-deployment-authority";
import { expect, it } from "vitest";

import { openLocalHistoryEventStage } from "./history-event-local-staging.js";

/** The child intentionally omits source conditions and loads the actual compiled package entries. */
it("admits a root-verified identity through the compiled replayed-ledger binding entry and rejects structural copies", async () => {
  const directory = await mkdtemp(
    join(tmpdir(), "journey-compiled-replay-binding-"),
  );
  const stage = await openLocalHistoryEventStage();
  try {
    const authority = await createPublishedWatcherDeploymentAuthority({
      deployment: stage.deployment,
      directory,
      fundingProfiles: [],
      programCommitments: {
        "computation-thread-policy-v1": createHash("sha256")
          .update(
            JSON.stringify({
              computationThreadPolicyId:
                stage.deployment.contracts.computationThread.policyId,
            }),
          )
          .digest("hex"),
      },
    });
    const script = `
      import assert from "node:assert/strict";
      import { readFile } from "node:fs/promises";
      import { loadWatcherVerifiedDeploymentAuthority } from "midgard-watcher";
      import { verifyWatcherUserEventScriptBinding, readWatcherUserEventScriptBinding } from "midgard-watcher/replayed-ledger";
      assert.ok(import.meta.resolve("midgard-watcher").endsWith("/dist/index.js"));
      assert.ok(import.meta.resolve("midgard-watcher/replayed-ledger").endsWith("/dist/replayed-ledger.js"));
      const [path, ruleBundlePath, blueprintPath] = process.argv.slice(1);
      const authority = await loadWatcherVerifiedDeploymentAuthority({path, ruleBundlePath});
      const deploymentIdentity = authority.deploymentIdentity;
      const blueprintBytes = await readFile(blueprintPath);
      const binding = verifyWatcherUserEventScriptBinding({deploymentIdentity, blueprintBytes});
      const description = readWatcherUserEventScriptBinding({binding, deploymentIdentity});
      assert.equal(description.deploymentFingerprint, deploymentIdentity.manifestId);
      assert.equal(description.blueprintHash, deploymentIdentity.blueprintHash);
      assert.throws(() => verifyWatcherUserEventScriptBinding({deploymentIdentity: {...deploymentIdentity}, blueprintBytes}), /verifiedDeploymentIdentity/);
      assert.throws(() => readWatcherUserEventScriptBinding({binding: {...binding}, deploymentIdentity}), /userEventScriptBinding/);
      process.stdout.write("compiled binding admitted; structural identities and bindings refused");
    `;
    const result = await promisify(execFile)(
      process.execPath,
      [
        "--input-type=module",
        "-e",
        script,
        authority.authorityPath,
        authority.ruleBundlePath,
        authority.blueprintPath,
      ],
      { env: { ...process.env, NODE_OPTIONS: "" }, maxBuffer: 2_000_000 },
    );
    expect(result.stdout).toBe(
      "compiled binding admitted; structural identities and bindings refused",
    );
  } finally {
    stage.close();
    await rm(directory, { recursive: true, force: true });
  }
});
