import { createHash } from "node:crypto";
import { mkdtemp, readFile } from "node:fs/promises";
import { join } from "node:path";

import { createPublishedWatcherDeploymentAuthority } from "midgard-watcher/tests/support/published-deployment-authority";
import { expect, it } from "vitest";

import { loadJourneyContext } from "./live-context.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;

it.skipIf(runDirectory === undefined)(
  "reopens the exact published authority and rejects a changed release",
  async () => {
    const { deployment } = await loadJourneyContext(runDirectory!);
    const directory = await mkdtemp(
      join(runDirectory!, "work/authority-check-"),
    );
    const input = {
      deployment,
      directory,
      fundingProfiles: [],
      programCommitments: {
        "computation-thread-policy-v1": createHash("sha256")
          .update(
            JSON.stringify({
              computationThreadPolicyId:
                deployment.contracts.computationThread.policyId,
            }),
          )
          .digest("hex"),
      },
    };
    const first = await createPublishedWatcherDeploymentAuthority(input);
    const original = await readFile(first.authorityPath, "utf8");
    const reopened = await createPublishedWatcherDeploymentAuthority(input);
    expect(await readFile(reopened.authorityPath, "utf8")).toBe(original);
    expect(reopened.nativeDeployment).toEqual(first.nativeDeployment);
    expect(reopened.deploymentAuthority.deploymentIdentity).toEqual(
      first.deploymentAuthority.deploymentIdentity,
    );
    await expect(
      createPublishedWatcherDeploymentAuthority({
        ...input,
        programCommitments: {
          "computation-thread-policy-v1": "ff".repeat(32),
        },
      }),
    ).rejects.toThrow("Saved watcher authority differs");
    expect(await readFile(first.authorityPath, "utf8")).toBe(original);
  },
);
