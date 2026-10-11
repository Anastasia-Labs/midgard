import { existsSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as committeeConfig from "da-committee-node/config";
import { afterEach, describe, expect, it } from "vitest";

import { DEPLOYMENT_PROFILE } from "../src/devnet-stack/build.js";
import { preflightCommitteeConfigs } from "../src/devnet-stack/da.js";
import {
  type Identities,
  loadIdentities,
} from "../src/devnet-stack/identities.js";
import { makeLayout, type RunEnv } from "../src/devnet-stack/layout.js";
import { preflightNodeConfig } from "../src/devnet-stack/node-env.js";

const dirs: string[] = [];
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

/** A generated run with nothing but its identities: no chain, no deployment. */
const generatedRun = (
  change: (identities: Identities) => Identities = (identities) => identities,
) => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-stack-preflight-"));
  dirs.push(dir);
  const layout = makeLayout(dir);
  return {
    layout,
    run: {
      runId: "preflight",
      composeProject: "preflight",
      networkMagic: 424242,
      ogmiosPort: 2337,
      kupoPort: 1442,
      postgresPort: 5432,
      postgresUser: "midgard",
      postgresPassword: "midgard",
      postgresDatabase: "midgard",
      cardanoImage: "cardano-node",
      postgresImage: "postgres",
      portOffset: 0,
    } satisfies RunEnv,
    identities: change(loadIdentities(layout)),
    artifacts: {
      nativeOwnerBinary: join(layout.bin, "architecture-g-owner"),
      nativeOwnerSha256: "00".repeat(32),
      transportBinary: join(layout.bin, "midgard-l1-node-transport"),
    },
  };
};

describe("devnet node and DA configuration preflight", () => {
  it("checks the run's DA configurations before any chain or deployment exists", () => {
    const context = generatedRun();
    preflightCommitteeConfigs(context, committeeConfig);
    // Nothing is written: the committee's directories come with its specs.
    expect(existsSync(context.layout.committeeData(0))).toBe(false);
    expect(existsSync(context.layout.committeeData(1))).toBe(false);
  });

  // The node refuses a profile other than the compiled one, so a clean run of
  // its whole parser needs the profile the controller builds.
  it.skipIf(SELECTED_DEPLOYMENT_PROFILE.name !== DEPLOYMENT_PROFILE)(
    `checks the run's node configuration before any chain or deployment exists (needs ${DEPLOYMENT_PROFILE} compiled)`,
    async () => {
      const context = generatedRun();
      await preflightNodeConfig(context);
      expect(existsSync(context.layout.nodeData)).toBe(false);
    },
  );

  it("refuses a run whose node configuration the node's parser rejects", async () => {
    const context = generatedRun((identities) => ({
      ...identities,
      seeds: { ...identities.seeds, merge: "not a seed phrase" },
    }));
    // The node's own seed-phrase check refuses the merge wallet.
    await expect(preflightNodeConfig(context)).rejects.toThrow(
      "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX is not a valid wallet seed phrase",
    );
  });

  it("refuses a run whose committee configuration a member's parser rejects", () => {
    const context = generatedRun((identities) => ({
      ...identities,
      libp2p: { ...identities.libp2p, committee1: "ab" },
    }));
    // The committee's own key-source check refuses member 1's libp2p seed.
    expect(() => preflightCommitteeConfigs(context, committeeConfig)).toThrow(
      "DA_LIBP2P_PRIVATE_KEY_SOURCE seed must be 32 bytes",
    );
  });

  it("refuses a run whose public retained-DA configuration the reader's parser rejects", () => {
    const context = generatedRun((identities) => ({
      ...identities,
      libp2p: { ...identities.libp2p, retained: "ab" },
    }));
    // The reader's own key-source check refuses its libp2p seed.
    expect(() => preflightCommitteeConfigs(context, committeeConfig)).toThrow(
      "DA_PUBLIC_RETAINED_DA_PRIVATE_KEY_SOURCE seed must be 32 bytes",
    );
  });
});
