/**
 * `midgard-user withdraw` against a deployed emulator scene and a stub node
 * serving the L2 output: the node checks on first contact, the landed order,
 * and a rerun that needs no node.
 */
import { existsSync } from "node:fs";
import { join } from "node:path";

import {
  encodeMidgardAddressText,
  midgardAddressFromText,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { type UserNode } from "../src/node.js";
import {
  printedOutcome,
  SCENE_TIMEOUT_MS,
  setupUserCliHarness,
  stubNode,
  tempDirectory,
  type UserCliHarness,
} from "./support/user-cli.js";

const TEST_TIMEOUT_MS = 180_000;

let harness: UserCliHarness;
let outRefCounter = 0;

/** A fresh L2 output reference per case, so record ids never collide. */
const nextOutRef = () => {
  outRefCounter += 1;
  return `${outRefCounter.toString(16).padStart(2, "0").repeat(32)}#0`;
};

const withdrawArgs = (submissionId: string, l2OutRef: string) => [
  "withdraw",
  "--submission-id",
  submissionId,
  "--l2-out-ref",
  l2OutRef,
  "--l1-address",
  harness.scene.user.address,
  "--node-endpoint",
  "http://127.0.0.1:9/unused",
  "--json",
];

const ownOutput = (address?: string) => ({
  address: address ?? harness.scene.user.address,
  assets: { lovelace: 5_000_000n },
});

beforeAll(async () => {
  harness = await setupUserCliHarness();
}, SCENE_TIMEOUT_MS);

describe("midgard-user withdraw", () => {
  it(
    "lands a withdrawal of a committed output, and a rerun needs no node",
    async () => {
      const root = tempDirectory("withdraw");
      const l2OutRef = nextOutRef();
      const node = stubNode(harness.manifest.manifestId, ownOutput());
      const result = await harness.run(withdrawArgs("w1", l2OutRef), {
        recordDir: root,
        overrides: { connectNode: async () => node },
      });
      expect(result.stderr).not.toContain("midgard-user:");
      expect(result.code).toBe(0);
      const outcome = printedOutcome(result.stdout);
      expect(outcome).toMatchObject({ kind: "Withdrawal", attempts: 1 });
      expect(
        (await harness.scene.lucid.transactionStatus(outcome.admissionTxHash))
          .status,
      ).toBe("confirmed");

      const unreachable: UserNode = {
        deploymentManifestId: async () => {
          throw new Error("the node was contacted");
        },
        utxoByOutRef: async () => {
          throw new Error("the node was contacted");
        },
        txStatus: async () => {
          throw new Error("the node was contacted");
        },
      };
      const rerun = await harness.run(withdrawArgs("w1", l2OutRef), {
        recordDir: root,
        overrides: { connectNode: async () => unreachable },
      });
      expect(rerun.stderr).not.toContain("midgard-user:");
      expect(rerun.code).toBe(0);
      expect(printedOutcome(rerun.stdout).admissionTxHash).toBe(
        outcome.admissionTxHash,
      );
    },
    TEST_TIMEOUT_MS,
  );

  it(
    "lands a withdrawal of a protected-address deposit output (no producing L2 tx)",
    async () => {
      const protectedAddress = encodeMidgardAddressText(
        protectMidgardAddress(
          midgardAddressFromText(harness.scene.user.address),
        ),
      );
      const result = await harness.run(withdrawArgs("w2", nextOutRef()), {
        recordDir: tempDirectory("withdraw-protected"),
        overrides: {
          connectNode: async () =>
            stubNode(
              harness.manifest.manifestId,
              ownOutput(protectedAddress),
              "not_found",
            ),
        },
      });
      expect(result.stderr).not.toContain("midgard-user:");
      expect(result.code).toBe(0);
    },
    TEST_TIMEOUT_MS,
  );

  const refusedBeforeAnyRecord = async (node: UserNode, expected: string) => {
    const root = tempDirectory("withdraw-refused");
    const result = await harness.run(withdrawArgs("refused", nextOutRef()), {
      recordDir: root,
      overrides: { connectNode: async () => node },
    });
    expect(result.code).toBe(1);
    expect(result.stderr).toContain(expected);
    expect(existsSync(join(root, harness.manifest.manifestId))).toBe(false);
  };

  it.each([
    "accepted",
    "pending_commit",
    "awaiting_local_recovery",
    "validating",
    "queued",
    "rejected",
  ])(
    "refuses an output whose producing L2 tx is %s until it commits",
    async (status) => {
      await refusedBeforeAnyRecord(
        stubNode(harness.manifest.manifestId, ownOutput(), status),
        `is ${status}, not committed; wait for the producing tx to commit`,
      );
    },
  );

  it("refuses an output owned by another key", async () => {
    await refusedBeforeAnyRecord(
      stubNode(
        harness.manifest.manifestId,
        ownOutput(harness.scene.deployer.address),
      ),
      "Selected L2 UTxO is owned by",
    );
  });

  it("refuses an output owned by a script", async () => {
    const scriptAddress = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_script(CML.ScriptHash.from_hex("22".repeat(28))),
    )
      .to_address()
      .to_bech32();
    await refusedBeforeAnyRecord(
      stubNode(harness.manifest.manifestId, ownOutput(scriptAddress)),
      "Selected L2 UTxO must be owned by a key credential.",
    );
  });

  it("refuses a node serving another deployment", async () => {
    await refusedBeforeAnyRecord(
      stubNode("ab".repeat(32), ownOutput()),
      `not this manifest's ${harness.manifest.manifestId}`,
    );
  });

  it("refuses an output the node does not have", async () => {
    await refusedBeforeAnyRecord(
      stubNode(harness.manifest.manifestId, undefined),
      "The node has no unspent L2 output",
    );
  });
});
