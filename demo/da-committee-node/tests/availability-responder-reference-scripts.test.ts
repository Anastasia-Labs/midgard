import { referenceScriptAuthUnit } from "@al-ft/midgard-sdk";
import { Emulator, Lucid, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { availabilityResponderReferenceScripts } from "../src/availability/reference-scripts.js";
import type { MidgardDeploymentContract } from "../src/l1/deployment.js";
import { loadDaDeploymentFixture } from "./helpers/deployment-fixture.js";

/**
 * The nine responder roles, written out rather than derived from the module
 * under test: the readiness gate's whole job is to bind each of these names to
 * exactly one deployed, authenticated reference script, so the name list is the
 * contract and must not be re-read from the production role table.
 */
const EXPECTED_ROLES = [
  "availability-challenge minting",
  "availability-challenge spending",
  "availability-challenge bond withdrawal",
  "availability-challenge open withdrawal",
  "availability-challenge settle withdrawal",
  "availability-challenge close withdrawal",
  "availability-challenge timeout withdrawal",
  "state-queue minting",
  "state-queue spending",
] as const;

const fixture = async () => {
  const deployment = await loadDaDeploymentFixture("Preprod");
  const lucid = await Lucid(new Emulator([]), "Preprod");
  const roles: readonly [string, MidgardDeploymentContract][] = [
    ["availability-challenge minting", deployment.availabilityChallenge.mint],
    ["availability-challenge spending", deployment.availabilityChallenge.spend],
    ...Object.entries(deployment.availabilityChallengeYields).map(
      ([arm, contract]): [string, MidgardDeploymentContract] => [
        `availability-challenge ${arm} withdrawal`,
        contract,
      ],
    ),
    ["state-queue minting", deployment.stateQueue.mint],
    ["state-queue spending", deployment.stateQueue.spend],
  ];
  const contractByRole = new Map<string, MidgardDeploymentContract>(roles);
  const utxos: UTxO[] = roles.map(([role, contract]) => {
    if (!contract.refScriptOutRef)
      throw new Error("Fixture role has no deployed reference");
    return {
      ...contract.refScriptOutRef,
      address: "reference-script-address",
      assets: {
        lovelace: 4_000_000n,
        [referenceScriptAuthUnit(deployment.referenceScriptAuthPolicyId, role)]:
          1n,
      },
      scriptRef: contract.script,
    };
  });
  return {
    deployment,
    contractByRole,
    utxos,
    reader: (registered = true) => ({
      config: () => lucid.config(),
      utxosByOutRef: async () => utxos,
      rewardAccountAt: async () => ({ registered, rewards: 0n, poolId: null }),
    }),
  };
};

describe("availability responder reference readiness", () => {
  it("resolves every named role to its own deployed, authenticated reference", async () => {
    const { deployment, contractByRole, reader } = await fixture();

    const resolved = await availabilityResponderReferenceScripts(
      reader(),
      deployment,
    );

    // Exactly these nine names, no extras and no duplicates collapsing two
    // roles onto one key.
    expect(Object.keys(resolved).sort()).toEqual([...EXPECTED_ROLES].sort());
    for (const role of EXPECTED_ROLES) {
      const contract = contractByRole.get(role);
      expect(contract, `no fixture contract for ${role}`).toBeDefined();
      const utxo = resolved[role]!;
      // The outref, the script bytes and the role-scoped auth token must all
      // belong to *this* role: a permuted or duplicated mapping fails here.
      expect({
        txHash: utxo.txHash,
        outputIndex: utxo.outputIndex,
        scriptRef: utxo.scriptRef,
        authToken:
          utxo.assets[
            referenceScriptAuthUnit(
              deployment.referenceScriptAuthPolicyId,
              role,
            )
          ],
      }).toEqual({
        txHash: contract!.refScriptOutRef!.txHash,
        outputIndex: contract!.refScriptOutRef!.outputIndex,
        scriptRef: contract!.script,
        authToken: 1n,
      });
    }
  });

  it("refuses a missing role NFT and an unregistered withdrawal", async () => {
    const { deployment, reader, utxos } = await fixture();
    await expect(
      availabilityResponderReferenceScripts(reader(false), deployment),
    ).rejects.toThrow(/Unregistered availability-challenge bond withdrawal/);
    utxos[0] = { ...utxos[0]!, assets: { lovelace: 4_000_000n } };
    await expect(
      availabilityResponderReferenceScripts(reader(), deployment),
    ).rejects.toThrow(/unauthenticated availability-challenge minting/);
  });

  it("refuses a role whose auth NFT belongs to a different role", async () => {
    const { deployment, reader, utxos } = await fixture();
    // Keep everything else valid: swap only the role-scoped auth tokens of the
    // two state-queue references, so both UTxOs still carry exactly one
    // reference-script auth NFT of the right policy.
    const mintUnit = referenceScriptAuthUnit(
      deployment.referenceScriptAuthPolicyId,
      "state-queue minting",
    );
    const spendUnit = referenceScriptAuthUnit(
      deployment.referenceScriptAuthPolicyId,
      "state-queue spending",
    );
    utxos[7] = {
      ...utxos[7]!,
      assets: { lovelace: 4_000_000n, [spendUnit]: 1n },
    };
    utxos[8] = {
      ...utxos[8]!,
      assets: { lovelace: 4_000_000n, [mintUnit]: 1n },
    };

    await expect(
      availabilityResponderReferenceScripts(reader(), deployment),
    ).rejects.toThrow(/unauthenticated state-queue minting/);
  });

  it("refuses a reference carrying another role's script bytes", async () => {
    const { deployment, reader, utxos } = await fixture();
    // Only the script payload is wrong; the outref and the auth NFT still name
    // the availability-challenge spending role.
    utxos[1] = {
      ...utxos[1]!,
      scriptRef: deployment.stateQueue.spend.script,
    };

    await expect(
      availabilityResponderReferenceScripts(reader(), deployment),
    ).rejects.toThrow(/unauthenticated availability-challenge spending/);
  });
});
