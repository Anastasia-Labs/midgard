import { rm } from "node:fs/promises";

import { CML } from "@lucid-evolution/lucid";

import { watcherDeploymentReleaseFinalityAuthority } from "../../src/runtime/deployment-identity.js";
import { makeWatcherDeploymentAuthorityFixture } from "./deployment-authority-fixture.js";

export const directories: string[] = [];

export const closers: (() => void)[] = [];

export const cleanupFundingRecoveryFixtures = async (): Promise<void> => {
  for (const close of closers.splice(0)) close();
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
};

export const key = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x44));

export const walletAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(key.to_public().hash()),
)
  .to_address()
  .to_bech32();

export const deploymentIdentity =
  makeWatcherDeploymentAuthorityFixture().result;

export const finality =
  watcherDeploymentReleaseFinalityAuthority(deploymentIdentity);

export const sourcesFor = (payloadEnvelopeCbor: Buffer) => [
  {
    sourceId: "libp2p-test",
    fetchPayloadByHeaderHash: async () => ({
      ok: true as const,
      provenance: {
        trustClass: "public_or_permissionless_da" as const,
        sourceId: "libp2p-test/peer-a",
        grade: "security" as const,
      },
      sourceId: "libp2p-test",
      sourcePeerId: "peer-a",
      payloadEnvelopeCbor,
      attempts: [],
    }),
  },
];
