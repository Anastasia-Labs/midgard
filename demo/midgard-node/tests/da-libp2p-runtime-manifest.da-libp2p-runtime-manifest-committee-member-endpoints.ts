import "./da-libp2p-runtime-manifest.da-libp2p-runtime-manifest-profiles.js";

import { describe, expect, it } from "vitest";

import { parseDaProducerPublicationManifest } from "../src/da/libp2p-producer.js";
import { generateDaLibp2pRuntimeManifest } from "../src/da/libp2p-runtime-manifest.js";
import {
  COMMITTEE_KEY,
  DA_VKEY,
  PRODUCER_KEY,
  PUBLIC_RETAINED_DA_KEY,
  writeFinalizedDeploymentInfo,
} from "./da-libp2p-runtime-manifest.write-finalized-deployment-info.js";

describe("DA libp2p runtime manifest committee member endpoints", () => {
  const SECOND_COMMITTEE_KEY = `seed:${"00".repeat(31)}04`;
  const SECOND_DA_VKEY = "33".repeat(32);
  const twoMembers = (
    first: { readonly port: number; readonly host?: string } | undefined,
    second: { readonly port: number; readonly host?: string } | undefined,
  ) => [
    {
      signerIndex: 0,
      daVkey: DA_VKEY,
      libp2pPrivateKeySource: COMMITTEE_KEY,
      roles: ["committee", "coordinator", "retrieval"],
      ...(first === undefined ? {} : { endpoint: first }),
    },
    {
      signerIndex: 1,
      daVkey: SECOND_DA_VKEY,
      libp2pPrivateKeySource: SECOND_COMMITTEE_KEY,
      roles: ["committee", "retrieval"],
      ...(second === undefined ? {} : { endpoint: second }),
    },
  ];
  const generate = async (
    options: Partial<Parameters<typeof generateDaLibp2pRuntimeManifest>[0]>,
  ) => {
    const deploymentInfo = await writeFinalizedDeploymentInfo();
    return generateDaLibp2pRuntimeManifest({
      target: "producer",
      profile: "host",
      contractDeploymentInfoPath: deploymentInfo.path,
      network: "Preprod",
      producerPrivateKeySource: PRODUCER_KEY,
      publicRetainedDaPrivateKeySource: PUBLIC_RETAINED_DA_KEY,
      threshold: 2,
      committeeMembers: twoMembers({ port: 39001 }, { port: 39011 }),
      ...options,
    });
  };
  const tcpPort = (address: string): string =>
    /\/tcp\/(\d+)\//u.exec(`${address}/`)![1]!;

  it("addresses co-hosted members at their own ports", async () => {
    const producer = await generate({});
    const members = producer.da_committee.members;
    expect(members.map(({ multiaddrs }) => tcpPort(multiaddrs[0]!))).toEqual([
      "39001",
      "39011",
    ]);
    expect(
      parseDaProducerPublicationManifest(producer, {
        DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_KEY,
      }).threshold,
    ).toBe(2);

    const second = await generate({ target: "committee", localSignerIndex: 1 });
    const transport = second.da_transport;
    expect(transport.listen_multiaddrs).toEqual(["/ip4/127.0.0.1/tcp/39011"]);
    expect(tcpPort(transport.announce_multiaddrs[0]!)).toBe("39011");
  });

  it("gives compose and public members their own hosts", async () => {
    const manifest = await generate({
      profile: "public",
      producerPublicHost: "producer.example",
      committeePublicHost: "da-0.example",
      committeeMembers: twoMembers(undefined, {
        host: "da-1.example",
        port: 39001,
      }),
    });
    const members = manifest.da_committee.members;
    expect(members[0]!.multiaddrs[0]).toContain(
      "/dns4/da-0.example/tcp/39001/",
    );
    expect(members[1]!.multiaddrs[0]).toContain(
      "/dns4/da-1.example/tcp/39001/",
    );
  });

  it("rejects members that would share one endpoint", async () => {
    await expect(
      generate({ committeeMembers: twoMembers(undefined, undefined) }),
    ).rejects.toThrow(
      "committee member 1 and committee member 0 must not share the libp2p endpoint 127.0.0.1:39001",
    );
    await expect(
      generate({
        committeeMembers: twoMembers({ port: 39001 }, { port: 39002 }),
      }),
    ).rejects.toThrow(
      "committee member 1 and producer must not share the libp2p endpoint 127.0.0.1:39002",
    );
    await expect(
      generate({
        committeeMembers: twoMembers({ port: 39001 }, { port: 39003 }),
      }),
    ).rejects.toThrow(/public retained DA must not share/);
  });

  it("rejects member hosts the profile fixes or cannot publish", async () => {
    await expect(
      generate({
        committeeMembers: twoMembers(
          { port: 39001 },
          { host: "10.0.0.2", port: 39011 },
        ),
      }),
    ).rejects.toThrow("committee member 1 host is fixed by the host profile");
    await expect(
      generate({
        profile: "public",
        producerPublicHost: "producer.example",
        committeePublicHost: "da-0.example",
        committeeMembers: twoMembers(undefined, {
          host: "127.0.0.1",
          port: 39011,
        }),
      }),
    ).rejects.toThrow(/committee member 1 public host must not be local-only/);
  });
});
